//===- llvm-link.cpp - Low-level LLVM linker ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This utility may be invoked in the following manner:
//  llvm-link a.bc b.bc c.bc -o x.bc
//
//===----------------------------------------------------------------------===//

#include "LibraryLinker.h"
#include "llvm/Linker.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/ToolOutputFile.h"
#include <memory>
using namespace llvm;

enum LibraryLinkage { Dynamic, Static };

static cl::list<LibraryLinkage>
LinkDynamicLibraries("B", cl::Prefix, cl::ZeroOrMore, cl::ValueRequired,
                     cl::desc("Control library linkage"),
                     cl::values(
                      clEnumValN(Dynamic, "dynamic", "Link against shared libraries"),
                      clEnumValN(Static,  "static",  "Do not link against shared libraries"),
                      clEnumValEnd));

static cl::list<std::string>
InputFilenames(cl::Positional, cl::OneOrMore,
               cl::desc("<input bitcode files>"));

static cl::list<std::string>
LibrarySearchPaths("L", cl::Prefix, cl::ZeroOrMore, 
                   cl::desc("Library search paths"),
                   cl::value_desc("dir"));

static cl::alias
LibrarySearchPathsA("-library-path", cl::desc("Alias for -L"),
                    cl::value_desc("dir"), cl::aliasopt(LibrarySearchPaths));

static cl::list<std::string>
Libraries("l", cl::Prefix, cl::ZeroOrMore,
          cl::desc("Libraries"), cl::value_desc("library"));

static cl::alias
LibrariesA("-library", cl::desc("Alias for -l"), cl::value_desc("library"),
           cl::aliasopt(Libraries));

static cl::opt<std::string>
OutputFilename("o", cl::desc("Override output filename"), cl::init("-"),
               cl::value_desc("filename"));

static cl::opt<bool>
Force("f", cl::desc("Enable binary output on terminals"));

static cl::opt<bool>
OutputAssembly("S",
         cl::desc("Write output as LLVM assembly"), cl::Hidden);

static cl::opt<bool>
Verbose("v", cl::desc("Print information about actions taken"));

static cl::opt<bool>
DumpAsm("d", cl::desc("Print assembly as linked"), cl::Hidden);

static cl::opt<bool>
NoStdLib("nostdlib",
         cl::desc("Only search directories specified on the command line."));


int main(int argc, char **argv) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);
  
  LLVMContext &Context = getGlobalContext();
  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.
  cl::ParseCommandLineOptions(argc, argv, "llvm linker\n");

  std::string Modname;
  if (OutputFilename == "-")
    Modname = "<stdout>";
  else {
    sys::Path P(OutputFilename);
    P.eraseSuffix();
    Modname = P.str();
  }


  // Craft a new linker and add in search paths
  LibraryLinker L(argv[0], Modname, Context);
  L.addPaths(LibrarySearchPaths);
  if (!NoStdLib) {
    L.addSystemPaths();
  }

  if (Verbose) {
    L.setFlags(LibraryLinker::Verbose);
  }

  // Link in modules, archives, and libraries
  std::vector<std::string>::const_iterator FileIt = InputFilenames.begin();
  std::vector<std::string>::const_iterator LibIt = Libraries.begin();
  std::vector<LibraryLinkage>::const_iterator LDLIt = LinkDynamicLibraries.begin();
  bool OnlyStatic = false;

  while (true) {
    unsigned int LibPos = -1, FilePos = -1, LDLPos = -1;
    if (LibIt != Libraries.end())
      LibPos = Libraries.getPosition(LibIt - Libraries.begin());
    if (FileIt != InputFilenames.end())
      FilePos = InputFilenames.getPosition(FileIt - InputFilenames.begin());
    if (LDLIt != LinkDynamicLibraries.end())
      LDLPos = LinkDynamicLibraries.getPosition(LDLIt - LinkDynamicLibraries.begin());
    
    // If LDLPos is less than both FilePos and LibPos, update
    // OnlyStatic.
    if (LDLPos < FilePos && LDLPos < LibPos)
    {
      OnlyStatic = *LDLIt++ == Static;
      continue;
    }

    // Otherwise, FilePos or LibPos is smaller (or all are -1).
    if (FilePos < LibPos) {
      // Link in a module or archive
      const std::string &FileName = *FileIt++;
      sys::Path P;
      if (!P.set(FileName)) {
        errs() << argv[0] << ": invalid file name: '" << FileName << "'\n";
        return 1;
      }

      bool IsNative;
      if (P.isArchive()) {
        // Link the archive in if it will resolve symbols
        if (L.linkInArchive(P, IsNative))
        {
          errs() << argv[0] << ": error linking archive: '" << FileName << "'\n";
          return 1;
        }
      }
      else if (P.isBitcodeFile()) {
        // Link the bitcode file in
        if (L.linkInFile(P, IsNative)) {
          errs() << argv[0] << ": error linking bitcode file: '" << FileName << "'\n";
          return 1;
        }
      }
      else {
        // Not an archive nor bitcode so attempt to parse it as LLVM
        // assembly.
        SMDiagnostic Err;
        std::auto_ptr<Module> M(ParseIRFile(P.str(), Err, Context));
        if (M.get() == 0) {
          errs() << argv[0] << ": error parsing LLVM assembly file: '" << FileName << "'\n";
          return 1;
        }
        std::string ErrMessage;
        if (L.linkInModule(M.get(), &ErrMessage)) {
          errs() << argv[0] << ": link error in '" << FileName
                 << "': " << ErrMessage << "\n";
          return 1;
        }
      }
      continue;
    }

    if (LibPos < FilePos) {
      // Link in library or archive
      const std::string &LibName = *LibIt++;
      sys::Path P = L.FindLibrary(LibName, OnlyStatic);
      if (P.isEmpty()) {
        errs() << argv[0] << ": library not found for: '-l" << LibName << "'\n";
        return 1;
      }

      bool IsNative;
      if (P.isArchive()) {
        if (L.linkInArchive(P, IsNative))
        {
          errs() << argv[0] << ": error linking archive: '" << P.str() << "'\n";
          return 1;
        }
      }
      else {
        // If this is not an archive, then it is a dynamic library and
        // the linker is responsible for linking it in. Ignore it.
        assert(P.isDynamicLibrary() || P.isBitcodeFile());
        if (Verbose)
          errs() << "Not linking in dynamic library '" << P.str() << "'\n";
      }
      continue;
    }
    // All done
    assert(LDLPos == (unsigned)-1 && FilePos == (unsigned)-1 && LibPos == (unsigned)-1);
    break;
  }

  Module &Composite = *L.getModule();
  if (DumpAsm) errs() << "Here's the assembly:\n" << Composite;

  std::string ErrorInfo;
  tool_output_file Out(OutputFilename.c_str(), ErrorInfo,
                       raw_fd_ostream::F_Binary);
  if (!ErrorInfo.empty()) {
    errs() << ErrorInfo << '\n';
    return 1;
  }

  if (verifyModule(Composite)) {
    errs() << argv[0] << ": linked module is broken!\n";
    return 1;
  }

  if (Verbose) errs() << "Writing bitcode...\n";
  if (OutputAssembly) {
    Out.os() << Composite;
  } else if (Force || !CheckBitcodeOutputToConsole(Out.os(), true))
    WriteBitcodeToFile(&Composite, Out.os());

  // Declare success.
  Out.keep();

  return 0;
}
