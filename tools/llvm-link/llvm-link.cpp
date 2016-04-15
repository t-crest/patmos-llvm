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
#include "llvm/Linker/Linker.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileUtilities.h"
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

static cl::list<std::string> OverridingInputs(
    "override", cl::ZeroOrMore, cl::value_desc("filename"),
    cl::desc(
        "input bitcode file which can override previously defined symbol(s)"));

// Option to simulate function importing for testing. This enables using
// llvm-link to simulate ThinLTO backend processes.
static cl::list<std::string> Imports(
    "import", cl::ZeroOrMore, cl::value_desc("function:filename"),
    cl::desc("Pair of function name and filename, where function should be "
             "imported from bitcode in filename"));

// Option to support testing of function importing. The function index
// must be specified in the case were we request imports via the -import
// option, as well as when compiling any module with functions that may be
// exported (imported by a different llvm-link -import invocation), to ensure
// consistent promotion and renaming of locals.
static cl::opt<std::string> FunctionIndex("functionindex",
                                          cl::desc("Function index filename"),
                                          cl::init(""),
                                          cl::value_desc("filename"));

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
Internalize("internalize", cl::desc("Internalize linked symbols"));

static cl::opt<bool>
OnlyNeeded("only-needed", cl::desc("Link only needed symbols"));

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
SuppressWarnings("suppress-warnings", cl::desc("Suppress all linking warnings"),
                 cl::init(false));

static cl::opt<bool>
    PreserveModules("preserve-modules",
                    cl::desc("Preserve linked modules for testing"));

static cl::opt<bool> PreserveBitcodeUseListOrder(
    "preserve-bc-uselistorder",
    cl::desc("Preserve use-list order when writing LLVM bitcode."),
    cl::init(true), cl::Hidden);

static cl::opt<bool> PreserveAssemblyUseListOrder(
    "preserve-ll-uselistorder",
    cl::desc("Preserve use-list order when writing LLVM assembly."),
    cl::init(false), cl::Hidden);

static cl::opt<bool>
NoStdLib("nostdlib",
         cl::desc("Only search directories specified on the command line."));

static bool isFileType(const std::string &FileName, sys::fs::file_magic type)
{
  sys::fs::file_magic result;
  if (sys::fs::identify_magic(FileName, result)) {
    return false;
  }
  return result == type;
}

static void diagnosticHandler(const DiagnosticInfo &DI) {
  unsigned Severity = DI.getSeverity();
  switch (Severity) {
  case DS_Error:
    errs() << "ERROR: ";
    break;
  case DS_Warning:
    if (SuppressWarnings)
      return;
    errs() << "WARNING: ";
    break;
  case DS_Remark:
  case DS_Note:
    llvm_unreachable("Only expecting warnings and errors");
  }

  DiagnosticPrinterRawOStream DP(errs());
  DI.print(DP);
  errs() << '\n';
}

static void diagnosticHandlerWithContext(const DiagnosticInfo &DI, void *C) {
  diagnosticHandler(DI);
}

int main(int argc, char **argv) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);

  LLVMContext &Context = getGlobalContext();
  Context.setDiagnosticHandler(diagnosticHandlerWithContext, nullptr, true);

  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.
  cl::ParseCommandLineOptions(argc, argv, "llvm linker\n");

  std::string Modname;
  if (OutputFilename == "-")
    Modname = "<stdout>";
  else {
    Modname = llvm::sys::path::stem(OutputFilename);
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

  unsigned Flags = Linker::Flags::None;
  if (Internalize)
    Flags |= Linker::Flags::InternalizeLinkedSymbols;
  if (OnlyNeeded)
    Flags |= Linker::Flags::LinkOnlyNeeded;

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
      if (!sys::fs::exists(FileName)) {
        errs() << argv[0] << ": invalid file name: '" << FileName << "'\n";
        return 1;
      }

      bool IsNative;
      if (isFileType(FileName, sys::fs::file_magic::archive)) {
        // Link the archive in if it will resolve symbols
        if (L.linkInArchive(FileName, IsNative))
        {
          errs() << argv[0] << ": error linking archive: '" << FileName << "'\n";
          return 1;
        }
      }
      else if (isFileType(FileName, sys::fs::file_magic::bitcode)) {
        // Link the bitcode file in
        if (L.linkInFile(FileName, IsNative, Flags)) {
          errs() << argv[0] << ": error linking bitcode file: '" << FileName << "'\n";
          return 1;
        }
      }
      else {
        // Not an archive nor bitcode so attempt to parse it as LLVM
        // assembly.
        SMDiagnostic Err;
        std::unique_ptr<Module> M(parseIRFile(FileName, Err, Context));
        if (M.get() == 0) {
          errs() << argv[0] << ": error parsing LLVM assembly file: '" << FileName << "'\n";
          return 1;
        }
        if (L.linkInModule(std::move(M), Flags)) {
          errs() << argv[0] << ": link error in '" << FileName << "\n";
          return 1;
        }
      }
      continue;
    }

    if (LibPos < FilePos) {
      // Link in library or archive
      const std::string &LibName = *LibIt++;
      std::string FileName = L.FindLibrary(LibName, OnlyStatic);
      if (FileName.empty()) {
        errs() << argv[0] << ": library not found for: '-l" << LibName << "'\n";
        return 1;
      }

      bool IsNative;
      if (isFileType(FileName, sys::fs::file_magic::archive)) {
        if (L.linkInArchive(FileName, IsNative))
        {
          errs() << argv[0] << ": error linking archive: '" << FileName << "'\n";
          return 1;
        }
      }
      else {
        // If this is not an archive, then it is a dynamic library and
        // the linker is responsible for linking it in. Ignore it.
        if (Verbose)
          errs() << "Not linking in dynamic library '" << FileName << "'\n";
      }
      continue;
    }
    // All done
    assert(LDLPos == (unsigned)-1 && FilePos == (unsigned)-1 && LibPos == (unsigned)-1);
    break;
  }

  std::vector<std::string>::const_iterator OverrideIt = OverridingInputs.begin();
  while (OverrideIt != OverridingInputs.end()) {
    const std::string &FileName = *OverrideIt++;
    if (!sys::fs::exists(FileName)) {
      errs() << argv[0] << ": invalid file name: '" << FileName << "'\n";
      return 1;
    }

    bool IsNative;
    if (isFileType(FileName, sys::fs::file_magic::bitcode)) {
      // Link the bitcode file in
      if (L.linkInFile(FileName, IsNative, Flags | Linker::Flags::OverrideFromSrc)) {
        errs() << argv[0] << ": error linking bitcode file: '" << FileName << "'\n";
        return 1;
      }
    }
    else {
      // Not an archive nor bitcode so attempt to parse it as LLVM
      // assembly.
      SMDiagnostic Err;
      std::unique_ptr<Module> M(parseIRFile(FileName, Err, Context));
      if (M.get() == 0) {
        errs() << argv[0] << ": error parsing LLVM assembly file: '" << FileName << "'\n";
        return 1;
      }
      if (L.linkInModule(std::move(M), Flags | Linker::Flags::OverrideFromSrc)) {
        errs() << argv[0] << ": link error in '" << FileName << "\n";
        return 1;
      }
    }
  }

  Module &Composite = *L.getModule();
  if (DumpAsm) errs() << "Here's the assembly:\n" << Composite;

  std::error_code EC;
  tool_output_file Out(OutputFilename, EC, sys::fs::F_None);
  if (EC) {
    errs() << EC.message() << '\n';
    return 1;
  }

  if (verifyModule(Composite, &errs())) {
    errs() << argv[0] << ": linked module is broken!\n";
    return 1;
  }

  if (Verbose) errs() << "Writing bitcode...\n";
  if (OutputAssembly) {
    Composite.print(Out.os(), nullptr, PreserveAssemblyUseListOrder);
  } else if (Force || !CheckBitcodeOutputToConsole(Out.os(), true))
    WriteBitcodeToFile(&Composite, Out.os(), PreserveBitcodeUseListOrder);

  // Declare success.
  Out.keep();

  return 0;
}
