//===- lib/Linker/Linker.cpp - Basic Linker functionality  ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains basic Linker functionality that all usages will need.
//
//===----------------------------------------------------------------------===//

#include "LibraryLinker.h"
#include "llvm/Linker.h"
#include "llvm/IR/Module.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Config/config.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/system_error.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/Bitcode/Archive.h"
#include <memory>
#include <set>
using namespace llvm;

// The new path "interface" is truly ugly crap.. so just going with it and
// making this code ugly as well with lots of copy-pasting and local functions..
static sys::fs::file_magic getFileType(const std::string &FileName)
{
  sys::fs::file_magic result;
  if (sys::fs::identify_magic(FileName, result) != error_code::success()) {
    return sys::fs::file_magic::unknown;
  }
  return result;
}

static bool isFileType(const std::string &FileName, sys::fs::file_magic type)
{
  sys::fs::file_magic result;
  if (sys::fs::identify_magic(FileName, result) != error_code::success()) {
    return false;
  }
  return result == type;
}

static bool isArchive(const std::string &FileName) {
  return isFileType(FileName, sys::fs::file_magic::archive);
}

static bool isBitcodeFile(const std::string &FileName) {
  return isFileType(FileName, sys::fs::file_magic::bitcode);
}

static bool isDynamicLibrary(const std::string &FileName) {
  return isFileType(FileName, sys::fs::file_magic::elf_shared_object) ||
         isFileType(FileName, sys::fs::file_magic::macho_dynamic_linker) ||
         isFileType(FileName, sys::fs::file_magic::macho_dynamically_linked_shared_lib) ||
         isFileType(FileName, sys::fs::file_magic::macho_dynamically_linked_shared_lib_stub);
}

LibraryLinker::LibraryLinker(StringRef progname, StringRef modname,
               LLVMContext& C, unsigned flags):
  Linker(new Module(modname, C)),
  Context(C),
  LibPaths(),
  Flags(flags),
  Error(),
  ProgramName(progname) { }

LibraryLinker::LibraryLinker(StringRef progname, Module* aModule,
    unsigned flags) :
  Linker(aModule),
  Context(aModule->getContext()),
  LibPaths(),
  Flags(flags),
  Error(),
  ProgramName(progname) { }

LibraryLinker::~LibraryLinker() {
}

bool
LibraryLinker::error(StringRef message) {
  Error = message;
  if (!(Flags&QuietErrors))
    errs() << ProgramName << ": error: " << message << "\n";
  return true;
}

bool
LibraryLinker::warning(StringRef message) {
  Error = message;
  if (!(Flags&QuietWarnings))
    errs() << ProgramName << ": warning: " << message << "\n";
  return false;
}

void
LibraryLinker::verbose(StringRef message) {
  if (Flags&Verbose)
    errs() << "  " << message << "\n";
}

void
LibraryLinker::addPath(const std::string& path) {
  LibPaths.push_back(path);
}

void
LibraryLinker::addPaths(const std::vector<std::string>& paths) {
  for (unsigned i = 0, e = paths.size(); i != e; ++i)
    LibPaths.push_back(paths[i]);
}

void
LibraryLinker::addSystemPaths() {
  LibPaths.insert(LibPaths.begin(),std::string("./"));
}

// LoadObject - Read in and parse the bitcode file named by FN and return the
// module it contains (wrapped in an auto_ptr), or auto_ptr<Module>() and set
// Error if an error occurs.
std::auto_ptr<Module>
LibraryLinker::LoadObject(const std::string &FN) {
  std::string ParseErrorMessage;
  Module *Result = 0;

  OwningPtr<MemoryBuffer> Buffer;
  if (error_code ec = MemoryBuffer::getFileOrSTDIN(FN.c_str(), Buffer))
    ParseErrorMessage = "Error reading file '" + FN + "'" + ": "
                      + ec.message();
  else
    Result = ParseBitcodeFile(Buffer.get(), Context, &ParseErrorMessage);

  if (Result)
    return std::auto_ptr<Module>(Result);
  Error = "Bitcode file '" + FN + "' could not be loaded";
  if (ParseErrorMessage.size())
    Error += ": " + ParseErrorMessage;
  return std::auto_ptr<Module>();
}

// IsLibrary - Determine if "Name" is a library in "Directory". Return
// a non-empty sys::Path if its found, an empty one otherwise.
// If OnlyStatic is true, do not check for dynamic library extensions.
static inline std::string IsLibrary(StringRef Name,
                                  const std::string &Directory,
                                  bool OnlyStatic) {

  // Just why???
  SmallString<1024> FullPath(Directory);

  // Try the libX.a form
  sys::path::append(FullPath, "lib" + Name + ".a");
  if (isArchive(FullPath.str()))
    return FullPath.str();

  // Try the libX.bca form
  sys::path::replace_extension(FullPath, ".bca");
  if (isArchive(FullPath.str()))
    return FullPath.str();

  if (!OnlyStatic) {
    // Try the libX.so (or .dylib) form
    sys::path::replace_extension(FullPath, LTDL_SHLIB_EXT);
    if (isDynamicLibrary(FullPath.str()))  // Native shared library?
      return FullPath.str();
    if (isBitcodeFile(FullPath.str()))    // .so file containing bitcode?
      return FullPath.str();

    // Try libX form, to make it possible to add dependency on the
    // specific version of .so, like liblzma.so.1.0.0
    sys::path::replace_extension(FullPath, "");
    if (isDynamicLibrary(FullPath.str()))  // Native shared library?
      return FullPath.str();
    if (isBitcodeFile(FullPath.str()))    // .so file containing bitcode?
      return FullPath.str();
  }

  // Not found .. fall through

  // Indicate that the library was not found in the directory.
  return "";
}

/// FindLib - Try to convert Filename into the name of a file that we can open,
/// if it does not already name a file we can open, by first trying to open
/// Filename, then libFilename.[suffix] for each of a set of several common
/// library suffixes, in each of the directories in LibPaths. Returns an empty
/// Path if no matching file can be found.
///
std::string
LibraryLinker::FindLibrary(StringRef Filename, bool OnlyStatic) {
  // Determine if the pathname can be found as it stands.
  std::string FilePath(Filename);
  if (sys::fs::exists(FilePath) &&
      (isArchive(FilePath) || isDynamicLibrary(FilePath)))
    return FilePath;

  // Iterate over the directories in Paths to see if we can find the library
  // there.
  for (unsigned Index = 0; Index != LibPaths.size(); ++Index) {
    std::string Directory(LibPaths[Index]);
    std::string FullPath = IsLibrary(Filename, Directory, OnlyStatic);
    if (!FullPath.empty())
      return FullPath;
  }
  return "";
}

/// GetAllUndefinedSymbols - calculates the set of undefined symbols that still
/// exist in an LLVM module. This is a bit tricky because there may be two
/// symbols with the same name but different LLVM types that will be resolved to
/// each other but aren't currently (thus we need to treat it as resolved).
///
/// Inputs:
///  M - The module in which to find undefined symbols.
///
/// Outputs:
///  UndefinedSymbols - A set of C++ strings containing the name of all
///                     undefined symbols.
///
static void
GetAllUndefinedSymbols(Module *M, std::set<std::string> &UndefinedSymbols) {
  std::set<std::string> DefinedSymbols;
  UndefinedSymbols.clear();

  // If the program doesn't define a main, try pulling one in from a .a file.
  // This is needed for programs where the main function is defined in an
  // archive, such f2c'd programs.
  Function *Main = M->getFunction("main");
  if (Main == 0 || Main->isDeclaration())
    UndefinedSymbols.insert("main");

  for (Module::iterator I = M->begin(), E = M->end(); I != E; ++I)
    if (I->hasName()) {
      if (I->isDeclaration())
        UndefinedSymbols.insert(I->getName());
      else if (!I->hasLocalLinkage()) {
        assert(!I->hasDLLImportLinkage()
               && "Found dllimported non-external symbol!");
        DefinedSymbols.insert(I->getName());
      }      
    }

  for (Module::global_iterator I = M->global_begin(), E = M->global_end();
       I != E; ++I)
    if (I->hasName()) {
      if (I->isDeclaration())
        UndefinedSymbols.insert(I->getName());
      else if (!I->hasLocalLinkage()) {
        assert(!I->hasDLLImportLinkage()
               && "Found dllimported non-external symbol!");
        DefinedSymbols.insert(I->getName());
      }      
    }

  for (Module::alias_iterator I = M->alias_begin(), E = M->alias_end();
       I != E; ++I)
    if (I->hasName())
      DefinedSymbols.insert(I->getName());

  // Prune out any defined symbols from the undefined symbols set...
  for (std::set<std::string>::iterator I = UndefinedSymbols.begin();
       I != UndefinedSymbols.end(); )
    if (DefinedSymbols.count(*I))
      UndefinedSymbols.erase(I++);  // This symbol really is defined!
    else
      ++I; // Keep this symbol in the undefined symbols list
}

/// LinkInArchive - opens an archive library and link in all objects which
/// provide symbols that are currently undefined.
///
/// Inputs:
///  Filename - The pathname of the archive.
///
/// Return Value:
///  TRUE  - An error occurred.
///  FALSE - No errors.
bool
LibraryLinker::linkInArchive(const std::string &Filename, bool &is_native) {
  // Make sure this is an archive file we're dealing with
  if (!isArchive(Filename))
    return error("File '" + Filename + "' is not an archive.");

  // Open the archive file
  verbose("Linking archive file '" + Filename + "'");

  // Find all of the symbols currently undefined in the bitcode program.
  // If all the symbols are defined, the program is complete, and there is
  // no reason to link in any archive files.
  std::set<std::string> UndefinedSymbols;
  GetAllUndefinedSymbols(getModule(), UndefinedSymbols);

  if (UndefinedSymbols.empty()) {
    verbose("No symbols undefined, skipping library '" + Filename + "'");
    return false;  // No need to link anything in!
  }

  std::string ErrMsg;
  std::auto_ptr<Archive> AutoArch (
    Archive::OpenAndLoadSymbols(Filename, Context, &ErrMsg));

  Archive* arch = AutoArch.get();

  if (!arch)
    return error("Cannot read archive '" + Filename +
                 "': " + ErrMsg);
  if (!arch->isBitcodeArchive()) {
    is_native = true;
    return false;
  }
  is_native = false;

  // Save a set of symbols that are not defined by the archive. Since we're
  // entering a loop, there's no point searching for these multiple times. This
  // variable is used to "set_subtract" from the set of undefined symbols.
  std::set<std::string> NotDefinedByArchive;

  // Save the current set of undefined symbols, because we may have to make
  // multiple passes over the archive:
  std::set<std::string> CurrentlyUndefinedSymbols;

  do {
    CurrentlyUndefinedSymbols = UndefinedSymbols;

    // Find the modules we need to link into the target module.  Note that arch
    // keeps ownership of these modules and may return the same Module* from a
    // subsequent call.
    SmallVector<Module*, 16> Modules;
    if (!arch->findModulesDefiningSymbols(UndefinedSymbols, Modules, &ErrMsg))
      return error("Cannot find symbols in '" + Filename +
                   "': " + ErrMsg);

    // If we didn't find any more modules to link this time, we are done
    // searching this archive.
    if (Modules.empty())
      break;

    // Any symbols remaining in UndefinedSymbols after
    // findModulesDefiningSymbols are ones that the archive does not define. So
    // we add them to the NotDefinedByArchive variable now.
    NotDefinedByArchive.insert(UndefinedSymbols.begin(),
        UndefinedSymbols.end());

    // Loop over all the Modules that we got back from the archive
    for (SmallVectorImpl<Module*>::iterator I=Modules.begin(), E=Modules.end();
         I != E; ++I) {

      // Get the module we must link in.
      std::string moduleErrorMsg;
      Module* aModule = *I;
      if (aModule != NULL) {
        if (aModule->MaterializeAll(&moduleErrorMsg))
          return error("Could not load a module: " + moduleErrorMsg);

        verbose("  Linking in module: " + aModule->getModuleIdentifier());

        // Link it in
        if (linkInModule(aModule, &moduleErrorMsg))
          return error("Cannot link in module '" +
                       aModule->getModuleIdentifier() + "': " + moduleErrorMsg);
      } 
    }
    
    // Get the undefined symbols from the aggregate module. This recomputes the
    // symbols we still need after the new modules have been linked in.
    GetAllUndefinedSymbols(getModule(), UndefinedSymbols);

    // At this point we have two sets of undefined symbols: UndefinedSymbols
    // which holds the undefined symbols from all the modules, and
    // NotDefinedByArchive which holds symbols we know the archive doesn't
    // define. There's no point searching for symbols that we won't find in the
    // archive so we subtract these sets.
    set_subtract(UndefinedSymbols, NotDefinedByArchive);

    // If there's no symbols left, no point in continuing to search the
    // archive.
    if (UndefinedSymbols.empty())
      break;
  } while (CurrentlyUndefinedSymbols != UndefinedSymbols);

  return false;
}

/// LinkInLibrary - links one library into the HeadModule.
///
bool LibraryLinker::linkInLibrary(StringRef Lib, bool& is_native) {
  is_native = false;
  // Determine where this library lives.
  std::string Pathname = FindLibrary(Lib);
  if (Pathname.empty())
    return error("Cannot find library '" + Lib.str() + "'");

  // If its an archive, try to link it in
  switch (getFileType(Pathname)) {
    default: llvm_unreachable("Bad file type identification");
    case sys::fs::file_magic::unknown:
      return warning("Supposed library '" + Lib.str() + "' isn't a library.");

    case sys::fs::file_magic::bitcode:
      // LLVM ".so" file.
      if (linkInFile(Pathname, is_native))
        return true;
      break;

    case sys::fs::file_magic::archive:
      if (linkInArchive(Pathname, is_native))
        return error("Cannot link archive '" + Pathname + "'");
      break;

    case sys::fs::file_magic::elf_relocatable:
    case sys::fs::file_magic::elf_shared_object:
    case sys::fs::file_magic::macho_object:
    case sys::fs::file_magic::macho_fixed_virtual_memory_shared_lib:
    case sys::fs::file_magic::macho_dynamically_linked_shared_lib:
    case sys::fs::file_magic::macho_dynamically_linked_shared_lib_stub:
    case sys::fs::file_magic::coff_object:
    case sys::fs::file_magic::coff_import_library:
      is_native = true;
      break;
  }
  return false;
}

/// LinkInFile - opens a bitcode file and links in all objects which
/// provide symbols that are currently undefined.
///
/// Inputs:
///  File - The pathname of the bitcode file.
///
/// Outputs:
///  ErrorMessage - A C++ string detailing what error occurred, if any.
///
/// Return Value:
///  TRUE  - An error occurred.
///  FALSE - No errors.
///
bool LibraryLinker::linkInFile(const std::string &File, bool &is_native) {
  is_native = false;
  
  // Check for a file of name "-", which means "read standard input"
  if (File == "-") {
    std::auto_ptr<Module> M;
    OwningPtr<MemoryBuffer> Buffer;
    error_code ec;
    if (!(ec = MemoryBuffer::getSTDIN(Buffer))) {
      if (!Buffer->getBufferSize()) {
        Error = "standard input is empty";
      } else {
        M.reset(ParseBitcodeFile(Buffer.get(), Context, &Error));
        if (M.get())
          if (!linkInModule(M.get(), &Error))
            return false;
      }
    }
    return error("Cannot link stdin: " + ec.message());
  }

  // Determine what variety of file it is.
  if (!sys::fs::exists(File))
    return error("Cannot find linker input '" + File + "'");

  switch (getFileType(File)) {
    default: llvm_unreachable("Bad file type identification");
    case sys::fs::file_magic::unknown:
      return warning("Ignoring file '" + File +
                   "' because does not contain bitcode.");

    case sys::fs::file_magic::archive:
      // A user may specify an ar archive without -l, perhaps because it
      // is not installed as a library. Detect that and link the archive.
      if (linkInArchive(File, is_native))
        return true;
      break;

    case sys::fs::file_magic::bitcode: {
      verbose("Linking bitcode file '" + File + "'");
      std::auto_ptr<Module> M(LoadObject(File));
      if (M.get() == 0)
        return error("Cannot load file '" + File + "': " + Error);
      if (linkInModule(M.get(), &Error))
        return error("Cannot link file '" + File + "': " + Error);

      verbose("Linked in file '" + File + "'");
      break;
    }

    case sys::fs::file_magic::elf_relocatable:
    case sys::fs::file_magic::elf_shared_object:
    case sys::fs::file_magic::macho_object:
    case sys::fs::file_magic::macho_fixed_virtual_memory_shared_lib:
    case sys::fs::file_magic::macho_dynamically_linked_shared_lib:
    case sys::fs::file_magic::macho_dynamically_linked_shared_lib_stub:
    case sys::fs::file_magic::coff_object:
    case sys::fs::file_magic::coff_import_library:
      is_native = true;
      break;
  }
  return false;
}

