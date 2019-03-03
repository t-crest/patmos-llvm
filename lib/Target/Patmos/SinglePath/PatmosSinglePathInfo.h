//==-- PatmosSinglePathInfo.h - Analysis Pass for SP CodeGen -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
// This file defines a pass to compute imformation for single-path converting
// seleced functions.
//
//===---------------------------------------------------------------------===//


#ifndef _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
#define _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_

#include <Patmos.h>
#include <PatmosTargetMachine.h>
#include <llvm/IR/Module.h>
#include <llvm/ADT/BitVector.h>
#include <llvm/CodeGen/MachineFunctionPass.h>

#include "SPScope.h"

#include <vector>
#include <set>
#include <map>

namespace llvm {

  class MachineLoop;



///////////////////////////////////////////////////////////////////////////////

  /// PatmosSinglePathInfo - Single-Path analysis pass
  class PatmosSinglePathInfo : public MachineFunctionPass {
    private:
      const PatmosTargetMachine &TM;
      const PatmosSubtarget &STC;
      const PatmosInstrInfo *TII;

      /// Set of functions yet to be analyzed
      std::set<std::string> FuncsRemain;

      /// Root SPScope
      SPScope *Root;

      /// Analyze a given MachineFunction
      void analyzeFunction(MachineFunction &MF);

      /// Fail with an error if MF is irreducible.
      void checkIrreducibility(MachineFunction &MF) const;

    public:
      /// Pass ID
      static char ID;

      /// isEnabled - Return true if there are functions specified to
      /// to be converted to single-path code.
      static bool isEnabled();

      /// isEnabled - Return true if a particular function is specified to
      /// to be converted to single-path code.
      static bool isEnabled(const MachineFunction &MF);

      static bool isConverting(const MachineFunction &MF);

      static bool isRoot(const MachineFunction &MF);

      static bool isReachable(const MachineFunction &MF);

      static bool isMaybe(const MachineFunction &MF);

      /// getRootNames - Fill a set with the names of
      /// single-path root functions
      static void getRootNames(std::set<std::string> &S);

      /// PatmosSinglePathInfo - Constructor
      PatmosSinglePathInfo(const PatmosTargetMachine &tm);

      /// doInitialization - Initialize SinglePathInfo pass
      virtual bool doInitialization(Module &M);

      /// doFinalization - Finalize SinglePathInfo pass
      virtual bool doFinalization(Module &M);

      /// getAnalysisUsage - Specify which passes this pass depends on
      virtual void getAnalysisUsage(AnalysisUsage &AU) const;

      /// runOnMachineFunction - Run the SP converter on the given function.
      virtual bool runOnMachineFunction(MachineFunction &MF);

      /// getPassName - Return the pass' name.
      virtual const char *getPassName() const {
        return "Patmos Single-Path Info";
      }

      /// print - Convert to human readable form
      virtual void print(raw_ostream &OS, const Module* = 0) const;

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
      /// dump - Dump the state of this pass
      virtual void dump() const;
#endif

      /// isToConvert - Return true if the function should be if-converted
      bool isToConvert(MachineFunction &MF) const;

      /// getRootScope - Return the Root SPScope for this function
      SPScope *getRootScope() const { return Root; }

      /// getScopeFor - Return the innermost scope of an MBB
      SPScope *getScopeFor(const PredicatedBlock *MBB) const;

      /// walkRoot - Walk the top-level SPScope
      void walkRoot(SPScopeWalker &walker) const;
  };

///////////////////////////////////////////////////////////////////////////////

  // Allow clients to walk the list of nested SPScopes
  template <> struct GraphTraits<PatmosSinglePathInfo*> {
    typedef SPScope NodeType;
    typedef SPScope::child_iterator ChildIteratorType;

    static NodeType *getEntryNode(const PatmosSinglePathInfo *PSPI) {
      return PSPI->getRootScope();
    }
    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->child_begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->child_end();
    }
  };




} // end of namespace llvm

#endif // _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
