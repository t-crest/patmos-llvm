//===- lib/CodeGen/PMLExport.h -----------------------------------------===//
//
//               YAML definitions for exporting LLVM datastructures
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_PML_EXPORT_H_
#define LLVM_CODEGEN_PML_EXPORT_H_

#include "llvm/IR/Module.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/ValueMap.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineModulePass.h"
#include "llvm/PML.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"

#include <list>

/////////////////
/// PML Export //
/////////////////

namespace llvm {

  class MachineLoop;

  /// Provides information about machine instructions, can be overloaded for
  /// specific targets.
  class PMLInstrInfo {
  public:
    virtual ~PMLInstrInfo() {}

    typedef std::vector<StringRef>                StringList;
    typedef const std::vector<MachineBasicBlock*> MBBList;
    typedef std::vector<MachineFunction*>         MFList;

    // TODO merge getCalleeNames and getCallees somehow (return struct that
    // contains both names and MFs)

    /// getCalleeNames - get the names of the possible called functions.
    /// If a callee has no name, it is omitted.
    virtual StringList getCalleeNames(MachineFunction &Caller,
                                      const MachineInstr *Instr);

    /// getCallees - get possible callee functions for a call.
    /// If the name of a callee is known but not in this module, it is omitted.
    virtual MFList getCallees(const Module &M, MachineModuleInfo &MMI,
                              MachineFunction &MF, const MachineInstr *Instr);

    virtual MFList getCalledFunctions(const Module &M,
                                      MachineModuleInfo &MMI,
                                      MachineFunction &MF);

    virtual MBBList getBranchTargets(MachineFunction &MF,
                                     const MachineInstr *Instr);

    /// get number of delay slots for this instruction, if any
    virtual unsigned getBranchDelaySlots(const MachineInstr *Instr) {
      return 0;
    }

    /// get number of bytes this instruction takes
    virtual int getSize(const MachineInstr *Instr);
  };

  /// Base class for all exporters
  class PMLExport {

  public:
    PMLExport() {}

    virtual ~PMLExport() {}

    virtual void initialize(const Module &M) {}

    virtual void finalize(const Module &M) {}

    virtual void serialize(MachineFunction &MF) =0;

    virtual void writeOutput(yaml::Output *Output) =0;
  };


  // --------------- Standard exporters --------------------- //

  // TODO we could factor out the code to generate the object to export and
  // do the Doc related stuff separately, if needed for efficiency reasons, or
  // to update existing yaml-docs.

  class PMLBitcodeExport : public PMLExport {
  private:

    yaml::PMLDoc YDoc;
    Pass &P;

    yaml::FlowFact *createLoopFact(const BasicBlock *BB, yaml::Name RHS,
                                   bool UserAnnot = false) const;

  public:
    PMLBitcodeExport(TargetMachine &TM, ModulePass &mp)
    : YDoc(TM.getTargetTriple()), P(mp) {}

    virtual ~PMLBitcodeExport() { }

    // initialize module-level information
    virtual void initialize(const Module &M) { }

    // export bitcode function
    virtual void serialize(MachineFunction &MF);

    // export module-level information during finalize()
    virtual void finalize(const Module &M) { }

    virtual void writeOutput(yaml::Output *Output) {
      yaml::PMLDoc *DocPtr = &YDoc; *Output << DocPtr;
    }

    yaml::PMLDoc& getPMLDoc() { return YDoc; }

    virtual bool doExportInstruction(const Instruction* Instr) {
      return true;
    }

    virtual yaml::Name getOpcode(const Instruction *Instr);

    /// get a description for this instruction
    virtual void printDesc(raw_ostream &os, const Instruction *Instr);

    virtual void exportInstruction(yaml::Instruction* I,
                                   const Instruction* II);
  };



  class PMLMachineExport : public PMLExport {
  private:
    yaml::PMLDoc YDoc;

    PMLInstrInfo *PII;

    yaml::ValueFact *createMemGVFact(const MachineInstr *MI,
                                     yaml::MachineInstruction *I,
                                     std::set<const GlobalValue*> &GVs) const;
  protected:
    Pass &P;
    TargetMachine &TM;

    const TargetInstrInfo *TII;

  public:
    PMLMachineExport(TargetMachine &tm, ModulePass &mp,
                     PMLInstrInfo *pii = 0)
      : YDoc(tm.getTargetTriple()), P(mp), TM(tm), TII(TM.getInstrInfo())
    {
      // TODO needs to be deleted properly!
      PII = pii ? pii : new PMLInstrInfo();
    }

    virtual ~PMLMachineExport() {}

    virtual void serialize(MachineFunction &MF);

    virtual void writeOutput(yaml::Output *Output) {
      yaml::PMLDoc *DocPtr = &YDoc; *Output << DocPtr;
    }

    yaml::PMLDoc& getPMLDoc() { return YDoc; }

    virtual bool doExportInstruction(const MachineInstr *Instr) {
      return true;
    }

    virtual yaml::Name getOpcode(const MachineInstr *Instr);

    /// get a description for this instruction
    virtual void printDesc(raw_ostream &os, const MachineInstr *Instr);

    virtual void exportInstruction(MachineFunction &MF,
                                   yaml::MachineInstruction *I,
                                   const MachineInstr *Instr,
                                   bool BundledWithPred);
    virtual void exportCallInstruction(MachineFunction &MF,
                                   yaml::MachineInstruction *I,
                                   const MachineInstr *Instr);
    virtual void exportBranchInstruction(MachineFunction &MF,
                                   yaml::MachineInstruction *I,
                                   const MachineInstr *Instr);
    virtual void exportMemInstruction(MachineFunction &MF,
                                   yaml::MachineInstruction *I,
                                   const MachineInstr *Instr);

    virtual void exportArgumentRegisterMapping(
                                      yaml::MachineFunction *F,
                                      const MachineFunction &MF);

    virtual void exportSubfunctions(MachineFunction &MF,
                                    yaml::MachineFunction *PMF) { }
    virtual void exportLoopInfo(MachineFunction &MF,
                                yaml::PMLDoc &YDoc,
                                MachineLoop *Loop) { }
  };

  class PMLRelationGraphExport : public PMLExport {
  private:
    yaml::PMLDoc YDoc;
    Pass &P;

  public:
    PMLRelationGraphExport(TargetMachine &TM, ModulePass &mp)
      : YDoc(TM.getTargetTriple()), P(mp) {}

    virtual ~PMLRelationGraphExport() {}

    /// Build the Control-Flow Relation Graph connection between
    /// machine code and bitcode
    virtual void serialize(MachineFunction &MF);

    virtual void writeOutput(yaml::Output *Output) {
      yaml::PMLDoc *DocPtr = &YDoc; *Output << DocPtr;
    }

    yaml::PMLDoc& getPMLDoc() { return YDoc; }

  private:

    /// Generate (heuristic) MachineBlock->EventName
    /// and IR-Block->EventName maps
    /// (1) if all forward-CFG predecessors of (MBB originating from BB) map to
    ///     no or a different IR block, MBB generates a BB event.
    /// (2) if there is a MBB generating a event BB, the basic block BB also
    ///     generates this event
    void buildEventMaps(MachineFunction &MF,
                        std::map<const BasicBlock*,StringRef> &BitcodeEventMap,
                        std::map<MachineBasicBlock*,StringRef> &MachineEventMap,
                        std::set<StringRef> &TabuList);

    class BackedgeInfo {
    private:
      MachineLoopInfo &MLI;
    public:
      BackedgeInfo(MachineLoopInfo &mli) : MLI(mli) {}
      ~BackedgeInfo() {}
      /// Check whether Source -> Target is a back-edge
      bool isBackEdge(MachineBasicBlock *Source, MachineBasicBlock *Target);
    };

  };

  // ---------------------- Export Passes ------------------------- //


  // TODO this pass is currently implemented to work as machine-code module
  // pass. It should either support running on bitcode only as well, or
  // implement another pass for that.
  class PMLModuleExportPass : public MachineModulePass {

    static char ID;

    typedef std::vector<PMLExport*>        ExportList;
    typedef std::vector<std::string>       StringList;
    typedef std::list<MachineFunction*>    MFQueue;
    typedef std::set<MachineFunction*>     MFSet;

    ExportList Exporters;

    PMLInstrInfo *PII;

    StringRef   OutFileName;
    std::string BitcodeFile;
    StringList  Roots;
    bool        SerializeAll;

    MFSet   FoundFunctions;
    MFQueue Queue;

  protected:
    /// Constructor to be used by sub-classes, passes the pass ID to the super
    /// class. You need to setup a PMLInstrInfo using setPMLInstrInfo before
    /// using the exporter.
    PMLModuleExportPass(char &ID, TargetMachine &TM, StringRef filename,
                        ArrayRef<std::string> roots, bool SerializeAll);

    /// Set the PMLInstrInfo to be used. Takes ownership over the
    /// InstrInfo object.
    void setPMLInstrInfo(PMLInstrInfo *pii) { PII = pii; }

  public:
    /// Construct a new PML export pass. The pass will take ownership of the
    /// given PMLInstrInfo.
    PMLModuleExportPass(TargetMachine &TM, StringRef filename,
                        ArrayRef<std::string> roots, PMLInstrInfo *pii,
                        bool SerializeAll);

    virtual ~PMLModuleExportPass() {
      while(!Exporters.empty()) {
        delete Exporters.back();
        Exporters.pop_back();
      }
      if (PII) delete PII;
    }

    PMLInstrInfo *getPMLInstrInfo() { return PII; }

    /// Add an exporter to the pass. Exporters will be deleted when this pass
    /// is deleted.
    void addExporter(PMLExport *PE) { Exporters.push_back(PE); }

    void writeBitcode(std::string& bitcodeFile) { BitcodeFile = bitcodeFile; }

    virtual const char *getPassName() const {
      return "YAML/PML Module Export";
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const;

    virtual bool runOnMachineModule(const Module &M);

  protected:

    virtual bool doInitialization(Module &M);

    virtual bool doFinalization(Module &M);

    void addCalleesToQueue(const Module &M, MachineModuleInfo &MMI,
                           MachineFunction &MF);

    void addToQueue(const Module &M, MachineModuleInfo &MMI,
                    std::string FnName);

    void addToQueue(MachineFunction *MF);

  };

} // end namespace llvm

#endif
