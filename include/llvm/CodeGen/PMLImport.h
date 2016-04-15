//==- PMLImport.h - Import PML information --------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass is used to import PML infos and provide them to LLVM passes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_PMLIMPORT_H
#define LLVM_CODEGEN_PMLIMPORT_H

#include "llvm/Pass.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/PML.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/ValueMap.h"

namespace llvm {

  class  MachineDominatorTree;
  struct MachinePostDominatorTree;

  class MachineInstr;
  class MachineBasicBlock;
  class MachineFunction;
  class Module;

  class PMLQuery;
  class PMLBitcodeQuery;
  class PMLMCQuery;

  /// TODO maybe move this code to PML.h, reuse for export and relation graph.
  typedef StringMap<StringRef> PMLLabelMap;

  //===--------------------------------------------------------------------===//
  /// PMLFunctionInfo - Allows to query information about imported PML functions
  ///
  /// Provides for example about mappings of basic blocks
  ///
  class PMLFunctionInfo {
  protected:
    bool IsBitcode;

    /// Map of block label (mapsto) -> ID (name)
    PMLLabelMap BlockLabels;

    /// Create an empty, unmapped function info.
    PMLFunctionInfo(bool bitcode) : IsBitcode(bitcode) {}

  private:
    PMLFunctionInfo(const PMLFunctionInfo &); // Not implemented
    PMLFunctionInfo& operator=(const PMLFunctionInfo &); // Not implemented

  public:
    virtual ~PMLFunctionInfo() {}

    bool isBitcode() const { return IsBitcode; }

    virtual bool hasMapping() const = 0;

    virtual yaml::Name getName() const = 0;

    virtual bool hasBlock(const yaml::Name& Name) const = 0;

    /// Does there a mapping to a block name exist for a given block?
    bool hasBlockMapping(StringRef Label) const;
    bool hasBlockMapping(const BasicBlock &BB) const;
    bool hasBlockMapping(const MachineBasicBlock &MBB) const;

    yaml::Name getBlockName(StringRef Label) const;
    yaml::Name getBlockName(const BasicBlock &BB) const;
    yaml::Name getBlockName(const MachineBasicBlock &MBB) const;

    /// Get the label of a block by its ID (name) at the current level.
    /// Return either the label or an empty string if there is no mapping.
    virtual StringRef getBlockLabel(const yaml::Name& Name) const = 0;

    static StringRef getBlockLabel(const BasicBlock &BB);
    static StringRef getBlockLabel(const MachineBasicBlock &MBB);

    /// Get a unique label of a memory instruction
    virtual yaml::Name getMemInstrLabel(const yaml::ProgramPoint *PP) = 0;
  };

  //===--------------------------------------------------------------------===//
  /// PMLFunctionInfoT - Template implementation of PMLFunctionInfo
  ///
  /// Type T is instantiated to BasicBlock or MachineBlock, for bitcode and
  /// machinecode, respectively.
  ///
  template<typename BlockT, bool bitcode>
  class PMLFunctionInfoT : public PMLFunctionInfo {
  private:
    typedef StringMap<BlockT*> BlockMap;
    typedef StringMap<StringMap<int> > MemInstrLabelMap;

    yaml::Function<BlockT> *Function;

    /// Map of block ID (name) -> Block
    BlockMap Blocks;
    /// Map of block ID -> (instr ID -> MemInstrLabel)
    MemInstrLabelMap MemInstrLabels;

    PMLFunctionInfoT() : PMLFunctionInfo(bitcode), Function(0) {}

  public:
    PMLFunctionInfoT(yaml::Function<BlockT> &F)
    : PMLFunctionInfo(bitcode), Function(&F)
    {
      reloadBlockInfos();
    }

    void reloadBlockInfos();

    virtual bool hasMapping() const { return Function != NULL; }

    virtual yaml::Name getName() const {
      return Function ? Function->FunctionName : yaml::Name("");
    }

    static PMLFunctionInfoT<BlockT, bitcode> &getEmptyInfo() {
      static PMLFunctionInfoT<BlockT, bitcode> emptyInfo;
      return emptyInfo;
    }

    virtual bool hasBlock(const yaml::Name& Name) const;

    virtual StringRef getBlockLabel(const yaml::Name& Name) const;

    virtual yaml::Name getMemInstrLabel(const yaml::ProgramPoint *PP);

    BlockT* getBlock(const yaml::Name &Name) const;

    yaml::Function<BlockT> *getFunction() const {
      return Function;
    }
  };

  typedef PMLFunctionInfoT<yaml::BitcodeBlock,true>  PMLBitcodeFunctionInfo;
  typedef PMLFunctionInfoT<yaml::MachineBlock,false> PMLMachineFunctionInfo;

  typedef StringMap<PMLFunctionInfo*> PMLFunctionInfoMap;


  //===--------------------------------------------------------------------===//
  /// PMLLevelInfo - Provides PML information about machine code or bitcode
  ///
  /// Function- and block-IDs are only valid within a level, and need proper
  /// transformation when referenced, by label and/or by a relation-graph.
  ///
  class PMLLevelInfo {
  private:
    yaml::ReprLevel Level;
    bool IsBitcode;

    /// Map of function label (mapsto) -> ID (name)
    PMLLabelMap  FunctionLabels;

    // Map of function ID (name) to function infos.
    PMLFunctionInfoMap FunctionInfos;

    PMLLevelInfo(const PMLLevelInfo&); // Not implemented
    const PMLLevelInfo &operator=(const PMLLevelInfo&); // Not implemented
  public:
    PMLLevelInfo(yaml::ReprLevel lvl) : Level(lvl)
    {
      IsBitcode = (lvl == yaml::level_bitcode);
    }
    ~PMLLevelInfo()
    {
      for (PMLFunctionInfoMap::iterator i = FunctionInfos.begin(),
           ie = FunctionInfos.end(); i != ie; i++)
      {
        delete i->second;
      }
    }

    yaml::ReprLevel getLevel() const { return Level; }

    bool isBitcode() const { return IsBitcode; }

    void addFunctionInfo(yaml::BitcodeFunction &F);
    void addFunctionInfo(yaml::MachineFunction &F);

    bool hasFunctionMapping(const Function &F) const {
      return !getFunctionInfo(F).hasMapping();
    }
    bool hasFunctionMapping(const MachineFunction &F) const {
      return !getFunctionInfo(F).hasMapping();
    }

    yaml::Name getFunctionName(const Function &F) const;
    yaml::Name getFunctionName(const MachineFunction &F) const;

    PMLFunctionInfo &getFunctionInfo(const yaml::Name &Name) const;
    PMLFunctionInfo &getFunctionInfo(const Function &F) const {
      return getFunctionInfo(getFunctionName(F));
    }
    PMLFunctionInfo &getFunctionInfo(const MachineFunction &F) const {
      return getFunctionInfo(getFunctionName(F));
    }
  };

  class PMLImport : public ImmutablePass {
  private:
    virtual void anchor();

    yaml::PMLDoc YDoc;

    bool Initialized;

  public:
    static char ID;

    PMLImport()
    : ImmutablePass(ID), Initialized(false), BitcodeLevel(0), MachineLevel(0)
    {
      PassRegistry &Registry = *PassRegistry::getPassRegistry();
      initializePMLImportPass(Registry);
    }

    ~PMLImport() {
      deletePMLIndex();
    }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.setPreservesAll();
    }

    virtual void initializePass();

    /// Check if any PML infos are actually available.
    bool isInitialized() const;

    /// Create a new query object that can be used to access the imported PML
    /// infos. Returns either a new query object or null if no data is available
    /// for the given source level.
    PMLBitcodeQuery* createBitcodeQuery(Pass &AnalysisProvider,
                     const Function &F,
                     yaml::ReprLevel SrcLevel = yaml::level_machinecode);

    /// Create a new query object that can be used to access the imported PML
    /// infos. Returns either a new query object or null if no data is available
    /// for the given source level.
    PMLMCQuery* createMCQuery(Pass &AnalysisProvider, const MachineFunction &MF,
                     yaml::ReprLevel SrcLevel = yaml::level_machinecode);

  private:

    // TODO at some point we could use the PML level field to encode a phase
    // or stage at which the infos where generated and decide based on the
    // bitcode-functions or machine-functions field if a function is bitcode
    // or machinecode. Level would then be something like 'export' or
    // 'pre-ifconvert', analysis results might be attached at different levels.
    // Then those single pointers should become maps of
    // level->(Machine|Bitcode)LevelInfo, and the query classes should become
    // level-aware, either by creating a query for a specific level (requiring
    // either the user or this class to find the (closest) level that has the
    // necessary analysis results attached to them) or by searching all/some
    // levels for analysis results and appropriately transforming them back.
    PMLLevelInfo *BitcodeLevel;
    PMLLevelInfo *MachineLevel;

    void deletePMLIndex();

    void rebuildPMLIndex();
  };


  //===--------------------------------------------------------------------===//
  /// PMLQuery - class to query information from the PML database
  ///
  /// Used to e.g. get information about imported analyses results.
  /// This query class is currently designed to work only intra-procedurally.
  /// To support inter-procedural optimization and analysis, a PMLModuleQuery
  /// should probably be introduced.
  ///
  class PMLQuery {
  private:
    bool IgnoreTraces;

  protected:
    yaml::PMLDoc &YDoc;
    PMLLevelInfo &SrcLevel;
    PMLFunctionInfo &FI;

    Pass &AnalysisProvider;

    MachineDominatorTree *MDom;
    MachinePostDominatorTree *MPostDom;

    PMLQuery(yaml::PMLDoc &doc, const yaml::Name &Function, PMLLevelInfo &lvl,
             Pass &ap)
    : IgnoreTraces(true), YDoc(doc), SrcLevel(lvl),
      FI(lvl.getFunctionInfo(Function)),
      AnalysisProvider(ap), MDom(0), MPostDom(0)
    {}
    virtual ~PMLQuery() {}

  public:
    typedef std::vector<const yaml::ValueFact *> ValueFactList;
    typedef StringMap<ValueFactList> ValueFactsMap;

    void setIgnoreTraces(bool ignore) { IgnoreTraces = ignore; }
    bool doIgnoreTraces() { return IgnoreTraces; }

    virtual void resetAnalyses();

    /// TODO define a sane set of 'isAvailable' functions (?)
    bool hasFlowFacts(bool CheckForFunction = false) const;
    bool hasValueFacts(bool CheckForFunction = false) const;
    bool hasTimings(bool CheckForFunction = false) const;


    /// Map Block name to value
    typedef StringMap<double>   BlockDoubleMap;
    typedef StringMap<uint64_t> BlockUIntMap;

    /// Get a map of all criticality values for all MBBs for which a block
    /// mapping exists.
    bool getBlockCriticalityMap(BlockDoubleMap &Criticalities);

    // Get a memory instruction label for a given program point.
    // Returns an empty label if the value fact is not a mem instruction.
    yaml::Name getMemInstrLabel(const yaml::ProgramPoint *PP) const {
      return FI.getMemInstrLabel(PP);
    }

  protected:
    bool matches(const yaml::Name &Origin, yaml::ReprLevel Level) const;

    bool matches(const yaml::ProgramPoint *PP) const;

    bool matches(const yaml::Scope *S) const;

    template<typename T>
    typename StringMap<T>::iterator getDominatorEntry(StringMap<T> &Map,
                                             MachineBasicBlock &MBB,
                                             bool PostDom, bool &StrictDom);

    template<typename T>
    T getMaxDominatorValue(StringMap<T> &Map, MachineBasicBlock &MBB,
                           T Default);
  };

  class PMLBitcodeQuery : public PMLQuery {
  private:
    friend class PMLImport;

    // const Function &F;

  protected:
    PMLBitcodeQuery(yaml::PMLDoc &doc, const Function& f, PMLLevelInfo &lvl,
                    Pass &AnalysisProvider)
    : PMLQuery(doc, lvl.getFunctionName(f), lvl, AnalysisProvider)
    {}

  public:

  };

  class PMLMCQuery : public PMLQuery {
  private:
    friend class PMLImport;

    // const MachineFunction &MF;

  protected:
    PMLMCQuery(yaml::PMLDoc &doc, const MachineFunction& mf, PMLLevelInfo &lvl,
               Pass &AnalysisProvider)
    : PMLQuery(doc, lvl.getFunctionName(mf), lvl, AnalysisProvider)
    {}

  public:

    /// Get the criticality of a single block, based on the pre-calculated
    /// criticalty map. If the block is not in the map, the CFG structure
    /// of the function is used to calculate the criticality based on the
    /// values in the map.
    double getCriticality(BlockDoubleMap &Criticalities,
                          MachineBasicBlock &MBB, double Default = 1.0);

    /// return value facts referring to memory access information
    /// in a map BBName -> ValueFact
    bool getMemFacts(const MachineFunction &MF, ValueFactsMap &MemFacts) const;

    ValueFactList& getBBMemFacts(ValueFactsMap &MemFacts,
                                 const MachineBasicBlock &MBB) const;
  };

  /// This analysis pass provides a simple interface to PMLQuery and allows
  /// reusing results for several passes.
  class PMLMachineFunctionImport : public MachineFunctionPass {
  private:

    MachineFunction *MF;

    PMLMCQuery *PQ;

    PMLQuery::BlockDoubleMap Criticalities;
    PMLQuery::BlockUIntMap Frequencies;

  public:
    static char ID;

    PMLMachineFunctionImport()
    : MachineFunctionPass(ID), MF(0), PQ(0)
    {
      initializePMLMachineFunctionImportPass(*PassRegistry::getPassRegistry());
    }

    virtual ~PMLMachineFunctionImport() {
      if (PQ) delete PQ;
    }



    void getAnalysisUsage(AnalysisUsage &AU) const override;

    void reset();

    virtual bool runOnMachineFunction(MachineFunction &MF);

    /// check if any results are available.
    bool isAvailable() { return PQ; }

    PMLMCQuery *getQuery() { return PQ; }

    void loadCriticalityMap();

    void loadFrequencyMap();

    double getCriticalty(MachineBasicBlock *FromBB,
                         MachineBasicBlock *ToBB = NULL,
                         double Default = -1.0);

    std::pair<double, int64_t> getCriticalyFreqPair(MachineBasicBlock *FromBB,
                         MachineBasicBlock *ToBB = NULL,
                         double DefaultCrit = -1.0, int64_t DefaultFreq = -1);

    int64_t getWCETFrequency(MachineBasicBlock *FromBB,
                             MachineBasicBlock *ToBB = NULL,
                             int64_t Default = -1);
  };

}

#endif
