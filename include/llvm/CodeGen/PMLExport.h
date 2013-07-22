//===- lib/CodeGen/PMLExport.h -----------------------------------------===//
//
//               YAML definitions for exporting LLVM datastructures
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_PML_EXPORT_H_
#define LLVM_PML_EXPORT_H_

#include "llvm/Module.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineModulePass.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetInstrInfo.h"

#include <list>

/// YAML serialization for LLVM modules (machinecode,bitcode)
/// produces one or more documents of type llvm::yaml::Doc.
/// Serializing to a sequence of documents reduces the memory
/// footprint; during analysis, documents are usually linked into
/// a single document.


/// Utility for declaring that a std::vector of a particular *pointer*type
/// should be considered a YAML sequence. Must only be used in namespace
/// llvm/yaml.
#define YAML_IS_PTR_SEQUENCE_VECTOR(_type)                                   \
    template<>                                                               \
    struct SequenceTraits< std::vector<_type*> > {                           \
      static size_t size(IO &io, std::vector<_type*> &seq) {                 \
        return seq.size();                                                   \
      }                                                                      \
      static _type& element(IO &io, std::vector<_type*> &seq, size_t index) {\
        if ( index >= seq.size() )                                           \
          seq.resize(index+1);                                               \
        return *seq[index];                                                  \
      }                                                                      \
    };

#define YAML_IS_SEQUENCE_VECTOR(_type)                                      \
    template<>                                                              \
    struct SequenceTraits< std::vector<_type> > {                           \
      static size_t size(IO &io, std::vector<_type> &seq) {                 \
        return seq.size();                                                  \
      }                                                                     \
      static _type& element(IO &io, std::vector<_type> &seq, size_t index) {\
        if ( index >= seq.size() )                                          \
          seq.resize(index+1);                                              \
        return seq[index];                                                  \
      }                                                                     \
    };                                                                      \

#define YAML_IS_PTR_SEQUENCE_VECTOR_1(_type)                                 \
    template<typename _member_type>                                          \
    struct SequenceTraits< std::vector<_type<_member_type>*> > {             \
      static size_t size(IO &io, std::vector<_type<_member_type>*> &seq) {   \
          return seq.size();                                                 \
      }                                                                      \
      static _type<_member_type>& element(IO &io,                            \
          std::vector<_type<_member_type>*> &seq, size_t index) {            \
          if ( index >= seq.size() )                                         \
            seq.resize(index+1);                                             \
          return *seq[index];                                                \
      }                                                                      \
    };


/// Utility for deleting owned members of an object
#define DELETE_MEMBERS(vec) \
    while(! vec.empty()) { \
      delete vec.back(); \
      vec.pop_back(); \
    } \

namespace llvm {
namespace yaml {


/// A string representing an identifier (string,index,address)
struct Name {
  // String representation
  std::string NameStr;
  // Empty Name
  Name() : NameStr("") {}
  /// Name from string (copy)
  Name(const StringRef& name) : NameStr(name.str()) {}
  Name& operator=( const StringRef& name ) {
    NameStr.assign(name.str());
    return *this;
  }
  /// Name from unsigned integer
  Name(uint64_t name) : NameStr(utostr(name)) {}
  /// get name as string
  StringRef getName() const {
    return StringRef(NameStr);
  }
  /// get name as unsigned integer
  uint64_t getNameAsInteger(unsigned Radix = 10) const {
    uint64_t IntName;
    getName().getAsInteger(Radix, IntName);
    return IntName;
  }
};

/// Comparing two names
bool operator==(const Name n1, const Name n2);

template<>
struct ScalarTraits<Name> {
  static void output(const Name &value, void*, llvm::raw_ostream &out) {
    out << value.getName();
  }
  static StringRef input(StringRef scalar, void*, Name &value) {
    value.NameStr = scalar.str();
    return StringRef();
  }
};
template <> struct SequenceTraits< std::vector<Name> > {
  static size_t size(IO &io, std::vector<Name> &seq) {
    return seq.size();
  }
  static Name& element(IO &io, std::vector<Name> &seq, size_t index) {
    if ( index >= seq.size() )
      seq.resize(index+1);
    return seq[index];
  }
  static const bool flow = true;
};

/// Representation Level (source, bitcode, machinecode)
enum ReprLevel { level_bitcode, level_machinecode };
template <>
struct ScalarEnumerationTraits<ReprLevel> {
  static void enumeration(IO &io, ReprLevel& level) {
    io.enumCase(level, "bitcode", level_bitcode);
    io.enumCase(level, "machinecode", level_machinecode);
  }
};

/// Instruction Specification (generic)
struct Instruction {
  uint64_t Index;
  int64_t Opcode;
  // std::string Descr;
  std::vector<Name> Callees;
  Instruction(uint64_t index) : Index(index), Opcode(0) {}
  void addCallee(const StringRef function) {
    Callees.push_back(yaml::Name(function));
  }
  bool hasCallees() {
    return ! Callees.empty();
  }
  static const bool flow = true;
};
template <>
struct MappingTraits<Instruction> {
  static void mapping(IO &io, Instruction& Ins) {
    io.mapRequired("index",   Ins.Index);
    io.mapOptional("opcode",  Ins.Opcode, (int64_t) -1);
    io.mapOptional("callees", Ins.Callees);
    // StringRef InsDescr(Ins.Descr);
    // io.mapOptional("description", InsDescr, StringRef(""));
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(Instruction)

/// Generic MachineInstruction Specification
enum BranchType { branch_none, branch_unconditional, branch_conditional,
                  branch_indirect, branch_call, branch_return, branch_any };
template <>
struct ScalarEnumerationTraits<BranchType> {
  static void enumeration(IO &io, BranchType& branchtype) {
    io.enumCase(branchtype, "", branch_none);
    io.enumCase(branchtype, "unconditional", branch_unconditional);
    io.enumCase(branchtype, "conditional", branch_conditional);
    io.enumCase(branchtype, "indirect", branch_indirect);
    io.enumCase(branchtype, "call", branch_call);
    io.enumCase(branchtype, "return", branch_return);
    io.enumCase(branchtype, "any", branch_any);
  }
};
struct GenericMachineInstruction : Instruction {

  uint64_t Size;

  enum BranchType BranchType;
  std::vector<Name> BranchTargets;
  unsigned BranchDelaySlots;

  GenericMachineInstruction(uint64_t Index) :
  Instruction(Index), Size(0), BranchType(branch_none) {}
};
template <>
struct MappingTraits<GenericMachineInstruction> {
  static void mapping(IO &io, GenericMachineInstruction& Ins) {
    MappingTraits<Instruction>::mapping(io,Ins);
    io.mapOptional("size",          Ins.Size);
    io.mapOptional("branch-type",   Ins.BranchType, branch_none);
    io.mapOptional("branch-delay-slots", Ins.BranchDelaySlots, 0U);
    io.mapOptional("branch-targets", Ins.BranchTargets, std::vector<Name>());
  }
  static const bool flow = true;
};
YAML_IS_PTR_SEQUENCE_VECTOR(GenericMachineInstruction)

/// Basic Block Specification (generic)
template<typename InstructionT>
struct Block {
  Name BlockName;
  Name MapsTo;
  std::vector<Name> Successors;
  std::vector<Name> Predecessors;
  std::vector<Name> Loops;
  std::vector<InstructionT*> Instructions;
  Block(StringRef name): BlockName(name) {}
  Block(uint64_t index) : BlockName(index) {}
  ~Block() { DELETE_MEMBERS(Instructions); }
  /// Add an instruction to the block
  /// Block takes ownership of instruction
  InstructionT* addInstruction(InstructionT* Ins) {
    Instructions.push_back(Ins);
    return Ins;
  }
};
template <typename InstructionT>
struct MappingTraits< Block<InstructionT> > {
  static void mapping(IO &io, Block<InstructionT>& Block) {
    io.mapRequired("name",         Block.BlockName);
    io.mapRequired("successors",   Block.Successors);
    io.mapRequired("predecessors", Block.Predecessors);
    io.mapOptional("loops",        Block.Loops);
    io.mapOptional("mapsto",       Block.MapsTo, Name(""));
    io.mapOptional("instructions", Block.Instructions);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR_1(Block)


/// Function formal argument to register mapping
struct Argument {
  Name ArgName;
  uint64_t Index;
  std::vector<Name> Registers;
  Argument(StringRef name, uint64_t index) : ArgName(name), Index(index) {}
  void addReg(const StringRef regname) {
    Registers.push_back(yaml::Name(regname));
  }
  static const bool flow = true;
};
template <>
struct MappingTraits<Argument> {
  static void mapping(IO &io, Argument& Arg) {
    io.mapRequired("name",      Arg.ArgName);
    io.mapRequired("index",     Arg.Index);
    io.mapRequired("registers", Arg.Registers);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(Argument)


/// basic functions
template <typename BlockT>
struct Function {
  Name FunctionName;
  ReprLevel Level;
  Name MapsTo;
  StringRef Hash;
  std::vector<Argument*> Arguments;
  std::vector<BlockT*> Blocks;
  Function(StringRef name) : FunctionName(name) {}
  Function(uint64_t name) : FunctionName(name) {}
  ~Function() {
    DELETE_MEMBERS(Arguments);
    DELETE_MEMBERS(Blocks);
  }
  Argument* addArgument(Argument *Arg) {
    Arguments.push_back(Arg);
    return Arg;
  }
  BlockT* addBlock(BlockT *B) {
    Blocks.push_back(B);
    return B;
  }
};
template <typename BlockT>
struct MappingTraits< Function<BlockT> > {
  static void mapping(IO &io, Function<BlockT>& fn) {
    io.mapRequired("name",      fn.FunctionName);
    io.mapRequired("level",     fn.Level);
    io.mapOptional("mapsto",    fn.MapsTo, Name(""));
    io.mapOptional("arguments", fn.Arguments);
    io.mapOptional("hash",      fn.Hash);
    io.mapRequired("blocks",    fn.Blocks);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR_1(Function)

typedef Block<Instruction> BitcodeBlock;
typedef Function<BitcodeBlock> BitcodeFunction;

/// Relation Node Type
enum RelationNodeType { rnt_entry, rnt_exit, rnt_progress, rnt_src, rnt_dst };
template <>
struct ScalarEnumerationTraits<RelationNodeType> {
  static void enumeration(IO &io, RelationNodeType& rnty) {
    io.enumCase(rnty, "entry", rnt_entry);
    io.enumCase(rnty, "exit", rnt_exit);
    io.enumCase(rnty, "progress", rnt_progress);
    io.enumCase(rnty, "src", rnt_src);
    io.enumCase(rnty, "dst", rnt_dst);
  }
};

/// Relation Graph Nodes
struct RelationNode {
  Name NodeName;
  RelationNodeType NodeType;
  Name SrcBlock;
  Name DstBlock;
  std::vector<Name> SrcSuccessors;
  std::vector<Name> DstSuccessors;
  RelationNode(Name name, RelationNodeType type)
    : NodeName(name), NodeType(type) {}
  void addSuccessor(RelationNode *RN, bool IsSrcNode) {
    if(IsSrcNode)
      SrcSuccessors.push_back(RN->NodeName);
    else
      DstSuccessors.push_back(RN->NodeName);
  }
  void setBlock(Name N, bool IsSrcBlock) {
    if(IsSrcBlock) setSrcBlock(N);
    else setDstBlock(N);
  }
  void setSrcBlock(Name N) { SrcBlock = N; }
  void setDstBlock(Name N) { DstBlock = N; }
};

template <>
struct MappingTraits< RelationNode > {
  static void mapping(IO &io, RelationNode &node) {
    io.mapRequired("name",           node.NodeName);
    io.mapRequired("type",           node.NodeType);
    io.mapOptional("src-block",      node.SrcBlock, Name(""));
    io.mapOptional("dst-block",      node.DstBlock, Name(""));
    io.mapOptional("src-successors", node.SrcSuccessors, std::vector<Name>());
    io.mapOptional("dst-successors", node.DstSuccessors, std::vector<Name>());
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(RelationNode)

/// Relation Graph Scope
struct RelationScope {
  Name Function; // XXX: should be a scope, really
  ReprLevel Level;
  RelationScope(Name f, ReprLevel level) : Function(f), Level(level) {}
};
template <>
struct MappingTraits< RelationScope > {
  static void mapping(IO &io, RelationScope &scope) {
    io.mapRequired("function", scope.Function);
    io.mapRequired("level",scope.Level);
  }
};

/// Relation Graph construction status (everything except 'valid' is a bug)
/// valid: no problems during construction
/// loop: construction worked fine, but CFRG contains  no-progress loop
/// corrected: initial mapping did not include all path, but tabu list corrected the problem
/// incomplete: no sensible mapping that includes all paths from both graphs was found
enum RelationGraphStatus { rg_status_valid, rg_status_loop, rg_status_corrected, rg_status_incomplete };
template <>
struct ScalarEnumerationTraits<RelationGraphStatus> {
  static void enumeration(IO &io, RelationGraphStatus& status) {
    io.enumCase(status, "valid", rg_status_valid);
    io.enumCase(status, "corrected", rg_status_corrected);
    io.enumCase(status, "incomplete", rg_status_incomplete);
  }
};

/// Relation Graphs
struct RelationGraph {
  static const int EntryIndex = 0;
  static const int ExitIndex  = 1;
  int NextIndex;
  RelationGraphStatus Status;
  RelationScope *SrcScope;
  RelationScope *DstScope;
  std::vector<RelationNode*> RelationNodes;
  RelationGraph(RelationScope *src, RelationScope *dst)
  : Status(rg_status_valid), SrcScope(src), DstScope(dst)
  {
    RelationNodes.push_back(new RelationNode(yaml::Name(EntryIndex),rnt_entry));
    RelationNodes.push_back(new RelationNode(yaml::Name(ExitIndex),rnt_exit));
    NextIndex = 2;
  }
  ~RelationGraph() {
    delete SrcScope;
    delete DstScope;
    DELETE_MEMBERS(RelationNodes);
  }
  RelationNode *getEntryNode() { return RelationNodes[EntryIndex]; }
  RelationNode *getExitNode()  { return RelationNodes[ExitIndex]; }
  /// add a relation node (owned by graph)
  RelationNode* addNode(RelationNodeType ty) {
    RelationNode *N = new RelationNode(yaml::Name(NextIndex++),ty);
    RelationNodes.push_back(N);
    return N;
  }
};
template <>
struct MappingTraits< RelationGraph > {
  static void mapping(IO &io, RelationGraph &RG) {
    io.mapRequired("src",   *RG.SrcScope);
    io.mapRequired("dst",   *RG.DstScope);
    io.mapRequired("nodes", RG.RelationNodes);
    io.mapOptional("status", RG.Status);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(RelationGraph)

// Flow Facts
//////////////////////////////////////////////////////////////////////////////

enum CmpOp { cmp_less_equal, cmp_equal };
template <>
struct ScalarEnumerationTraits<CmpOp> {
  static void enumeration(IO &io, CmpOp &op) {
    io.enumCase(op, "less-equal", cmp_less_equal);
    io.enumCase(op, "equal", cmp_equal);
  }
};

struct ProgramPoint {
  Name Function;
  Name Block;
  Name Instruction;
  /// XXX: add edges-target etc.
  /// for loop scopes
  Name Loop;
};
template <>
struct MappingTraits< ProgramPoint > {
  static void mapping(IO &io, ProgramPoint &PP) {
    io.mapRequired("function", PP.Function);
    io.mapOptional("block", PP.Block, Name(""));
    io.mapOptional("instruction", PP.Instruction, Name(""));
    io.mapOptional("loop", PP.Loop, Name(""));
  }
};

struct Term {
  ProgramPoint* PP;
  int64_t Factor;
  Term() : PP(0), Factor(0) {}
  Term(ProgramPoint *pp, int64_t factor) : PP(pp), Factor(factor) {}
};

template <>
struct MappingTraits< Term > {
  static void mapping(IO &io, Term &Term) {
    io.mapRequired("factor", Term.Factor);
    io.mapRequired("program-point", *(Term.PP));
  }
};

YAML_IS_SEQUENCE_VECTOR(Term)

struct FlowFact {
  ProgramPoint* Scope;
  std::vector<Term> TermsLHS;
  CmpOp Comparison;
  Name ConstRHS;
  Name SymbRHS;
  ReprLevel Level;
  Name Origin;
  Name Classification;

  // Add term (PP should have been created by this flow fact)
  void addTermLHS(ProgramPoint *PP, int64_t Factor) {
    TermsLHS.push_back(Term(PP, Factor));
  }

  // Factory / Memory Management
  std::vector<ProgramPoint*> Storage;
  ~FlowFact() {
    for(std::vector<ProgramPoint*>::iterator I = Storage.begin(), E = Storage.end(); I != E; ++I)
      delete *I;
  }
  ProgramPoint* createLoop(const Name& Function, const Name& Loop) {
    ProgramPoint *LPP = new ProgramPoint();
    Storage.push_back(LPP);
    LPP->Function = Function;
    LPP->Loop = Loop;
    return LPP;
  }
  ProgramPoint* createBlock(const Name& Function, const Name& Block) {
    ProgramPoint *BPP = new ProgramPoint();
    Storage.push_back(BPP);
    BPP->Function = Function;
    BPP->Block = Block;
    return BPP;
  }
};


template <>
struct MappingTraits< FlowFact > {
  static void mapping(IO &io, FlowFact &FF) {
    io.mapRequired("scope", *(FF.Scope));
    io.mapRequired("lhs", FF.TermsLHS);
    io.mapRequired("op", FF.Comparison);
    io.mapRequired("rhs", FF.ConstRHS);
    io.mapOptional("rhs-symb", FF.SymbRHS);
    io.mapRequired("level", FF.Level);
    io.mapRequired("origin", FF.Origin);
    io.mapOptional("classification", FF.Classification, Name(""));
  }
};

YAML_IS_PTR_SEQUENCE_VECTOR(FlowFact)

// PML Documents
//////////////////////////////////////////////////////////////////////////////

/// Each document defines a version of the format, and either the
/// generic architecture, or a specialized one. Architecture specific
/// properties are defined in the architecture description.
struct GenericFormat {
  typedef GenericMachineInstruction MachineInstruction;
  typedef Block<MachineInstruction> MachineBlock;
  typedef Function<MachineBlock> MachineFunction;
};


struct Doc {
  StringRef FormatVersion;
  StringRef TargetTriple;
  std::vector<BitcodeFunction*> BitcodeFunctions;
  std::vector<GenericFormat::MachineFunction*> MachineFunctions;
  std::vector<RelationGraph*> RelationGraphs;
  std::vector<FlowFact*> FlowFacts;

  Doc(StringRef TargetTriple)
    : FormatVersion("pml-0.1"),
      TargetTriple(TargetTriple) {}
  ~Doc() {
    DELETE_MEMBERS(BitcodeFunctions);
    DELETE_MEMBERS(MachineFunctions);
    DELETE_MEMBERS(RelationGraphs);
    DELETE_MEMBERS(FlowFacts);
  }
  /// Add a function, which is owned by the document afterwards
  void addFunction(BitcodeFunction *F) {
    BitcodeFunctions.push_back(F);
  }
  /// Add a machine function, which is owned by the document afterwards
  void addMachineFunction(GenericFormat::MachineFunction* MF) {
    MachineFunctions.push_back(MF);
  }
  /// Add a relation graph, which is owned by the document afterwards
  void addRelationGraph(RelationGraph* RG) {
    RelationGraphs.push_back(RG);
  }
  /// Add a flowfact, which is owned by the document afterwards
  void addFlowFact(FlowFact* FF) {
    FlowFacts.push_back(FF);
  }
};
template <>
struct MappingTraits< Doc > {
  static void mapping(IO &io, Doc& doc) {
    io.mapRequired("format",   doc.FormatVersion);
    io.mapRequired("triple",   doc.TargetTriple);
    io.mapOptional("bitcode-functions",doc.BitcodeFunctions);
    io.mapOptional("machine-functions",doc.MachineFunctions);
    io.mapOptional("relation-graphs",doc.RelationGraphs);
    io.mapOptional("flowfacts", doc.FlowFacts);
  }
};


} // end namespace yaml
} // end namespace llvm

/////////////////
/// PML Export //
/////////////////

namespace llvm {

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
    virtual unsigned getBranchDelaySlots(const MachineInstr *Instr) { return 0; }

    /// get number of bytes this instruction takes
    /// FIXME: we should rely on getDescr()->getSize(), but this does not
    /// work for inline assembler at the moment.
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
    yaml::Doc YDoc;
    Pass &P;

  public:
    PMLBitcodeExport(TargetMachine &TM, ModulePass &mp)
    : YDoc(TM.getTargetTriple()), P(mp) {}

    virtual ~PMLBitcodeExport() {}

    virtual void serialize(MachineFunction &MF);

    virtual void writeOutput(yaml::Output *Output) { *Output << YDoc; }

    yaml::Doc& getDoc() { return YDoc; }

    virtual bool doExportInstruction(const Instruction* Instr) { return true; }

    virtual void exportInstruction(yaml::Instruction* I, const Instruction* II);
  };



  class PMLMachineExport : public PMLExport {
  private:
    yaml::Doc YDoc;

    PMLInstrInfo *PII;
    Pass &P;

  protected:
    TargetMachine &TM;

  public:
    PMLMachineExport(TargetMachine &tm, ModulePass &mp,
                     PMLInstrInfo *pii = 0)
      : YDoc(tm.getTargetTriple()), P(mp), TM(tm)
    {
      // TODO needs to be deleted properly!
      PII = pii ? pii : new PMLInstrInfo();
    }

    virtual ~PMLMachineExport() {}

    virtual void serialize(MachineFunction &MF);

    virtual void writeOutput(yaml::Output *Output) { *Output << YDoc; }

    yaml::Doc& getDoc() { return YDoc; }

    virtual bool doExportInstruction(const MachineInstr *Instr) { return true; }

    virtual void exportInstruction(MachineFunction &MF,
                                   yaml::GenericMachineInstruction *I,
                                   const MachineInstr *Instr,
                                   SmallVector<MachineOperand, 4> &Conditions,
                                   bool HasBranchInfo,
                                   MachineBasicBlock *TrueSucc,
                                   MachineBasicBlock *FalseSucc);
    virtual void exportCallInstruction(MachineFunction &MF,
                                   yaml::GenericMachineInstruction *I,
                                   const MachineInstr *Instr);
    virtual void exportBranchInstruction(MachineFunction &MF,
                                   yaml::GenericMachineInstruction *I,
                                   const MachineInstr *Instr,
                                   SmallVector<MachineOperand, 4> &Conditions,
                                   bool HasBranchInfo,
                                   MachineBasicBlock *TrueSucc,
                                   MachineBasicBlock *FalseSucc);

    virtual void exportArgumentRegisterMapping(
                                      yaml::GenericFormat::MachineFunction *F,
                                      const MachineFunction &MF);

  };

  class PMLRelationGraphExport : public PMLExport {
  private:
    yaml::Doc YDoc;
    Pass &P;

  public:
    PMLRelationGraphExport(TargetMachine &TM, ModulePass &mp)
      : YDoc(TM.getTargetTriple()), P(mp) {}

    virtual ~PMLRelationGraphExport() {}

    /// Build the Control-Flow Relation Graph connection machine code and bitcode
    virtual void serialize(MachineFunction &MF);

    virtual void writeOutput(yaml::Output *Output) { *Output << YDoc; }

    yaml::Doc& getDoc() { return YDoc; }

  private:

    /// Generate (heuristic) MachineBlock->EventName and IR-Block->EventName maps
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

    MFSet   FoundFunctions;
    MFQueue Queue;

  protected:
    PMLModuleExportPass(char &ID, TargetMachine &TM, StringRef filename,
                        ArrayRef<std::string> roots, PMLInstrInfo *PII = 0);
  public:
    PMLModuleExportPass(TargetMachine &TM, StringRef filename,
                        ArrayRef<std::string> roots, PMLInstrInfo *PII = 0);

    virtual~PMLModuleExportPass() {
      DELETE_MEMBERS(Exporters);
    }

    void addExporter(PMLExport *PE) { Exporters.push_back(PE); }

    void writeBitcode(std::string& bitcodeFile) { BitcodeFile = bitcodeFile; }

    virtual const char *getPassName() const {return "YAML/PML Module Export";}

    virtual void getAnalysisUsage(AnalysisUsage &AU) const;

    virtual bool runOnMachineModule(const Module &M);

  protected:

    virtual bool doInitialization(Module &M);

    virtual bool doFinalization(Module &M);

    void addCalleesToQueue(const Module &M, MachineModuleInfo &MMI,
                           MachineFunction &MF);

    void addToQueue(const Module &M, MachineModuleInfo &MMI, std::string FnName);

    void addToQueue(MachineFunction *MF);

  };

} // end namespace llvm

#undef DELETE_MEMBERS
#endif
