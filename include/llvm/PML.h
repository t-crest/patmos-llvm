//===- lib/CodeGen/PMLExport.h -----------------------------------------===//
//
//               YAML definitions for exporting LLVM datastructures
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_PML_H_
#define LLVM_CODEGEN_PML_H_

#include "llvm/IR/Module.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "llvm/DebugInfo.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/ToolOutputFile.h"

#include <list>
#include <sstream>

/// YAML serialization for LLVM modules (machinecode,bitcode)
/// produces one or more documents of type llvm::yaml::Doc.
/// Serializing to a sequence of documents reduces the memory
/// footprint; during analysis, documents are usually linked into
/// a single document.
///
/// All structs take ownership over their fields, if not otherwise noted.
/// At the moment, program points, contexts, .. are not shared between objects
/// due to ownership, and to make update algorithms easier. Pointer fields can
/// be set to NULL if they are not initialized or have no value.
/// NB: As each struct takes ownership of its members, it is necessary to
/// either disable copy constructors, or provide custom copy constructors
/// and assignment operators that copy the data referenced by pointers (see
/// Term for an example).

/// Utility for declaring that a std::vector of a particular *pointer*type
/// should be considered a YAML sequence. Must only be used in namespace
/// llvm/yaml.
/// Note: New pointer elements are initialized to NULL, they are initialized
/// with a new object in the mapping() functions. This might allow to share
/// objects that are referenced in YAML, but ownership has to be reworked
/// first.
#define YAML_IS_PTR_SEQUENCE_VECTOR(_type)                                   \
    template<>                                                               \
    struct SequenceTraits< std::vector<_type*> > {                           \
      static size_t size(IO &io, std::vector<_type*> &seq) {                 \
        return seq.size();                                                   \
      }                                                                      \
      static _type*& element(IO &io, std::vector<_type*> &seq, size_t index) \
      {                                                                      \
        if ( index >= seq.size() )                                           \
          seq.resize(index+1);                                               \
        return seq[index];                                                   \
      }                                                                      \
    };

#define YAML_IS_SEQUENCE_VECTOR(_type)                                       \
    template<>                                                               \
    struct SequenceTraits< std::vector<_type> > {                            \
      static size_t size(IO &io, std::vector<_type> &seq) {                  \
        return seq.size();                                                   \
      }                                                                      \
      static _type& element(IO &io, std::vector<_type> &seq, size_t index) { \
        if ( index >= seq.size() )                                           \
          seq.resize(index+1);                                               \
        return seq[index];                                                   \
      }                                                                      \
    };                                                                       \

#define YAML_IS_PTR_SEQUENCE_VECTOR_1(_type)                                 \
    template<typename _member_type>                                          \
    struct SequenceTraits< std::vector<_type<_member_type>*> > {             \
      static size_t size(IO &io, std::vector<_type<_member_type>*> &seq) {   \
          return seq.size();                                                 \
      }                                                                      \
      static _type<_member_type>*& element(IO &io,                           \
                       std::vector<_type<_member_type>*> &seq, size_t index) \
      {                                                                      \
          if ( index >= seq.size() )                                         \
            seq.resize(index+1);                                             \
          return seq[index];                                                 \
      }                                                                      \
    };

#define YAML_IS_SEQUENCE_VECTOR_1(_type)                                     \
    template<typename _member_type>                                          \
    struct SequenceTraits< std::vector<_type<_member_type>> > {              \
      static size_t size(IO &io, std::vector<_type<_member_type>> &seq) {    \
          return seq.size();                                                 \
      }                                                                      \
      static _type<_member_type>& element(IO &io,                            \
          std::vector<_type<_member_type>> &seq, size_t index)               \
      {                                                                      \
          if ( index >= seq.size() )                                         \
            seq.resize(index+1);                                             \
          return seq[index];                                                 \
      }                                                                      \
    };


/// Delete elements in a vector of pointers
#define DELETE_PTR_VEC(VEC)  \
    while(! (VEC).empty()) {   \
      delete (VEC).back();     \
      (VEC).pop_back();        \
    }

/// Copy vector of pointers
#define COPY_PTR_VEC(DST, SRC, TY)			    					 \
	for(unsigned I = 0, E = (SRC).size(); I!=E; ++I) {   \
  	(DST).push_back(new TY(*SRC[I]));			     				 \
  }

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
  /// Name from unsigned integer
  Name(uint64_t name) : NameStr(utostr(name)) {}

  Name& operator=( const StringRef& name ) {
    NameStr.assign(name.str());
    return *this;
  }
  bool operator==(const Name& n2) const {
    return this->NameStr == n2.NameStr;
  }
  bool operator!=(const Name& n2) const {
    return this->NameStr != n2.NameStr;
  }

  bool empty() const {
    return NameStr.empty();
  }

  bool isInteger() const {
    return NameStr.find_first_not_of("0123456789") == std::string::npos;
  }

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
/// Memory Access Mode (load, store)
enum MemMode { memmode_none, memmode_load, memmode_store };
template <>
struct ScalarEnumerationTraits<MemMode> {
  static void enumeration(IO &io, MemMode& memmodetype) {
    io.enumCase(memmodetype, "", memmode_none);
    io.enumCase(memmodetype, "load", memmode_load);
    io.enumCase(memmodetype, "store", memmode_store);
  }
};

/// Instruction Specification (generic)
struct Instruction {
  Name Index;
  Name Opcode;
  Name Desc;
  Name Marker;
  std::vector<Name> Callees;
  enum MemMode MemMode;

  Instruction(uint64_t index)
    : Index(index), MemMode(memmode_none) {}

  void addCallee(const StringRef function) {
    Callees.push_back(yaml::Name(function));
  }
  bool hasCallees() {
    return ! Callees.empty();
  }
};
template <>
struct MappingTraits<Instruction*> {
  static void mapping(IO &io, Instruction *&Ins) {
    if (!Ins) Ins = new Instruction(0);
    io.mapRequired("index",   Ins->Index);
    io.mapOptional("opcode",  Ins->Opcode, yaml::Name());
    io.mapOptional("desc",    Ins->Desc, yaml::Name());
    io.mapOptional("marker",  Ins->Marker, yaml::Name());
    io.mapOptional("callees", Ins->Callees);
    io.mapOptional("memmode",   Ins->MemMode, memmode_none);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(Instruction)

/// MachineInstruction Specification
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
struct MachineInstruction : Instruction {

  unsigned Size;
  int64_t  Address;

  enum BranchType BranchType;
  std::vector<Name> BranchTargets;
  unsigned BranchDelaySlots;
  unsigned StackCacheArg;
  unsigned StackCacheFill;
  unsigned StackCacheSpill;
  Name MemType;

  bool Bundled;

  MachineInstruction(uint64_t Index)
  : Instruction(Index), Size(0), Address(-1), BranchType(branch_none),
    BranchDelaySlots(0), StackCacheArg(0), StackCacheFill(0), StackCacheSpill(0),
    MemType(Name("")), Bundled(false) {}
};
template <>
struct MappingTraits<MachineInstruction*> {
  static void mapping(IO &io, MachineInstruction *&Ins) {
    if (!Ins) Ins = new MachineInstruction(0);
    io.mapRequired("index",         Ins->Index);
    io.mapOptional("opcode",        Ins->Opcode, Name(""));
    io.mapOptional("desc",          Ins->Desc, Name(""));
    io.mapOptional("marker",        Ins->Marker, Name(""));
    io.mapOptional("callees",       Ins->Callees);
    io.mapOptional("size",          Ins->Size);
    io.mapOptional("address",       Ins->Address, (int64_t) -1);
    io.mapOptional("branch-type",   Ins->BranchType, branch_none);
    io.mapOptional("branch-delay-slots", Ins->BranchDelaySlots, 0U);
    io.mapOptional("branch-targets", Ins->BranchTargets, std::vector<Name>());
    io.mapOptional("stack-cache-argument", Ins->StackCacheArg, 0U);
    io.mapOptional("stack-cache-fill", Ins->StackCacheFill, 0U);
    io.mapOptional("stack-cache-spill", Ins->StackCacheSpill, 0U);
    io.mapOptional("memmode",   Ins->MemMode, memmode_none);
    io.mapOptional("memtype",   Ins->MemType, Name(""));
    io.mapOptional("bundled",       Ins->Bundled, false);
  }
  static const bool flow = true;
};
YAML_IS_PTR_SEQUENCE_VECTOR(MachineInstruction)

/// Basic Block Specification (generic)
template<typename InstructionT>
struct Block {
  Name BlockName;
  Name MapsTo;
  int64_t Address;
  std::vector<Name> Successors;
  std::vector<Name> Predecessors;
  std::vector<Name> Loops;
  std::vector<InstructionT*> Instructions;
  Name Loc;

  typedef std::vector<InstructionT*> InstrList;

  Block(StringRef name): BlockName(name), Address(-1) {}
  Block(uint64_t index) : BlockName(index), Address(-1) {}
  ~Block() { DELETE_PTR_VEC(Instructions); }

  /// Add an instruction to the block
  /// Block takes ownership of instruction
  InstructionT* addInstruction(InstructionT* Ins) {
    Instructions.push_back(Ins);
    return Ins;
  }
  /// Set source location hint for this block
  /// Does not update the location once a valid source location has been set
  /// (ie. after first instruction that mapped to a debug location).
  void setSrcLocOnce(const DebugLoc &dl, MDNode *ScopeMD) {
    if (Loc.empty() && !dl.isUnknown()) {
      DIScope Scope(ScopeMD);
      assert((!Scope || Scope.isScope()) &&
        "Scope of a DebugLoc should be null or a DIScope.");
      std::stringstream ss;
      if (Scope)
        ss << Scope.getFilename().str() << ":";
      ss << dl.getLine();
      Loc = ss.str();
    }
  }
private:
  Block(const Block<InstructionT>&);            // Disable copy constructor
  Block* operator=(const Block<InstructionT>&); // Disable assignment
};
template <typename InstructionT>
struct MappingTraits< Block<InstructionT>* > {
  static void mapping(IO &io, Block<InstructionT> *&B) {
    if (!B) B = new Block<InstructionT>(0ULL);
    io.mapRequired("name",         B->BlockName);
    io.mapOptional("mapsto",       B->MapsTo, Name(""));
    io.mapOptional("address",      B->Address, (int64_t) -1);
    io.mapRequired("predecessors", B->Predecessors);
    io.mapRequired("successors",   B->Successors);
    io.mapOptional("loops",        B->Loops);
    io.mapOptional("src-hint",     B->Loc, Name(""));
    io.mapOptional("instructions", B->Instructions);
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
};
template <>
struct MappingTraits<Argument*> {
  static void mapping(IO &io, Argument*& Arg) {
    if (!Arg) Arg = new Argument("", 0);
    io.mapRequired("name",      Arg->ArgName);
    io.mapRequired("index",     Arg->Index);
    io.mapRequired("registers", Arg->Registers);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(Argument)


struct Subfunction {
  Name SFName;
  std::vector<Name> Blocks;

  Subfunction(uint64_t index) : SFName(index) {}
  Subfunction(StringRef name) : SFName(name) {}

  void addBlock(StringRef block) {
    Blocks.push_back(yaml::Name(block));
  }
  void addBlock(uint64_t index) {
    Blocks.push_back(yaml::Name(index));
  }
};
template <>
struct MappingTraits<Subfunction*> {
  static void mapping(IO &io, Subfunction*& S) {
    if (!S) S = new Subfunction("");
    io.mapRequired("name",      S->SFName);
    io.mapRequired("blocks",    S->Blocks);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(Subfunction)


/// basic functions
template <typename BlockT>
struct Function {
  Name FunctionName;
  ReprLevel Level;
  Name MapsTo;
  StringRef Hash;
  std::vector<Argument*> Arguments;
  std::vector<BlockT*> Blocks;
  std::vector<Subfunction*> Subfunctions;

  Function(StringRef name) : FunctionName(name), Level(level_bitcode) {}
  Function(uint64_t name)  : FunctionName(name), Level(level_bitcode) {}
  ~Function() {
    DELETE_PTR_VEC(Arguments);
    DELETE_PTR_VEC(Blocks);
    DELETE_PTR_VEC(Subfunctions);
  }

  Argument* addArgument(Argument *Arg) {
    Arguments.push_back(Arg);
    return Arg;
  }
  BlockT* addBlock(BlockT *B) {
    Blocks.push_back(B);
    return B;
  }
  Subfunction* addSubfunction(Subfunction *S) {
    Subfunctions.push_back(S);
    return S;
  }
private:
  Function(const Function<BlockT>&);            // Disable copy constructor  
  Function* operator=(const Function<BlockT>&); // Disable assignment

};
template <typename BlockT>
struct MappingTraits< Function<BlockT>* > {
  static void mapping(IO &io, Function<BlockT> *&Fn) {
    if (!Fn) Fn = new Function<BlockT>(0ULL);
    io.mapRequired("name",        Fn->FunctionName);
    io.mapRequired("level",       Fn->Level);
    io.mapOptional("mapsto",      Fn->MapsTo, Name(""));
    io.mapOptional("arguments",   Fn->Arguments);
    io.mapOptional("hash",        Fn->Hash);
    io.mapRequired("blocks",      Fn->Blocks);
    io.mapOptional("subfunctions",Fn->Subfunctions);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR_1(Function)

typedef Block<Instruction> BitcodeBlock;
typedef Function<BitcodeBlock> BitcodeFunction;
typedef Block<MachineInstruction> MachineBlock;
typedef Function<MachineBlock> MachineFunction;

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
  RelationNode(StringRef name, RelationNodeType type)
    : NodeName(name), NodeType(type) {}
  RelationNode(uint64_t name, RelationNodeType type)
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
struct MappingTraits< RelationNode* > {
  static void mapping(IO &io, RelationNode *&node) {
    if (!node) node = new RelationNode("", rnt_entry);
    io.mapRequired("name",           node->NodeName);
    io.mapRequired("type",           node->NodeType);
    io.mapOptional("src-block",      node->SrcBlock, Name(""));
    io.mapOptional("dst-block",      node->DstBlock, Name(""));
    io.mapOptional("src-successors", node->SrcSuccessors, std::vector<Name>());
    io.mapOptional("dst-successors", node->DstSuccessors, std::vector<Name>());
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(RelationNode)

/// Relation Graph Scope
struct RelationScope {
  Name Function; // XXX: should be a scope, really
  ReprLevel Level;

  RelationScope(Name      f, ReprLevel level) : Function(f), Level(level) {}
  RelationScope(StringRef f, ReprLevel level) : Function(f), Level(level) {}
  RelationScope(uint64_t  f, ReprLevel level) : Function(f), Level(level) {}
};
template <>
struct MappingTraits< RelationScope* > {
  static void mapping(IO &io, RelationScope *&scope) {
    if (!scope) scope = new RelationScope("", level_bitcode);
    io.mapRequired("function", scope->Function);
    io.mapRequired("level",scope->Level);
  }
};

/// Relation Graph construction status (everything except 'valid' is a bug)
/// valid: no problems during construction
/// loop: construction worked fine, but CFRG contains  no-progress loop
/// corrected: initial mapping did not include all path, but tabu list corrected the problem
/// incomplete: no sensible mapping that includes all paths from both graphs was found
enum RelationGraphStatus { rg_status_valid, rg_status_loop, rg_status_corrected,
                           rg_status_incomplete };
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
    RelationNodes.push_back(new RelationNode(EntryIndex,rnt_entry));
    RelationNodes.push_back(new RelationNode(ExitIndex,rnt_exit));
    NextIndex = 2;
  }
  ~RelationGraph() {
    if (SrcScope) delete SrcScope;
    if (DstScope) delete DstScope;
    DELETE_PTR_VEC(RelationNodes);
  }

  RelationNode *getEntryNode() { return RelationNodes[EntryIndex]; }
  RelationNode *getExitNode()  { return RelationNodes[ExitIndex]; }

  /// add a relation node (owned by graph)
  RelationNode* addNode(RelationNodeType ty) {
    RelationNode *N = new RelationNode(NextIndex++,ty);
    RelationNodes.push_back(N);
    return N;
  }
private:
  RelationGraph(const RelationGraph&);               // Disable copy constructor
  RelationGraph* operator=(const RelationGraph&);    // Disable assignment
};
template <>
struct MappingTraits< RelationGraph* > {
  static void mapping(IO &io, RelationGraph *&RG) {
    if (!RG) RG = new RelationGraph(0, 0);
    io.mapRequired("src",   RG->SrcScope);
    io.mapRequired("dst",   RG->DstScope);
    io.mapRequired("nodes", RG->RelationNodes);
    io.mapOptional("status", RG->Status);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(RelationGraph)


// Generic program references
//////////////////////////////////////////////////////////////////////////////

struct ContextEntry {
  Name Callsite;
  Name Loop;
  uint64_t Offset; // only meaningful if Loop is defined
  uint64_t Step;  // only meaningful if Loop is defined

  ContextEntry(Name loop, uint64_t offset, uint64_t step) : Loop(loop), Offset(offset), Step(step)  {}
  ContextEntry(Name callsite)                             : Callsite(callsite)                      {}
  // for YAML I/O
  ContextEntry() {}
};
template <>
struct MappingTraits< ContextEntry* > {
  static void mapping(IO &io, ContextEntry *&C) {
    if (!C) C = new ContextEntry();
    io.mapOptional("callsite", C->Callsite, Name(""));
    io.mapOptional("loop",     C->Loop,     Name(""));
    io.mapOptional("offset",   C->Offset,   uint64_t(0));
    io.mapOptional("step",     C->Step,     uint64_t(0));
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(ContextEntry)


struct Scope {
  Name Function;
  Name Loop;
  std::vector<ContextEntry*> Context;

  Scope(const Name& f) : Function(f) {}
  ~Scope() {
    DELETE_PTR_VEC(Context);
  }

  Scope(const Scope& Src)
  : Function(Src.Function), Loop(Src.Loop)
  {
    COPY_PTR_VEC(Context, Src.Context, ContextEntry);
  }
  Scope& operator=(Scope& Src) {
    if (this == &Src) return *this;
    Function = Src.Function;
    Loop     = Src.Loop;
    DELETE_PTR_VEC(Context);
    COPY_PTR_VEC(Context, Src.Context, ContextEntry);
    return *this;
  }
};
template <>
struct MappingTraits< Scope* > {
  static void mapping(IO &io, Scope *&S) {
    if (!S) S = new Scope(Name(""));
    io.mapRequired("function", S->Function);
    io.mapOptional("loop",     S->Loop,    Name(""));
    io.mapOptional("context",  S->Context, std::vector<ContextEntry*>());
  }
};


struct ProgramPoint {
  Name Function;
  Name Block;
  Name Instruction;
  Name EdgeSource;
  Name EdgeTarget;
  Name Marker;
  std::vector<ContextEntry*> Context;
  ProgramPoint() {}
  ~ProgramPoint() {
    DELETE_PTR_VEC(Context);
  }
  static ProgramPoint *CreateFunction(const Name &Function) {
    ProgramPoint *PP = new ProgramPoint();
    PP->Function = Function;
    return PP;
  }
  static ProgramPoint *CreateBlock(const Name &Function, const Name &Block) {
    ProgramPoint *PP = CreateFunction(Function);
    PP->Block = Block;
    return PP;
  }
  static ProgramPoint *CreateInstruction(const Name &Function, const Name &Block, const Name &Instruction) {
    ProgramPoint *PP = CreateBlock(Function, Block);
    PP->Instruction = Instruction;
    return PP;
  }
  static ProgramPoint *CreateMarker(const Name &Marker) {
    ProgramPoint *PP = new ProgramPoint();
    PP->Marker = Marker;
    return PP;
  }
  // Custom copy constructor and assignment, as the Context members are owned by ProgramPoint
  ProgramPoint(const ProgramPoint& Src) {
    copyFrom(Src);
  }
  ProgramPoint& operator=(const ProgramPoint& Src) {
    if(this == &Src)
      return *this;
    copyFrom(Src);
    return *this;
  }
private:
  void copyFrom(const ProgramPoint& Src) {
    Function    = Src.Function;
    Block       = Src.Block;
    Instruction = Src.Instruction;
    EdgeSource  = Src.EdgeSource;
    EdgeTarget  = Src.EdgeTarget;
    Marker      = Src.Marker;
    DELETE_PTR_VEC(Context);
    COPY_PTR_VEC(Context, Src.Context, ContextEntry);
  }
};
template <>
struct MappingTraits< ProgramPoint* > {
  static void mapping(IO &io, ProgramPoint *&PP) {
    if (!PP) PP = new ProgramPoint();
    io.mapOptional("function",    PP->Function, Name(""));
    io.mapOptional("block",       PP->Block, Name(""));
    io.mapOptional("instruction", PP->Instruction, Name(""));
    io.mapOptional("edgesource",  PP->EdgeSource, Name(""));
    io.mapOptional("edgetarget",  PP->EdgeTarget, Name(""));
    io.mapOptional("marker", PP->Marker, Name(""));
    io.mapOptional("context",     PP->Context, std::vector<ContextEntry*>());
  }
};


// Value Facts
//////////////////////////////////////////////////////////////////////////////

struct Value {
  Name Symbol;
  int64_t Min;
  int64_t Max;
  Value() : Min(INT64_MIN), Max(INT64_MAX) {}
  Value(Name symbol) : Symbol(symbol), Min(INT64_MIN), Max(INT64_MAX) {}
  Value(int64_t min, int64_t max) : Min(min), Max(max) {}
};
template <>
struct MappingTraits< Value > {
  static void mapping(IO &io, Value &V) {
    io.mapOptional("symbol", V.Symbol, Name(""));
    io.mapOptional("min", V.Min, (int64_t)INT64_MIN);
    io.mapOptional("max", V.Max, (int64_t)INT64_MAX);
  }
};
YAML_IS_SEQUENCE_VECTOR(Value)

struct ValueFact {
  Name Origin;
  ReprLevel Level;
  Name Variable;
  int Width;
  Name Symbol;
  std::vector<Value> Values;
  ProgramPoint *PP;

  ValueFact(ReprLevel lvl) : Level(lvl), Width(0), PP(0) {}
  ~ValueFact() {
    if (PP) delete PP;
  }

  void addValue(int64_t min, int64_t max) {
    Values.push_back(Value(min,max));
  }

  void addValue(Name symbol) {
    Values.push_back(Value(symbol));
  }
private:
  ValueFact(const ValueFact&);            // Disable copy constructor
  ValueFact* operator=(const ValueFact&); // Disable assignment
};
template <>
struct MappingTraits< ValueFact* > {
  static void mapping(IO &io, ValueFact *&VF) {
    if (!VF) VF = new ValueFact(level_machinecode);
    io.mapRequired("level",    VF->Level);
    io.mapOptional("origin",   VF->Origin,   Name(""));
    io.mapOptional("variable", VF->Variable);
    io.mapOptional("width",    VF->Width, 0);
    io.mapOptional("values",   VF->Values);
    io.mapOptional("program-point", VF->PP);
  }
};
YAML_IS_PTR_SEQUENCE_VECTOR(ValueFact)


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


struct Term {
  ProgramPoint* PP;
  int64_t Factor;

  Term() : PP(0), Factor(0) {}
  Term(ProgramPoint *pp, int64_t factor) : PP(pp), Factor(factor) {}
  ~Term() {
    if (PP) delete PP;
  }
  // We need a custom copy-constructor, as Term owns PP
  Term(const Term& Src) : PP(0), Factor(Src.Factor) {
    if(Src.PP) PP = new ProgramPoint(*Src.PP);
  }

  Term& operator=(const Term& Src) {
    if(this == &Src)
      return *this;
    if (PP) delete PP;
    if (Src.PP) PP = new ProgramPoint(*Src.PP);
    else        PP = 0;
    Factor = Src.Factor;
    return *this;
  }
};
template <>
struct MappingTraits< Term > {
  static void mapping(IO &io, Term &Term) {
    io.mapRequired("factor", Term.Factor);
    io.mapRequired("program-point", Term.PP);
  }
};

YAML_IS_SEQUENCE_VECTOR(Term)

struct FlowFact {
  Name      Origin;
  ReprLevel Level;
  Name      Classification;
  Scope    *ScopeRef;
  std::vector<Term> TermsLHS;
  CmpOp     Comparison;
  Name      RHS;

  FlowFact(ReprLevel lvl) : Level(lvl), ScopeRef(0), Comparison(cmp_equal) {}
  ~FlowFact() {
    if (ScopeRef) delete ScopeRef;
  }

  // Add term (PP should have been created by this flow fact)
  void addTermLHS(ProgramPoint *PP, int64_t Factor) {
    TermsLHS.push_back(Term(PP, Factor));
  }

  void setLoopScope(const Name& Function, const Name& Loop) {
    if (ScopeRef) delete ScopeRef;
    ScopeRef = new Scope(Function);
    ScopeRef->Loop = Loop;
  }

private:
  FlowFact(const FlowFact&);            // Disable copy constructor
  FlowFact* operator=(const FlowFact&); // Disable assignment

};
template <>
struct MappingTraits< FlowFact* > {
  static void mapping(IO &io, FlowFact *&FF) {
    if (!FF) FF = new FlowFact(level_bitcode);
    io.mapRequired("scope",  FF->ScopeRef);
    io.mapRequired("lhs",    FF->TermsLHS);
    io.mapRequired("op",     FF->Comparison);
    io.mapRequired("rhs",    FF->RHS);
    io.mapRequired("level",  FF->Level);
    io.mapRequired("origin", FF->Origin);
    io.mapOptional("classification", FF->Classification, Name(""));
  }
};

YAML_IS_PTR_SEQUENCE_VECTOR(FlowFact)

// Timing
//////////////////////////////////////////////////////////////////////////////

struct ProfileEntry {
  ProgramPoint *Reference;
  uint64_t Cycles;
  uint64_t WCETContribution;
  uint64_t WCETFrequency;
  double   Criticality;
  uint64_t CritFrequency;

  // only for yaml import.
  ProfileEntry()
  : Reference(0), Cycles(0), WCETContribution(0), WCETFrequency(0),
    Criticality(-1.0), CritFrequency(0)
  {
  }
  ~ProfileEntry() {
    if (Reference) delete Reference;
  }

  void setReference(ProgramPoint *PP) {
    if (Reference) delete Reference;
    Reference = PP;
  }

  bool hasCriticality() const {
    return Criticality >= 0.0;
  }
private:
  ProfileEntry(const ProfileEntry&);            // Disable copy constructor
  ProfileEntry* operator=(const ProfileEntry&); // Disable assignment
};
template <>
struct MappingTraits< ProfileEntry* > {
  static void mapping(IO &io, ProfileEntry *&P) {
    if (!P) P = new ProfileEntry();
    io.mapRequired("reference",         P->Reference);
    io.mapOptional("cycles",            P->Cycles);
    io.mapOptional("wcet-contribution", P->WCETContribution);
    io.mapOptional("wcet-frequency",    P->WCETFrequency);
    io.mapOptional("criticality",       P->Criticality, -1.0);
    io.mapOptional("crit-frequency",    P->CritFrequency);
  }
};

YAML_IS_PTR_SEQUENCE_VECTOR(ProfileEntry)


struct Timing {
  Name Origin;
  ReprLevel Level;
  Scope *ScopeRef;
  int64_t Cycles;
  std::vector<ProfileEntry*> Profile;

  Timing(ReprLevel lvl) : Origin(""), Level(lvl), ScopeRef(0), Cycles(0) {}
  ~Timing() {
    if (ScopeRef) delete ScopeRef;
    DELETE_PTR_VEC(Profile);
  }
private:
  Timing(const Timing&);            // Disable copy constructor
  Timing* operator=(const Timing&); // Disable assignment
};
template <>
struct MappingTraits< Timing* > {
  static void mapping(IO &io, Timing *&T) {
    if (!T) T = new Timing(level_machinecode);
    io.mapOptional("origin",  T->Origin);
    io.mapOptional("level",   T->Level);
    io.mapOptional("scope",   T->ScopeRef);
    io.mapRequired("cycles",  T->Cycles);
    io.mapOptional("profile", T->Profile);
  }
};

YAML_IS_PTR_SEQUENCE_VECTOR(Timing)

// PML Documents
//////////////////////////////////////////////////////////////////////////////

struct PMLDoc {
  StringRef FormatVersion;
  StringRef TargetTriple;
  std::vector<BitcodeFunction*> BitcodeFunctions;
  std::vector<MachineFunction*> MachineFunctions;
  std::vector<RelationGraph*>   RelationGraphs;
  std::vector<FlowFact*>  FlowFacts;
  std::vector<ValueFact*> ValueFacts;
  std::vector<Timing*>    Timings;

  PMLDoc()
    : FormatVersion("pml-0.1"), TargetTriple("") {}

  PMLDoc(StringRef TargetTriple)
    : FormatVersion("pml-0.1"),
      TargetTriple(TargetTriple) {}

  ~PMLDoc() {
    DELETE_PTR_VEC(BitcodeFunctions);
    DELETE_PTR_VEC(MachineFunctions);
    DELETE_PTR_VEC(RelationGraphs);
    DELETE_PTR_VEC(ValueFacts);
    DELETE_PTR_VEC(FlowFacts);
    DELETE_PTR_VEC(Timings);
  }
  /// Add a function, which is owned by the document afterwards
  void addFunction(BitcodeFunction *F) {
    BitcodeFunctions.push_back(F);
  }
  /// Add a machine function, which is owned by the document afterwards
  void addMachineFunction(MachineFunction* MF) {
    MachineFunctions.push_back(MF);
  }
  /// Add a relation graph, which is owned by the document afterwards
  void addRelationGraph(RelationGraph* RG) {
    RelationGraphs.push_back(RG);
  }
  /// Add a valuefact, which is owned by the document afterwards
  void addValueFact(ValueFact* VF) {
    ValueFacts.push_back(VF);
  }
  /// Add a flowfact, which is owned by the document afterwards
  void addFlowFact(FlowFact* FF) {
    FlowFacts.push_back(FF);
  }

  bool empty() {
    return BitcodeFunctions.empty() && MachineFunctions.empty() &&
           RelationGraphs.empty() && ValueFacts.empty() &&
           FlowFacts.empty() && Timings.empty();
  }

  /// Merge another PML doc into this one, transferring ownership of all childs.
  void mergePML(PMLDoc &Doc) {
    // TODO check for duplicate keys, merge recursively?

    if (TargetTriple.empty()) TargetTriple = Doc.TargetTriple;

    BitcodeFunctions.insert(BitcodeFunctions.end(),
                            Doc.BitcodeFunctions.begin(),
                            Doc.BitcodeFunctions.end());
    Doc.BitcodeFunctions.clear();
    MachineFunctions.insert(MachineFunctions.end(),
                            Doc.MachineFunctions.begin(),
                            Doc.MachineFunctions.end());
    Doc.MachineFunctions.clear();
    RelationGraphs.insert(RelationGraphs.end(),
                            Doc.RelationGraphs.begin(),
                            Doc.RelationGraphs.end());
    Doc.RelationGraphs.clear();
    ValueFacts.insert(ValueFacts.end(),
                            Doc.ValueFacts.begin(),
                            Doc.ValueFacts.end());
    Doc.ValueFacts.clear();
    FlowFacts.insert(FlowFacts.end(),
                            Doc.FlowFacts.begin(),
                            Doc.FlowFacts.end());
    Doc.FlowFacts.clear();
    Timings.insert(Timings.end(),
                            Doc.Timings.begin(),
                            Doc.Timings.end());
    Doc.Timings.clear();
  }

private:
  PMLDoc(const PMLDoc&);            // Disable copy constructor
  PMLDoc* operator=(const PMLDoc&); // Disable assignment
};
template <>
struct MappingTraits< PMLDoc* > {
  static void mapping(IO &io, PMLDoc *&doc) {
    if (!doc) doc = new PMLDoc();
    io.mapRequired("format",     doc->FormatVersion);
    io.mapRequired("triple",     doc->TargetTriple);
    io.mapOptional("bitcode-functions",doc->BitcodeFunctions);
    io.mapOptional("machine-functions",doc->MachineFunctions);
    io.mapOptional("relation-graphs",doc->RelationGraphs);
    io.mapOptional("flowfacts",  doc->FlowFacts);
    io.mapOptional("valuefacts", doc->ValueFacts);
    io.mapOptional("timing",     doc->Timings);
  }
};

struct PMLDocList {
  typedef std::vector<PMLDoc*>::iterator iterator;
  typedef std::vector<PMLDoc*>::const_iterator const_iterator;

  std::vector<PMLDoc*> YDocs;

  ~PMLDocList() {
    DELETE_PTR_VEC(YDocs);
  }

  /// Merge all documents into a single document and clear this list.
  void mergeInto(PMLDoc &YDoc) {
    for (iterator i = YDocs.begin(), ie = YDocs.end(); i != ie; i++) {
      YDoc.mergePML(**i);
    }
    DELETE_PTR_VEC(YDocs);
  }
};

} // end namespace yaml
} // end namespace llvm

LLVM_YAML_IS_DOCUMENT_LIST_VECTOR(PMLDoc*)

#undef DELETE_PTR_VEC
#undef COPY_PTR_VEC
#endif
