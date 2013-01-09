//===- lib/Support/PMLExport.h -----------------------------------------===//
//
//               YAML definitions for exporting LLVM datastructures
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_PML_EXPORT_H_
#define LLVM_PML_EXPORT_H_

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetInstrInfo.h"


/// YAML serialization for LLVM modules (machinecode,bitcode)
/// produces one or more documents of type llvm::yaml::Doc.
/// Serializing to a sequence of documents reduces the memory
/// footprint; during analysis, documents are usually linked intp
/// a single document.

/// Utility for declaring that a std::vector of a particular *pointer*type
/// should be considered a YAML sequence. Must only be used in namespace
/// llvm/yaml.
#define IS_PTR_SEQUENCE_VECTOR(_type)                                        \
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
#define IS_PTR_SEQUENCE_VECTOR_1(_type)                                      \
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
    /// Name from string
    Name(const StringRef& name) : NameStr(name.str()) {}
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
bool operator==(const Name n1, const Name n2) {
    return n1.NameStr == n2.NameStr;
}

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
    std::vector<Name> Callees;
    Instruction(uint64_t index) : Index(index) {}
    void addCallee(const StringRef function) {
        Callees.push_back(yaml::Name(function));
    }
    bool hasCallees() {
        return ! Callees.empty();
    }
};
template <>
struct MappingTraits<Instruction> {
    static void mapping(IO &io, Instruction& Ins) {
        io.mapRequired("index",   Ins.Index);
        io.mapOptional("opcode",  Ins.Opcode, (int64_t) -1);
        io.mapOptional("callees", Ins.Callees);
    }
};
IS_PTR_SEQUENCE_VECTOR(Instruction)

/// Generic MachineInstruction Specification
enum BranchType { branch_none, branch_unconditional, branch_conditional, branch_any };
template <>
struct ScalarEnumerationTraits<BranchType> {
    static void enumeration(IO &io, BranchType& branchtype) {
        io.enumCase(branchtype, "", branch_none);
        io.enumCase(branchtype, "unconditional", branch_unconditional);
        io.enumCase(branchtype, "conditional", branch_conditional);
        io.enumCase(branchtype, "any", branch_any);
    }
};
struct GenericMachineInstruction : Instruction {
    uint64_t Size;
    enum BranchType BranchType;
    std::vector<Name> BranchTargets;
    GenericMachineInstruction(uint64_t Index) :
      Instruction(Index), Size(0), BranchType(branch_none) {}
};
template <>
struct MappingTraits<GenericMachineInstruction> {
    static void mapping(IO &io, GenericMachineInstruction& Ins) {
        MappingTraits<Instruction>::mapping(io,Ins);
        io.mapOptional("size",          Ins.Size);
        io.mapOptional("branch-type",   Ins.BranchType, branch_none);
        io.mapOptional("branch-targets",Ins.BranchTargets, std::vector<Name>());
    }
};
IS_PTR_SEQUENCE_VECTOR(GenericMachineInstruction)

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
IS_PTR_SEQUENCE_VECTOR_1(Block)

/// basic functions
template <typename BlockT>
struct Function {
    Name FunctionName;
    ReprLevel Level;
    Name MapsTo;
    StringRef Hash;
    std::vector<BlockT*> Blocks;
    Function(StringRef name) : FunctionName(name) {}
    Function(uint64_t name) : FunctionName(name) {}
    ~Function() { DELETE_MEMBERS(Blocks); }
    BlockT* addBlock(BlockT *B) {
        Blocks.push_back(B);
        return B;
    }
};
template <typename BlockT>
struct MappingTraits< Function<BlockT> > {
    static void mapping(IO &io, Function<BlockT>& fn) {
        io.mapRequired("name",    fn.FunctionName);
        io.mapRequired("level",   fn.Level);
        io.mapOptional("mapsto",  fn.MapsTo, Name(""));
        io.mapOptional("hash",    fn.Hash);
        io.mapRequired("blocks",  fn.Blocks);
    }
};
IS_PTR_SEQUENCE_VECTOR_1(Function)

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
IS_PTR_SEQUENCE_VECTOR(RelationNode)

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

/// Relation Graphs
struct RelationGraph {
    static const int EntryIndex = 0;
    static const int ExitIndex  = 1;
    int NextIndex;
    RelationScope *SrcScope;
    RelationScope *DstScope;
    std::vector<RelationNode*> RelationNodes;
    RelationGraph(RelationScope *src, RelationScope *dst) : SrcScope(src), DstScope(dst) {
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
    }
};
IS_PTR_SEQUENCE_VECTOR(RelationGraph)


/// Each document defines a version of the format, and either the
/// generic architecture, or a specialized one. Architecture specific
/// properties are defined in the architecture description.
struct GenericArchitecture {
    static const std::string ArchName;
    typedef GenericMachineInstruction MachineInstruction;
    typedef Block<MachineInstruction> MachineBlock;
    typedef Function<MachineBlock> MachineFunction;
};
const std::string GenericArchitecture::ArchName = "generic";

template <typename Arch> struct Doc {
    StringRef FormatVersion;
    StringRef Architecture;
    std::vector<BitcodeFunction*> BitcodeFunctions;
    std::vector<typename Arch::MachineFunction*> MachineFunctions;
    std::vector<RelationGraph*> RelationGraphs;
    Doc(StringRef version) : FormatVersion(version), Architecture(Arch::ArchName) {}
    ~Doc() {
        DELETE_MEMBERS(BitcodeFunctions);
        DELETE_MEMBERS(MachineFunctions);
        DELETE_MEMBERS(RelationGraphs);
    }
    /// Add a function, which is owned by the document afterwards
    void addFunction(BitcodeFunction *F) {
        BitcodeFunctions.push_back(F);
    }
    /// Add a machine function, which is owned by the document afterwards
    void addMachineFunction(typename Arch::MachineFunction* MF) {
        MachineFunctions.push_back(MF);
    }
    /// Add a relation graph, which is owned by the document afterwards
    void addRelationGraph(RelationGraph* RG) {
        RelationGraphs.push_back(RG);
    }
};
template <typename Arch>
struct MappingTraits< Doc<Arch> > {
    static void mapping(IO &io, Doc<Arch>& doc) {
        io.mapRequired("format",   doc.FormatVersion);
        io.mapRequired("arch",     doc.Architecture);
        io.mapOptional("bitcode-functions",doc.BitcodeFunctions);
        io.mapOptional("machine-functions",doc.MachineFunctions);
        io.mapOptional("relation-graphs",doc.RelationGraphs);
    }
};


} // end namespace yaml
} // end namespace llvm


/// Unfortunately, the interface for accessing successors differs
/// between machine block and bitcode block, therefore we need tis
/// trait in order to avoid code duplication
namespace llvm {
namespace yaml {

/// Utility class to generalize parts of the relation graph construction
template <typename Block> struct FlowGraphTrait {};
template <> struct FlowGraphTrait<const BasicBlock> {
  typedef llvm::succ_const_iterator succ_iterator;
  inline static succ_iterator succ_begin(const BasicBlock *BB) { return llvm::succ_begin(BB); }
  inline static succ_iterator succ_end(const BasicBlock *BB) { return llvm::succ_end(BB); }
  static Name getName(const BasicBlock *BB) {
    return Name(BB->getName());
  }
};
template <> struct FlowGraphTrait<MachineBasicBlock> {
  typedef MachineBasicBlock::succ_iterator succ_iterator;
  inline static succ_iterator succ_begin(MachineBasicBlock *MBB) { return MBB->succ_begin(); }
  inline static succ_iterator succ_end(MachineBasicBlock *MBB) { return MBB->succ_end(); }
  static Name getName(MachineBasicBlock*BB) {
    return Name(BB->getNumber());
  }
};

} // end namespace yaml
} // end namespace llvm


namespace llvm {

  class PMLExport {

    TargetMachine *TM;

    // Information for the current function
    MachineLoopInfo *LI;

  public:
    PMLExport(TargetMachine *tm)
      : TM(tm), LI(0) {}


    void initialize(yaml::Output *Output);

    void finalize(yaml::Output *Output);


    template <typename ArchTrait>
    void serialize(MachineFunction &MF, MachineLoopInfo* LI,
                   yaml::Doc<ArchTrait>& YDoc)
    {
      this->LI = LI;
      serializeMachineFunction(MF, YDoc);
      serializeFunction(MF.getFunction(), YDoc);
      serializeRelationGraph(MF, YDoc);
    }

    template<typename ArchTrait>
    void serializeMachineFunction(MachineFunction &MF,
                                  yaml::Doc<ArchTrait>& doc)
    {
      yaml::GenericArchitecture::MachineFunction *F =
         new yaml::GenericArchitecture::MachineFunction(MF.getFunctionNumber());
      F->MapsTo = yaml::Name(MF.getFunction()->getName());
      F->Level = yaml::level_machinecode;
      yaml::GenericArchitecture::MachineBlock *B;
      for (MachineFunction::iterator BB = MF.begin(), E = MF.end(); BB != E;
          ++BB)
      {
        B = F->addBlock(
            new yaml::GenericArchitecture::MachineBlock(BB->getNumber()));

        for (MachineBasicBlock::const_pred_iterator BBPred = BB->pred_begin(),
            E = BB->pred_end(); BBPred != E; ++BBPred)
          B->Predecessors.push_back(yaml::Name((*BBPred)->getNumber()));
        for (MachineBasicBlock::const_succ_iterator BBSucc = BB->succ_begin(),
            E = BB->succ_end(); BBSucc != E; ++BBSucc)
          B->Successors.push_back(yaml::Name((*BBSucc)->getNumber()));

        B->MapsTo = yaml::Name(BB->getName());

        // export loop information
        MachineLoop *Loop = LI->getLoopFor(BB);
        while (Loop) {
          B->Loops.push_back(yaml::Name(Loop->getHeader()->getNumber()));
          Loop = Loop->getParentLoop();
        }

        // export instruction and branch Information
        MachineBasicBlock *TrueSucc = 0, *FalseSucc = 0;
        SmallVector<MachineOperand, 4> Conditions;
        const TargetInstrInfo *TII = TM->getInstrInfo();
        bool HasBranchInfo = !TII->AnalyzeBranch(*BB, TrueSucc, FalseSucc,
            Conditions, false);
        unsigned Index = 0;
        for (MachineBasicBlock::iterator Ins = BB->begin(), E = BB->end();
            Ins != E; ++Ins) {
          yaml::GenericMachineInstruction *I = B->addInstruction(
              new yaml::GenericMachineInstruction(Index++));
          I->Size = Ins->getDesc().getSize();
          I->Opcode = Ins->getOpcode();

          // FIXME: this is still the hackish implementation from the OTAP prototype
          if (Ins->getDesc().isCall()) {
            // read jump table (patmos specific: operand[3] of BR(CF)?Tu?)
            // read call (patmos specific: operand[2] of call)
            for (MachineInstr::const_mop_iterator Op = Ins->operands_begin(),
                E = Ins->operands_end(); Op != E; ++Op) {
              if (Op->isGlobal()) {
                I->addCallee(Op->getGlobal()->getName());
              }
              else if (Op->isSymbol()) {
                I->addCallee(StringRef(Op->getSymbolName()));
              }
            }
            if (!I->hasCallees()) {
              errs()
                  << "[mc2yml] Warning: no known callee for MC instruction in "
                  << MF.getFunction()->getName() << "\n";
              I->addCallee(StringRef("__any__"));
            }
          }

          if (Ins->getDesc().isBranch()) {
            if (Ins->getDesc().isConditionalBranch()) {
              I->BranchType = yaml::branch_conditional;
              if (HasBranchInfo && TrueSucc)
                I->BranchTargets.push_back(yaml::Name(TrueSucc->getNumber()));
            }
            else if (Ins->getDesc().isUnconditionalBranch()) {
              I->BranchType = yaml::branch_unconditional;
              MachineBasicBlock *USucc =
                  Conditions.empty() ? TrueSucc : FalseSucc;
              if (HasBranchInfo && USucc)
                I->BranchTargets.push_back(yaml::Name(USucc->getNumber()));
            }
            else {
              I->BranchType = yaml::branch_any;
            }
          }
          else {
            I->BranchType = yaml::branch_none;
          }
        }
      }

      // TODO: we do not compute a hash yet
      F->Hash = StringRef("0");
      doc.addMachineFunction(F);
    }

    template<typename T>
    void serializeFunction(const Function *const_function, yaml::Doc<T> &doc)
    {
      Function *BF = const_cast<Function*>(const_function);
      yaml::BitcodeFunction *F = new yaml::BitcodeFunction(BF->getName());

      F->Level = yaml::level_bitcode;
      yaml::BitcodeBlock *B;
      for (Function::iterator BI = BF->begin(), BE = BF->end(); BI != BE;
          ++BI) {
        B = F->addBlock(new yaml::BitcodeBlock(BI->getName()));
        /// B->MapsTo = (maybe C-source debug info?)
        for (pred_iterator PI = pred_begin(&*BI), PE = pred_end(&*BI); PI != PE;
            ++PI) {
          B->Predecessors.push_back(yaml::Name((*PI)->getName()));
        }
        for (succ_iterator SI = succ_begin(&*BI), SE = succ_end(&*BI); SI != SE;
            ++SI) {
          B->Successors.push_back(yaml::Name((*SI)->getName()));
        }
        unsigned Index = 0;
        for (BasicBlock::iterator II = BI->begin(), IE = BI->end(); II != IE;
            ++II) {
          yaml::Instruction *I = B->addInstruction(
              new yaml::Instruction(Index++));
          I->Opcode = II->getOpcode();
          if (CallInst *CI = dyn_cast<CallInst>(II)) {
            if (Function *F = CI->getCalledFunction()) {
              I->addCallee(F->getName());
            }
            else {
              // TODO: we still have no information about indirect calls
              I->addCallee(StringRef("__any__"));
            }
          }
        }
      }
      // TODO: we do not compute a hash yet
      F->Hash = StringRef("0");
      doc.addFunction(F);
    }

  private:

    /// RelationGraph utility class to manage maps from Blocks to Predecessor Lists
    template <typename Block>
    class EventQueue {
      typedef std::vector<yaml::RelationNode*> PredList;
      std::map<Block, PredList* > KeyedQueues;
    public:
      ~EventQueue() {
        for(iterator I = KeyedQueues.begin(), E = KeyedQueues.end(); I!=E; ++I) {
          delete I->second;
        }
      }
      void addItem(Block& Key, yaml::RelationNode *Pred) {
        if(KeyedQueues.count(Key) == 0) {
          KeyedQueues.insert( make_pair(Key, new PredList()) );
        }
        KeyedQueues[Key]->push_back(Pred);
      }
      typedef typename std::map<Block,PredList*>::iterator iterator;
      iterator begin() { return KeyedQueues.begin(); }
      iterator end() { return   KeyedQueues.end(); }
    };

    /// RelationGraph utility class to maintain candidates for progress nodes
    template <typename Block>
    class EventQueueMap {
      std::vector<yaml::RelationNode*> ExitPreds;
      std::map<StringRef, EventQueue<Block>*> EMap;
    public:
      ~EventQueueMap() {
        for(iterator I = EMap.begin(), E = EMap.end(); I!=E; ++I) {
          delete I->second;
        }
      }
      void addItem(StringRef Event, Block& Key, yaml::RelationNode *Pred) {
        if(EMap.count(Event) == 0) {
          EMap.insert( std::make_pair(Event,new EventQueue<Block>()) );
        }
        EMap[Event]->addItem(Key, Pred);
      }
      void addExitPredecessor(yaml::RelationNode *Pred) {
        ExitPreds.push_back(Pred);
      }
      std::vector<yaml::RelationNode*>& getExitPredecessors() {
        return ExitPreds;
      }
      bool hasExitPredecessors() {
        return !ExitPreds.empty();
      }
      // Caller takes ownership of removed queue
      EventQueue<Block>* remove(const StringRef& Event) {
        iterator It = EMap.find(Event);
        if(It == EMap.end())
          return 0;
        EventQueue<Block>* Queue = It->second;
        EMap.erase(It);
        return Queue;
      }
      typedef typename std::map<StringRef, EventQueue<Block>*>::iterator iterator;
      iterator begin() { return EMap.begin(); }
      iterator end()   { return EMap.end(); }
    };

    /// Mapping from T to events (strings)
    template <typename T>
    struct EventMap {
        typedef std::map<T,StringRef> type;
    };

    /// Progress nodes are characterized by a pair of bitcode/machine block
    typedef std::pair <const BasicBlock*,MachineBasicBlock*> ProgressID;

  public:

    /// Build the Control-Flow Relation Graph connection machine code and bitcode
    template<typename T>
    void serializeRelationGraph(MachineFunction &MF, yaml::Doc<T> &doc) {
      Function *BF = const_cast<Function*>(MF.getFunction());
      if (!BF)
        return;

      // unmatched events, used as tabu list, and for error reporting
      std::set<StringRef> TabuEvents;
      std::set<StringRef> UnmatchedEvents;

      // As the LLVM mapping is not always good enough, we might have had unmatched events
      // In this case, we use the list of unmatched machinecode events as Tabu List, and
      // retry (with a retry limit)
      unsigned TriesLeft = 3;
      yaml::RelationGraph *RG = 0;
      while (TriesLeft-- > 0) {
        if (RG)
          delete RG; // also deletes scopes and nodes

        /// Create Graph
        yaml::RelationScope *DstScope = new yaml::RelationScope(
            yaml::Name(MF.getFunctionNumber()), yaml::level_machinecode);
        yaml::RelationScope *SrcScope = new yaml::RelationScope(
            yaml::Name(BF->getName()), yaml::level_bitcode);
        RG = new yaml::RelationGraph(SrcScope, DstScope);
        RG->getEntryNode()->setSrcBlock(
            yaml::FlowGraphTrait<const BasicBlock>::getName(
                &BF->getEntryBlock()));
        RG->getEntryNode()->setDstBlock(
            yaml::FlowGraphTrait<MachineBasicBlock>::getName(&MF.front()));
        UnmatchedEvents.clear();

        /// Event Maps
        EventMap<const BasicBlock*>::type IEventMap;
        EventMap<MachineBasicBlock*>::type MEventMap;

        /// Known and visited relation nodes
        std::map<ProgressID, yaml::RelationNode*> RMap;
        std::set<yaml::RelationNode*> RVisited;
        std::vector<std::pair<ProgressID, yaml::RelationNode*> > RTodo;

        /// Build event maps using RUnmatched as tabu list
        buildEventMaps(MF, IEventMap, MEventMap, TabuEvents);

        /// We first queue the entry node
        RTodo.push_back(
            std::make_pair(std::make_pair(&BF->getEntryBlock(), &MF.front()),
                RG->getEntryNode()));

        /// while there is an unprocessed progress node (n -> IBB,MBB)
        while (!RTodo.empty()) {
          std::pair<ProgressID, yaml::RelationNode*> Item = RTodo.back();
          RTodo.pop_back();
          yaml::RelationNode *RN = Item.second;
          if (RVisited.count(RN) > 0)
            continue;
          const BasicBlock *IBB = Item.first.first;
          MachineBasicBlock *MBB = Item.first.second;
          EventQueueMap<const BasicBlock*> IEvents;
          EventQueueMap<MachineBasicBlock*> MEvents;

          /// Expand both at the bitcode and machine level (starting with IBB and MBB, resp.),
          /// which results in new src/dst nodes being created, and two bitcode and machinecode-level maps from events
          /// to a list of (bitcode/machine block, list of RG predecessor blocks) pairs
          expandProgressNode(RG, RN, yaml::rnt_src, IBB, IEventMap, IEvents);
          expandProgressNode(RG, RN, yaml::rnt_dst, MBB, MEventMap, MEvents);

          /// For each event and corresponding bitcode list IList and machinecode MList, create a progress
          /// node (iblock,mblock) for every pair ((iblock,ipreds),(mblock,mpreds)) \in (IList x MList) and add
          /// edges from all ipreds and mpreds to that progress node
          addProgressNodes(RG, IEvents, MEvents, RMap, RTodo, UnmatchedEvents);
        }
        if (UnmatchedEvents.empty()) { // No unmatched events this time
          break;
        }
        else if (TabuEvents.empty()) {
          errs() << "[mc2yml] Warning: inconsistent initial mapping for "
              << MF.getFunction()->getName() << " (retrying)\n";
        }
        TabuEvents.insert(UnmatchedEvents.begin(), UnmatchedEvents.end());
      }
      if (!UnmatchedEvents.empty()) {
        errs() << "[mc2yml] Error: failed to find a correct event mapping for "
            << MF.getFunction()->getName() << ": ";
        for (std::set<StringRef>::iterator I = UnmatchedEvents.begin(), E =
            UnmatchedEvents.end(); I != E; ++I) {
          errs() << *I << ",";
        }
        errs() << "\n";
      }
      doc.addRelationGraph(RG);
    }

  private:

    /// Generate (heuristic) MachineBlock->EventName and IR-Block->EventName maps
    /// (1) if all forward-CFG predecessors of (MBB originating from BB) map to no or a different IR block,
    ///     MBB generates a BB event.
    /// (2) if there is a MBB generating a event BB, the basic block BB also generates this event
    void buildEventMaps(MachineFunction &MF,
                        std::map<const BasicBlock*,StringRef> &BitcodeEventMap,
                        std::map<MachineBasicBlock*,StringRef> &MachineEventMap,
                        std::set<StringRef> &TabuList);

    /// Add progress nodes after expanding the bitcode and machine code subgraphs
    void addProgressNodes(yaml::RelationGraph *RG,
        EventQueueMap<const BasicBlock*> &BitcodeEvents,
        EventQueueMap<MachineBasicBlock*> &MachineEvents,
        std::map<ProgressID, yaml::RelationNode*>& RMap,
        std::vector<std::pair<ProgressID, yaml::RelationNode*> >& RTodo,
        std::set<StringRef> &UnmatchedEvents);

    /// Check whether Source -> Target is a backedge
    bool isBackEdge(MachineBasicBlock *Source, MachineBasicBlock *Target);

    /// Expand progress node N either at the machine code (Block=MachineBasicBlock) or bitcode (Block=BasicBlock)
    /// level. The RelationGraphHelperTrait<BlockType> class provides the machine/bitcode specific
    /// functionality
    template<typename Block>
      void
      expandProgressNode(yaml::RelationGraph *RG,
          yaml::RelationNode *ProgressNode, yaml::RelationNodeType type,
          Block* StartBlock, typename EventMap<Block*>::type EventMap,
          EventQueueMap<Block*>& Events)
      {
        typedef yaml::FlowGraphTrait<Block> Trait;
        std::vector<std::pair<yaml::RelationNode*, Block*> > Queue;
        std::map<Block*, yaml::RelationNode*> Created;
        std::set<Block*> Visited;
        Queue.push_back(std::make_pair(ProgressNode, StartBlock));
        while (!Queue.empty()) {
          // expand unexpanded, queued items
          std::pair<yaml::RelationNode*, Block*> Item = Queue.back();
          Queue.pop_back();
          yaml::RelationNode* RN = Item.first;
          Block* BB = Item.second;
          if (Visited.count(BB) > 0)
            continue;
          Visited.insert(BB);
          for (typename Trait::succ_iterator I = Trait::succ_begin(BB), E =
              Trait::succ_end(BB); I != E; ++I) {
            Block *BB2 = *I;
            if (EventMap.count(BB2) == 0) {
              // successor generates no event -> add 'type' relation node, queue successor
              if (Created.count(BB2) == 0) {
                DEBUG(dbgs() << "Internal node for "
                             << Trait::getName(BB2).getName() << "("
                             << ((type==yaml::rnt_src) ? "src" : "dst")
                             << ") created\n");
                yaml::RelationNode *NewRelationNode = RG->addNode(type);
                NewRelationNode->setBlock(Trait::getName(BB2),
                    type == yaml::rnt_src);
                Created.insert(std::make_pair(BB2, NewRelationNode));
              }
              yaml::RelationNode *RN2 = Created[BB2];
              RN->addSuccessor(RN2, type == yaml::rnt_src);
              Queue.push_back(std::make_pair(RN2, BB2));
            }
            else {
              // successor generates event -> queue event
              Events.addItem(EventMap[BB2], BB2, RN);
            }
          }
          // No successors -> exit event
          if (Trait::succ_begin(BB) == Trait::succ_end(BB)) {
            Events.addExitPredecessor(RN);
          }
        }
      }

  };

  /// PMLExportManager - Handle creation of YAML output, stream
  /// writing, initialization of PML-exporter, ..
  class PMLExportManager {

    TargetMachine *TM;

    std::string OutFileName;
    tool_output_file *OutFile;
    yaml::Output *Output;

    std::vector<PMLExport*> Exporter;

  public:

    PMLExportManager(std::string& filename, TargetMachine *TM)
     : TM(TM), OutFileName(filename), OutFile(0), Output(0)
    {}

    ~PMLExportManager() {
      while (!Exporter.empty()) {
        delete Exporter.back();
        Exporter.pop_back();
      }
    }

    void addExporter(PMLExport* PE) { Exporter.push_back(PE); }

    void addDefaultExporter() {
      Exporter.push_back(new PMLExport(TM));
    }

    void initialize();

    void finalize();

    template <typename ArchTrait>
    void serialize(MachineFunction &MF, MachineLoopInfo* LI,
                   yaml::Doc<ArchTrait>& YDoc)
    {
      for (std::vector<PMLExport*>::iterator it = Exporter.begin(),
           end = Exporter.end(); it != end; it++) {
        (*it)->serialize(MF, LI, YDoc);
      }
    }

    template <typename ArchTrait>
    void writeDoc(yaml::Doc<ArchTrait>& YDoc) {
      *Output << YDoc;
    }
  };


} // end namespace llvm


#endif
