//===- lib/Support/YAMLExport.h -----------------------------------------===//
//
//               YAML definitions for exporting LLVM datastructures
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_YAML_EXPORT_H_
#define LLVM_YAML_EXPORT_H_

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"

/// Utility for declaring that a std::vector of a particular *pointer*type
/// should be considered a YAML sequence. Must only be used in namespace
/// llvm/yaml.
#define LLVM_YAML_IS_PSEQUENCE_VECTOR(_type)                                 \
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

} /* namespace yaml */ } /* namespace llvm */
LLVM_YAML_IS_FLOW_SEQUENCE_VECTOR(llvm::yaml::Name)
namespace llvm { namespace yaml {

/// Representation Level (source, bitcode, machinecode)
enum ReprLevel { level_bitcode, level_machinecode };
template <>
struct ScalarEnumerationTraits<ReprLevel> {
    static void enumeration(IO &io, ReprLevel& level) {
        io.enumCase(level, "bitcode", level_bitcode);
        io.enumCase(level, "machinecode", level_machinecode);
    }
};

/// Qualified reference to a basic block
struct BlockRef {
    Name Function;
    Name Block;
    BlockRef(Name fun, Name block) : Function(fun), Block(block) {}
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
};
template <>
struct MappingTraits<Instruction> {
    static void mapping(IO &io, Instruction& Ins) {
        io.mapRequired("index",   Ins.Index);
        io.mapOptional("opcode",  Ins.Opcode, (int64_t) -1);
        io.mapOptional("callees", Ins.Callees);
    }
};
LLVM_YAML_IS_PSEQUENCE_VECTOR(Instruction)

/// Instruction Specification (machine code)
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
struct MInstruction : Instruction {
    uint64_t Size;
    enum BranchType BranchType;
    Name BranchTarget;
    MInstruction(uint64_t Index) : Instruction(Index), BranchType(branch_none) {}
};
template <>
struct MappingTraits<MInstruction> {
    static void mapping(IO &io, MInstruction& Ins) {
        MappingTraits<Instruction>::mapping(io,Ins);
        io.mapOptional("size",          Ins.Size, (uint64_t) 4);
        io.mapOptional("branch-type",   Ins.BranchType, branch_none);
        io.mapOptional("branch-target", Ins.BranchTarget, yaml::Name(""));
    }
};
LLVM_YAML_IS_PSEQUENCE_VECTOR(MInstruction)

/// Basic Block Specification (generic)
template<typename InstructionT>
struct BlockT {
    Name BlockName;
    Name MapsTo;
    std::vector<Name> Successors;
    std::vector<Name> Predecessors;
    std::vector<Name> Loops;
    std::vector<InstructionT*> Instructions;
    BlockT(uint64_t index) : BlockName(index) {}
    ~BlockT() {
        while(! Instructions.empty()) {
            InstructionT* Ins = Instructions.back();
            delete Ins;
            Instructions.pop_back();
        }
    }
    /// Add an instruction to the block
    /// Block takes ownership of instruction
    InstructionT* addInstruction(InstructionT* Ins) {
        Instructions.push_back(Ins);
        return Ins;
    }
};

template <typename InstructionT>
struct MappingTraits< BlockT<InstructionT> > {
    static void mapping(IO &io, BlockT<InstructionT>& Block) {
        io.mapRequired("name",         Block.BlockName);
        io.mapRequired("successors",   Block.Successors);
        io.mapRequired("predecessors", Block.Predecessors);
        io.mapOptional("loops",        Block.Loops);
        io.mapOptional("mapsto",       Block.MapsTo);
        io.mapOptional("instructions", Block.Instructions);
    }
};
template<typename InstructionT>
struct SequenceTraits< std::vector<BlockT<InstructionT>*> > {
    static size_t size(IO &io, std::vector<BlockT<InstructionT>*> &seq) {
        return seq.size();
    }
    static BlockT<InstructionT>& element(IO &io, std::vector<BlockT<InstructionT>*> &seq, size_t index) {
        if ( index >= seq.size() )
          seq.resize(index+1);
        return *seq[index];
    }
};
typedef BlockT<Instruction>   Block;
typedef BlockT<MInstruction> MBlock;

/// basic functions
template <typename BlockT>
struct FunctionT {
    Name FunctionName;
    ReprLevel Level;
    Name MapsTo;
    StringRef Hash;
    std::vector<BlockT*> Blocks;
    FunctionT(StringRef name) : FunctionName(name) {}
    FunctionT(uint64_t name) : FunctionName(name) {}
    ~FunctionT() {
        while(! Blocks.empty()) {
            BlockT* Block = Blocks.back();
            delete Block;
            Blocks.pop_back();
        }
    }
    BlockT* addBlock(BlockT *B) {
        Blocks.push_back(B);
        return B;
    }
};
template <typename BlockT>
struct MappingTraits< FunctionT<BlockT> > {
    static void mapping(IO &io, FunctionT<BlockT>& fn) {
        io.mapRequired("function",    fn.FunctionName);
        io.mapRequired("level",   fn.Level);
        io.mapOptional("mapsto",  fn.MapsTo);
        io.mapOptional("hash",    fn.Hash);
        io.mapRequired("blocks",  fn.Blocks);
    }
};
typedef FunctionT<Block>   Function;
typedef FunctionT<MBlock> MFunction;

} // end namespace yaml
} // end namespace llvm

#endif
