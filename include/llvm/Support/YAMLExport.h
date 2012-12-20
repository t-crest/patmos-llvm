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
    Name BranchTarget;
    GenericMachineInstruction(uint64_t Index) : Instruction(Index), BranchType(branch_none) {}
};
template <>
struct MappingTraits<GenericMachineInstruction> {
    static void mapping(IO &io, GenericMachineInstruction& Ins) {
        MappingTraits<Instruction>::mapping(io,Ins);
        io.mapOptional("size",          Ins.Size, (uint64_t) 4);
        io.mapOptional("branch-type",   Ins.BranchType, branch_none);
        io.mapOptional("branch-target", Ins.BranchTarget, yaml::Name(""));
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
    Doc(StringRef version) : FormatVersion(version), Architecture(Arch::ArchName) {}
    ~Doc() {
        DELETE_MEMBERS(BitcodeFunctions);
        DELETE_MEMBERS(MachineFunctions);
    }
    /// Add a function, which is owned by the document afterwards
    void addFunction(BitcodeFunction *F) {
        BitcodeFunctions.push_back(F);
    }
    /// Add a machine function, which is owned by the document afterwards
    void addMachineFunction(typename Arch::MachineFunction* MF) {
        MachineFunctions.push_back(MF);
    }
};
template <typename Arch>
struct MappingTraits< Doc<Arch> > {
    static void mapping(IO &io, Doc<Arch>& doc) {
        io.mapRequired("format",   doc.FormatVersion);
        io.mapRequired("arch",     doc.Architecture);
        io.mapOptional("bitcode-functions",doc.BitcodeFunctions);
        io.mapOptional("machine-functions",doc.MachineFunctions);
    }
};


} // end namespace yaml
} // end namespace llvm

#endif
