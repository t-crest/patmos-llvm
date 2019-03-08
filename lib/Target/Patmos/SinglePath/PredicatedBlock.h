//==-- PredicatedBlock.cpp - A predicated MachineBasicBlock --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
//===---------------------------------------------------------------------===//

#ifndef TARGET_PATMOS_SINGLEPATH_PREDICATEDBLOCK_H_
#define TARGET_PATMOS_SINGLEPATH_PREDICATEDBLOCK_H_

#include "llvm/CodeGen/MachineBasicBlock.h"

#include <map>
#include <set>

namespace llvm {

  /// We template PredicatedBlock such that we can use mocked MBBs when testing it.
  /// This template shouldn't be used directly outside test code, instead use 'PredicatedBlock'.
  template<class MachineBasicBlock, class MachineInstr>
  class _PredicatedBlock {
    /// For easy reference
    typedef _PredicatedBlock<MachineBasicBlock, MachineInstr> PredicatedBlock;
  public:

    /// Constructs a new instance where all instructions in the
    /// given MBB are predicated by the given predicate.
    _PredicatedBlock(MachineBasicBlock *mbb):
      MBB(*mbb)
    {}

    /// Get the MachineBasicBlock
    MachineBasicBlock *getMBB() const
    {
      return &MBB;
    }

    /// Get the list of predicates the MBBs instructions
    /// are predicated by
    std::set<unsigned> getBlockPredicates() const
    {
      std::set<unsigned> result;
      for(auto const &pair: InstrPred)
      {
        result.insert(pair.second);
      }
      return result;
    }

    /// Sets all of the MBB's instructions to be predicated by the given predicate.
    /// Should be used with care.
    void setPredicate(unsigned pred)
    {
      InstrPred.clear();
      for( auto instr_iter = MBB.begin(), end = MBB.end(); instr_iter != end; instr_iter++){
        MachineInstr* instr = &(*instr_iter);
        assert(InstrPred.find(instr) == InstrPred.end());
        InstrPred.insert(std::make_pair(instr, pred));
      }
    }

    void dump(raw_ostream& os, unsigned indent) const
    {
      os.indent(indent) << "PredicatedBlock(" << &MBB << "):\n";
      os.indent(indent + 2) << "InstrPreds:{";
      for(auto pair: InstrPred)
      {
        os << "(" << pair.first << "," << pair.second << "), ";
      }
      os <<"}\n";
      os.indent(indent + 2) << "Definitions:{";
      for(auto t: Definitions)
      {
        os << "(" << std::get<0>(t) << ", " << std::get<1>(t) << ", " << std::get<2>(t) << "), ";
      }
      os <<"}\n";
      os.indent(indent + 2) << "ExitTargets:{";
      for(auto t: ExitTargets)
      {
        os << t << ", ";
      }
      os <<"}\n";
    }

    /// Returns a list of predicates that are defined by this block, paired with the block
    /// that uses the predicate and the guard predicate of the condition that gives it value.
    /// The first element is the predicate being defined, the second is the block that use the
    /// the predicate, and the last element is the guard of the defining condition.
    /// A predicate definition is where it gets its true/false value that the next
    /// block uses to predicate some of its instructions.
    std::vector<std::tuple<unsigned, const PredicatedBlock*, unsigned>>
    getDefinitions() const
    {
      std::vector<std::tuple<unsigned, const PredicatedBlock*, unsigned>> result;
      result.insert(result.end(), Definitions.begin(), Definitions.end());
      return result;
    }

    /// Add a predicate definition to this block, paired with the block that uses
    /// that predicate and the predicate of teh condition tha gives it value.
    /// A predicate definition is where it gets its true/false value that the next
    /// block uses to predicate some of its instructions.
    void addDefinition(unsigned pred, const PredicatedBlock* b, unsigned guardPred)
    {
      Definitions.push_back(std::make_tuple(pred, b, guardPred));
    }

    std::vector<const PredicatedBlock*> getExitTargets() const
    {
      return std::vector<const PredicatedBlock*>(ExitTargets.begin(), ExitTargets.end());
    }

    void addExitTarget(const PredicatedBlock *block)
    {
      assert(std::find_if(MBB.succ_begin(), MBB.succ_end(), [&](auto o){return o == block->getMBB();}) != MBB.succ_end());
      ExitTargets.push_back(block);
    }

  private:

    /// The MBB that this instance manages the predicates for.
    MachineBasicBlock &MBB;

    /// A mapping of which predicate each instruction is predicated by.
    std::map<MachineInstr*, unsigned> InstrPred;

    /// A list of predicates that are defined by this block, I.e. at runtime
    /// the predicate's true/false value is calculated in this block.
    /// The second element is a block that uses the predicate
    /// to predicate some of its instructions.
    /// The last element is the predicate of the condition that gives the first
    /// element its value.
    std::vector<std::tuple<unsigned, const PredicatedBlock*, unsigned>> Definitions;

    std::vector<const PredicatedBlock*> ExitTargets;
  };

  /// Untemplated version of _PredicatedBlock. To be used by non-test code.
  typedef _PredicatedBlock<MachineBasicBlock, MachineInstr> PredicatedBlock;

}

#endif /* TARGET_PATMOS_SINGLEPATH_PREDICATEDBLOCK_H_ */
