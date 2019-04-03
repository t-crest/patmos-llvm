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
  template<class MachineBasicBlock, class MachineInstr, class MachineOperand>
  class _PredicatedBlock {
    /// For easy reference
    typedef _PredicatedBlock<MachineBasicBlock, MachineInstr, MachineOperand> PredicatedBlock;
  public:

    struct Definition{
      unsigned predicate, guard;
      const PredicatedBlock* useBlock;
      MachineOperand condPred, condFlag;

      bool operator==(const Definition &o) const{
        return
            predicate == o.predicate  &&
            guard     == o.guard      &&
            useBlock  == o.useBlock;
      }
    };

    /// Constructs a new instance where all instructions in the
    /// given MBB are predicated by the given predicate.
    _PredicatedBlock(MachineBasicBlock *mbb):
      MBB(mbb)
    {}

    /// Get the MachineBasicBlock
    MachineBasicBlock *getMBB() const
    {
      return MBB;
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
      for( auto instr_iter = MBB->begin(), end = MBB->getFirstTerminator(); instr_iter != end; instr_iter++){
        MachineInstr* instr = &(*instr_iter);
        assert(InstrPred.find(instr) == InstrPred.end());
        InstrPred.insert(std::make_pair(instr, pred));
      }

      // Reassign successor predicates
      for(auto iter = Successors.begin(), end = Successors.end(); iter != end; iter++){
        iter->second = pred;
      }
    }

    std::map<const MachineInstr*, unsigned> getInstructionPredicates() const {
      std::map<const MachineInstr*, unsigned> result;
      result.insert(InstrPred.begin(), InstrPred.end());
      return result;
    }

    void dump(raw_ostream& os, unsigned indent) const
    {
      os.indent(indent) << "PredicatedBlock(" << MBB << "):\n";
      os.indent(indent + 2) << "InstrPreds:{";
      for(auto pair: InstrPred)
      {
        os << "(" << pair.first << "," << pair.second << "), ";
      }
      os <<"}\n";
      os.indent(indent + 2) << "Definitions:{";
      for(auto def: Definitions)
      {
        os << "(" << def.predicate << ", " << def.guard << ", " << def.useBlock << "), ";
      }
      os <<"}\n";
      os.indent(indent + 2) << "ExitTargets:{";
      for(auto t: ExitTargets)
      {
        os << t << ", ";
      }
      os <<"}\n";
      os.indent(indent + 2) << "Successors:{";
      for(auto t: Successors)
      {
        os << "(" << t.first << ", " << t.second << "), ";
      }
      os <<"}\n";
      os.indent(indent + 2) << "Remnants:{";
      for(auto t: Remnants)
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
    std::vector<Definition>
    getDefinitions() const
    {
      std::vector<Definition> result;
      result.insert(result.end(), Definitions.begin(), Definitions.end());
      return result;
    }

    /// Add a predicate definition to this block, paired with the block that uses
    /// that predicate and the predicate of the condition that gives it value.
    /// A predicate definition is where it gets its true/false value that the next
    /// block uses to predicate some of its instructions.
    void addDefinition(unsigned pred, unsigned guard, const PredicatedBlock* useBlock,
        MachineOperand condition, MachineOperand condFlag)
    {
      Definitions.push_back(Definition{pred, guard, useBlock, condition, condFlag});
    }

    std::vector<const PredicatedBlock*> getExitTargets() const
    {
      return std::vector<const PredicatedBlock*>(ExitTargets.begin(), ExitTargets.end());
    }

    void addExitTarget(const PredicatedBlock *block)
    {
      assert(std::find_if(MBB->succ_begin(), MBB->succ_end(), [&](auto o){return o == block->getMBB();}) != MBB->succ_end());
      ExitTargets.push_back(block);
    }

    void bundleWith(const PredicatedBlock* b2){
      auto mbb1 = getMBB(), mbb2 = b2->getMBB();
      assert(mbb1->getParent() == mbb2->getParent());

      while(mbb2->begin() != mbb2->getFirstTerminator()){
        auto inst = &(*mbb2->begin());
        mbb2->remove(inst);
        mbb1->insert(mbb1->getFirstInstrTerminator(), inst);
        InstrPred.insert(std::make_pair(inst, b2->InstrPred.at(inst)));
      }

      while(mbb2->getFirstTerminator() != mbb2->end()){
        auto inst = &(*mbb2->begin());
        mbb2->remove(inst);
        mbb1->insert(mbb1->end(), inst);
      }

      Definitions.insert(Definitions.end(), b2->Definitions.begin(), b2->Definitions.end());
      ExitTargets.insert(ExitTargets.end(), b2->ExitTargets.begin(), b2->ExitTargets.end());
      Remnants.insert(b2->Remnants.begin(), b2->Remnants.begin());
      Remnants.insert(mbb2);
      Successors.insert(b2->Successors.begin(), b2->Successors.end());
    }

    void replaceUseOfBlockWith(PredicatedBlock* oldBlock, PredicatedBlock* newBlock){
      for(auto iter = Definitions.begin(), end = Definitions.end(); iter != end; iter ++){
        if((*iter).useBlock == oldBlock){
          (*iter).useBlock = newBlock;
        }
      }

      for(auto iter = ExitTargets.begin(), end = ExitTargets.end(); iter != end; iter ++){
        if(*iter == oldBlock){
          *iter = newBlock;
        }
      }

      auto found = std::find_if(Successors.begin(), Successors.end(),
          [&](auto succ){return succ.first == oldBlock;});
      if(found != Successors.end()){
        auto pred = found->second;
        Successors.erase(found);
        assert(std::find_if(Successors.begin(), Successors.end(),
                          [&](auto succ){return succ.first == oldBlock;}) == Successors.end());
        Successors.insert(std::make_pair(newBlock, pred));
      }
    }

    std::set<MachineBasicBlock*> bundledMBBs(){
      return std::set<MachineBasicBlock*>(Remnants.begin(), Remnants.end());
    }

    void addSuccessor(const PredicatedBlock *block, unsigned pred){
      Successors.insert(std::make_pair(block, pred));
    }

    std::map<const PredicatedBlock*, unsigned> getSuccessors() const {
      return std::map<const PredicatedBlock*, unsigned>(Successors.begin(), Successors.end());
    }

  private:

    /// The MBB that this instance manages the predicates for.
    MachineBasicBlock *MBB;

    /// The MBBs that were bundled into this blocks MBB.
    std::set<MachineBasicBlock*> Remnants;

    /// A mapping of which predicate each instruction is predicated by.
    std::map<const MachineInstr*, unsigned> InstrPred;

    /// A list of predicates that are defined by this block, I.e. at runtime
    /// the predicate's true/false value is calculated in this block.
    /// The second element is a block that uses the predicate
    /// to predicate some of its instructions.
    /// The last element is the predicate of the condition that gives the first
    /// element its value.
    std::vector<Definition> Definitions;

    std::vector<const PredicatedBlock*> ExitTargets;

    std::map<const PredicatedBlock*, unsigned> Successors;

  };

  /// Untemplated version of _PredicatedBlock. To be used by non-test code.
  typedef _PredicatedBlock<MachineBasicBlock, MachineInstr, MachineOperand> PredicatedBlock;

}

#endif /* TARGET_PATMOS_SINGLEPATH_PREDICATEDBLOCK_H_ */
