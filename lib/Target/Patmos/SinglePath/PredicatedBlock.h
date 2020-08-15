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
#include "llvm/IR/Metadata.h"
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

    /// A definition of a predicate that is defined by a block, I.e. at runtime,
    /// the predicate's true/false value is calculated in the block.
    struct Definition{
      /// The predicate being defined
      unsigned predicate; 

      /// The guard under which the condition is calculated
      unsigned guard;

      /// The block that uses the predicate to guard some of its instructions.
      const PredicatedBlock* useBlock;

      /// Which predicate register the condition result is in.
      MachineOperand condPred; 

      /// Whether the condition predicate register has the negate flag on it.
      /// I.e. if the condition is in '$p1', then this flag is disabled, while '!p1'
      /// has it enabled.
      MachineOperand condFlag;

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
      for( auto instr_iter = MBB->instr_begin(), end = MBB->instr_end(); instr_iter != end; instr_iter++){
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
      printID(os.indent(indent)) << ":\n";
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
      printInstructions(os, indent + 2);
    }

    raw_ostream& printID(raw_ostream& os) const {
      return os << "BB#" << MBB->getNumber()<< " [" << this << "](" << MBB << ")";
    }

    void printInstructions(raw_ostream& os, unsigned indent) const{
      auto MF = MBB->getParent();
      os.indent(indent) << MBB->getFullName() <<":\n";
      for( auto MI = MBB->instr_begin(), ME = MBB->getFirstInstrTerminator();
              MI != ME; ++MI) {
        auto instructionPredicates = getInstructionPredicates();
        os.indent(indent + 2) << "[" << &(*MI) << "](";
        if(instructionPredicates.count(MI)){
          os << getInstructionPredicates().at(MI);
        }else{
          os << "-";
        }
        os << ") ";
        MI->print(os, &(MF->getTarget()), false);
        printMetaData(&(*MI), os);
      }
      os.indent(indent + 2) << "-\n";
      for( auto MI = MBB->getFirstTerminator(), ME = MBB->end();
              MI != ME; ++MI) {
        auto instructionPredicates = getInstructionPredicates();
        os.indent(indent + 2) << "[" << &(*MI) << "](";
        if(instructionPredicates.count(MI)){
          os << getInstructionPredicates().at(MI);
        }else{
          os << "-";
        }
        os << ") ";
        MI->print(os, &(MF->getTarget()), false);
        printMetaData(&(*MI), os);
      }
      os << "\n";
    }

    /// Returns a list of definitions assigned to this block.
    /// The list is ordered, such that the first definition's code should
    /// precede the second definition's in the block. The ordering
    /// ensures that no predicate overwritten by one definition is
    /// a guard of a following definition.
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
    void addDefinition(Definition newDef)
    {
      auto earliest_insert_pos = Definitions.begin(), latest_insert_pos = Definitions.end();

      for(auto iter = Definitions.begin(), end = Definitions.end(); iter<end; iter++) {
        auto def = *iter;
        if(def.guard == newDef.predicate) {
          earliest_insert_pos = std::next(iter);
        }
        if(def.predicate == newDef.guard && iter < latest_insert_pos){
          latest_insert_pos = iter;
        }
      }
      assert(earliest_insert_pos <= latest_insert_pos && "Couldn't find a valid definition ordering");

      Definitions.insert(latest_insert_pos, newDef);
    }

    /// Removes all definitions assigned to this block.
    void dropDefinitions()
    {
      Definitions.clear();
    }

    /// Gets the list of blocks that success this one,
    /// but are not part of the same loop.
    /// 'Exit' refers to the edge between this block and the 
    /// target 'exiting' the loop.
    std::vector<const PredicatedBlock*> getExitTargets() const
    {
      return std::vector<const PredicatedBlock*>(ExitTargets.begin(), ExitTargets.end());
    }

    /// Assign the given block as an exit target of this one.
    /// An exit target is a successor block to this block
    /// that is not part of the same loop.
    /// 'Exit' refers to the edge between this block and the 
    /// target 'exiting' the loop.
    void addExitTarget(const PredicatedBlock *block)
    {
      assert(std::find_if(MBB->succ_begin(), MBB->succ_end(), [&](auto o){return o == block->getMBB();}) != MBB->succ_end());
      ExitTargets.push_back(block);
    }

    /// Merge the given block into this one, adding all its
    /// predicates, successors, definitions, etc. to this one.
    void merge(const PredicatedBlock* b2)
    {
      InstrPred.insert(b2->InstrPred.begin(), b2->InstrPred.end());

	  // Ensure new definitions are ordered correctly.
	  for(auto def: b2->Definitions){
		addDefinition(def);
	  }

      ExitTargets.insert(ExitTargets.end(), b2->ExitTargets.begin(), b2->ExitTargets.end());
      Remnants.insert(b2->Remnants.begin(), b2->Remnants.end());
      Remnants.insert(b2->getMBB());
      Successors.insert(b2->Successors.begin(), b2->Successors.end());
    }

    /// Replace any reference to the first block by references to the second block.
    void replaceUseOfBlockWith(PredicatedBlock* oldBlock, PredicatedBlock* newBlock)
    {
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

    /// Gets the list of MBB that were bundled with this block using 'merge()'
	/// or 'replaceMbb()'
    std::set<MachineBasicBlock*> bundledMBBs() const 
    {
      return std::set<MachineBasicBlock*>(Remnants.begin(), Remnants.end());
    }

    /// Adds the given block as a successor of this one with
    /// the given predicate guarding the branch leading to that block.
    void addSuccessor(const PredicatedBlock *block, unsigned pred)
    {
      Successors.insert(std::make_pair(block, pred));
    }

    /// Gets all successors to this block with which predicates
    /// that dictate whether their respective branches are taken.
    std::map<const PredicatedBlock*, unsigned> getSuccessors() const 
    {
      return std::map<const PredicatedBlock*, unsigned>(Successors.begin(), Successors.end());
    }

    /// Replaces which MBB this block is managing.
    /// The old MBB is added to the list of 'bundled blocks'
    void replaceMbb(MachineBasicBlock* newMbb)
    {
      Remnants.insert(MBB);
      MBB = newMbb;
    }

    /// Whether this block is a result of bundling at least 2 blocks.
    bool bundled() const 
    {
      return !Remnants.empty();
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
    /// The list must be ordered, such that the first definition's code should
    /// precede the second definition's in the block. The ordering
    /// ensures that no predicate overwritten by one definition is
    /// a guard of a following definition.
    std::vector<Definition> Definitions;

    std::vector<const PredicatedBlock*> ExitTargets;

    /// A map over successor blocks and which predicates take that branch.
    std::map<const PredicatedBlock*, unsigned> Successors;

    void printMetaData(MachineInstr* instr, raw_ostream& os) const {
      for(int i = 0; i< instr->getNumOperands(); i++){
        auto &p = instr->getOperand(i);
        if ( p.isMetadata()){
          const MDNode *n = p.getMetadata();
          errs() << "\t^MetaData: ";
          n->print(errs());
          errs() << "\n";
        }
      }
    }

  };

  /// Untemplated version of _PredicatedBlock. To be used by non-test code.
  typedef _PredicatedBlock<MachineBasicBlock, MachineInstr, MachineOperand> PredicatedBlock;

}

#endif /* TARGET_PATMOS_SINGLEPATH_PREDICATEDBLOCK_H_ */
