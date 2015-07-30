//===-- PatmosCallGraphBuilder.cpp - Codegen-based call graph construction.===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Build a module-level call graph at the machine-level.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-call-graph-builder"

#include "PatmosCallGraphBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <set>

using namespace llvm;

INITIALIZE_PASS(PatmosCallGraphBuilder, "patmos-mcg",
                "Patmos Call Graph Builder", false, true)

namespace llvm {
  char PatmosCallGraphBuilder::ID = 0;

  const char *MCallGraph::EntrySymbol = "_start";

  //----------------------------------------------------------------------------

  MCGSite *MCGNode::findSite(const MachineInstr *MI) const
  {
    for(MCGSites::const_iterator i(Sites.begin()), ie(Sites.end()); i != ie;
        i++) {
      if ((*i)->getMI() == MI)
        return *i;
    }

    return NULL;
  }

  std::string MCGNode::getLabel() const
  {
    std::string tmps;
    raw_string_ostream tmp(tmps);

    if (isUnknown())
      tmp << "<UNKNOWN-"<< *T << ">";
    else
      tmp << MF->getFunction()->getName();

    return tmps;
  }

  void MCGNode::print(raw_ostream &OS) const {
    OS << (IsInSCC ? "*" : "") << getLabel();
  }

  void MCGNode::dump() const
  {
    dbgs() << "MCGNode: " << getLabel() << "\n";
  }

  //----------------------------------------------------------------------------

  void MCGSite::print(raw_ostream &OS) const {
    if (MI) {
      MachineBasicBlock *MBB = MI->getParent();
      OS << "BB#" << MBB->getNumber() << ":"
             << std::distance(MBB->instr_begin(),
                              MachineBasicBlock::instr_iterator(MI)) << ":";
    }

    OS << *Caller << (IsInSCC ? " *--> " : " --> ") << *Callee;
  }

  void MCGSite::dump(bool short_format) const
  {
    dbgs() << *this;
    if (!short_format && MI)
      dbgs() << "\t" << *MI;
    dbgs() << "\n";
  }

  //----------------------------------------------------------------------------

  static bool isEmptyStructPointer(Type *Ty)
  {
    if (PointerType *PTy = Ty ? dyn_cast<PointerType>(Ty) : NULL) {
      if (StructType *STy = dyn_cast<StructType>(PTy->getElementType())) {
        return (STy->getNumContainedTypes() == 0);
      }
    }

    return false;
  }

  // return value: -1 recursive dependence
  //                0 not isomorphic
  //                1 isomorphic
  int MCallGraph::areTypesIsomorphic(Type *DstTy, Type *SrcTy)
  {
    if (DstTy == NULL || SrcTy == NULL)
      return DstTy == SrcTy;

    // normalize parameter order
    if (DstTy > SrcTy)
      return areTypesIsomorphic(SrcTy, DstTy);

    // TODO: this is a hack to fix-up an error in clang's LLVM code generator,
    // which messes up certain structs.
    if (isEmptyStructPointer(DstTy) && SrcTy->isPointerTy())
      return 1;

    // TODO: this is a hack to fix-up an error in clang's LLVM code generator,
    // which messes up certain structs.
    if (isEmptyStructPointer(SrcTy) && DstTy->isPointerTy())
      return 1;

    // Two types with differing kinds are clearly not isomorphic.
    if (DstTy->getTypeID() != SrcTy->getTypeID())
      return 0;

    // check for recursion and known equivalent types
    equivalent_types_t::const_iterator tmp(EQ.find(std::make_pair(DstTy,
                                                                  SrcTy)));
    if (tmp != EQ.end()) {
      return tmp->second;
    }

    // Two identical types are clearly isomorphic.  Remember this
    // non-speculatively.
    if (DstTy == SrcTy) {
      return 1;
    }

    // Okay, we have two types with identical kinds that we haven't seen before.

    // assume types are not identical for now
    EQ[std::make_pair(DstTy, SrcTy)] = 0;

    // If this is an opaque struct type, special case it.
    if (StructType *SSTy = dyn_cast<StructType>(SrcTy)) {
      if (SSTy->isOpaque()) {
        assert(false);
        return 1;
      }

      if (cast<StructType>(DstTy)->isOpaque()) {
        assert(false);
        return 1;
      }
    }

    // If the number of subtypes disagree between the two types, then we fail.
    if (SrcTy->getNumContainedTypes() != DstTy->getNumContainedTypes())
      return 0;

    // Fail if any of the extra properties (e.g. array size) of the type disagree.
    if (isa<IntegerType>(DstTy))
      return 0;  // bitwidth disagrees.
    if (PointerType *PT = dyn_cast<PointerType>(DstTy)) {
      if (PT->getAddressSpace() != cast<PointerType>(SrcTy)->getAddressSpace())
        return 0;

    } else if (FunctionType *FT = dyn_cast<FunctionType>(DstTy)) {
      if (FT->isVarArg() != cast<FunctionType>(SrcTy)->isVarArg())
        return 0;
    } else if (StructType *DSTy = dyn_cast<StructType>(DstTy)) {
      StructType *SSTy = cast<StructType>(SrcTy);
      if (DSTy->isLiteral() != SSTy->isLiteral() ||
          DSTy->isPacked() != SSTy->isPacked())
        return 0;
    } else if (ArrayType *DATy = dyn_cast<ArrayType>(DstTy)) {
      if (DATy->getNumElements() != cast<ArrayType>(SrcTy)->getNumElements())
        return 0;
    } else if (VectorType *DVTy = dyn_cast<VectorType>(DstTy)) {
      if (DVTy->getNumElements() != cast<ArrayType>(SrcTy)->getNumElements())
        return 0;
    }

    // Otherwise, we speculate that these two types will line up and recursively
    // check the subelements.

    // ok, we need to recurs, mark the types accordingly.
    EQ[std::make_pair(DstTy, SrcTy)] = -1;

    int retval = 1;
    for (unsigned i = 0, e = SrcTy->getNumContainedTypes(); i != e; ++i) {
      switch (areTypesIsomorphic(DstTy->getContainedType(i),
                                 SrcTy->getContainedType(i))) {
      case 0:
        // ok, types do not match
        EQ[std::make_pair(DstTy, SrcTy)] = 0;
        return 0;
      case -1:
        retval = -1;
        break;
      }
    }

    // seems the types match
    EQ[std::make_pair(DstTy, SrcTy)] = retval;

    // If everything seems to have lined up, then everything is great.
    return retval;
  }

  bool MCallGraph::isInSCC(MachineInstr *MI)
  {
    if (!MI)
      return false;

    MachineBasicBlock *MBB = MI->getParent();
    MachineFunction *MF = MBB->getParent();

    for(scc_iterator<MachineFunction*> i(scc_begin(MF)), ie(scc_end(MF));
        i != ie; i++)
    {
      if (i.hasLoop())
      {
        if (std::find((*i).begin(), (*i).end(), MBB) != (*i).end())
          return true;
      }
    }

    return false;
  }

  MCGNode *MCallGraph::makeMCGNode(MachineFunction *MF)
  {
    // does a call graph node for the machine function exist?
    for(MCGNodes::const_iterator i(Nodes.begin()), ie(Nodes.end()); i != ie;
        i++) {
      if ((*i)->getMF() == MF)
        return *i;
    }

    // construct a new call graph node for the MachineFunction
    MCGNode *newMCGN = new MCGNode(MF);
    Nodes.push_back(newMCGN);

    return newMCGN;
  }

  MCGNode *MCallGraph::getUnknownNode(Type *T)
  {
    // does a call graph node for the Type exist?
    for(MCGNodes::const_iterator i(Nodes.begin()), ie(Nodes.end()); i != ie;
        i++) {
      if ((*i)->isUnknown()) {
        if (areTypesIsomorphic((*i)->getType(), T)) {
          // mark all elements with -1 as 1
          for(equivalent_types_t::iterator j(EQ.begin()), je(EQ.end()); j != je;
              j++) {
            j->second = j->second * j->second;
          }
          return *i;
        }
        else {
          // erase all elements with -1
          for(equivalent_types_t::iterator j(EQ.begin()), je(EQ.end());
              j != je;) {
            if (j->second == -1)
              EQ.erase(j++);
            else
              j++;
          }
        }
      }
    }

    // construct a new call graph node for the Type
    MCGNode *newMCGN = new MCGNode(T);
    Nodes.push_back(newMCGN);

    return newMCGN;
  }

  MCGSite *MCallGraph::makeMCGSite(MCGNode *Caller, MachineInstr *MI,
                                   MCGNode *Callee)
  {
    // TODO: check also for non-natural loops here
    // check if the call site is in a loop
    bool is_site_in_SCC = isInSCC(MI);

    // allocate the call site
    MCGSite *newSite = new MCGSite(Caller, MI, Callee, is_site_in_SCC);

    // store the site with the graph
    Sites.push_back(newSite);

    // append the site to the caller call graph node
    Caller->Sites.push_back(newSite);

    // append the site to the calling sites of the call graph node
    Callee->CallingSites.push_back(newSite);

    return newSite;
  }

  void MCallGraph::markNodesInSCC()
  {
    // Work list of nodes being
    typedef std::set<MCGNode*> MCGNodeSet;
    MCGNodeSet WL;
    for(scc_iterator<MCallGraph> i(scc_begin(*this)), ie(scc_end(*this));
        i != ie; i++)
    {
      // See if the node is in an SCC of the call graph --> mark it directly ...
      if (i.hasLoop())
      {
        WL.insert((*i).begin(), (*i).end());
      }
      else
      {
        // ok the node is not in an SCC of the call graph, but maybe one of
        // its call sites it in a loop
        MCGNode *MCGN = *(*i).begin();
        const MCGSites &calling(MCGN->getCallingSites());
        for(MCGSites::const_iterator j(calling.begin()), je(calling.end());
            j != je; j++)
        {
          if ((*j)->isInSCC())
          {
            WL.insert(MCGN);
            break;
          }
        }
      }
    }

    // now mark all descendents of a node
    while (!WL.empty())
    {
      // pop an element from the work list
      MCGNode *MCGN = *WL.begin();
      WL.erase(MCGN);

      // skip dead nodes
      if (MCGN->isDead())
        continue;

      // mark it as being in an SCC
      MCGN->IsInSCC = true;

      // mark all its descendents as well
      const MCGSites &called(MCGN->getSites());
      for(MCGSites::const_iterator j(called.begin()), je(called.end()); j != je;
          j++)
      {
        MCGNode *callee = (*j)->getCallee();
        if (!callee->isInSCC())
        {
          WL.insert(callee);
        }
      }
    }
  }

  void MCallGraph::dump() const
  {
    for(MCGSites::const_iterator i(Sites.begin()), ie(Sites.end()); i != ie;
        i++) {
      dbgs() << "  ";
      (*i)->dump(false);
      dbgs() << "\n";
    }
  }

  void MCallGraph::view() const
  {
    ViewGraph(*this, "MCallGraph");
  }

  MCallGraph::~MCallGraph()
  {
    for(MCGNodes::const_iterator i(Nodes.begin()), ie(Nodes.end());
        i != ie; i++) {
      delete *i;
    }

    for(MCGSites::const_iterator i(Sites.begin()), ie(Sites.end()); i != ie;
        i++) {
      delete *i;
    }
  }

  //----------------------------------------------------------------------------

  void MCallSubGraph::view()
  {
    ViewGraph(*this, "MCallSubGraph");
  }

  //----------------------------------------------------------------------------

  void PatmosCallGraphBuilder::visitCallSites(const Module &M, MachineFunction *MF)
  {
    // get the machine-level module information.
    MachineModuleInfo &MMI(getAnalysis<MachineModuleInfo>());

    // make a call graph node, also for functions that are never called.
    MCGNode *MCGN = MCG.makeMCGNode(MF);

    for(MachineFunction::iterator i(MF->begin()), ie(MF->end()); i != ie;
        i++) {
      for(MachineBasicBlock::instr_iterator j(i->instr_begin()),
          je(i->instr_end()); j != je; j++) {

        if (j->isCall()) {
          // get target
          const MachineOperand &MO(j->getOperand(2));

          const Function *F = NULL;
          Type *T = NULL;

          // try to find the target of the call
          if (MO.isGlobal()) {
            // is the global value a function?
            F = dyn_cast<Function>(MO.getGlobal());
          }
          else if (MO.isSymbol()) {
            // find the function in the current module
            F = dyn_cast_or_null<Function>(M.getNamedValue(MO.getSymbolName()));
          }

          if (j->hasOneMemOperand()) {
            // try at least to get the function's type
            const Value *Callee = (*j->memoperands_begin())->getValue();
            T = Callee ? Callee->getType() : NULL;
          }

          // does a MachineFunction exist for F?
          MachineFunction *MF = F ? MMI.getMachineFunction(F) : NULL;

          // construct a new call site
          MCG.makeMCGSite(MCGN, j,
                          MF ? MCG.makeMCGNode(MF) : MCG.getUnknownNode(T));
        }
      }
    }
  }

  MCGNode *PatmosCallGraphBuilder::getMCGNode(const Module &M, const char *name)
  {
    Function *F = dyn_cast_or_null<Function>(M.getNamedValue(name));
    if (F) {
      // get the machine-level module information for M.
      MachineModuleInfo &MMI(getAnalysis<MachineModuleInfo>());

      // get the MachineFunction
      MachineFunction *MF = MMI.getMachineFunction(F);

      if (MF)
        return MCG.makeMCGNode(MF);
    }

    return NULL;
  }

  void PatmosCallGraphBuilder::markLive_(MCGNode *N)
  {
    // skip nodes that have been visited before
    if (!N || !N->isDead())
      return;

    N->markLive();

    for(MCGSites::const_iterator i(N->getSites().begin()),
        ie(N->getSites().end()); i != ie; i++) {
      markLive_((*i)->getCallee());
    }
  }

  void PatmosCallGraphBuilder::markLive(MCGNode *N)
  {
    if (!N)
      return;

    N->markLive();

    for(MCGSites::const_iterator i(N->getSites().begin()),
        ie(N->getSites().end()); i != ie; i++) {
      markLive_((*i)->getCallee());
    }
  }

  bool PatmosCallGraphBuilder::runOnMachineModule(const Module &M)
  {
    // get the machine-level module information for M.
    MachineModuleInfo &MMI(getAnalysis<MachineModuleInfo>());

    // visit all functions in the module
    for(Module::const_iterator i(M.begin()), ie(M.end()); i != ie; i++) {
      // get the machine-level function
      MachineFunction *MF = MMI.getMachineFunction(i);

      // find all call-sites in the MachineFunction
      if (MF) {
        MCGNode *MCGN = MCG.makeMCGNode(MF);

        // visit each call site within that function
        visitCallSites(M, MF);

        // represent external callers
        const Function *F = MF->getFunction();
        Type *T = F ? F->getType() : NULL;
        if (i->hasAddressTaken() && F->getName() != MCallGraph::EntrySymbol) {
          MCG.makeMCGSite(MCG.getUnknownNode(T), NULL, MCGN);
        }
      }
    }

    // discover live/dead functions
    MCGNode *entry = MCG.getEntryNode();
    if (entry)
      markLive(entry);

    // Mark live nodes to be within SCCs (loops or recursion)
    MCG.markNodesInSCC();

    DEBUG(
      std::string tmp;
      raw_fd_ostream of("mcg.dot", tmp);
      WriteGraph(of, MCG);
    );

    return false;
  }
}

/// createPatmosCallGraphBuilder - Returns a new PatmosCallGraphBuilder.
ModulePass *llvm::createPatmosCallGraphBuilder() {
  return new PatmosCallGraphBuilder();
}

