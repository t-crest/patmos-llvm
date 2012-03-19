#include "llvm/ADT/SCCIterator.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Regex.h"
#include "llvm/Module.h"

#include <list>
#include <set>
#include <map>

using namespace llvm;

namespace {


  struct DomLeaves : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid

    DomLeaves() : FunctionPass(ID) {
      initializeDomLeavesPass(*PassRegistry::getPassRegistry());
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<DominatorTree>();
      AU.addRequired<PostDominatorTree>();
      AU.setPreservesAll();
    }

    virtual bool runOnFunction(Function &F);
  };
}

char DomLeaves::ID = 0;
INITIALIZE_PASS(DomLeaves, "domleaves",
                "prototype pass", false, false)

// Public interface to the pass
FunctionPass *llvm::createDomLeavesPass() {
  return new DomLeaves();
}

bool DomLeaves::runOnFunction(Function &F) {
  DominatorTree &DT = getAnalysis<DominatorTree>();
  PostDominatorTree &PDT = getAnalysis<PostDominatorTree>();

  struct leaves_t { std::set<BasicBlock*> DT, PDT; } leaves;

  for (Function::iterator I = F.begin(), E = F.end(); I != E; ++I) {
    DomTreeNode *N = DT.getNode(I);
    if (!N->getNumChildren()) {
      DEBUG(dbgs() << I->getName() << " is a dom-leaf\n");
      leaves.DT.insert(I);
    }
    N = PDT.getNode(I);
    if (!N->getNumChildren()) {
      DEBUG(dbgs() << I->getName() << " is a post-dom-leaf\n");
      leaves.PDT.insert(I);
    }
  }

  std::list<BasicBlock*> inters;
  std::set_intersection(leaves.DT.begin(), leaves.DT.end(),
                        leaves.PDT.begin(), leaves.PDT.end(),
                        std::back_inserter(inters));

  std::map<BasicBlock*, int> SCCMap;
  unsigned sccNum = 0;
  unsigned sccNonTriv = 0;
  DEBUG(errs() << "SCCs for Function " << F.getName() << " in PostOrder:");
  for (scc_iterator<Function*> SCCI = scc_begin(&F),
         E = scc_end(&F); SCCI != E; ++SCCI) {
    std::vector<BasicBlock*> &nextSCC = *SCCI;
    DEBUG(errs() << "\nSCC #" << sccNum << " : ");
    for (std::vector<BasicBlock*>::const_iterator I = nextSCC.begin(),
           E = nextSCC.end(); I != E; ++I) {
      SCCMap[*I] = sccNum;
      DEBUG(errs() << (*I)->getName() << ", ");
    }
    if (nextSCC.size() > 1)
      sccNonTriv++;
    sccNum++;
  }
  DEBUG(errs() << "\n");

  std::vector<int> SCCs;
  for (std::list<BasicBlock*>::iterator I = inters.begin(), E = inters.end();
       E != I; ++I) {
    DEBUG(dbgs() << "leaf: " << (*I)->getName() << "\n");
    SCCs.push_back(SCCMap[*I]);
    DEBUG(dbgs() << "scc idx: " << SCCMap[*I] << "\n");
  }

  std::sort(SCCs.begin(), SCCs.end());
  SCCs.resize(std::unique(SCCs.begin(), SCCs.end()) - SCCs.begin());

  DEBUG(DT.dump());
  DEBUG(PDT.dump());
  std::string err, sep = "\t";
  SmallVector<StringRef, 4> matches;
  Regex rx("([a-z0-9]*).ll");
  assert(rx.isValid(err));
  const std::string &modname = F.getParent()->getModuleIdentifier();
  rx.match(modname, &matches);
  DEBUG(errs() << "#BBs\t#leaves\t#SCCs\t#SCC-leaves\n");
  errs() << (matches.size() > 1 ? matches[1] : StringRef(modname))
    << "::" << F.getName() << sep << F.size() << sep << sccNonTriv
    << sep << inters.size() <<  sep << SCCs.size() << "\n";

  //ViewGraph(&DT, "dom");
  //ViewGraph(&PDT, "post-dom");
  return false;
}
