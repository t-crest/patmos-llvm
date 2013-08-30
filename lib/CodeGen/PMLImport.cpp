//===-- PMLImport.cpp -----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Import PML to provide external analysis results to LLVM passes.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "pml-import"

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/CodeGen/PML.h"
#include "llvm/CodeGen/PMLImport.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachinePostDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace llvm;


static cl::opt<std::string> ImportFile("mimport-pml",
   cl::desc("Read external analysis results from PML file"),
   cl::init(""));

INITIALIZE_PASS(PMLImport, "pml-import", "PML Import", false, true)

char PMLImport::ID = 0;

void PMLImport::anchor() {}

///////////////////////////////////////////////////////////////////////////////

bool PMLImport::isInitialized() const {
  return Initialized;
}

void PMLImport::initializePass()
{
  Initialized = false;

  if (ImportFile.empty()) {
    return;
  }

  OwningPtr<MemoryBuffer> Buf;
  if (MemoryBuffer::getFileOrSTDIN(ImportFile, Buf))
    return;

  yaml::Input Input(Buf->getBuffer());

  yaml::PMLDocList Docs;

  Input >> Docs.YDocs;

  if (!Input.error()) {
    return;
  }

  Docs.mergeInto(YDoc);

  rebuildPMLIndex();

  Initialized = true;
}

void PMLImport::deletePMLIndex() {
  if (BitcodeLevel) delete BitcodeLevel;
  if (MachineLevel) delete MachineLevel;
  BitcodeLevel = 0;
  MachineLevel = 0;
}

void PMLImport::rebuildPMLIndex() {
  deletePMLIndex();

  // We could check if we actually have any documents with that level, but meh..
  BitcodeLevel = new PMLLevelInfo(yaml::level_bitcode);
  MachineLevel = new PMLLevelInfo(yaml::level_machinecode);

  for (std::vector<yaml::BitcodeFunction*>::iterator
       i = YDoc.BitcodeFunctions.begin(), ie = YDoc.BitcodeFunctions.end();
       i != ie; i++)
  {
    BitcodeLevel->addFunctionInfo(**i);
  }
  for (std::vector<yaml::MachineFunction*>::iterator
       i = YDoc.MachineFunctions.begin(), ie = YDoc.MachineFunctions.end();
       i != ie; i++)
  {
    MachineLevel->addFunctionInfo(**i);
  }
}

PMLBitcodeQuery* PMLImport::createBitcodeQuery(Pass &AnalysisProvider,
                                               const Function &F,
                                               yaml::ReprLevel SrcLevel)
{
  if (!Initialized) return 0;
  PMLLevelInfo *Lvl = (SrcLevel == yaml::level_bitcode) ? BitcodeLevel :
                                                          MachineLevel;
  if (!Lvl) return 0;
  return new PMLBitcodeQuery(YDoc, F, *Lvl, AnalysisProvider);
}

PMLMCQuery* PMLImport::createMCQuery(Pass &AnalysisProvider,
                                     const MachineFunction &MF,
                                     yaml::ReprLevel SrcLevel)
{
  if (!Initialized) return 0;
  PMLLevelInfo *Lvl = (SrcLevel == yaml::level_bitcode) ? BitcodeLevel :
                                                          MachineLevel;
  if (!Lvl) return 0;
  return new PMLMCQuery(YDoc, MF, *Lvl, AnalysisProvider);
}


bool PMLFunctionInfo::hasBlockMapping(StringRef Label) const
{
  if (!hasMapping()) return false;
  return hasBlock(getBlockName(Label).getName());
}

bool PMLFunctionInfo::hasBlockMapping(const BasicBlock &BB) const
{
  if (!hasMapping()) return false;
  return hasBlock(getBlockName(BB).getName());
}

bool PMLFunctionInfo::hasBlockMapping(const MachineBasicBlock &MBB) const
{
  if (!hasMapping()) return false;
  return hasBlock(getBlockName(MBB).getName());
}

yaml::Name PMLFunctionInfo::getBlockName(StringRef Label) const
{
  // If we do not have a mapping for this function, just use the label as name.
  // We never use the block number as name in this case, as this is not safe.
  if (!hasMapping()) return Label;

  PMLLabelMap::const_iterator it = BlockLabels.find(Label);
  if (it != BlockLabels.end()) {
    return yaml::Name(it->second);
  }
  // not found in the label map. Maybe the label is valid, else there is no
  // mapping.
  return hasBlock(Label) ? Label : "";
}

yaml::Name PMLFunctionInfo::getBlockName(const BasicBlock &BB) const
{
  return getBlockName(getBlockLabel(BB));
}

yaml::Name PMLFunctionInfo::getBlockName(const MachineBasicBlock &MBB) const
{
  return getBlockName(getBlockLabel(MBB));
}

StringRef PMLFunctionInfo::getBlockLabel(const BasicBlock &BB)
{
  return BB.getName();
}

StringRef PMLFunctionInfo::getBlockLabel(const MachineBasicBlock &MBB)
{
  // For now, the label of a MBB is simply the BB label, otherwise we have no
  // label for now (do not use the block number as label, it is not a stable
  // mapping).
  if (!MBB.getBasicBlock()) return "";
  return MBB.getBasicBlock()->getName();
}


template<typename BlockT, bool isBitcode>
StringRef PMLFunctionInfoT<BlockT,isBitcode>::getBlockLabel(const yaml::Name& Name) const
{
  typename BlockMap::const_iterator it = Blocks.find(Name.getName());
  if (it != Blocks.end()) {
    BlockT *BB = it->second;
    if (!BB->MapsTo.empty()) return BB->MapsTo.getName();

    // We have a block but is has no label mapped to it. Is the name the label?
    // Assume that non-numeric names are actual labels
    // TODO this might be too eager, maybe just return "" here?
    if (!BB->BlockName.isInteger()) {
      return BB->BlockName.getName();
    }
  }
  return "";
}

template<typename BlockT, bool bitcode>
void PMLFunctionInfoT<BlockT,bitcode>::reloadBlockInfos()
{
  BlockLabels.clear();
  Blocks.clear();

  if (!Function) return;

  for (typename std::vector<BlockT*>::iterator i = Function->Blocks.begin(),
       ie = Function->Blocks.end(); i != ie; i++)
  {
    BlockT *BB = *i;
    if (!BB->MapsTo.empty()) {
      BlockLabels.GetOrCreateValue(BB->MapsTo.getName(),
                                   BB->BlockName.getName());
    }
    Blocks.GetOrCreateValue(BB->BlockName.getName(), BB);
  }
}

template<typename BlockT, bool bitcode>
bool PMLFunctionInfoT<BlockT,bitcode>::hasBlock(const yaml::Name &N) const
{
  return Blocks.find(N.getName()) != Blocks.end();
}

template<typename BlockT, bool bitcode>
BlockT* PMLFunctionInfoT<BlockT,bitcode>::getBlock(const yaml::Name &N) const
{
  return Blocks.lookup(N.getName());
}

/* Note: this code generates warnings about missing declarations with clang 3.3.
 * However, we do not actually need it anyway, because we instantiate all
 * possible template types in the header anyway.
template PMLFunctionInfoT<yaml::BitcodeBlock,true>;
template PMLFunctionInfoT<yaml::MachineBlock,false>;
*/


void PMLLevelInfo::addFunctionInfo(yaml::BitcodeFunction &F)
{
  assert(IsBitcode &&
         "Adding bitcode functions to machine level is not supported");

  if (!F.MapsTo.empty()) {
    FunctionLabels.GetOrCreateValue(F.MapsTo.getName(),
                                    F.FunctionName.getName());
  }
  PMLFunctionInfo *FI = new PMLBitcodeFunctionInfo(F);
  FunctionInfos.GetOrCreateValue(F.FunctionName.getName(), FI);
}

void PMLLevelInfo::addFunctionInfo(yaml::MachineFunction &F)
{
  assert(!IsBitcode &&
         "Adding machine functions to bitcode level is not supported");

  if (!F.MapsTo.empty()) {
    FunctionLabels.GetOrCreateValue(F.MapsTo.getName(),
                                    F.FunctionName.getName());
  }
  PMLFunctionInfo *FI = new PMLMachineFunctionInfo(F);
  FunctionInfos.GetOrCreateValue(F.FunctionName.getName(), FI);
}

yaml::Name PMLLevelInfo::getFunctionName(const Function &F) const
{
  // check if there is a mapping for this function to another name
  PMLLabelMap::const_iterator it = FunctionLabels.find(F.getName());
  if (it != FunctionLabels.end()) {
    // .. should usually not happen
    return it->second;
  }
  // By default, just use the name of bitcode functions as name.
  return F.getName();
}

yaml::Name PMLLevelInfo::getFunctionName(const MachineFunction &F) const
{
  // If we do not have a bitcode function, we have no label for this function,
  // hence no way to map the label to a name. Not using the function number as
  // label as this is not safe.
  if (!F.getFunction()) return yaml::Name("");

  // check if there is a mapping for this function to another name
  PMLLabelMap::const_iterator it = FunctionLabels.find(F.getName());
  if (it != FunctionLabels.end()) {
    // .. should usually find a mapping.
    return it->second;
  }

  // By default, just use the name of bitcode functions as name. Again not
  // using the function number as name for unmapped functions here for safety.
  return F.getName();

}

PMLFunctionInfo &PMLLevelInfo::getFunctionInfo(const yaml::Name &Name) const
{
  PMLFunctionInfoMap::const_iterator it = FunctionInfos.find(Name.getName());
  if (it != FunctionInfos.end()) {
    return *it->second;
  }
  if (IsBitcode) {
    return PMLBitcodeFunctionInfo::getEmptyInfo();
  } else {
    return PMLMachineFunctionInfo::getEmptyInfo();
  }
}



void PMLQuery::resetAnalyses()
{
  MDom = 0;
  MPostDom = 0;
}

bool PMLQuery::hasValueFacts(bool CheckForFunction) const
{
  for (std::vector<yaml::ValueFact*>::iterator i = YDoc.ValueFacts.begin(),
       ie = YDoc.ValueFacts.end(); i != ie; i++)
  {
    yaml::ValueFact *VF = *i;

    if (!matches(VF->Origin, VF->Level)) continue;
    if (CheckForFunction && !matches(VF->PP)) continue;

    return true;
  }
  return false;
}

bool PMLQuery::hasFlowFacts(bool CheckForFunction) const
{
  for (std::vector<yaml::FlowFact*>::const_iterator i = YDoc.FlowFacts.begin(),
       ie = YDoc.FlowFacts.end(); i != ie; i++)
  {
    const yaml::FlowFact *FF = *i;

    if (!matches(FF->Origin, FF->Level)) continue;
    if (CheckForFunction && !matches(FF->ScopeRef)) continue;

    return true;
  }
  return false;
}

bool PMLQuery::hasTimings(bool CheckForFunction) const
{
  for (std::vector<yaml::Timing*>::const_iterator i = YDoc.Timings.begin(),
       ie = YDoc.Timings.end(); i != ie; i++)
  {
    const yaml::Timing *T = *i;

    if (!matches(T->Origin, T->Level)) continue;
    if (CheckForFunction) {
      if (!matches(T->ScopeRef)) continue;

      // TODO iterate over all profiles, check for references to the function
    }
    return true;
  }
  return false;
}


bool PMLQuery::matches(const yaml::Name &Origin, yaml::ReprLevel Level) const
{
  if (IgnoreTraces && Origin.getName() == "trace") return false;
  if (Level != SrcLevel.getLevel()) return false;
  return true;
}

bool PMLQuery::matches(const yaml::ProgramPoint *PP) const
{
  if (!PP) return false;
  // TODO check for context
  return PP->Function == FI.getName();
}

bool PMLQuery::matches(const yaml::Scope *S) const
{
  if (!S) return false;
  // TODO check for context
  return S->Function == FI.getName();
}

template<typename T>
typename StringMap<T>::iterator PMLQuery::getDominatorEntry(StringMap<T> &Map,
                                               MachineBasicBlock &MBB,
                                               bool PostDom, bool &StrictDom)
{
  StringRef Name;

  // Check the node itself without looking into the dom tree, but only
  // if we are not looking for strict dominators only.
  if (!StrictDom) {
    Name = FI.getBlockName(MBB).getName();

    // Check if we have a direct mapping
    BlockDoubleMap::iterator it = Map.find(Name);
    if (it != Map.end()) {
      return it;
    }
  }
  MachineDomTreeNode *Node;

  // Check any strict dominators for a mapping
  if (PostDom) {
    if (!MPostDom) MPostDom =
                  &AnalysisProvider.getAnalysis<MachinePostDominatorTree>();

    Node = MPostDom->getNode(&MBB);
  } else {
    if (!MDom) MDom = &AnalysisProvider.getAnalysis<MachineDominatorTree>();

    Node = MDom->getNode(&MBB);
  }
  if (!Node) {
    report_fatal_error("Could not find dominator tree node for basic block " +
                       MBB.getFullName());
  }

  // We already checked the node itself, go to direct dominator
  Node = Node->getIDom();

  while (Node) {
    Name = FI.getBlockName(*Node->getBlock()).getName();

    // Check if we have a direct mapping
    BlockDoubleMap::iterator it = Map.find(Name);
    if (it != Map.end()) {
      StrictDom = true;
      return it;
    }

    Node = Node->getIDom();
  }

  return Map.end();
}

template<typename T>
T PMLQuery::getMaxDominatorValue(StringMap<T> &Map,
                                 MachineBasicBlock &MBB, T Default)
{
  if (Map.empty()) return Default;

  bool StrictDom = false;

  typename StringMap<T>::iterator it =
                                 getDominatorEntry(Map, MBB, false, StrictDom);
  T DomValue = (it != Map.end()) ? it->second : Default;

  if (it != Map.end() && !StrictDom) {
    // We have an entry in the map that matches exactly, no need to continue
    return DomValue;
  }

  typename StringMap<T>::iterator pit =
                                 getDominatorEntry(Map, MBB, true, StrictDom);
  T PostDomValue = (pit != Map.end()) ? pit->second : Default;

  return std::max(DomValue, PostDomValue);
}


bool PMLQuery::getBlockCriticalityMap(BlockDoubleMap &Criticalitites)
{
  // Search all timing infos for criticalities for all blocks of the function
  bool found = false;
  for (std::vector<yaml::Timing*>::const_iterator i = YDoc.Timings.begin(),
       ie = YDoc.Timings.end(); i != ie; i++)
  {
    const yaml::Timing *T = *i;
    if (!matches(T->Origin, T->Level)) continue;

    for (std::vector<yaml::ProfileEntry*>::const_iterator
         pi = T->Profile.begin(), pie = T->Profile.end(); pi != pie; pi++)
    {
      const yaml::ProfileEntry *P = *pi;
      if (!P->hasCriticality()) continue;
      if (!matches(P->Reference)) continue;

      // Get the name of either a block reference, or the source of an edge ref.
      StringRef Block = P->Reference->Block.empty() ?
                        P->Reference->EdgeSource.getName() :
                        P->Reference->Block.getName();
      if (Block.empty()) continue;

      double Crit = std::max(Criticalitites.lookup(Block), P->Criticality);
      Criticalitites[Block] = Crit;

      found = true;
    }
  }

  return found;
}

double PMLMCQuery::getCriticality(BlockDoubleMap &Criticalities,
                                  MachineBasicBlock &MBB, double Default)
{
  return getMaxDominatorValue(Criticalities, MBB, Default);
}
