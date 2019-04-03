//===-- PatmosFunctionSplitter.cpp - Split functions to fit into the cache ===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass splits functions into smaller regions that fit into Patmos' method
// cache. Patmos provides special branch instructions to transfer control from
// one such region into another, however, this might be costly.
//
// The main constraints that have to be ensured by this pass are:
//      (1) The regions do not exceed the size of the method cache, which can
//          be specified by a command-line option.
//      (2) Transfer from one region to another may only go to the *first* (or 
//          entry) block of the respective target region, i.e., regions are 
//          single-entry regions, which may contain cycles though.
//
// The pass first eliminates all cycles in the CFG by iteratively processing 
// SCCs in the CFG and removing their back edges (similar to Ramalingam's loop
// forest construction). For non-natural loops a new header block is inserted.
// For each header of an SCC the total size of the SCC is computed.
//
// Artificial non-natural loop headers never become region headers, instead
// all their successors become new region headers (which are NOT loop headers of
// that non-natural loop), or their whole SCC is merged into an (non-empty)
// region.
//
// Once the CFG is acyclic, the blocks are processed in a topological order (the
// order itself is not relevant). We then grow regions by adding new blocks when
// they are visited as follows:
//   a.) for blocks that are not loop headers: if all its predecessors are in
//       the same region and the region + the block fit into the method cache,
//       add the block to the region.
//   b.) for loop headers of natural loops: if all predecessors are in the same
//       region and the region + the complete loop fit into the method cache,
//       add the entire loop to the region. Otherwise, start a new region at the
//       header.
//   c.) for artificial loop headers of non-natural loops: if all predecessors 
//       are in the same region and the region + the complete loop fit into the 
//       method cache, add the entire loop to the region. Otherwise, start a new
//       region at all successors of the header.
//
// Jump tables require some special handling, since either all targets of the
// table either have to be region entries or have to be in the same region as 
// all indirect branches using that table.
// 
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-function-splitter"
#undef PATMOS_TRACE_SCCS
#undef PATMOS_TRACE_VISITS
#undef PATMOS_TRACE_EMIT
#undef PATMOS_TRACE_FIXUP
#undef PATMOS_DUMP_ALL_SCC_DOTS
//#define PATMOS_TRACE_SCCS
//#define PATMOS_TRACE_VISITS
//#define PATMOS_TRACE_EMIT
//#define PATMOS_TRACE_FIXUP
//#define PATMOS_DUMP_ALL_SCC_DOTS


#include "Patmos.h"
#include "PatmosAsmPrinter.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachinePostDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/PMLImport.h"
#include "llvm/MC/MCNullStreamer.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ELF.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Timer.h"
#include "llvm/Target/TargetLoweringObjectFile.h"

#include <map>
#include <sstream>
#include <iostream>
#include <algorithm>

using namespace llvm;

/// DisableFunctionSplitter - Option to disable the function splitter.
static cl::opt<bool> DisableFunctionSplitter(
  "mpatmos-disable-function-splitter",
  cl::init(false),
  cl::desc("Disable the Patmos function splitter."),
  cl::Hidden);

static cl::opt<bool> DisableFunctionSplitterRewrite(
  "mpatmos-disable-function-splitter-rewrite",
  cl::init(false),
  cl::desc("Disable the rewriting of code in the Patmos function splitter."),
  cl::Hidden);

static cl::opt<int> PreferSubfunctionSize(
    "mpatmos-preferred-subfunction-size",
    cl::init(256),
    cl::desc("Preferred maximum size of subfunctions, defaults to "
        "mpatmos-max-subfunction-size if 0. Larger basic blocks and inline asm "
        "are not split. (default: 256)"));

static cl::opt<int> PreferSCCSize(
    "mpatmos-preferred-scc-size",
    cl::init(0),
    cl::desc("Preferred maximum size for SCC subfunctions, defaults to "
             "mpatmos-preferred-subfunction-size if 0. (default: 0)"));

static cl::opt<int> MaxSubfunctionSize(
    "mpatmos-max-subfunction-size",
    cl::init(1024),
    cl::desc("Maximum size of subfunctions after function splitting, defaults "
             "to the method cache size if set to 0. (default: 1024)"));

static cl::opt<bool> SplitCallBlocks(
    "mpatmos-split-call-blocks",
    cl::init(true),
    cl::desc("Split basic blocks containing calls into own subfunctions."));

/// EnableShowCFGs - Option to enable the rendering of annotated CFGs.
static cl::opt<bool> EnableShowCFGs(
  "mpatmos-function-splitter-cfgs",
  cl::init(false),
  cl::desc("Show CFGs after the Patmos function splitter."),
  cl::Hidden);

static cl::opt<std::string> StatsFile(
    "mpatmos-function-splitter-stats",
    cl::desc("Write splitting statistics to the given file"),
    cl::Hidden);

static cl::opt<bool> AppendStatsFile(
    "mpatmos-function-splitter-stats-append",
    cl::desc("Append to splitting statistics file instead of recreating it"),
    cl::Hidden);

namespace llvm {

  STATISTIC(TotalFunctions, "Number of functions visited.");
  STATISTIC(SplitFunctions, "Number of functions splitted.");
  STATISTIC(TotalBlocks, "Number of basic blocks in regions.");
  STATISTIC(TotalRegions, "Number of regions created.");
  STATISTIC(MaxRegionBlocks, "Maximum number of blocks in any region.");
  STATISTIC(MaxFunctionBlocks, "Maximum number of blocks in any function.");
  STATISTIC(MaxFunctionRegions, "Maximum number of regions in any function.");

  STATISTIC(FallthroughFixups, "Branches added for fall-throughs after function splitting");
  STATISTIC(BranchRewrites, "Branches rewritten to BRCFs");
  STATISTIC(NOPsInserted, "NOPs inserted by function splitter");
  STATISTIC(PostDomsFound, "Post dominators checked");
  STATISTIC(PostDomsAdded, "Post dominators added by increasing region size");

  class ablock;
  class agraph;

  typedef std::vector<ablock*> ablocks;
  /// must be a list, not a vector, to keep iterator valid
  typedef std::list<unsigned> idlist;
  typedef std::set<unsigned> idset;

  /// ablock - a node in a transformed copy of the CFG.
  class ablock
  {
  public:
    /// ID of the block
    const unsigned ID;

    /// The parent graph
    agraph * const G;

    /// The block's MBB -- this might be NULL for artificial loop headers of 
    /// non-natural loops or switches.
    MachineBasicBlock * const MBB;

    /// The block's fallthrough target -- NULL if the MBB does not fall
    /// through or if the block does not have an MBB associated with it.
    ablock * FallthroughTarget;

    /// Flag indicating whether the block or the code region it represents
    /// contains a call instruction.
    bool HasCall;

    /// Flag indicating whether some block of the SCC represented by the block
    /// contains a call.
    bool HasCallinSCC;

    /// Indices of the jump table that this node references.
    idset JTIDs;

    /// Number of branch instructions in this block (conditional or uncond.).
    unsigned NumBranches;

    /// The size of the basic block, not including any fixup margins.
    unsigned Size;

    /// For loop headers: keep track of the total size of the entire SCC of the 
    /// loop header, including possible fixup margins.
    /// \see transformSCCs
    unsigned SCCSize;

    /// For loop headers: blocks in the SCC of the loop header
    ablocks SCC;

    /// The region assigned to a basic block. This is computed late by 
    /// computeRegions. This can either be NULL if not yet assigned,
    /// the region of the predecessor if the block is found the first time,
    /// the block itself if it has been marked as region header, or the
    /// actual region of the block when it has been finished.
    /// A block may only be marked as header by emitRegion.
    /// \see computeRegions
    ablock *Region;

    /// Number of predecessors. This is computed late by countPredecessors and
    /// is used to update the ready list during the topological sorting.
    /// \see countPredecessors
    /// \see visitBlock
    /// \see computeRegions
    unsigned NumPreds;

    /// Create a block.
    ablock(PatmosTargetMachine &PTM, unsigned id, agraph* g,
           MachineBasicBlock *mbb = NULL)
    : ID(id), G(g), MBB(mbb), FallthroughTarget(0),
      HasCall(false), HasCallinSCC(false),
      NumBranches(0), Size(0),
      SCCSize(0), Region(NULL), NumPreds(0)
    {
      const PatmosInstrInfo *PII = PTM.getInstrInfo();

      // Count number of branches, size of block, and check for calls
      if (MBB) {
        for(MachineBasicBlock::instr_iterator t(MBB->instr_begin()),
            te(MBB->instr_end()); t != te; t++)
        {
          MachineInstr *mi = &*t;

          if (mi->isBundle()) continue;
          Size += PII->getInstrSize(mi);

          if (PII->hasCall(mi))
            HasCall = true;

          if (mi->isBranch())
            NumBranches++;
        }
      }
    }

    std::string getName() const
    {
      std::stringstream s;
      if (MBB)
        s << MBB->getName().str() << "[" << ID << "]";
      else {
        s << "header" << ID;
      }

      return s.str();
    }

    /// canExtendRegion - check if the block can be added to the given region.
    bool canExtendRegion(ablock *region) const {
      return Region == NULL || Region == region;
    }

    bool isRegionHeader() const {
      return Region == this;
    }

    bool isArtificialHeader() const {
      return !MBB;
    }

    bool isArtificialJumptableHeader() const {
      return !MBB && JTIDs.size() > 0;
    }

    bool isSCCHeader() const {
      return SCCSize > 0;
    }

    bool hasAddressTaken() const {
      if (MBB)
        return MBB->hasAddressTaken();
      return false;
    }
  };

  /// aedge - an edge in a transformed copy of the CFG.
  class aedge
  {
  public:
    /// The edge's source.
    ablock * Src;

    /// The edge's destination.
    ablock * Dst;

    /// Create an edge.
    aedge(ablock *src, ablock *dst) : Src(src), Dst(dst)
    {
    }

    bool operator<(const aedge &b) {
      return Src->ID < b.Src->ID ||
            (Src->ID == b.Src->ID && Dst->ID < b.Dst->ID);
    }
  };

  struct CompareBlock {
    bool operator()(const ablock *lhs, const ablock *rhs) const {
      // making sets of blocks deterministic
      return lhs->ID < rhs->ID;
    }
  };

  typedef std::multimap<ablock*, aedge*, CompareBlock> aedges;
  typedef std::set<ablock*, CompareBlock> ablock_set;
  typedef std::vector<aedge*> aedge_vector;

  typedef struct {
    ablock *block;
    double criticality;
  } ready_block;

  struct SortCrit {
    bool operator()(const ready_block &lhs, const ready_block &rhs) const {
      // On the ready list, we sort by highest criticality first, then
      // lowest ID first
      if (lhs.criticality > rhs.criticality) return true;
      return lhs.block->ID < rhs.block->ID;
    }
  } SortCritCmp;

  typedef std::vector<ready_block> ready_set;

  /// agraph - a transformed copy of the CFG.
  class agraph
  {
  public:
    /// The graph's blocks
    ablocks Blocks;

    /// The graph's edges.
    aedges Edges;

    /// The graph's back edges.
    aedges BackEdges;

    /// List of jump tables of this function.
    std::vector<ablocks> Jumptables;

    /// The original machine function of the graph.
    MachineFunction *MF;

    /// Target machine information
    PatmosTargetMachine &PTM;
    const PatmosSubtarget &STC;
    const PatmosInstrInfo &PII;

    unsigned PreferredRegionSize;
    unsigned PreferredSCCSize;
    unsigned MaxRegionSize;

    PMLMCQuery *PML;
    PMLQuery::BlockDoubleMap Criticalities;

    MachinePostDominatorTree &MPDT;

    /// Construct a graph from a machine function.
    agraph(MachineFunction *mf, PatmosTargetMachine &tm, PMLMCQuery *pml,
           MachinePostDominatorTree &mpdt, unsigned int preferredRegionSize,
           unsigned int preferredSCCSize, unsigned int maxRegionSize)
    : MF(mf), PTM(tm), STC(tm.getSubtarget<PatmosSubtarget>()),
      PII(*tm.getInstrInfo()),
      PreferredRegionSize(preferredRegionSize),
      PreferredSCCSize(preferredSCCSize), MaxRegionSize(maxRegionSize),
      PML(pml), MPDT(mpdt)
    {
      Blocks.reserve(mf->size());

      // create blocks
      unsigned id = 0;
      std::map<const MachineBasicBlock*, ablock*> MBBtoA;
      ablock *pred = 0;
      for(MachineFunction::iterator i(mf->begin()), ie(mf->end());
          i != ie; i++) {
        // make a block
        ablock *ab = new ablock(PTM, id++, this, i);

        // Keep track of fallthough edges
        if (pred && mayFallThrough(PTM, pred->MBB)) {
          pred->FallthroughTarget = ab;
        }
        pred = ab;

        // store block
        MBBtoA[i] = ab;
        Blocks.push_back(ab);
      }

      // create edges
      for(MachineFunction::const_iterator i(mf->begin()), ie(mf->end());
          i != ie; i++) {

        // get ablock of source
        ablock *s = MBBtoA[i];

        for(MachineBasicBlock::const_succ_iterator j(i->succ_begin()),
            je(i->succ_end()); j != je; j++) {
          // get ablock of destination
          ablock *d = MBBtoA[*j];

          // make and store the edge
          aedge *e = new aedge(s, d);
          Edges.insert(std::make_pair(s, e));
        }
      }

      // Scan all jump tables, merge all targets of the jump table into a
      // single node.
      MachineJumpTableInfo *JTI = mf->getJumpTableInfo();
      if (JTI) {
        const std::vector<MachineJumpTableEntry> &JTs = JTI->getJumpTables();

        Jumptables.resize(JTs.size());

        std::vector<bool> visited;
        visited.resize(JTs.size());

        // Build up jumptable infos
        for (size_t idx = 0; idx < JTs.size(); idx++) {

          // get a set of all distinct targets in the jump table
          ablock_set entries;
          for (std::vector<MachineBasicBlock*>::const_iterator
               it = JTs[idx].MBBs.begin(), ie = JTs[idx].MBBs.end();
               it != ie; it++)
          {
            ablock *d = MBBtoA[*it];
            d->JTIDs.insert(idx);

            entries.insert(d);
          }

          DEBUG(dbgs() << "Found jumptable (size " << JTs[idx].MBBs.size()
                       << ", targets: " << entries.size() << "):");

          // collect all jump-table entries.
          for (ablock_set::iterator it = entries.begin(), ie = entries.end();
               it != ie; it++)
          {
            ablock *d = *it;
            DEBUG(dbgs() << " " << d->getName());

            Jumptables[idx].push_back(d);
          }

          visited[idx] = false;

          DEBUG(dbgs() << "\n");
        }

        // Find overlapping jump-tables, create artificial headers
        for (size_t idx = 0; idx < JTs.size(); idx++) {
          // skip overlapping jump-tables
          if (visited[idx]) continue;

          ablock_set entries;

          // transitively find all entries of the jump-tables
          idlist jtids;
          jtids.push_back(idx);

          visited[idx] = true;

          // note that jtids.end() changes during iterations
          for (idlist::iterator jit = jtids.begin(); jit != jtids.end(); jit++)
          {
            ablocks &jt = Jumptables[*jit];

            // check all jump-table entries
            for (ablocks::iterator it = jt.begin(), ie = jt.end();
                 it != ie; it++)
            {
              ablock *entry = *it;

              // skip already found entries
              if (!entries.insert(entry).second) {
                continue;
              }

              // find overlapping jump tables
              for (idset::iterator k = entry->JTIDs.begin(),
                   ke = entry->JTIDs.end(); k != ke; k++)
              {
                if (!visited[*k]) {
                  jtids.push_back(*k);
                  visited[*k] = true;
                }
              }
            }
          }

          // Redirect all edges to the jump-table entries to a new header
          aedge_vector ingoing;
          findIngoingEdges(entries, ingoing);

          ablock *header = createHeader(entries, ingoing);

          // Connect entries to a loop
          makeSCC(header, entries);

          // Remember the jump-table IDs to mark it as a jump-table header
          header->JTIDs.insert(jtids.begin(), jtids.end());
        }
      }

      if (PML) {
        PML->getBlockCriticalityMap(Criticalities);
      }
    }

    /// mayFallThrough - Return true in case the block terminates with a 
    /// non-barrier branch or without any branch at all, false in case the
    /// block terminates with a barrier branch.
    static bool mayFallThrough(PatmosTargetMachine &PTM, MachineBasicBlock *MBB)
    {
      return PTM.getInstrInfo()->mayFallthrough(*MBB);
    }

    static unsigned int getInstrSize(MachineInstr *MI, PatmosTargetMachine &PTM)
    {
      return PTM.getInstrInfo()->getInstrSize(MI);
    }

    /// getBBSize - Size of the basic block in bytes.
    static unsigned int getBBSize(MachineBasicBlock *MBB,
                                  PatmosTargetMachine &PTM)
    {
      unsigned int size = 0;
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie; i++)
      {
        if (i->isBundle()) continue;
        size += getInstrSize(i, PTM);
      }

      return size;
    }

    /// hasCall - Check whether the basic block contains a call instruction.
    static bool hasCall(MachineBasicBlock *MBB, PatmosTargetMachine &PTM)
    {
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie; i++)
      {
        if (PTM.getInstrInfo()->hasCall(i))
          return true;
      }

      return false;
    }

    /// findIngoingEdges - Add all ingoing edges of blocks to entering.
    void findIngoingEdges(const ablock_set &blocks, aedge_vector &entering)
    {
      for(aedges::iterator j(Edges.begin()), je(Edges.end()); j != je;
          j++) {
        ablock *dst = j->second->Dst;
        if (blocks.count(dst)) {
          entering.push_back(j->second);
        }
      }
    }

    /// createHeader - Create a new artificial header, and redirect edges
    /// to the new header.
    ablock *createHeader(ablock_set &headers, aedge_vector &entering)
    {
      // create header
      ablock *header = new ablock(PTM, Blocks.size(), this);
      Blocks.push_back(header);

      // redirect edges leading to the headers
      for(aedge_vector::iterator j(entering.begin()), je(entering.end());
          j != je; j++) {
        (*j)->Dst = header;
      }

      // make edges from the new header to the old ones
      for(ablock_set::iterator j(headers.begin()), je(headers.end());
          j != je; j++) {
        aedge *e = new aedge(header, *j);
        Edges.insert(std::make_pair(header, e));
      }

      return header;
    }

    /// makeSCC - make a SCC by chaining all blocks to the header (we assume
    /// all blocks are reachable from the header).
    void makeSCC(ablock *header, ablock_set &blocks)
    {
      ablock *last = NULL;

      // chain up all blocks
      for(ablock_set::iterator j(blocks.begin()), je(blocks.end());
          j != je; j++)
      {
        if (last) {
          aedge *e = new aedge(last, *j);
          Edges.insert(std::make_pair(last, e));
        }
        last = *j;
      }

      // connect the last block to the header
      if (last) {
        aedge *e = new aedge(last, header);
        Edges.insert(std::make_pair(last, e));
      }
    }


    /// Hold information on a graph node during Tarjan's SCC algorithm.
    struct tarjan_node_info
    {
      int DFS_index;
      int Low_link;

      /// Default initialization of node infos.
      tarjan_node_info() : DFS_index(-1), Low_link(-1)
      {
      }
    };

    /// A vector of node infos for Tarjan's SCC algorithm.
    typedef std::vector<tarjan_node_info> tarjan_node_info_set;

    /// A vector of strongly connected components, i.e., vectors of node indices.
    typedef std::vector<ablocks> scc_vector;

    /// Recursive part of Tarjan's SCC algorithm.
    /// \see scc_tarjan
    /// @param node The current node visited by the DFS.
    /// @param dfs_index The current DFS index.
    /// @param nodes Stack holding the nodes of the SCC under construction.
    /// @param node_infos Auxiliary infos on nodes.
    /// @param scc_result The final set of SCCs.
    void scc_tarjan_(ablock *node, int &dfs_index, ablocks &nodes,
                     tarjan_node_info_set &node_infos,
                     scc_vector &scc_result) const
    {
      unsigned node_id = node->ID;

      // Set depth and DFS index of current node
      node_infos[node_id].DFS_index = dfs_index;
      node_infos[node_id].Low_link = dfs_index;

      // increment DFS index
      dfs_index++;

      // push the node on the stack
      nodes.push_back(node);

      // visit successor nodes and check whether the current node is the root of
      // an SCC
      for(aedges::const_iterator i(Edges.lower_bound(node)),
          ie(Edges.upper_bound(node)); i != ie; i++) {
        // get destination
        ablock *dst = i->second->Dst;
        unsigned dst_id = dst->ID;
        assert(i->second->Src == node);

        // has the successor been visited?
        if (node_infos[dst_id].DFS_index == -1)
        {
          // visit successor and update the current node's low link.
          scc_tarjan_(dst, dfs_index, nodes, node_infos, scc_result);

          node_infos[node_id].Low_link = std::min(node_infos[node_id].Low_link,
                                                  node_infos[dst_id].Low_link);
        }
        else if (std::find(nodes.begin(), nodes.end(), dst) != nodes.end())
        {
          // i is on the stack --> update low link
          node_infos[node_id].Low_link = std::min(node_infos[node_id].Low_link,
                                                  node_infos[dst_id].DFS_index);
        }
      }

      // if the current node is the root of an SCC, make a new SCC.
      if (node_infos[node_id].Low_link == node_infos[node_id].DFS_index)
      {
        scc_result.resize(scc_result.size()+1);

        ablock *top = NULL;
        do
        {
          top = nodes.back();

          // add the node to the SCC
          scc_result.back().push_back(top);

          nodes.pop_back();
        } while (top != node);
      }
    }

    /// Compute the set of strongly connected components of the graph.
    /// \see R. Tarjan, Depth-First Search and Linear Graph Algorithms
    /// @return A set of SCCs computed by Tarjan's algorithm.
    scc_vector scc_tarjan() const
    {
      int dfs_index = 0;
      tarjan_node_info_set node_infos(Blocks.size());

      ablocks nodes;
      nodes.reserve(Blocks.size());

      scc_vector scc_result;

      for(ablocks::const_iterator i(Blocks.begin()), ie(Blocks.end()); i != ie;
          i++) {
        if (node_infos[(*i)->ID].DFS_index == -1) {
          scc_tarjan_(*i, dfs_index, nodes, node_infos, scc_result);
        }
      }

      return scc_result;
    }

    /// transformSCCs - Transform the graph by removing all cycles, while
    /// preserving dominance.
    /// All SCCs with *more* than one headers are transformed as follows:
    ///   a) create a new block
    ///   b) redirect all entry edges to that block
    ///   c) create an edge from that block to each header
    ///   d) remove all backedges
    ///   e) repeat until the graph becomes acyclic
    ///
    /// This is inspired by Ramalingam.
    /// \see G. Ramalingam, On Loops, Dominators, and Dominance Frontiers
    void transformSCCs()
    {
      // collect all headers
      ablock_set all_headers;

#ifdef PATMOS_DUMP_ALL_SCC_DOTS
      unsigned int cnt = 0;
#endif

      bool changed = true;
      while(changed) {
        changed = false;

        // compute SCCs
        scc_vector sccs(scc_tarjan());

        for(scc_vector::iterator i(sccs.begin()), ie(sccs.end()); i != ie;
            i++) {
          ablocks &scc = *i;

          // skip trivial SCCs
          if (scc.size() == 1) {
            // check for self-edges
            ablock *tmp = *scc.begin();
            bool has_selfedge = false;
            for(aedges::iterator j(Edges.lower_bound(tmp)),
                je(Edges.upper_bound(tmp)); j != je && !has_selfedge; j++) {
              has_selfedge |= (j->second->Src == tmp) &&
                              (j->second->Dst == tmp);
            }

            if (!has_selfedge)
              continue;
          }
          else if (scc.empty()) {
            continue;
          }

#ifdef PATMOS_DUMP_ALL_SCC_DOTS
          write(cnt++);
#endif

          ablock_set headers;
          aedge_vector entering;
          for(aedges::iterator j(Edges.begin()), je(Edges.end()); j != je;
              j++) {
            ablock *src = j->second->Src;
            ablock *dst = j->second->Dst;
            if (std::find(scc.begin(), scc.end(), src) == scc.end() &&
                std::find(scc.begin(), scc.end(), dst) != scc.end()) {
              headers.insert(dst);
              entering.push_back(j->second);
            }
          }

          // check for dead code, this is not supported here.
          if (headers.empty()) {
            llvm_unreachable("SCC has no entry edges: function "
                             "contains dead code");
          }

          // transforms SCCs and remove cycles
          ablock *header = *headers.begin();
          if (headers.size() > 1) {
            // create a new header, redirect all edges from outside of the SCC 
            // to the previous headers to that new header, and append edges from
            // the new header to each previous header.
            header = createHeader(headers, entering);

            // fix-up surrounding SCCs -- the header has to be part of these.
            ablock *scc_node = *scc.begin();
            for(ablock_set::iterator j(all_headers.begin()),
                je(all_headers.end()); j != je; j++) {

              // see if a node of the current SCC is in the other --> all nodes
              // of the current SCC are in the other, including the header
              ablocks &other_scc = (*j)->SCC;
              if (std::find(other_scc.begin(), other_scc.end(), scc_node) !=
                    other_scc.end()) {
                other_scc.push_back(header);
              }
            }
          }

          // remove all back-edges to any header.
          // the headers are thus no longer part of any SCC, since they only
          // have incoming edges from blocks not in SCCs.
          for(aedges::iterator j(Edges.begin()), je(Edges.end()); j != je;) {
            ablock *src = j->second->Src;
            ablock *dst = j->second->Dst;
            if (std::find(scc.begin(), scc.end(), src) != scc.end() &&
                std::find(headers.begin(), headers.end(),
                          dst) != headers.end())
            {
              BackEdges.insert(std::make_pair(j->first, j->second));
              Edges.erase(j);
              j = Edges.begin();
              changed = true;
            }
            else {
              j++;
            }
          }

          // compute the combined size of the SCC.
          if (scc.size() > 1) {
            unsigned int scc_size = 0;
            bool has_call_in_scc = false;
            for(ablocks::iterator i(scc.begin()), ie(scc.end()); i != ie; i++) {
              assert(!(*i)->isArtificialHeader() ||
                     (*i)->isArtificialJumptableHeader());
              // could be an artificial jumptable header
              if (!(*i)->isArtificialHeader()) {
                scc_size += (*i)->Size + getMaxBlockMargin(PTM, (*i)->MBB);
                has_call_in_scc |= (*i)->HasCall;
              }
            }

#ifdef PATMOS_TRACE_SCCS
#ifndef PATMOS_DUMP_ALL_SCC_DOTS
            unsigned cnt = 0;
#endif
            DEBUG(
              dbgs() << "Found SCC, header " << header->getName()
                     << ", id: " << cnt
                     << ", numblocks: " << scc.size()
                     << ", size: " << scc_size
                     << ", calls: " << has_call_in_scc <<  "\n";
              dbgs() << "  Headers:";
              for(ablock_set::iterator i(headers.begin()), ie(headers.end());
                  i != ie; i++)
              {
                dbgs() << " " << (*i)->getName();
              }
              dbgs() << "\n";
              dbgs() << "  Blocks:";
              for(ablocks::iterator i(scc.begin()), ie(scc.end()); i != ie; i++)
              {
                dbgs() << " " << (*i)->getName();
              }
              dbgs() << "\n";
            );
#endif

            // make the header an SCC header and update the infos.
            assert(header && header->SCCSize == 0 && scc_size != 0);
            header->HasCallinSCC = has_call_in_scc;
            header->SCCSize = scc_size;
            header->SCC = scc;
            all_headers.insert(header);
          }
        }
      }
    }

    /// countPredecessors - compute the number of predecessors for each block.
    void countPredecessors()
    {
      for(aedges::const_iterator i(Edges.begin()), ie(Edges.end()); i != ie;
          i++) {
        i->second->Dst->NumPreds++;
      }
    }

    void makeReady(ready_set &ready, ablock *block) {
      ready_block rb;
      rb.block = block;

      if (PML && block->MBB) {
        rb.criticality = PML->getCriticality(Criticalities, *block->MBB);
      } else if (PML) {
        double maxCrit = 0.0;
        for (ablocks::iterator i = block->SCC.begin(), ie = block->SCC.end();
             i != ie; i++)
        {
          if (!(*i)->MBB) continue;
          maxCrit = std::max(maxCrit,
                             PML->getCriticality(Criticalities, *(*i)->MBB));
        }
        rb.criticality = maxCrit;
      } else {
        rb.criticality = 1.0;
      }

      // add the block to the sorted ready list
      ready.push_back(rb);
      std::sort(ready.begin(), ready.end(), SortCritCmp);
    }

    bool isReady(ready_set &ready, ablock *block) {
      for(ready_set::iterator i(ready.begin()), ie(ready.end()); i != ie; i++) {
        if (i->block == block) return true;
      }
      return false;
    }

    /// selectRegion - Chose a region to process next. the order does not really
    /// matter here -- so just make it independent of pointer values.
    ablock *selectRegion(ablock_set &regions)
    {
      ablock *minID = NULL;
      for(ablock_set::iterator i(regions.begin()), ie(regions.end()); i != ie;
          i++) {

        // take the block with the smallest ID first (determinism)
        if (!minID || minID->ID > (*i)->ID)
          minID = *i;
      }

      return minID;
    }

    /// selectBlock - select the next block to be visited.
    /// if the current block is a fall-through, prefer that fall-through, 
    /// otherwise take the block with the smallest ID (deterministic).
    ready_set::iterator selectBlock(ablock *region, ready_set &ready, ablock *last)
    {
      // check if the fall-through is ready
      MachineBasicBlock *fallthrough = last && last->FallthroughTarget ?
                                       last->FallthroughTarget->MBB : NULL;

      double maxCrit = ready.begin()->criticality;

      ready_set::iterator fttarget = ready.end();
      for(ready_set::iterator i(ready.begin()), ie(ready.end()); i != ie; i++) {

        // check for fall-through
        if (i->block->MBB == fallthrough) {
          fttarget = i;
          break;
        }
      }

      // Prefer the fallthrough block if it is sufficiently critical
      if (fttarget != ready.end() && fttarget->criticality > maxCrit - 0.1) {
        return fttarget;
      }

      // Otherwise just use the (deterministic) ordering of the ready list
      return ready.begin();
    }

    /// emitRegion - mark a block as new region entry, taking care of artificial
    /// loop or jump-table headers.
    void emitRegion(ablock *region, ablock *block,
                    ready_set &ready, ablock_set &regions)
    {
      // Note: We only mark a block as header by this function. Whenever we
      // mark a block as header, we immediately add it to the regions list.

      // We should never emit a block twice as region header. Once it has been
      // emitted, NumPreds is set to 0, so emitSCC will skip any edges to this
      // block. If a block as incoming edges from an artificial header, then
      // it only has a single incoming edge from a single header by construction
      // as back-edges to loop-headers are removed and edges to jump-table
      // entries point to the header, so it will only be marked as region
      // header once the artificial header is marked as region header.
      assert(block->Region != block && "Block has already been marked as "
                                       "region header");

      // mark as processed so this block will not become ready.
      block->NumPreds = 0;

      if (block->MBB) {
        // Insert the block directly into the regions list, it has already
        // been ready.
        regions.insert(block);
        block->Region = block;

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << " creating region " << block->getName() << "\n");
#endif

      } else {
        // mark all headers of a non-natural loop or a jump-table as new regions
        for(aedges::const_iterator i(Edges.lower_bound(block)),
            ie(Edges.upper_bound(block)); i != ie; i++)
        {
          emitRegion(region, i->second->Dst, ready, regions);
        }
      }
    }

    /// emitSCC - Emit the basic blocks of an SCC and update the ready list.
    void emitSCC(ablock *region, ablocks &scc, ready_set &ready,
                 ablock_set &regions, ablocks &order)
    {
      // emit blocks and update ready list
      for(ablocks::iterator i(scc.begin()), ie(scc.end()); i != ie; i++) {
        assert((*i)->Region == region || (*i)->Region == NULL);
        (*i)->Region = region;

#ifdef PATMOS_TRACE_EMIT
        DEBUG(dbgs() << "    emit:" << (*i)->getName() << "\n");
#endif

        // skip artificial loop headers -- as part of a surrounding SCCs
        if (!(*i)->MBB)
          continue;

        // emit the block
        // TODO: optimize order for fall-through
        assert(std::find(order.begin(), order.end(), *i) == order.end());
        order.push_back(*i);

        // make successors of this SCC ready
        for(aedges::iterator j(Edges.lower_bound(*i)),
            je(Edges.upper_bound(*i)); j != je; j++) {
          ablock *dst = j->second->Dst;

          // skip processed blocks and blocks marked as region header
          if (dst->NumPreds == 0) {
            continue;
          }
          // skip blocks in this SCC
          if (std::find(scc.begin(), scc.end(), dst) != scc.end()) {
            continue;
          }

          // check for predecessors from other regions
          if (dst->Region == NULL || dst->Region == region) {
            // first time the successor is seen or all its predecessors are
            // in the current region: remember the region of its predecessor,
            // so that we detect a region mismatch when another predecessor is
            // assigned to a different region.
            assert(dst != region);
            dst->Region = region;

            // decrement the predecessor counter and add to ready list
            if(--dst->NumPreds == 0) {
              makeReady(ready, dst);

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "      making successor " << dst->getName()
                       << " ready\n");
#endif
            }
          }
          else {
            // a region mismatch -> the successor or the loop header of its
            // SCC have to be region entries
            emitRegion(region, dst, ready, regions);

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "      making successor " << dst->getName()
                       << " a new region\n");
#endif
          }
        }
      }
    }

    /// hasRegionHeaders - check if a SCC contains blocks that have been
    /// marked as region headers other than the current region.
    bool hasRegionHeaders(ablock *region, ablocks &scc) {
      for(ablocks::iterator i(scc.begin()), ie(scc.end()); i != ie; i++) {
        if ((*i)->Region != region && (*i)->Region != NULL) {
          return true;
        }
      }
      return false;
    }

    /// increaseRegion - check if we can add all blocks in the SCC to the
    /// region. If it is not possible to increase the region, return false.
    /// This assumes that the region is structurally allowed to be extended
    /// with the SCC.
    bool increaseRegion(ablock *region, ablock *header, ablocks &scc,
                        unsigned &scc_size,
                        bool &has_call, unsigned &region_size,
                        ready_set &ready, ablock_set &regions,
                        unsigned preferred_size)
    {
      // Special case: if we are already starting a new region (the header
      // is the current region entry) with a single block, then emit it in any
      // case, and do not add anything to the SCC.
      if (header == region && scc.size() == 1) {
        // Don't forget to update the region info..
        region_size += scc_size;
        region->HasCall |= has_call;
        return true;
      }

      // If a block has its address taken (e.g. is part of a  computed goto),
      // it needs to start a new region.
      for(ablocks::iterator i(scc.begin()), ie(scc.end()); i != ie; i++) {
        if ((*i)->hasAddressTaken())
          return false;
      }

      unsigned int maxSize = preferred_size;
      bool isPostDom = false;

      // If the block post-dominates the header, add it in any case as long
      // as it fits into the cache
      if (region->MBB && header->MBB && header->SCCSize == 0 &&
          MPDT.properlyDominates(header->MBB, region->MBB))
      {
        maxSize = MaxRegionSize;
        isPostDom = true;
        PostDomsFound++;
      }

      // Check for size only after we checked for headers to allow large
      // basic blocks.
      if (region_size + scc_size > maxSize) {
        return false;
      }

      // Split blocks with calls into their own region.
      // If the block has a call, make it a region header. If the current
      // region contains a call, start a new region with this block.
      if (SplitCallBlocks && (region->HasCall || has_call)) {
        return false;
      }

      // we should not have a region mismatch when calling this function,
      // otherwise this SCC should not have become ready in emitSCC in the
      // first place.
      assert(header->canExtendRegion(region) || header->isRegionHeader());

      // update the region's total size
      // should we do this in the caller? Nah, would just duplicate the code..
      region_size += scc_size;
      region->HasCall |= has_call;

      // update statistics
      if (isPostDom && region_size > PreferredRegionSize) PostDomsAdded++;

      return true;
    }

    /// visitBlock - visit a block: check whether it can be merged with the
    /// region of its predecessors or whether the block starts a new region.
    void visitBlock(ablock *region, unsigned &region_size, ablock *block,
                    ready_set &ready, ablock_set &regions, ablocks &order)
    {
#ifdef PATMOS_TRACE_VISITS
      DEBUG(dbgs() << "  visit: " << block->getName() << " (ready: " << ready.size() << ")");
#endif

      // Check for special case first: a natural loop header starting a new
      // region, try to emit the whole SCC with a different size limit.
      //
      // Note: Since the SCC header is a region header, other blocks in the
      // SCC might have been marked as region headers, so check for this.
      //
      // TODO maybe move the code to decide whether to emit a block or its
      // whole SCC into increaseRegion?
      if (region == block &&
          block->MBB && block->SCCSize > 0 && !block->HasCallinSCC &&
          !hasRegionHeaders(region, block->SCC) &&
          increaseRegion(region, block, block->SCC, block->SCCSize,
                         block->HasCallinSCC,
                         region_size, ready, regions, PreferredSCCSize))
      {
        // A region header of a natural SCC without calls, already size-checked

        // ensure that the SCC header is the region header
        ablocks blocks;
        blocks.reserve(block->SCC.size());
        ablocks::iterator it = std::find(block->SCC.begin(), block->SCC.end(),
                                         block);
        blocks.insert(blocks.end(), it, block->SCC.end());
        blocks.insert(blocks.end(), block->SCC.begin(), it);

        // emit all blocks of the SCC and update the ready list
        emitSCC(region, blocks, ready, regions, order);

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... emitted (SCC) " << region_size << "\n");
#endif
      }
      else if (block->SCCSize == 0 || region == block) {
        // regular block or a region header (without SCC, or a SCC not handled
        // above).

        assert((region != block || region_size == 0) &&
               "Block starts a region but region size is not null");
        assert((region != block || block->MBB) &&
               "Artificial SCC header is not supposed to start a new region");

        unsigned block_size = block->Size +
                             getMaxBlockMargin(PTM, region, region_size, block);
        bool has_call = block->HasCall;

#ifdef PATMOS_TRACE_VISITS
        DEBUG(dbgs() << "  block size: " << block_size << " ("
                     << block->Size << " + "
                     << getMaxBlockMargin(PTM, region, region_size, block)
                     << "), hasCall: " << has_call << "\n");
#endif

        // emit just a single block
        ablocks blocks;
        blocks.push_back(block);

        // Note that a non-region-header block cannot have incoming edges from
        // another region, all ready blocks are visited first before a new
        // region is started, and marked as new region header if increasing
        // the old region fails.
        if (increaseRegion(region, block, blocks, block_size, has_call,
                           region_size, ready, regions, PreferredRegionSize))
        {
          // emit the blocks of the SCC and update the ready list
          emitSCC(region, blocks, ready, regions, order);

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... emitted " << region_size << "\n");
#endif
        }
        else {
          // This should not be an artificial header here
          assert(block->MBB);
          // This should not be a region entry here
          assert(region != block && "increaseRegion failed on region header");

          // some block in the region, make it a new region header
          emitRegion(region, block, ready, regions);

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... new region\n");
#endif
        }
      }
      else {
        // loop header of some natural/non-natural loop or of a jump-table,
        // not a region entry

        // TODO If the region is quite small and the SCC has no calls, maybe
        // use SCCSize instead of RegionSize as threshold. Note that
        // increaseRegion might already increase the size limit on its own if
        // this loop header post-dominates the region.

        // Note: At this point we can be sure that the SCC does not contain
        // region headers, otherwise the header would be a region entry itself.
        if (increaseRegion(region, block, block->SCC, block->SCCSize,
                           block->HasCallinSCC,
                           region_size, ready, regions, PreferredRegionSize))
        {
          // emit all blocks of the SCC and update the ready list
          emitSCC(region, block->SCC, ready, regions, order);

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... emitted (SCC) " << region_size << "\n");
#endif
        }
        else {
          // mark the header(s) of a natural/non-natural loop or all jump table
          // entries as region headers
          emitRegion(region, block, ready, regions);

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... new region ("
                       << (block->MBB ? "loop" :
                          (block->JTIDs.size() ? "jump-table"
                                               : "non-natural loop")) << ")\n");
#endif
        }
      }
    }

    /// computeRegions - Compute the regions and an ordering of the basic 
    /// blocks.
    void computeRegions(ablocks &order)
    {
      // set of ready blocks, i.e., all predecessors were visited
      ready_set ready;

      // set of unprocessed regions
      ablock_set regions;

      // start with the CFG root
      ablock *root = Blocks.front();
      emitRegion(root, root, ready, regions);

      // initialize predecessor counts
      countPredecessors();

      // process all regions
      unsigned num_regions = 0;
      while(!regions.empty()) {
        // pop first region and process it
        ablock *region = selectRegion(regions);

        assert(regions.count(region) == 1 && "Region queue is broken.");
        regions.erase(region);

        // only real basic blocks can be region entries
        assert(region->MBB);

        // initialize ready list
        makeReady(ready, region);

        // keep track of the region's total size
        unsigned region_size = 0;

        // count the number of regions
        num_regions++;

        DEBUG(dbgs() << "Starting region " << num_regions << " with "
                     << region->getName() << "\n");

        while(!ready.empty()) {
          // choose the next block to visit
          ready_set::iterator it = selectBlock(region, ready,
                                           order.empty() ? NULL : order.back());
          const ready_block next = *it;
          ready.erase(it);

          // visit the block
          visitBlock(region, region_size, next.block, ready, regions, order);
        }

        DEBUG(dbgs() << "Region: " << region->getName() << ": "
                     << region_size << "\n");
      }

#ifndef NDEBUG
      // ensure that all blocks are assigned to a region
      int UnassignedBlocks = 0;
      for(ablocks::iterator i(Blocks.begin()), ie(Blocks.end()); i != ie; i++) {
        if ((*i)->Region == NULL && (*i)->MBB) {
          errs() << "Error: Block without region: " + (*i)->getName()
                 << ", SCCSize: " << (*i)->SCCSize << "\n";
          UnassignedBlocks++;
        }
      }
      assert(UnassignedBlocks == 0);
#endif

      DEBUG(dbgs() << "#Regions: " << num_regions << "\n");

      if (EnableShowCFGs)
        view();
    }

    /// fixupFallThrough - In case the given layout successor is not the 
    /// fall-through of the given block insert a jump and corresponding NOPs to
    /// the actual fall-through target.
    void fixupFallThrough(ablock *block, MachineBasicBlock *layout_successor) {
      MachineBasicBlock *fallthrough = block->MBB;
      ablock *target = block->FallthroughTarget;

      if (!target) {
        // No target, the block might contain a call to a noreturn function.
        return;
      }

      assert(target->MBB && "Fallthrough target is set but has no MBB.");

      // fix-up needed?
      if (target->MBB != layout_successor) {

        const TargetInstrInfo &TII = *MF->getTarget().getInstrInfo();

        if (PTM.getCodeModel() != CodeModel::Large || block->Region == target->Region) {
          // Encode branch in a single instruction
          unsigned Opc = block->Region == target->Region ? Patmos::BRu : Patmos::BRCFu;
          AddDefaultPred(BuildMI(*fallthrough, fallthrough->instr_end(),
                                 DebugLoc(), TII.get(Opc))).addMBB(target->MBB);
        } else {
          // Load branch target into temporary register
          AddDefaultPred(BuildMI(*fallthrough, fallthrough->instr_end(),
                                 DebugLoc(), TII.get(Patmos::LIl), Patmos::RTR))
            .addMBB(target->MBB);
          AddDefaultPred(BuildMI(*fallthrough, fallthrough->instr_end(),
                                 DebugLoc(), TII.get(Patmos::BRCFRu)))
            .addReg(Patmos::RTR, RegState::Kill);
        }

        MachineBasicBlock::iterator II = fallthrough->end();
        --II;
        unsigned cycles = PTM.getSubtargetImpl()->getDelaySlotCycles(&*II);
        if (PTM.getSubtargetImpl()->getCFLType() != PatmosSubtarget::CFL_NON_DELAYED
            && !PII.hasRegUse(&*II)) {
          cycles = PII.moveUp(*fallthrough, II);
        }

        // Pad the *end* of the delay slot with NOPs if needed.
        for (unsigned i = 0; i < cycles; i++) {
          AddDefaultPred(BuildMI(*fallthrough, fallthrough->instr_end(),
                                 DebugLoc(), TII.get(Patmos::NOP)));
          NOPsInserted++;
        }

        FallthroughFixups++;

        block->FallthroughTarget = NULL;

#ifdef PATMOS_TRACE_FIXUP
        DEBUG(dbgs() << "Fixup: " << fallthrough->getName() << "|"
                     <<(layout_successor ? layout_successor->getName() : "NULL")
                     <<  "-->" << target->getName() << "\n");
#endif
      }
      // TODO else check if there is an unconditional jump to the
      // successor, remove it if possible.
    }

    /// reorderBlocks - Reorder the basic blocks of the function, align them, 
    /// and fix-up fall-through branches
    void reorderBlocks(ablocks &order)
    {
      PatmosMachineFunctionInfo *PMFI= MF->getInfo<PatmosMachineFunctionInfo>();

      // keep track of fall-through blocks
      ablock *fallThrough = NULL;

      unsigned numBlocks = 0;
      unsigned numRegions = 0;

      // reorder the blocks and fix-up fall-through blocks
      MachineBasicBlock *last = order.back()->MBB;
      for(ablocks::iterator i(order.begin()), ie(order.end()); i != ie; i++) {
        MachineBasicBlock *MBB = (*i)->MBB;
        bool is_region_entry = (*i)->Region == (*i);

        // store region entries with the Patmos function info
        if (is_region_entry) {
          PMFI->addMethodCacheRegionEntry(MBB);

          numRegions++;

          // update statistics of last region
          if (MaxRegionBlocks < numBlocks)
            MaxRegionBlocks = numBlocks;
          TotalBlocks += numBlocks;
          numBlocks = 0;
        }

        numBlocks++;

        // don't move the last node before itself ;-)
        if (MBB != last)
          MBB->moveBefore(last);

        // fix-up fall-through blocks
        if (fallThrough) {
          fixupFallThrough(fallThrough, is_region_entry ? NULL : MBB);
        }

        // keep track of fall-through blocks
        // Note: we could just do (*i)->FallthroughTarget ? *i : NULL; here, but
        // this way it is more robust..
        fallThrough = mayFallThrough(PTM, MBB) ? *i : NULL;
      }

      // fix-up fall-through blocks
      if (fallThrough) {
        fixupFallThrough(fallThrough, NULL);
      }

      // update statistics of last region
      if (MaxRegionBlocks < numBlocks)
        MaxRegionBlocks = numBlocks;
      TotalBlocks += numBlocks;

      // update statistics of function
      if (MaxFunctionRegions < numRegions)
        MaxFunctionRegions = numRegions;
      if (MaxFunctionBlocks < order.size())
        MaxFunctionBlocks = order.size();
      TotalRegions += numRegions;

      // renumber blocks according to the new order
      MF->RenumberBlocks();
    }

    /// rewriteBranch - if the branch instruction jumps to the target, rewrite 
    /// its opcode.
    void rewriteBranch(MachineInstr *BR, unsigned int opcode,
                       MachineBasicBlock *target)
    {
      bool rewrite = false;

      if (BR->isIndirectBranch()) {
        if (BR->getOpcode() == Patmos::BRT || BR->getOpcode() == Patmos::BRTu) {
          unsigned index = BR->getOperand(3).getIndex();
          MachineJumpTableInfo *MJTI = MF->getJumpTableInfo();
          const std::vector<MachineBasicBlock*> &JTBBs(MJTI->getJumpTables()[index].MBBs);
          rewrite = std::find(JTBBs.begin(), JTBBs.end(), target) != JTBBs.end();
        } else {
          assert(BR->getOpcode() == Patmos::BRR || BR->getOpcode() == Patmos::BRRu);
          // always rewrite "true" indirect branches
          rewrite = true;
        }
      }
      else {
        rewrite = BR->getOperand(2).getMBB() == target;
      }

      if (rewrite) {
        MachineBasicBlock &MBB = *BR->getParent();

        DEBUG(dbgs() << "Rewrite: branch in " << MBB.getName()
                     << "[" << MBB.getNumber() << "]"
                     << " branching to " << target->getName()
                     << "[" << target->getNumber() << "]\n");

        MachineBasicBlock::instr_iterator II = BR;
        // move to the beginning of the BR bundle
        while (II->isBundledWithPred()) II--;

        if (PTM.getCodeModel() == CodeModel::Large) {
          // Load target into register when rewriting to BRCF with immediate
          if (opcode == Patmos::BRCF || opcode == Patmos::BRCFu) {
            AddDefaultPred(BuildMI(MBB, II, DebugLoc(), PII.get(Patmos::LIl), Patmos::RTR))
              .addOperand(BR->getOperand(BR->getNumExplicitOperands()-1));
            BR->RemoveOperand(BR->getNumExplicitOperands()-1);
            BR->addOperand(*MF, MachineOperand::CreateReg(Patmos::RTR, false, false, true));
         
            opcode = opcode == Patmos::BRCF ? Patmos::BRCFR : Patmos::BRCFRu;
          }
        }

        // Replace br with brcf, fix delay slot size
        BR->setDesc(PII.get(opcode));

        int delaySlots = PTM.getSubtargetImpl()->getCFLDelaySlotCycles(false);
        int cycles =     delaySlots -
                         PTM.getSubtargetImpl()->getCFLDelaySlotCycles(true);

        MachineBasicBlock::iterator I = II;

        // Only move the instruction up if it has no dependencies on other
        // instructions.
        // TODO We could check for latencies of operands here, but defs will
        // probably always be scheduled in the previous cycle anyway.
        if (PTM.getSubtargetImpl()->getCFLType() != PatmosSubtarget::CFL_NON_DELAYED
            && !PII.hasRegUse(&*I) && PII.canRemoveFromSchedule(MBB, I)) {
          cycles = PII.moveUp(MBB, I, cycles);
        }

        // Add NOPs at the *end* of the branch delay slot, if needed (to avoid
        // adding NOPs into an overlapping delay slot of another branch)
        PII.advanceCycles(MBB, I, delaySlots - cycles + 1);

        for (int i = 0; i < cycles; i++) {
          AddDefaultPred(BuildMI(MBB, I, DebugLoc(), PII.get(Patmos::NOP)));
          NOPsInserted++;
        }

        BranchRewrites++;
      }
    }

    /// rewriteEdge - Rewrite a branch associated with a CFG edge crossing from 
    /// one region to another to non-cache variants.
    void rewriteEdge(ablock *src, ablock *dst)
    {
      MachineBasicBlock *sbb = src->MBB;
      MachineBasicBlock *dbb = dst->MBB;

      // skip artificial loop header.
      if (!sbb)
         return;

      // destination is an artificial loop header, check all edges that lead 
      // to the real headers of the SCC
      if (!dbb) {
        for(aedges::const_iterator i(Edges.lower_bound(dst)),
            ie(Edges.upper_bound(dst)); i != ie; i++) {
          rewriteEdge(src, i->second->Dst);
        }
      }
      else if (src->Region != dst->Region) {
        // the two blocks are not in the same region, rewrite the branch ...

        for(MachineBasicBlock::instr_iterator j(sbb->instr_begin()),
            je(sbb->instr_end()); j != je; j++)
        {
          MachineInstr *mi = &*j;

          // skip non-terminator instructions and returns
          if (!mi->isTerminator() || mi->isReturn() || mi->isBundle())
            continue;

          switch (mi->getOpcode()) {
            // branches that may need rewriting
            case Patmos::BR:
              rewriteBranch(mi, Patmos::BRCF, dbb);        break;
            case Patmos::BRu:
              rewriteBranch(mi, Patmos::BRCFu, dbb);       break;
            case Patmos::BRR:
              rewriteBranch(mi, Patmos::BRCFR, dbb);       break;
            case Patmos::BRRu:
              rewriteBranch(mi, Patmos::BRCFRu, dbb);      break;
            case Patmos::BRT:
              rewriteBranch(mi, Patmos::BRCFT, dbb);       break;
            case Patmos::BRTu:
              rewriteBranch(mi, Patmos::BRCFTu, dbb);      break;

            // already rewritten branches - skip them
            case Patmos::BRCF:
            case Patmos::BRCFu:
            case Patmos::BRCFR:
            case Patmos::BRCFRu:
            case Patmos::BRCFT:
            case Patmos::BRCFTu:
              break;

            // unexpected ?
            default:
              assert(false);
              abort();
          }
        }
      }
    }

    /// rewriteCode - Rewrite branches crossing from one region to another
    /// to non-cache variants.
    void rewriteCode()
    {
      // check regular control-flow edges
      for(aedges::iterator i(Edges.begin()), ie(Edges.end()); i != ie;
          i++) {
        rewriteEdge(i->second->Src, i->second->Dst);
      }

      // also check back edges
      for(aedges::iterator i(BackEdges.begin()), ie(BackEdges.end()); i != ie;
          i++) {
        rewriteEdge(i->second->Src, i->second->Dst);
      }
    }

    /// applyRegions - reorder and align the basic blocks and fix-up branches.
    void applyRegions(ablocks &order) 
    {
      // rewrite branches to use the non-cache variants
      if (!DisableFunctionSplitterRewrite)
        rewriteCode();

      // reorder the basic blocks, while fixing up fall-through branches.
      // This is done after the branch rewrite, inserting the correct version of
      // branches here, so they can be properly scheduled.
      reorderBlocks(order);
    }

    /// transferSuccessors - replace uses of OldSucc to uses of NewSucc.
    ///
    /// Similar to MachineBasicBlock->ReplaceUsesOfBlockWith, but handles
    /// delay slots and jump tables properly.
    static void transferSuccessor(MachineBasicBlock *Pred,
                                  MachineBasicBlock *OldSucc,
                                  MachineBasicBlock *NewSucc)
    {
      MachineFunction *MF = Pred->getParent();

      assert(NewSucc && "Trying to replace successor with a null pointer.");

      Pred->replaceSuccessor(OldSucc, NewSucc);

      // Iterate over all instructions, there can be cond. and uncond. branches
      for(MachineBasicBlock::instr_iterator j(Pred->instr_begin()),
          je(Pred->instr_end()); j != je; j++)
      {
        MachineInstr *mi = &*j;

        // skip non-terminator instructions and returns
        if (!mi->isTerminator() || mi->isReturn() || mi->isBundle())
          continue;

        switch (mi->getOpcode()) {
          // direct branches that may need rewriting
          case Patmos::BR:
          case Patmos::BRu:
          case Patmos::BRCF:
          case Patmos::BRCFu:
          {
            assert(mi->getOperand(2).isMBB()
                   && "Illegal branch instruction format");
            if (mi->getOperand(2).getMBB() == OldSucc) {
              mi->getOperand(2).setMBB(NewSucc);
            }
            break;
          }
          // Handle indirect branches with jumptable index
          case Patmos::BRT:
          case Patmos::BRTu:
          case Patmos::BRCFT:
          case Patmos::BRCFTu:
          {
            assert(mi->getNumOperands() == 4);

            unsigned index = mi->getOperand(3).getIndex();
            MachineJumpTableInfo *MJTI = MF->getJumpTableInfo();
            MJTI->ReplaceMBBInJumpTable(index, OldSucc, NewSucc);

            break;
          }
          // handle indirect branches
          case Patmos::BRR:
          case Patmos::BRRu:
          case Patmos::BRCFR:
          case Patmos::BRCFRu:
            // TODO not much we can do here.. for now assume that we never
            // generate that. We could just do ReplaceMBBInJumpTable without
            // index however..
            assert(false &&
                   "Indirect branches without jumptable info is unsupported");
            break;

          // unexpected ?
          default:
            assert(false && "Unsupported terminator");
            abort();
        }
      }

    }

    /// splitBlockAtStart - Create a new basic block and insert it as a successor
    /// after the given machine basic block.
    static MachineBasicBlock *splitBlockAtStart(MachineBasicBlock *MBB)
    {
      // get current function
      MachineFunction *MF = MBB->getParent();
      MachineBasicBlock *newBB = MF->CreateMachineBasicBlock();

      // transfer predecessors of MBB to newBB
      while (!MBB->pred_empty()) {
        MachineBasicBlock *Pred = *MBB->pred_begin();
        transferSuccessor(Pred, MBB, newBB);
      }

      // insert MBB as successor of newBB
      newBB->addSuccessor(MBB, 1);

      // ensure that MBB can fall-through to the new block
      MF->insert(MachineFunction::iterator(MBB), newBB);

      return newBB;
    }

    static unsigned int getMaxBlockMargin(PatmosTargetMachine &PTM,
                                          ablock *region, unsigned &region_size,
                                          ablock *block)
    {
      bool mayFallthrough = block->FallthroughTarget != 0;

      // TODO analyze successors, check if all of them fit with max margins
      // into the region, then we only need a BR instead of BRCF
      bool mightExit = true;

      // check how many branches we actually have in this block!
      int numBranches = block->NumBranches;

      return getMaxBlockMargin(PTM, mightExit, mayFallthrough, numBranches);
    }

    static unsigned int getMaxBlockMargin(PatmosTargetMachine &PTM,
                                          MachineBasicBlock *MBB)
    {
      // TODO analyze MBB, count number of branches
      int numBranches = 0;

      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie; i++)
      {
        if (i->isBundle()) continue;

        if (i->isBranch()) numBranches++;
      }

      bool mayFallthrough = mayFallThrough(PTM, MBB);

      return getMaxBlockMargin(PTM, true, mayFallthrough, numBranches);
    }

    /// getMaxRegionMargin - Get the maximum number of bytes needed to be
    /// added to a basic block.
    /// mightExitRegion - we might exit the region after this block
    /// mightFallthrough - Does this block end with an unconditional branch?
    /// numBranchesToFix - Number of branches in the block that might exit the
    ///                    region.
    static unsigned int getMaxBlockMargin(PatmosTargetMachine &PTM,
                                        bool mightExitRegion = true,
                                        bool mightFallthrough = true,
                                        int numBranchesToFix = 0)
    {
      const PatmosSubtarget *PST = PTM.getSubtargetImpl();

      // TODO those values are static, calc them only once
      unsigned localDelay = PST->getCFLDelaySlotCycles(true);
      unsigned exitDelay = PST->getCFLDelaySlotCycles(false);

      unsigned blockAlign = (1u << PST->getMinBasicBlockAlignment());

      // We must be conservative here, the address is not known (and may change)
      unsigned alignSize = (blockAlign < 4) ? 0 : blockAlign - 4;

      // we already have a BR, we only need to add a NOP if we change to BRCF
      unsigned branch_fixups = numBranchesToFix * (exitDelay - localDelay) * 4;

      // we have to load the address into a register when not using the small code model
      if (PTM.getCodeModel() == CodeModel::Large) {
        branch_fixups += numBranchesToFix * 8;
      }

      if (mightFallthrough) {
        // we might need to add a BR/BRCF to replace the fallthrough, and NOPs
        // to fill the delay slots

        // BRCFs are cheaper in the small code model
        unsigned brcfCost = PTM.getCodeModel() == CodeModel::Large ? 12 : 4;

        // TODO this might be too conservative, as we might be able to move
        // branches up and do not need that many NOPs
        branch_fixups += brcfCost + (mightExitRegion ? exitDelay : localDelay) * 4;
      }

      return alignSize + branch_fixups;
    }

    /// Get the size of the delay slot of an instruction in bytes.
    static unsigned int getDelaySlotSize(MachineBasicBlock *MBB,
                                         MachineInstr *MI,
                                         PatmosTargetMachine &PTM)
    {
      int cycles = PTM.getSubtargetImpl()->getDelaySlotCycles(MI);
      const PatmosInstrInfo *PII = PTM.getInstrInfo();

      unsigned bytes = 0;

      MachineBasicBlock::iterator it = MI;
      for (; cycles > 0; ) {
        it++;
        assert(it != MBB->end() && "Reached end of MBB before end of delay slot");
        assert(!it->isInlineAsm() && "Inline asm should not be in delay slot");

        if (!PII->isPseudo(it)) {
          bytes += getInstrSize(it, PTM);
          --cycles;
        }
      }

      return bytes;
    }

    /// splitBlock - Split a basic block into smaller blocks that each fit into
    /// the method cache.
    static unsigned int splitBlock(MachineBasicBlock *MBB, unsigned int MaxSize,
                                   PatmosTargetMachine &PTM,
                                   MachineDominatorTree &MDT,
                                   MachinePostDominatorTree &MPDT)
    {
      unsigned int branchFixup = getMaxBlockMargin(PTM, true, false, 1);

      // make a new block
      unsigned int curr_size = getMaxBlockMargin(PTM);

      unsigned int cache_size = PTM.getSubtargetImpl()->getMethodCacheSize();

      unsigned int total_size = 0;
      // Note: we need to use an instr_iterator here, otherwise splice fails
      // horribly for some mysterious ilist bug.
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie; i++)
      {
        // skip over instructions inside bundles
        if (i->isInsideBundle()) {
          continue;
        }

        // get instruction size
        unsigned int i_size = agraph::getInstrSize(i, PTM);

        // for branches, assume we need to add a NOP to make it BRCF.
        if (i->isBranch()) i_size += branchFixup;

        // should we check for i_size > MaxSize as well and issue a warning?
        // => No, user should not get warnings if he cannot do anything about it

        if (i_size > cache_size) {
          report_fatal_error("Inline assembly in function " +
                             MBB->getParent()->getFunction()->getName() +
                             " is larger than the method cache size!");
        }

        // we must not split live ranges of the RTR register
        // luckily, they are short and do not cross basic blocks
        unsigned int tmp_live_margin = 0;
        if (PTM.getCodeModel() == CodeModel::Large &&
            i->definesRegister(Patmos::RTR) && !i->isBranch()) {
          MachineBasicBlock::instr_iterator k;
          for (k = llvm::next(i); k != ie; ++k) {
            tmp_live_margin += agraph::getInstrSize(k, PTM);
            if (k->killsRegister(Patmos::RTR)) {
              break;
            }
          }
          if (k == ie) {
            report_fatal_error("Temporary register defined but not killed in basic block");
          }
        }

        // ensure that we do not split inside delay slots
        unsigned int delay_slot_margin = i->hasDelaySlot()
                      ? getDelaySlotSize(MBB, i, PTM) : 0;

#ifndef NDEBUG
        const MachineInstr *FirstMI = PTM.getInstrInfo()->getFirstMI(i);
        assert(!isPatmosCFL(FirstMI->getOpcode(), FirstMI->getDesc().TSFlags)
               || (delay_slot_margin > 0));
#endif

        // check block + instruction size + max delay slot size of this instr.
        if (curr_size + i_size + tmp_live_margin + delay_slot_margin < MaxSize)
        {
          curr_size += i_size;
        }
        else
        {
          total_size += curr_size;

          DEBUG(dbgs() << "Splitting basic block at " << total_size << ": "
                << MBB->getFullName());

          // the current instruction does not fit -- split the block.
          MachineBasicBlock *newBB = splitBlockAtStart(MBB);

          // copy instructions over from the original block.
          newBB->splice(newBB->instr_begin(), MBB, MBB->instr_begin(), i);

          // update dominator and post-dominator trees for the new block
          MachineDomTreeNode *TN = MDT.getNode(MBB)->getIDom();
          // Check if the block has a dominator
          if (TN) {
            // - the new node is dominated by the dominator of the old node
            MDT.addNewBlock(newBB, TN->getBlock());
            // - the new node dominates the old block
            MDT.changeImmediateDominator(MBB, newBB);
          } else {
            // TODO Any other way to change the root node of the DomTree?
            //      At least do this after all other blocks are split, and skip
            //      updating the DomTree for individual blocks.
            MDT.runOnMachineFunction(*MBB->getParent());
          }
          // noreturn calls do not post-dominate and do not have a node in
          // the tree.
          if (MPDT.getNode(MBB)) {
            // - the new node is post-dominated by the old block
            MPDT.addNewBlock(newBB, MBB);
            // - the new block post-dominates all nodes post-dominated by
            //   the old block
            for (MachineDomTreeNode::const_iterator
                 it = MPDT.getNode(MBB)->getChildren().begin(),
                 ie = MPDT.getNode(MBB)->getChildren().end(); it != ie; it++)
            {
              MachineBasicBlock *preBB = (*it)->getBlock();
              if (preBB == newBB) continue;
              MPDT.changeImmediateDominator(preBB, newBB);
              // restart from beginning, with one post-dominated node less.
              it = MPDT.getNode(MBB)->getChildren().begin();
            }
          }

          // start anew, may fall through!
          curr_size = getMaxBlockMargin(PTM) + i_size;
          i = MBB->instr_begin();
        }
      }

      return total_size + curr_size;
    }

    void view()
    {
      ViewGraph(*this, "agraph");
    }

    /// write - dump a DOT file.
    void write(unsigned int i)
    {
      std::stringstream s;
      s << MF->getFunction()->getName().str() << "-" << i << ".dot";
      std::string err;
      raw_fd_ostream f(s.str().c_str(), err);
      WriteGraph(f, *this);
      f.close();
    }

    /// Free memory.
    ~agraph()
    {
      for(aedges::iterator i(Edges.begin()), ie(Edges.end()); i != ie; i++) {
        delete i->second;
      }

      for(aedges::iterator i(BackEdges.begin()), ie(BackEdges.end()); i != ie;
          i++) {
        delete i->second;
      }

      for(ablocks::iterator i(Blocks.begin()), ie(Blocks.end()); i != ie; i++) {
        delete *i;
      }

      if (PML) delete PML;
    }
  };

  /// Pass to split functions into smaller regions that fit into the size limits
  /// of the method cache.
  /// \see MethodCacheBlockSize, MethodCacheSize
  class PatmosFunctionSplitter : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    PatmosTargetMachine &PTM;
    const PatmosSubtarget &STC;

    void writeStats(StringRef Filename, MachineFunction &MF,
                    agraph &G, ablocks &order,
                    unsigned orig_size, const TimeRecord &Time)
    {
      std::string err;
      raw_fd_ostream f(Filename.str().c_str(), err, sys::fs::F_Append);

      // write a single line per function

      // <module>, <function>, "fun", <#BBs>, <origSize>, <time (ms)>
      f << "\"" << MF.getMMI().getModule()->getModuleIdentifier() << "\", ";
      f << "\"" << MF.getName() << "\", ";
      f << "\"fun\", ";
      f << MF.size() << ", " << orig_size << ", ";
      f << Time.getProcessTime() * 1000.0;
      f << "\n";

      // collect region statistics

      int BBs = 0;
      int RegionSize = 0;
      int EstRegionSize = 0;
      ablock *Header = *order.begin();

      ablocks::iterator i(order.begin()), ie(order.end());

      while (i != ie) {
        MachineBasicBlock *MBB = (*i)->MBB;

        // add current block to region stats
        if (MBB) {
          BBs++;
          RegionSize += agraph::getBBSize(MBB, PTM);
          EstRegionSize += (*i)->Size;
        }

        // continue to next block
        i++;

        // New region starts? Write out previous region stats
        if (i == ie || (*i)->Region != Header) {

          // write one line per region

          // <module>, <function>, "reg",
          f << "\"" << MF.getMMI().getModule()->getModuleIdentifier() << "\", ";
          f << "\"" << MF.getName() << "\", ";
          f << "\"reg\", ";

          // <#BBs>, <calc size>, <region size>, <HasCall>,
          f << BBs << ", " << EstRegionSize << ", " << RegionSize << ", "
            << Header->HasCall << ", ";
          // <isSCC>, <SCC size>, <CallInSCC>
          f << Header->isSCCHeader() << ", " << Header->SCCSize << ", "
            << Header->HasCallinSCC;

          f << "\n";

          // reset stats
          BBs = 0;
          RegionSize = 0;
          EstRegionSize = 0;

          if (i != ie) Header = (*i)->Region;
        }
      }

      f.close();
    }

  public:
    /// PatmosFunctionSplitter - Create a new instance of the function splitter.
    PatmosFunctionSplitter(PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), PTM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>())
    {
    }

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Function Splitter";
    }

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<PMLImport>();
      AU.addRequired<MachineDominatorTree>();
      AU.addRequired<MachinePostDominatorTree>();
      AU.addPreserved<MachineDominatorTree>();
      AU.addPreserved<MachinePostDominatorTree>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    virtual bool doInitialization(Module &)  {
      if (!StatsFile.empty() && !AppendStatsFile && sys::fs::exists(StatsFile))
      {
        sys::fs::remove(StatsFile.c_str());
      }
      return false;
    }

    /// runOnMachineFunction - Run the function splitter on the given function.
    bool runOnMachineFunction(MachineFunction &MF) {
      // the pass got disabled?
      if (DisableFunctionSplitter)
        return false;

      unsigned max_subfunc_size   = MaxSubfunctionSize  ? MaxSubfunctionSize
                                                     : STC.getMethodCacheSize();
      max_subfunc_size = std::min(max_subfunc_size, STC.getMethodCacheSize());

      unsigned prefer_subfunc_size = PreferSubfunctionSize ?
                                                       PreferSubfunctionSize
                                                     : max_subfunc_size;
      unsigned prefer_scc_size = PreferSCCSize ? PreferSCCSize
                                               : prefer_subfunc_size;
      prefer_subfunc_size = std::min(max_subfunc_size, prefer_subfunc_size);
      prefer_scc_size = std::min(max_subfunc_size, prefer_scc_size);

      unsigned total_size = 0;
      bool blocks_splitted = false;

      MachineDominatorTree &MDT = getAnalysis<MachineDominatorTree>();
      MachinePostDominatorTree &MPDT = getAnalysis<MachinePostDominatorTree>();

      for(MachineFunction::iterator i(MF.begin()), ie(MF.end()); i != ie; i++) {
        unsigned bb_size = agraph::getBBSize(i, PTM);

        // in case the block is larger than the method cache, split it and
        // update its
        //
        if (bb_size + agraph::getMaxBlockMargin(PTM, i) > max_subfunc_size)
        {
          bb_size = agraph::splitBlock(i, max_subfunc_size, PTM, MDT, MPDT);
          blocks_splitted = true;
        }

        total_size += bb_size;
      }

      DEBUG(dbgs() << "\nPatmos Function Splitter: "
                   << MF.getFunction()->getName() << ": " << total_size << "\n");

      TotalFunctions++;

      // splitting needed?
      if (total_size > prefer_subfunc_size) {

        bool CollectStats = !StatsFile.empty();
        TimeRecord Time;

        if (CollectStats) Time -= TimeRecord::getCurrentTime(true);

        // construct a copy of the CFG.
        PMLImport &PI = getAnalysis<PMLImport>();
        agraph G(&MF, PTM, PI.createMCQuery(*this, MF), MPDT,
                 prefer_subfunc_size, prefer_scc_size, max_subfunc_size);
        G.transformSCCs();

        // compute regions -- i.e., split the function
        ablocks order;
        G.computeRegions(order);
        assert(order.size() == MF.size());

        // update the basic block order and rewrite branches
        G.applyRegions(order);

        if (CollectStats) {
          Time += TimeRecord::getCurrentTime(false);

          writeStats(StatsFile, MF, G, order, total_size, Time);
        }

        SplitFunctions++;

        // Note: We rely on the PatmosEnsureAlignment pass to set alignments,
        // we do not do it in this pass.

        return true;
      }

      return blocks_splitted;
    }
  };

  char PatmosFunctionSplitter::ID = 0;
}

/// createPatmosFunctionSplitterPass - Returns a new PatmosFunctionSplitter
/// \see PatmosFunctionSplitter
FunctionPass *llvm::createPatmosFunctionSplitterPass(PatmosTargetMachine &tm) {
  return new PatmosFunctionSplitter(tm);
}

namespace llvm {
  template <> struct GraphTraits<agraph> {
    typedef ablock NodeType;
    class ChildIteratorType
    {
      aedges::iterator I;

    public:
      typedef aedges::iterator::iterator_category iterator_category;
      typedef aedges::iterator::difference_type difference_type;
      typedef aedges::iterator::pointer pointer;
      typedef aedges::iterator::reference reference;
      typedef NodeType value_type;

      ChildIteratorType(aedges::iterator i) : I(i) {
      }

      bool operator!=(ChildIteratorType a) {
        return I != a.I;
      }

      ChildIteratorType operator++() {
        ChildIteratorType tmp(I);
        I++;
        return tmp;
      }

      NodeType *operator*() {
        return I->second->Dst;
      }
    };

    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->G->Edges.lower_bound(N);
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->G->Edges.upper_bound(N);
    }

    static NodeType *getEntryNode(const agraph &G) {
      return G.Blocks.front();
    }

    // nodes_iterator/begin/end - Allow iteration over all nodes in the graph
    typedef ablocks::const_iterator nodes_iterator;
    static nodes_iterator nodes_begin(const agraph &G) {
      return G.Blocks.begin();
    }
    static nodes_iterator nodes_end  (const agraph &G) {
      return G.Blocks.end();
    }
    static unsigned       size       (const agraph &G)  {
      return G.Blocks.size();
    }
  };

  template<>
  struct DOTGraphTraits<agraph> : public DefaultDOTGraphTraits {
    typedef aedges::iterator EdgeIteratorType;

    DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

    static std::string getGraphName(const agraph &G) {
      return G.MF->getFunction()->getName();
    }

    template<typename T>
    static bool isNodeHidden(const T, const agraph &G) {
      return false;
    }

    std::string getNodeLabel(const ablock *AB, const agraph &G) {
      // block name -- or header name for non-MBB blocks
      std::stringstream s;
      s << AB->getName() << " (" << AB->NumPreds << ")";

      // block and SCC size
      s << "\n" << AB->Size << " (" << AB->SCCSize << ")";

      if (AB->Region)
        s << "\n" << AB->Region->getName();

      return s.str();
    }

    static std::string getNodeAttributes(const ablock *AB, const agraph &G) {
      return (AB->Region == AB) ? "color=\"red\"" :
              (!AB->Region) ? "color=\"gray\"" : "";
    }
  };
}
