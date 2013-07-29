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
// all indirect branches using that table. This is handled by turning the 
// successors or an indirect branch into an SCC, which is handled by case c) 
// from above.
// 
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-function-splitter"
#undef PATMOS_TRACE_VISITS
#undef PATMOS_TRACE_EMIT
#undef PATMOS_TRACE_FIXUP
#undef PATMOS_DUMP_ALL_SCC_DOTS

#include "Patmos.h"
#include "PatmosAsmPrinter.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
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
#include "llvm/Target/TargetLoweringObjectFile.h"

#include <map>
#include <sstream>
#include <iostream>

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

/// EnableShowCFGs - Option to enable the rendering of annotated CFGs.
static cl::opt<bool> EnableShowCFGs(
  "mpatmos-function-splitter-cfgs",
  cl::init(false),
  cl::desc("Show CFGs after the Patmos function splitter."),
  cl::Hidden);

namespace llvm {
  class ablock;
  class agraph;

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
  };

  typedef std::vector<ablock*> ablocks;
  typedef std::multimap<ablock*, aedge*> aedges;

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
    MachineBasicBlock * FallthroughTarget;

    /// Flag indicating whether the block or the code region it represents
    /// contains a call instruction.
    bool HasCall;

    /// Flag indicating whether some block of the SCC represented by the block
    /// contains a call.
    bool HasCallinSCC;

    /// The size of the basic block.
    unsigned Size;

    /// For loop headers: keep track of the total size of the entire SCC of the 
    /// loop header.
    /// \see transformSCCs
    unsigned SCCSize;

    /// For loop headers: blocks in the SCC of the loop header
    ablocks SCC;

    /// The region assigned to a basic block. This is computed late by 
    /// computeRegions.
    /// \see computeRegions
    ablock *Region;

    /// Number of predecessors. This is computed late by countPredecessors and
    /// is used to update the ready list during the topological sorting.
    /// \see countPredecessors
    /// \see visitBlock
    /// \see computeRegions
    unsigned NumPreds;

    /// Create a block.
    ablock(unsigned id, agraph* g, MachineBasicBlock *mbb = NULL,
           bool hascall = false,  unsigned size = 0) : ID(id), G(g), MBB(mbb),
                                                      FallthroughTarget(0),
                                                      HasCall(hascall),
                                                      HasCallinSCC(false),
                                                      Size(size), SCCSize(0),
                                                      Region(NULL), NumPreds(0)
    {
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
  };

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

    /// The original machine function of the graph.
    MachineFunction *MF;

    /// Target machine information
    PatmosTargetMachine &PTM;
    const PatmosSubtarget &STC;

    /// Construct a graph from a machine function.
    agraph(MachineFunction *mf, PatmosTargetMachine &tm) : MF(mf), PTM(tm),
        STC(tm.getSubtarget<PatmosSubtarget>())
    {
      Blocks.reserve(mf->size());

      // create blocks
      unsigned id = 0;
      std::map<const MachineBasicBlock*, ablock*> MBBtoA;
      ablock *pred = 0;
      for(MachineFunction::iterator i(mf->begin()), ie(mf->end());
          i != ie; i++) {
        // make a block
        ablock *ab = new ablock(id++, this, i, hasCall(i, PTM),
                                               getBBSize(i, PTM));

        // Keep track of fallthough edges
        if (pred && mayFallThrough(pred->MBB)) {
          pred->FallthroughTarget = ab->MBB;
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

      // check for blocks with indirect jumps via jump tables. Turn the block's 
      // successors into an SCC.
      for(ablocks::iterator i(Blocks.begin()), ie(Blocks.end()); i != ie; i++) {
        MachineBasicBlock *MBB = (*i)->MBB;

        for(MachineBasicBlock::instr_iterator t(MBB->instr_begin()),
            te(MBB->instr_end()); t != te; t++)
        {
          MachineInstr *mi = &*t;

          // skip non-branch instructions
          if (!mi->isTerminator())
            continue;

          if (mi->isIndirectBranch()) {
            ablock *last = NULL;
            for(MachineBasicBlock::succ_iterator i(MBB->succ_begin()),
                ie(MBB->succ_end()); i != ie; i++) {

              // get ablock of destination
              ablock *d = MBBtoA[*i];

              if (last) {
                aedge *e = new aedge(last, d);
                Edges.insert(std::make_pair(last, e));
              }

              last = d;
            }

            // get ablock of destination
            ablock *d = MBBtoA[*MBB->succ_begin()];
            aedge *e = new aedge(last, d);
            Edges.insert(std::make_pair(last, e));

            break;
          }
        }
      }
    }

    /// mayFallThrough - Return true in case the block terminates with a 
    /// non-barrier branche or without any branch at all, false in case the 
    /// block terminates with a barrier branch.
    static bool mayFallThrough(MachineBasicBlock *MBB)
    {
      if (MBB->succ_empty())
        return false;

      // find last terminator
      for(MachineBasicBlock::reverse_instr_iterator t(MBB->instr_rbegin()),
          te(MBB->instr_rend()); t != te; t++)
      {
        MachineInstr *mi = &*t;

        // skip non-terminator instructions
        if (!mi->isTerminator())
          continue;

        return !mi->isBarrier();
      }

      return true;
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

      // add some bytes in case we need to fix-up the fall-through
      return size + (mayFallThrough(MBB) ?
                PTM.getSubtargetImpl()->getCFLDelaySlotCycles(true) * 4 + 4: 0);
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

    typedef std::set<ablock*> ablock_set;
    typedef std::vector<aedge*> aedge_vector;

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
          if (headers.empty() && scc.size() > 1) {
            llvm_unreachable("SCC has no entry edges: function "
                             "contains dead code");
          }

          // transforms SCCs and remove cycles
          ablock *header = headers.empty() ? NULL : *headers.begin();
          if (headers.size() > 1) {
            // create a new header, redirect all edges from outside of the SCC 
            // to the previous headers to that new header, and append edges from
            // the new header to each previous header.

            // create header
            header = new ablock(Blocks.size(), this);
            Blocks.push_back(header);

            // redirect edges leading into the SCC
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

          // remove all back-edges
          for(aedges::iterator j(Edges.begin()), je(Edges.end()); j != je;) {
            ablock *src = j->second->Src;
            ablock *dst = j->second->Dst;
            if (std::find(scc.begin(), scc.end(), src) != scc.end() &&
                std::find(headers.begin(), headers.end(),
                          dst) != headers.end()) {
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
              assert((*i)->MBB);
              scc_size += getBBSize((*i)->MBB, PTM);
              has_call_in_scc |= (*i)->HasCall;
            }

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
    ablock *selectBlock(ablock *region, ablock_set &ready, ablock *last)
    {
      // check if the fall-through is ready
      MachineBasicBlock *fallthrough = last ? last->FallthroughTarget : NULL;

      ablock *minID = NULL;
      for(ablock_set::iterator i(ready.begin()), ie(ready.end()); i != ie;
          i++) {

        // check for fall-through
        if ((*i)->MBB == fallthrough)
          return *i;

        // take the block with the smallest ID first (determinism)
        if (!minID || minID->ID > (*i)->ID)
          minID = *i;
      }

      return minID;
    }

    /// emitSCC - Emit the basic blocks of an SCC and update the ready list.
    void emitSCC(ablock *region, ablocks &scc, ablock_set &ready,
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

        for(aedges::iterator j(Edges.lower_bound(*i)),
            je(Edges.upper_bound(*i)); j != je; j++) {
          ablock *dst = j->second->Dst;
          if (std::find(scc.begin(), scc.end(), dst) == scc.end()) {
            // skip processed blocks
            if (dst->NumPreds == 0) {
              continue;
            }

            // check the successor
            if (dst->Region == NULL || dst->Region == region) {
              // first time the successor is seen or all its predecessors are 
              // in the current region
              dst->Region = region;

              // decrement the predecessor counter and add to ready list
              if(--dst->NumPreds == 0) {
                ready.insert(dst);
              }
            }
            else {
              // a region mismatch -> the successor or the loop header of its 
              // SCC have to be region entries
              dst->Region = dst;
              dst->NumPreds = 0;

              if (dst->MBB) {
                regions.insert(dst);
              }
              else {
                // mark all headers of a non-natural loop as new regions
                for(aedges::const_iterator k(Edges.lower_bound(dst)),
                    ke(Edges.upper_bound(dst)); k != ke; k++) {
                  regions.insert(k->second->Dst);
                  k->second->Dst->Region = k->second->Dst;
                  k->second->Dst->NumPreds = 0;
                }
              }
            }
          }
        }
      }
    }

    /// visitBlock - visit a block: check whether it can be merged with the
    /// region of its predecessors or whether the block starts a new region.
    void visitBlock(ablock *region, unsigned &region_size, ablock *block,
                    ablock_set &ready, ablock_set &regions, ablocks &order)
    {
#ifdef PATMOS_TRACE_VISITS
      DEBUG(dbgs() << "  visit: " << block->getName());
#endif
      if (block->SCCSize == 0 || region == block) {
        // TODO check: why does this assertion not hold??
        //assert(block->SCCSize == 0 && "A region must not start with an SCC");

        unsigned block_size = block->Size +
                              getMaxBlockMargin(PTM, region, region_size, block);

        // regular block that is not a loop header or a loop header in its own
        // region
        if (region_size + block_size <= STC.getMethodCacheSize())
        {
          // update the region's total size
          region_size += block_size;
          region->HasCall |= block->HasCall;

          // emit the blocks of the SCC and update the ready list
          ablocks tmp;
          tmp.push_back(block);
          emitSCC(region, tmp, ready, regions, order);

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... emitted " << region_size << "\n");
#endif
        }
        else {
          assert(region != block);
          regions.insert(block);
          block->Region = block;
#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... new region\n");
#endif
        }
      }
      else {
        // loop header of some loop

        unsigned block_size = block->SCCSize +
                              getMaxBlockMargin(PTM, region, region_size, block);

        if (region_size + block_size <= STC.getMethodCacheSize())
        {
          // update the region's total size
          region_size += block_size;
          region->HasCall |= block->HasCallinSCC;

          // emit all blocks of the SCC and update the ready list
          emitSCC(region, block->SCC, ready, regions, order);

#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... emitted (SCC) " << region_size << "\n");
#endif
        }
        else if (block->MBB) {
          // mark the header of a natural loop as a new region
          regions.insert(block);
          block->Region = block;
#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... new region (loop)\n");
#endif
        }
        else {
          // mark all headers of a non-natural loop as new regions
          for(aedges::const_iterator i(Edges.lower_bound(block)),
              ie(Edges.upper_bound(block)); i != ie; i++) {
            regions.insert(i->second->Dst);
            i->second->Dst->Region = i->second->Dst;
          }
#ifdef PATMOS_TRACE_VISITS
          DEBUG(dbgs() << "... new region (non-natural loop)\n");
#endif
        }
      }
    }

    /// computeRegions - Compute the regions and an ordering of the basic 
    /// blocks.
    void computeRegions(ablocks &order)
    {
      // set of ready blocks, i.e., all predecessors were visited
      ablock_set ready;

      // set of unprocessed regions
      ablock_set regions;

      // start with the CFG root
      ablock *root = Blocks.front();
      regions.insert(root);
      root->Region = root;

      // initialize predecessor counts
      countPredecessors();

      // process all regions
      unsigned num_regions = 0;
      while(!regions.empty()) {
        // pop first region and process it
        ablock *region = selectRegion(regions);
        regions.erase(region);

        // only real basic blocks can be region entries
        assert(region->MBB);

        // initialize ready list
        ready.insert(region);

        // keep track of the region's total size
        unsigned region_size = 0;

        // count the number of regions
        num_regions++;

        while(!ready.empty()) {
          // choose the next block to visit
          ablock *next = selectBlock(region, ready, order.empty() ? NULL :
                                                                  order.back());
          ready.erase(next);

          // visit the block
          visitBlock(region, region_size, next, ready, regions, order);
        }

        DEBUG(dbgs() << "Region: " << region->getName() << ": "
                     << region_size << "\n");
      }

#ifndef NDEBUG
      // ensure that all blocks are assigned to a region
      int UnassignedBlocks = 0;
      for(ablocks::iterator i(Blocks.begin()), ie(Blocks.end()); i != ie; i++) {
        if ((*i)->Region == NULL) {
          errs() << "Error: Block without region: " + (*i)->getName() << "\n";
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
      MachineBasicBlock *target = block->FallthroughTarget;

      // fix-up needed?
      if (target != layout_successor) {
        const TargetInstrInfo &TII = *MF->getTarget().getInstrInfo();
        AddDefaultPred(BuildMI(*fallthrough, fallthrough->instr_end(),
                               DebugLoc(), TII.get(Patmos::BRu))).addMBB(target);
        for (unsigned i = 0;
             i < PTM.getSubtargetImpl()->getCFLDelaySlotCycles(true); i++) {
          AddDefaultPred(BuildMI(*fallthrough, fallthrough->instr_end(),
                                 DebugLoc(), TII.get(Patmos::NOP)));
        }

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

      // reorder the blocks and fix-up fall-through blocks
      MachineBasicBlock *last = order.back()->MBB;
      for(ablocks::iterator i(order.begin()), ie(order.end()); i != ie; i++) {
        MachineBasicBlock *MBB = (*i)->MBB;
        bool is_region_entry = (*i)->Region == (*i);

        // set alignment of region entries and store region entries with the 
        // Patmos function info
        if (is_region_entry) {
          MBB->setAlignment(log2(STC.getMethodCacheBlockSize()));
          PMFI->addMethodCacheRegionEntry(MBB);
        }

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
        fallThrough = mayFallThrough(MBB) ? *i : NULL;
      }

      // fix-up fall-through blocks
      if (fallThrough) {
        fixupFallThrough(fallThrough, NULL);
      }

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
        assert(BR->getNumOperands() == 4);

        unsigned index = BR->getOperand(3).getIndex();
        MachineJumpTableInfo *MJTI = MF->getJumpTableInfo();
        const std::vector<MachineBasicBlock*> &JTBBs(
                                             MJTI->getJumpTables()[index].MBBs);

        rewrite = std::find(JTBBs.begin(), JTBBs.end(), target) != JTBBs.end();
      }
      else {
        rewrite = BR->getOperand(2).getMBB() == target;
      }

      if (rewrite) {
        // Replace br with brcf, fix delay slot size
        const TargetInstrInfo &TII = *MF->getTarget().getInstrInfo();
        BR->setDesc(TII.get(opcode));

        MachineBasicBlock::iterator II = BR; II++;
        for (unsigned i = PTM.getSubtargetImpl()->getCFLDelaySlotCycles(true);
             i < PTM.getSubtargetImpl()->getCFLDelaySlotCycles(false); i++) {
          AddDefaultPred(BuildMI(*BR->getParent(), II,
                                 DebugLoc(), TII.get(Patmos::NOP)));
        }
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
          if (!mi->isTerminator() || mi->isReturn())
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
    /// to non-cache variants, also insert fix-ups on code regions containing
    /// calls.
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

      // insert fix-up code to handle calls within code regions -- skip the 
      // function's first block though since this is handled in the function 
      // epilog already.
      const TargetInstrInfo &TII = *MF->getTarget().getInstrInfo();
      for(ablocks::iterator i(++Blocks.begin()), ie(Blocks.end()); i != ie;
          i++) {
        // Only consider region entries that contain a call
        if ((*i)->Region == (*i) && (*i)->HasCall) {
          MachineBasicBlock *MBB = (*i)->MBB;

          // load long immediate of the current basic block's address into RFB
          AddDefaultPred(BuildMI(*MBB, MBB->instr_begin(), DebugLoc(),
                                 TII.get(Patmos::LIl),
                                 Patmos::RFB)).addMBB(MBB);
        }
      }
    }

    /// applyRegions - reorder and align the basic blocks and fix-up branches.
    void applyRegions(ablocks &order) 
    {
      // reorder the basic blocks -- while fixing up fall-through branches
      reorderBlocks(order);

      // rewrite branches to use the non-cache variants
      if (!DisableFunctionSplitterRewrite)
        rewriteCode();

      // ensure method alignment
      MF->ensureAlignment(log2(STC.getMethodCacheBlockSize()));
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

      Pred->replaceSuccessor(OldSucc, NewSucc);

      // Iterate over all instructions, there can be cond. and uncond. branches
      for(MachineBasicBlock::instr_iterator j(Pred->instr_begin()),
          je(Pred->instr_end()); j != je; j++)
      {
        MachineInstr *mi = &*j;

        // skip non-terminator instructions and returns
        if (!mi->isTerminator() || mi->isReturn())
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
      bool hasCall = block->SCCSize == 0 || region == block ? block->HasCall
                                                          : block->HasCallinSCC;
      bool needsCallFixup = (hasCall && !region->HasCall);

      // TODO analyze successors, check if all of them fit with max margins
      // into the region, then we only need a BR instead of BRCF
      bool mightExit = true;
      // TODO analyze MBB, check if we have a BR at the end.
      bool hasBranch = false;

      return getMaxBlockMargin(PTM, needsCallFixup, mightExit, hasBranch);
    }

    static unsigned int getMaxBlockMargin(PatmosTargetMachine &PTM,
                                          MachineBasicBlock *MBB)
    {
      // TODO analyze MBB, check if we have a BR at the end.
      bool hasBranch = false;
      return getMaxBlockMargin(PTM, true, true, hasBranch);
    }

    /// getMaxRegionMargin - Get the maximum number of bytes needed to be
    /// added to a basic block.
    /// needsCallFixup - does this block contain the first call in this region
    /// mightExitRegion - we might exit the region after this block
    /// hasBranch - Does this block end with a branch?
    static unsigned int getMaxBlockMargin(PatmosTargetMachine &PTM,
                                        bool needsCallFixup = true,
                                        bool mightExitRegion = true,
                                        bool hasBranch = false)
    {
      unsigned branch_fixups = 0;
      if (hasBranch) {
        // we already have a BR, we only need to add a NOP if we change to BRCF
        branch_fixups = mightExitRegion ? 4 : 0;
      } else {
        // we might need to add a BR/BRCF to replace the fallthrough, and NOPs
        // to fill the delay slots
        branch_fixups = 4 +
             PTM.getSubtargetImpl()->getCFLDelaySlotCycles(mightExitRegion) * 4;
      }
      return branch_fixups + (needsCallFixup ? 8 : 0);
    }

    /// splitBlock - Split a basic block into smaller blocks that each fit into
    /// the method cache.
    static unsigned int splitBlock(MachineBasicBlock *MBB, unsigned int MCSize,
                                   PatmosTargetMachine &PTM)
    {
      // make a new block
      unsigned int curr_size = getMaxBlockMargin(PTM);
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

        if (i_size > MCSize) {
          report_fatal_error("Inline assembly in function " +
                             MBB->getParent()->getFunction()->getName() +
                             " is larger than the method cache size!");
        }

        // ensure that we do not split inside delay slots
        unsigned int delay_slot_margin = i->hasDelaySlot()
                      ? PTM.getSubtargetImpl()->getMaxDelaySlotCodeSize(i) : 0;

        const MachineInstr *FirstMI = PTM.getInstrInfo()->getFirstMI(i);
        assert(!isPatmosCFL(FirstMI->getOpcode(), FirstMI->getDesc().TSFlags)
               || (delay_slot_margin > 0));

        // check block + instruction size + max delay slot size of this instr.
        if (curr_size + i_size + delay_slot_margin < MCSize)
        {
          curr_size += i_size;
        }
        else
        {
          total_size += curr_size;

          // the current instruction does not fit -- split the block.
          MachineBasicBlock *newBB = splitBlockAtStart(MBB);

          // copy instructions over from the original block.
          newBB->splice(newBB->instr_begin(), MBB, MBB->instr_begin(), i);

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

  public:
    /// PatmosFunctionSplitter - Create a new instance of the function splitter.
    PatmosFunctionSplitter(PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), PTM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>())
    {
      // TODO we could disable this pass if this is not a PatmosTargetMachine
    }

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Function Splitter";
    }

    /// runOnMachineFunction - Run the function splitter on the given function.
    bool runOnMachineFunction(MachineFunction &MF) {
      // the pass got disabled?
      if (DisableFunctionSplitter)
        return false;

      unsigned total_size = 0;
      for(MachineFunction::iterator i(MF.begin()), ie(MF.end()); i != ie; i++) {
        unsigned bb_size = agraph::getBBSize(i, PTM);

        // in case the block is larger than the method cache, split it and
        // update its
        //
        if (bb_size + agraph::getMaxBlockMargin(PTM, i) > STC.getMethodCacheSize())
        {
          bb_size = agraph::splitBlock(i, STC.getMethodCacheSize(), PTM);
        }

        total_size += bb_size;
      }

      DEBUG(dbgs() << "\nPatmos Function Splitter: "
                   << MF.getFunction()->getName() << ": " << total_size << "\n");

      // splitting needed?
      if (total_size > STC.getMethodCacheSize()) {
        // construct a copy of the CFG.
        agraph G(&MF, PTM);
        G.transformSCCs();

        // compute regions -- i.e., split the function
        ablocks order;
        G.computeRegions(order);
        assert(order.size() == MF.size());

        // update the basic block order and rewrite branches
        G.applyRegions(order);
      }

      return true;
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
