//==-- SPScope.h -  -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
//
//
//===---------------------------------------------------------------------===//

#ifndef TARGET_PATMOS_SINGLEPATH_SPSCOPE_H_
#define TARGET_PATMOS_SINGLEPATH_SPSCOPE_H_

#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/Support/Debug.h"
#include "boost/optional.hpp"

// define for more detailed debugging output
#define PATMOS_SINGLEPATH_TRACE

#ifdef PATMOS_SINGLEPATH_TRACE
#define DEBUG_TRACE(x) DEBUG(x)
#else
#define DEBUG_TRACE(x) /*empty*/
#endif

namespace llvm {

  class SPScopeWalker;

  class SPScope {

    public:
      /// iterator - Type for iterator through MBBs
      typedef std::vector<MachineBasicBlock*>::iterator iterator;

      /// child_iterator - Type for child iterator
      typedef std::vector<SPScope*>::iterator child_iterator;

      // Edge type
      typedef std::pair<const MachineBasicBlock *,
                        const MachineBasicBlock *> Edge;

      /// PredDefInfo - Class containing predicate definition information
      /// of one MachineBasicBlock.
      /// Instances for MBBs are stored in the PredDefs map.
      class PredDefInfo {
        private:
          typedef std::vector<std::pair<unsigned, Edge> > PredEdgeList;
          PredEdgeList Defs;
        public:
          typedef PredEdgeList::const_iterator iterator;
          void define(unsigned pred, const Edge e) {
            Defs.push_back(std::make_pair(pred, e));
          }
          iterator begin() const { return Defs.begin(); }
          iterator end() const { return Defs.end(); }
      };

      /// Node - a node used internally in the scope to construct a forward CFG
      /// of the scope MBBs
      class Node {
        public:
          typedef std::vector<Node*>::iterator child_iterator;
          Node(const MachineBasicBlock *mbb=NULL)
            : MBB(mbb), num(-1), ipdom(NULL) {}
          const MachineBasicBlock *MBB;
          int num;
          Node *ipdom;
          void connect(Node &n) {
            succs.push_back(&n);
            n.preds.push_back(this);
          }
          void connect(Node &n, Edge e) {
            outedges[&n] = e;
            connect(n);
          }
          unsigned long dout() { return succs.size(); }
          Edge *edgeto(Node *n) {
            if (outedges.count(n)) {
              return &outedges.at(n);
            }
            return (Edge *) NULL;
          }
          child_iterator succs_begin() { return succs.begin(); }
          child_iterator succs_end()   { return succs.end(); }
          child_iterator preds_begin() { return preds.begin(); }
          child_iterator preds_end()   { return preds.end(); }
        private:
          std::vector<Node *> succs;
          std::vector<Node *> preds;
          std::map<Node *, Edge> outedges;
      };


      /// constructor - Create a top-level SPScope
      /// @param entry            The entry MBB;
      /// @param isRootTopLevel   True when this scope is a top level scope of
      ///                         a single-path root function.
      explicit SPScope(MachineBasicBlock *entry, bool isRootTopLevel);

      /// constructor - Create a loop SPScope
      explicit SPScope(SPScope *parent, MachineLoop &loop);

      /// destructor - free the child scopes first, cleanup
      ~SPScope();

      /// getParent
      const SPScope *getParent() const { return Parent; }

      /// getHeader
      MachineBasicBlock *getHeader() const { return Blocks.front(); }

      /// getSuccMBB - Get the successors
      const std::vector<const MachineBasicBlock *> getSuccMBBs() const;

      /// getDepth - Get the nesting depth of the SPScope
      unsigned int getDepth() const { return Depth; }

      /// isTopLevel - Returs true if the SPScope is the top-level SPScope
      /// (not a loop)
      bool isTopLevel() const { return (NULL == Parent); }
      //
      /// isRootTopLevel - Returs true if the SPScope is the top-level SPScope
      /// of a single-path root function
      bool isRootTopLevel() const { return RootTopLevel; }

      /// isHeader - Returns true if the specified MBB is the header of this
      /// SPScope
      bool isHeader(const MachineBasicBlock *MBB) const;

      /// isMember - Returns true if the specified MBB is a member of this
      /// SPScope, (non-recursively)
      bool isMember(const MachineBasicBlock *MBB) const;

      /// isSubHeader - Returns true if the specified MBB is header of a
      /// subscope of this scope
      bool isSubHeader(MachineBasicBlock *MBB) const;

      /// hasLoopBound - Returs true if the SPScope is a loop and has a bound
      /// to be accounted for
      bool hasLoopBound() const { return (-1 != LoopBound); }

      /// getLoopBound - Return the loop bound for this SPScope
      int getLoopBound() const { return LoopBound; }

      /// walk - Walk this SPScope recursively
      void walk(SPScopeWalker &walker);

      /// getNumPredicates - Returns the number of predicates required for
      /// this function
      unsigned getNumPredicates() const { return PredCount; }

      /// getPredUse - Returns the guarding predicate for an MBB
      const std::vector<unsigned> * getPredUse(const MachineBasicBlock *) const;

      /// getDefInfo - Returns a pointer to a predicate definition info for
      /// a given MBB, or NULL if no pred info exists for the MBB.
      const PredDefInfo *getDefInfo( const MachineBasicBlock *) const;

      /// getOrCreateDefInfo - Returns a predicate definition info
      /// for a given MBB.
      PredDefInfo &getOrCreateDefInfo(const MachineBasicBlock *);

      /// getNumDefEdges - Returns the number of definition edges for a given
      /// predicate.
      unsigned getNumDefEdges(unsigned pred) const {
        return NumPredDefEdges.at(pred);
      }

      /// getBlocks - Returns the list of basic blocks in this SPScope,
      /// in topological order.
      const std::vector<MachineBasicBlock*> &getBlocks() const {
        return Blocks;
      }

      // dump() - Dump state of this SP scope and the subtree
      void dump(raw_ostream&) const;

      void computePredInfos(void);

      /// addMBB - Add an MBB to the SP scope
      void addMBB(MachineBasicBlock *MBB);

      class FCFG {
        public:
          explicit FCFG(const MachineBasicBlock *header) {
            toexit(nentry);
            nentry.connect(getNodeFor(header),
                std::make_pair((const MachineBasicBlock *)NULL, header));
          }
          Node nentry, nexit;
          Node &getNodeFor(const MachineBasicBlock *MBB) {
            if (!nodes.count(MBB)) {
              nodes.insert(std::make_pair(MBB, Node(MBB)));
            }
            return nodes.at(MBB);
          }
          void toexit(Node &n) { n.connect(nexit); }
          void toexit(Node &n, Edge &e) { n.connect(nexit, e); }
          void postdominators(void);
          raw_ostream& printNode(Node &n);
        private:
          std::map<const MachineBasicBlock*, Node> nodes;
          void _rdfs(Node *, std::set<Node*>&, std::vector<Node*>&);
          Node *_intersect(Node *, Node *);
      };
      // local foward CFG
      FCFG FCFG;

      void addChild(SPScope * child, MachineBasicBlock *childHeader)
      {
        HeaderMap[childHeader] = child;
        Subscopes.push_back(child);
        addMBB(childHeader);
      }

    private:

      /// Typedefs for CD, R and K
      typedef std::set<std::pair<Node*, Edge> > CD_map_entry_t;
      typedef std::map<const MachineBasicBlock*, CD_map_entry_t> CD_map_t;
      /**
       * A map over which predicate is the guard for each basic block.
       */
      typedef std::map<const MachineBasicBlock*, std::vector<unsigned>> MBBPredicates_t;

      void buildfcfg(void);
      /// toposort - sort blocks of this SPScope topologically
      void toposort(void);
      void ctrldep(void);
      void decompose(void);
      void dumpfcfg(void);
      CD_map_t CD;
      // algorithms
      void _walkpdt(Node *a, Node *b, Edge &e);
      void _walkpdt(Node *a, Node *b, Edge &e, Node *edgesrc);
      ////// SNIP /////////

      // parent SPScope
      SPScope *Parent;

      // loop latches
      SmallVector<MachineBasicBlock *, 4> Latches;

      // exit edges
      SmallVector<Edge, 4> ExitEdges;

      // flag that only is set to true if the scope is a top-level scope
      // of a single-path root function
      const bool RootTopLevel;

      // loop bound
      int LoopBound;

      // children as map: header MBB -> SPScope
      std::map<MachineBasicBlock*, SPScope*> HeaderMap;

      // MBBs contained
      std::vector<MachineBasicBlock*> Blocks;

      // sub-scopes
      std::vector<SPScope*> Subscopes;

      // nesting depth
      unsigned int Depth;

      /// Number of predicates used
      unsigned PredCount;

      /// Map MBBs to predicate they use
      MBBPredicates_t PredUse;

      /// PredDefs - Stores predicate define information for each basic block
      std::map<const MachineBasicBlock*, PredDefInfo> PredDefs;

      // number of defining edges for each predicate
      std::vector<unsigned> NumPredDefEdges;

      // get the dual edge of an edge
      Edge getDual(Edge &e) const;

    public:
      /// begin - Iterator begin for MBBs
      iterator begin() { return Blocks.begin(); }

      /// child_end - Iterator end for MBBs
      iterator end() { return Blocks.end(); }

      /// child_begin - Iterator begin for subloops
      child_iterator child_begin() { return Subscopes.begin(); }

      /// child_end - Iterator end for subloops
      child_iterator child_end() { return Subscopes.end(); }

      bool containsMbb(const MachineBasicBlock *mbb)
      {
        return std::find(Blocks.begin(), Blocks.end(), mbb) != Blocks.end();
      }

      boost::optional<SPScope*> findMBBScope(const MachineBasicBlock *mbb)
      {
        boost::optional<SPScope*> found = boost::none;
        for(auto i = Subscopes.begin(), end=Subscopes.end(); i != end; ++i)
        {
          SPScope* child = *i;
          boost::optional<SPScope*> temp = child->findMBBScope(mbb);
          //Ensure two different children don't have the same MBB
          assert(!(temp.is_initialized() && found.is_initialized()));
          if(temp.is_initialized()){
            found = temp;
          }
        }

        if(found.is_initialized()){
          return found;
        } else if(containsMbb(mbb)){
          return boost::make_optional(this);
        }else{
          return boost::none;
        }
      }
  };

///////////////////////////////////////////////////////////////////////////////

  class SPScopeWalker {
    public:
      virtual void nextMBB(MachineBasicBlock *) = 0;
      virtual void enterSubscope(SPScope *) = 0;
      virtual void exitSubscope(SPScope *) = 0;
      virtual ~SPScopeWalker() {};
  };

  // Allow clients to iterate over the Scope FCFG nodes
  template <> struct GraphTraits<SPScope*> {
    typedef SPScope::Node NodeType;
    typedef SPScope::Node::child_iterator ChildIteratorType;

    static NodeType *getEntryNode(SPScope *S) { return &S->FCFG.nentry; }
    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->succs_begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->succs_end();
    }
  };
}
#endif /* TARGET_PATMOS_SINGLEPATH_SPSCOPE_H_ */
