//==-- PatmosSinglePathInfo.h - Analysis Pass for SP CodeGen -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
// This file defines a pass to compute imformation for single-path converting
// seleced functions.
//
//===---------------------------------------------------------------------===//


#ifndef _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
#define _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_

#include <Patmos.h>
#include <PatmosTargetMachine.h>
#include <llvm/IR/Module.h>
#include <llvm/ADT/BitVector.h>
#include <llvm/CodeGen/MachineFunctionPass.h>

#include <vector>
#include <set>
#include <map>


// define for more detailed debugging output
#define PATMOS_SINGLEPATH_TRACE

#ifdef PATMOS_SINGLEPATH_TRACE
#define DEBUG_TRACE(x) DEBUG(x)
#else
#define DEBUG_TRACE(x) /*empty*/
#endif


namespace llvm {

  class MachineLoop;



///////////////////////////////////////////////////////////////////////////////

  // forward decl
  class SPScope;
  class SPScopeWalker;

  template <> struct GraphTraits<SPScope*>;

  /// PatmosSinglePathInfo - Single-Path analysis pass
  class PatmosSinglePathInfo : public MachineFunctionPass {
    private:
      const PatmosTargetMachine &TM;
      const PatmosSubtarget &STC;
      const PatmosInstrInfo *TII;

      /// Set of functions yet to be analyzed
      std::set<std::string> FuncsRemain;

      /// Root SPScope
      SPScope *Root;

      /// Map MBBs to their innermost scopes
      std::map<const MachineBasicBlock *, SPScope *> MBBScopeMap;

      /// Analyze a given MachineFunction
      void analyzeFunction(MachineFunction &MF);

      /// createSPScopeTree - Create an SPScope tree, return the root scope.
      /// The tree needs to be destroyed by the client,
      /// by deleting the root scope.
      SPScope *createSPScopeTree(MachineFunction &MF);

      /// Fail with an error if MF is irreducible.
      void checkIrreducibility(MachineFunction &MF) const;

    public:
      /// Pass ID
      static char ID;

      /// isEnabled - Return true if there are functions specified to
      /// to be converted to single-path code.
      static bool isEnabled();

      /// isEnabled - Return true if a particular function is specified to
      /// to be converted to single-path code.
      static bool isEnabled(const MachineFunction &MF);

      static bool isConverting(const MachineFunction &MF);

      static bool isRoot(const MachineFunction &MF);

      static bool isReachable(const MachineFunction &MF);

      static bool isMaybe(const MachineFunction &MF);

      /// getRootNames - Fill a set with the names of
      /// single-path root functions
      static void getRootNames(std::set<std::string> &S);

      /// PatmosSinglePathInfo - Constructor
      PatmosSinglePathInfo(const PatmosTargetMachine &tm);

      /// doInitialization - Initialize SinglePathInfo pass
      virtual bool doInitialization(Module &M);

      /// doFinalization - Finalize SinglePathInfo pass
      virtual bool doFinalization(Module &M);

      /// getAnalysisUsage - Specify which passes this pass depends on
      virtual void getAnalysisUsage(AnalysisUsage &AU) const;

      /// runOnMachineFunction - Run the SP converter on the given function.
      virtual bool runOnMachineFunction(MachineFunction &MF);

      /// getPassName - Return the pass' name.
      virtual const char *getPassName() const {
        return "Patmos Single-Path Info";
      }

      /// print - Convert to human readable form
      virtual void print(raw_ostream &OS, const Module* = 0) const;

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
      /// dump - Dump the state of this pass
      virtual void dump() const;
#endif

      /// isToConvert - Return true if the function should be if-converted
      bool isToConvert(MachineFunction &MF) const;

      /// getRootScope - Return the Root SPScope for this function
      SPScope *getRootScope() const { return Root; }

      /// getScopeFor - Return the innermost scope of an MBB
      SPScope *getScopeFor(const MachineBasicBlock *MBB) const;

      /// walkRoot - Walk the top-level SPScope
      void walkRoot(SPScopeWalker &walker) const;
  };

///////////////////////////////////////////////////////////////////////////////


  class SPScope {
    friend class PatmosSinglePathInfo;
    friend struct GraphTraits<SPScope*>;

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
      int getPredUse(const MachineBasicBlock *) const;

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


    private:

      /// Typedefs for CD, R and K
      typedef std::set<std::pair<Node*, Edge> > CD_map_entry_t;
      typedef std::map<const MachineBasicBlock*, CD_map_entry_t> CD_map_t;
      typedef std::vector<CD_map_entry_t>                  K_t;
      typedef std::map<const MachineBasicBlock*, unsigned> R_t;

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

      // local foward CFG
      FCFG FCFG;

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
      std::map<const MachineBasicBlock*, unsigned> PredUse;

      /// PredDefs - Stores predicate define information for each basic block
      std::map<const MachineBasicBlock*, PredDefInfo> PredDefs;

      // number of defining edges for each predicate
      std::vector<unsigned> NumPredDefEdges;

      /// addMBB - Add an MBB to the SP scope
      void addMBB(MachineBasicBlock *MBB);

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
  };

///////////////////////////////////////////////////////////////////////////////

  class SPScopeWalker {
    public:
      virtual void nextMBB(MachineBasicBlock *) = 0;
      virtual void enterSubscope(SPScope *) = 0;
      virtual void exitSubscope(SPScope *) = 0;
      virtual ~SPScopeWalker() {};
  };

///////////////////////////////////////////////////////////////////////////////

  // Allow clients to walk the list of nested SPScopes
  template <> struct GraphTraits<PatmosSinglePathInfo*> {
    typedef SPScope NodeType;
    typedef SPScope::child_iterator ChildIteratorType;

    static NodeType *getEntryNode(const PatmosSinglePathInfo *PSPI) {
      return PSPI->getRootScope();
    }
    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->child_begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->child_end();
    }
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

} // end of namespace llvm

#endif // _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
