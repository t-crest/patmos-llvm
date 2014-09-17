//===-- PatmosStackCacheAnalysis.cpp - Analysis of the stack-cache usage. -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Analyze the usage of the stack cache based on an machine-level call graph.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-stack-cache-analysis"
#undef PATMOS_PRINT_USER_BOUNDS
#undef PATMOS_TRACE_SENS_REMOVAL
#undef PATMOS_TRACE_BB_LIVEAREA
#undef PATMOS_TRACE_CG_OCCUPANCY
#undef PATMOS_TRACE_CG_OCCUPANCY_ILP
#undef PATMOS_TRACE_WORST_SITE_OCCUPANCY
#undef PATMOS_TRACE_PATH_OCCUPANCY
#undef PATMOS_TRACE_DETAILED_RESULTS

#include "Patmos.h"
#include "PatmosCallGraphBuilder.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosStackCacheAnalysis.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineModulePass.h"
#include "llvm/CodeGen/PMLExport.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <set>
#include <fstream>

/// Utility for deleting owned members of an object
#define DELETE_MEMBERS(vec) \
    while(! vec.empty()) { \
      delete vec.back(); \
      vec.pop_back(); \
    } \

#define YAML_MEMBER(type, name) \
  yaml::type y ## name

using namespace llvm;

INITIALIZE_PASS(PatmosStackCacheAnalysisInfo, "scainfo",
                "Stack Cache Analysis Info", false, true)
namespace llvm {
char PatmosStackCacheAnalysisInfo::ID = 0;

ModulePass *createPatmosStackCacheAnalysisInfo(const PatmosTargetMachine &tm) {
  return new PatmosStackCacheAnalysisInfo(tm);
}
}

/// This is expected to be a program that takes one argument specifying the name
/// of a LP file to solve and generate a file with the same name and an appended
/// suffix .sol containing the numeric value of the integer solution of the LP.
static cl::opt<std::string> Solve_ilp(
  "mpatmos-ilp-solver",
  cl::init("solve_ilp.sh"),
  cl::desc("Path to an ILP solver."),
  cl::Hidden);

/// Option to specify a file containing user-supplied bounds when solving ILP
/// problems (for regions of the call graph with recursion).
static cl::opt<std::string> BoundsFile(
  "mpatmos-stack-cache-analysis-bounds",
  cl::desc("File containing bounds for the stack cache analysis."),
  cl::Hidden);

/// EnableViewSCAGraph - Option to enable the rendering of the Spill Cost 
/// Analysis graph.
static cl::opt<bool> EnableViewSCAGraph(
  "mpatmos-view-sca-graph",
  cl::init(false),
  cl::desc("Show the Spill Cost analysis after the Patmos Stack Cache Analysis."),
  cl::Hidden);

/// EnableEnsureOpt - Option to enable ensure optimization during analysis
static cl::opt<bool> EnableEnsureOpt(
  "mpatmos-sca-remove-ensures",
  cl::init(false),
  cl::desc("Remove unnecessary ensure instructions during Stack Cache Analysis."),
  cl::Hidden);

/// RootOccupied - Debug option: start with fully occupied root node
static cl::opt<bool> RootOccupied(
  "mpatmos-sca-root-occupied",
  cl::init(false),
  cl::desc("Stack Cache Analysis assumes fully occupied root node."),
  cl::ReallyHidden);

static cl::opt<std::string> SCAPMLExport("mpatmos-sca-serialize",
   cl::desc("Export PML specification of generated machine code to FILE"),
   cl::init(""));

namespace llvm {
  /// Count the number of SENS instructions removed.
  STATISTIC(RemovedSENS, "SENS instructions removed (zero fills).");

  /// Count the number of SENS instructions that could be removed.
  STATISTIC(NonFillingSENS, "SENS instructions fill no data.");

  /// Count the number of SENS instructions that could not be removed.
  STATISTIC(FillingSENS, "SENS instructions filling data.");

  /// Count the number of SENS instructions that potentially have to fill the 
  /// total size given as their argument.
  STATISTIC(FullyFillingSENS, "SENS instructions fully filling (thereof).");

  /// Count the number of SRES instructions potentially spilling.
  STATISTIC(SpillingSRES, "SRES instructions spilling.");

  /// Count the number of SRES instructions potentially fully spilling the total 
  /// szie given as their argument.
  STATISTIC(FullySpillingSRES, "SRES instructions fully spilling (of spilling)");

  /// Count the number of SRES instructions that certainly do not spill.
  STATISTIC(NonSpillingSRES, "SRES instructions guaranteed to not spill.");

  /// Count the total number of nodes in the SCA graph.
  STATISTIC(TotalSCAGraphSize, "Total SCA graph size.");

  /// Count the total number of nodes in the pruned SCA graph.
  STATISTIC(PrunedSCAGraphSize, "Pruned SCA graph size.");

  /// Count the total number of ILPs solved.
  STATISTIC(ILPs, "Number of ILPs solved.");

  /// Count the total number of functions (excluding dead functions).
  STATISTIC(Functions, "Number of machine functions.");

  /// Prefixes for ILP variable names
  enum ilp_prefix {
    T,
    Z,
    W,
    OF,
    X
  };

  // forward definition.
  class SCANode;
  class SCAEdge;
  class SpillCostAnalysisGraph;
  bool operator <(const SCAEdge &a, const SCAEdge &b);
  llvm::raw_ostream &operator <<(llvm::raw_ostream &O, ilp_prefix Prefix);

  namespace yaml {
    template <>
      struct MappingTraits< SCANode >;
  }

  /// Set of SCANodes.
  typedef std::set<SCANode*> SCANodeSet;

  /// Set of SCAEdges.
  typedef std::set<SCAEdge> SCAEdgeSet;

  /// Map call graph nodes to spill cost information.
  typedef std::map<std::pair<MCGNode*, unsigned int>, SCANode*> MCGSCANodeMap;

  /// Link context-sensitive information on the spill costs at a call graph 
  /// nodes to calling contexts.
  class SCAEdge
  {
  private:
    /// The calling context's node.
    SCANode *Caller;

    /// The called context's node.
    SCANode *Callee;

    /// The corresponding call site.
    MCGSite *Site;
  public:
    SCAEdge(SCANode *caller, SCANode *callee, MCGSite *site) :
        Caller(caller), Callee(callee), Site(site)
    {
    }

    /// Return the calling context's node.
    SCANode *getCaller() const
    {
      return Caller;
    }

    /// Return the called context's node.
    SCANode *getCallee() const
    {
      return Callee;
    }

    /// Return the calling site.
    MCGSite *getSite() const
    {
      return Site;
    }
  };

  /// Context-sensitive information on the spill costs at a call graph node.
  class SCANode
  {
  private:
    /// Call graph node associated with spill costs.
    MCGNode *Node;

    /// Number of bytes occupied in the stack cache before entering the 
    /// function.
    unsigned int Occupancy;

    /// The maximum stack occupancy of the node.
    /// \see PatmosStackCacheAnalysis::computeMinMaxOccupancy
    unsigned int MaxOccupancy;

    /// Occupancy remaining from some parent.
    /// This is for debugging purposes only.
    unsigned int RemainingOccupancy;

    /// Cost associated with spilling at this node.
    unsigned int SpillCost;

    /// Flag indicating whether the CFG of the corresponding function contains
    /// a path without calls.
    bool HasCallFreePath;

    /// The parent calling contexts of this node.
    SCAEdgeSet Parents;

    /// The calling contexts originating from this node.
    SCAEdgeSet Children;

    /// Flag indicating whether this node should be visualized in DOT dumps.
    bool IsVisible;

    /// Flag indicating whether this node is valid. A node is valid when a path
    /// from the root node to the node exists that fits into the root node's 
    /// maximum occupancy (which is not limited by the stack cache size).
    bool IsValid;
  public:
    SCANode(MCGNode *node, unsigned int occupancy, unsigned int maxoccupancy,
            unsigned int spillcost, bool hascallfreepath) :
        Node(node), Occupancy(occupancy), MaxOccupancy(maxoccupancy), 
        RemainingOccupancy(0), SpillCost(spillcost), 
        HasCallFreePath(hascallfreepath), IsVisible(false), IsValid(false)
    {}

    void initYAML(const PatmosSubtarget &STC)
    {
      if (Node->getMF())
        yFunction = Node->getMF()->getName();
      else
        yFunction = "none";
      ySpillBlocks = SpillCost; // export in bytes
    }

    /// Returns the associated call graph node.
    MCGNode *getMCGNode() const
    {
      return Node;
    }

    /// Return the stack cache occupancy associated with the node.
    unsigned int getOccupancy() const
    {
      return Occupancy;
    }

    /// Return the spill cost associated with the node.
    unsigned int getSpillCost() const
    {
      return SpillCost;
    }

    /// Return the remaining occupancy wrt. the root node's maximum stack
    /// occupancy.
    unsigned int getRemainingOccupancy() const
    {
      return RemainingOccupancy;
    }

    /// Set the remaining occupancy wrt. the root node's maximum stack
    /// occupancy.
    void setRemainingOccupancy(unsigned int remainingoccupancy)
    {
      RemainingOccupancy = remainingoccupancy;
    }

    /// Return the maximum stack occupancy for this node.
    unsigned int getMaxOccupancy() const
    {
      return MaxOccupancy;
    }

    /// Return whether the node's CFG contains at least one call-free path.
    bool hasCallFreePath() const
    {
      return HasCallFreePath;
    }

    /// Set the node's visible flag to and propagate it to all its parents.
    /// Nodes that are not valid are automatically marked not to be visible.
    void setVisible()
    {
      if (!IsVisible && IsValid) {
        IsVisible = true;

        /// propagate to parents
        for(SCAEdgeSet::const_iterator i(Parents.begin()), ie(Parents.end());
            i != ie; i++) {
          i->getCaller()->setVisible();
        }
      }
    }

    /// Return whether this node should be visualized in DOT graph dumps.
    bool isVisible() const
    {
      return IsVisible;
    }

    /// The the valid flag of this node.
    void setValid()
    {
      IsValid = true;
    }

    /// Return whether this node is valid.
    /// \see IsValid
    bool isValid() const
    {
      return IsValid;
    }

    /// addParent - Create a link between this node and its parent.
    void addParent(SCANode *parent, MCGSite *site)
    {
      SCAEdge newEdge(parent, this, site);

      parent->Children.insert(newEdge);
      Parents.insert(newEdge);
    }

    /// Return the parents of this node.
    SCAEdgeSet &getParents()
    {
      return Parents;
    }

    /// Return the parents of this node.
    const SCAEdgeSet &getParents() const
    {
      return Parents;
    }

    /// Return the children of this node.
    SCAEdgeSet &getChildren()
    {
      return Children;
    }

    /// Return the children of this node.
    const SCAEdgeSet &getChildren() const
    {
      return Children;
    }

    YAML_MEMBER(Name, Id);
    YAML_MEMBER(Name, Function);
    YAML_MEMBER(Name, SpillBlocks);
    friend struct yaml::MappingTraits<SCANode*>;
  };

  /// A graph representing context-sensitive information on the spill costs at
  /// call graph nodes -- aka SCA graph or SC-SCA graph.
  class SpillCostAnalysisGraph
  {
  private:
    /// Spill cost information available for individual call graph nodes and
    /// calling contexts.
    MCGSCANodeMap Nodes;

    /// The root node of the spill cost graph.
    SCANode *Root;

    /// Subtarget information (stack cache sizes)
    const PatmosSubtarget &STC;

    unsigned yamlId;
  public:
    SpillCostAnalysisGraph(const PatmosSubtarget &s) : STC(s), yamlId(100) {}

    /// makeRoot - Construct the root node of the SCA graph.
    SCANode *makeRoot(MCGNode *node, unsigned int maxoccupancy,
                      bool hascallfreepath)
    {
      assert(Nodes.empty());
      unsigned rootoccupancy = RootOccupied ? STC.getStackCacheSize() : 0;

      // create the root node.
      Root = new SCANode(node, rootoccupancy, maxoccupancy, 0, hascallfreepath);

      Root->yId = yamlId++;

      // store the root node.
      Nodes[std::make_pair(node, rootoccupancy)] = Root;

      return Root;
    }

    /// makeNode - Construct a new SCA node or return an already existing one.
    /// Returns true when the node was newly created, false otherwise. The node
    /// itself is returned using the argument result.
    bool makeNode(MCGNode *node, unsigned int occupancy, unsigned int spillcost,
                  unsigned int maxoccupancy, bool hascallfreepath,
                  SCANode *&result)
    {
      MCGSCANodeMap::iterator tmp(Nodes.find(std::make_pair(node, occupancy)));

      result = NULL;
      if (tmp == Nodes.end()) {
        // create a new node
        result = new SCANode(node, occupancy, maxoccupancy, spillcost,
                             hascallfreepath);
        result->yId = yaml::Name(yamlId++);

        // store the newly created node
        Nodes[std::make_pair(node, occupancy)] = result;

        return true;
      }
      else {
        // use the existing node
        result = tmp->second;

        return false;
      }
    }

    /// deleteNode - Free the node and remove it from the node list as well as
    /// parent/children lists.
    void deleteNode(SCANode *N)
    {
      // remove the node from the node list
      Nodes.erase(std::make_pair(N->getMCGNode(), N->getOccupancy()));

      // remove all edges leading to the node in its parent's children lists
      for(SCAEdgeSet::const_iterator i(N->getParents().begin()),
          ie(N->getParents().end()); i != ie; i++) {
        i->getCaller()->getChildren().erase(*i);
      }

      // remove all edges originating from the node in its children's parents
      // lists
      for(SCAEdgeSet::const_iterator i(N->getChildren().begin()),
          ie(N->getChildren().end()); i != ie; i++) {
        i->getCallee()->getParents().erase(*i);
      }

      delete N;
    }

    /// Return the graph's root node.
    SCANode *getRoot() const
    {
      return Root;
    }

    /// getNodes - Return the nodes of the SCA graph.
    /// @return The nodes of the SCA graph.
    const MCGSCANodeMap &getNodes() const
    {
      return Nodes;
    }

    /// Free all SCA nodes associated with this graph.
    ~SpillCostAnalysisGraph()
    {
      for(MCGSCANodeMap::iterator i(Nodes.begin()), ie(Nodes.end()); i != ie;
          i++) {
        delete i->second;
      }
    }
  };

  /// Information concerning a specific SCC.
  struct SCCInfo {
    /// Text fragment to be inserted right after the objective function.
    std::string ObjectiveFunction;

    /// Text fragment to be inserted right after the constraints.
    std::string Constraints;

    /// Text fragment to be inserted right after the variable definition.
    std::string Variables;
  };

  /// Map function names to SCC infos.
  typedef std::map<std::string, SCCInfo> SCCInfos;

  /// Class to parse bounds specifications constraining paths through SCCs in
  /// the call graph during ILP solving.
  class BoundsInformation {
    /// Information specific to specific SCCs.
    SCCInfos Infos;

    /// appendDefaultConstraitns - append some default constraints, e.g., for 
    /// the vfprintf function.
    void appendDefaultConstraitns()
    {
      /// the vfprintf function calls itself, but only once.
      SCCInfo vfprintf = {"",
                          "usr:     + X_vfprintf_r <= 2\n",
                          ""};
      Infos["_vfprintf_r"] = vfprintf;
    }

    /// parseBoundsFile - read user defined bounds for the ILP solving from a
    /// file.
    /// The expected file format is based on lines, each line should look 
    /// something like this:
    /// [name]: {[text]}{[text]}{[text]} \n
    void parseBoundsFile(const std::string &boundsfile) {
      // nothing to read?
      if (boundsfile.empty())
        return;

      // open file
      std::ifstream IS(boundsfile.c_str());

      // worked?
      if (!IS.good()) {
        errs() << "Error: Failed to read from file '" << boundsfile << "'.\n";
      }

      // read one line at a time
      while(IS.good()) {
        std::string line;
        std::getline(IS, line, ';');

        if (IS.eof())
          continue;

        // the format of a line is:
        // [name]: {[text]} {[text]} {[text]} \n
        std::size_t name_start = line.find_first_not_of(" \t\n");
        std::size_t name_end = line.find_first_of(':', name_start);

        std::size_t objective_start = line.find_first_of('{', name_end);
        std::size_t objective_end = line.find_first_of('}', objective_start);

        std::size_t constraint_start = line.find_first_of('{', objective_end);
        std::size_t constraint_end = line.find_first_of('}', constraint_start);

        std::size_t variables_start = line.find_first_of('{', constraint_end);
        std::size_t variables_end = line.find_first_of('}', variables_start);

        if (name_start == std::string::npos || name_end == std::string::npos ||
            objective_start == std::string::npos ||
            objective_end == std::string::npos ||
            constraint_start == std::string::npos ||
            constraint_end == std::string::npos ||
            variables_start == std::string::npos ||
            variables_end == std::string::npos) {
          errs() << "Error: Invalid line in ILP bounds file: " << line << ".\n";
        }
        else {
          // extract bounds information
          std::string name(line.substr(name_start, name_end - name_start));
          std::string objective(line.substr(objective_start + 1,
                                          objective_end - objective_start - 1));
          std::string constraint(line.substr(constraint_start + 1,
                                        constraint_end - constraint_start - 1));
          std::string variables(line.substr(variables_start + 1,
                                          variables_end - variables_start - 1));

          // debug output
#ifdef PATMOS_PRINT_USER_BOUNDS
          dbgs() << "bounds: " << name << ": [" << objective << "]["
                                                << constraint << "]["
                                                << variables <<"]\n";
#endif // PATMOS_PRINT_USER_BOUNDS

          // add bounds information
          SCCInfo tmp = {objective, constraint, variables};
          Infos[name] = tmp;
        }
      }
    }
  public:
    /// Parse the bounds specification from a file.
    BoundsInformation(const std::string &boundsfile)
    {
      parseBoundsFile(boundsfile);
      appendDefaultConstraitns();
    }

    /// getInfo - Retrieve information for a specific SCC.
    const SCCInfo &getInfo(const MCGNodes &SCC) const {
      for(MCGNodes::const_iterator i(SCC.begin()), ie(SCC.end()); i != ie;
          i++) {
        // TODO: maybe add support for unknown functions.
        if (!(*i)->isUnknown() &&
            hasInfo((*i)->getMF()->getFunction()->getName()))
          return getInfo((*i)->getMF()->getFunction()->getName());
      }

      dbgs() << "Error: Missing bounds for SCC: (";
      for (MCGNodes::const_iterator i(SCC.begin()), ie(SCC.end()); i != ie;
           i++) {
        dbgs() << "'" << **i << "' ";
      }
      dbgs() << ")\nbounds available: ";
      for (SCCInfos::const_iterator i(Infos.begin()), ie(Infos.end()); i != ie;
           i++) {
        dbgs() << "'" << i->first << "' ";
      }
      dbgs() << "\n";
      assert(false && "Missing bounds for SCC during stack cache analysis.");
      abort();
    }

    /// getInfo - Retrieve information for a specific SCC represented by a
    /// function.
    const SCCInfo &getInfo(const std::string &function) const {
      SCCInfos::const_iterator tmp(Infos.find(function));
      assert(tmp != Infos.end());
      return tmp->second;
    }

    /// hasInfo - Check if information for a specific SCC represented by a
    /// function is available.
    bool hasInfo(const std::string &function) const {
      SCCInfos::const_iterator tmp(Infos.find(function));
      return tmp != Infos.end();
    }
  };

  namespace yaml {
    template <>
      struct MappingTraits<SCANode*> {
        static void mapping(IO &io, SCANode *&n) {
          assert(n);
          io.mapRequired("id", n->yId);
          io.mapRequired("function", n->yFunction);
          io.mapRequired("spillsize", n->ySpillBlocks);
        }
      };
    YAML_IS_PTR_SEQUENCE_VECTOR(SCANode)

    struct SCAEdge {
      Name Src;
      Name Dst;
      Name CallBlock;
      Name CallIndex;
      SCAEdge(Name src, Name dst, Name bb, unsigned idx)
        : Src(src), Dst(dst), CallBlock(bb), CallIndex(idx) {}
    };
    template <>
      struct MappingTraits<SCAEdge*> {
        static void mapping(IO &io, SCAEdge *&e) {
          assert(e);
          io.mapRequired("src", e->Src);
          io.mapRequired("dst", e->Dst);
          io.mapRequired("callblock", e->CallBlock);
          io.mapRequired("callindex", e->CallIndex);
        }
      };
    YAML_IS_PTR_SEQUENCE_VECTOR(SCAEdge)

    struct SCAGraph {
      std::vector<SCANode*> N;
      std::vector<SCAEdge*> E;
      SCAGraph() {}
      ~SCAGraph() {
        DELETE_MEMBERS(E);
      }
      void addEdge(SCANode *src, SCANode *dst,
          const MachineBasicBlock *MBB, unsigned index) {
        E.push_back(new SCAEdge(src->yId, dst->yId, MBB->getName(), index));
      }
    };

    template <>
      struct MappingTraits< SCAGraph > {
        static void mapping(IO &io, SCAGraph& g) {
          io.mapRequired("nodes",   g.N);
          io.mapRequired("edges",   g.E);
        }
      };

    struct SCADoc {
      SCAGraph SCAG;
    };
    template <>
    struct MappingTraits< SCADoc > {
      static void mapping(IO &io, SCADoc& doc) {
        io.mapRequired("sca-graph", doc.SCAG);
      }
    };
  } // end namespace yaml

  /// Pass to analyze the occupancy and displacement of Patmos' stack cache.
  class PatmosStackCacheAnalysis : public MachineModulePass {
  private:
    /// Work list of basic blocks.
    typedef std::set<MachineBasicBlock*> MBBs;

    /// Set of call graph nodes.
    typedef std::set<MCGNode*> MCGNodeSet;

    /// Set of call graph sites.
    typedef std::set<MCGSite*> MCGSiteSet;

    /// List of call graph SCCs and a flag indicating whether the SCC actually 
    /// contains loops.
    typedef std::vector<std::pair<MCGNodes, bool> > MCGNSCCs;

    /// Map basic blocks to an unsigned integer.
    typedef std::map<MachineBasicBlock*, unsigned int> MBBUInt;

    /// Map basic blocks to a boolean.
    typedef std::map<MachineBasicBlock*, bool> MBBBool;

    /// Map call graph nodes to booleans.
    typedef std::map<MCGNode*, bool> MCGNodeBool;

    /// Map call graph nodes to their (potentially trivial SCC).
    typedef std::map<MCGNode*, std::pair<MCGNodes, bool>*> MCGNodeSCC;

    /// Map call graph nodes to an unsigned integer.
    typedef std::map<MCGNode*, unsigned int> MCGNodeUInt;

    /// Map call sites to an unsigned integer.
    typedef std::map<MCGSite*, unsigned int> MCGSiteUInt;

    /// List of ensures and their effective sizes.
    typedef std::map<MachineInstr*, unsigned int> SIZEs;

    typedef std::map<const MachineInstr*, std::pair<MachineBasicBlock*,
                                                    unsigned> > MInstrIndex;

    /// Track for each call graph node the maximum stack occupancy.
    MCGNodeUInt MaxOccupancy;

    /// Track for each call graph node the minimum stack occupancy.
    MCGNodeUInt MinOccupancy;

    /// Track the worst-case stack occupancy before every call site, assuming
    /// a full stack cache on function entry.
    MCGSiteUInt WorstCaseSiteOccupancy;

    /// Track functions with call-free paths in them.
    MCGNodeBool IsCallFree;

    /// Subtarget information (stack cache block size)
    const PatmosSubtarget &STC;

    /// Instruction information
    const TargetInstrInfo &TII;

    /// Summarize the stack cache analysis results for reserve instructions as
    /// a spill cost graph.
    SpillCostAnalysisGraph SCAGraph;

    /// Bounds to solve ILPs during stack cache analysis.
    const BoundsInformation BI;

    MInstrIndex MiMap;
  public:
    /// Pass ID
    static char ID;

    PatmosStackCacheAnalysis(const PatmosTargetMachine &tm) :
        MachineModulePass(ID), STC(tm.getSubtarget<PatmosSubtarget>()),
        TII(*tm.getInstrInfo()), SCAGraph(STC), BI(BoundsFile)
    {
      initializePatmosCallGraphBuilderPass(*PassRegistry::getPassRegistry());
    }

    /// getAnalysisUsage - Inform the pass manager that nothing is modified.
    virtual void getAnalysisUsage(AnalysisUsage &AU) const
    {
      AU.setPreservesAll();
      AU.addRequired<PatmosCallGraphBuilder>();
      AU.addRequired<PatmosStackCacheAnalysisInfo>();

      ModulePass::getAnalysisUsage(AU);
    }

    /// getMinMaxOccupancy - Find the minimum/maximum stack occupancy for a call
    /// graph node, including all its children in the call graph.
    /// \see computeMinMaxOccupancy
    /// \see getBytesReserved
    unsigned int getMinMaxOccupancy(MCGNode *Node, bool Maximize) const {
      const MCGNodeUInt &map(Maximize ? MaxOccupancy : MinOccupancy);
      MCGNodeUInt::const_iterator nodeOccupancy(map.find(Node));

      if (Node->isDead())
        return 0;
      else if (nodeOccupancy == map.end())
        return Maximize ? std::numeric_limits<unsigned int>::max() : 0;
      else
        return nodeOccupancy->second;
    }

    /// getMinOccupancy - Find the computed minimum stack occupancy for a call
    /// graph node, including all its children in the call graph.
    unsigned int getMinOccupancy(MCGNode *Node) const {
      return std::min(STC.getStackCacheSize(), getMinMaxOccupancy(Node, false));
    }

    /// getMaxOccupancy - Find the computed maximum stack occupancy for a call
    /// graph node, including all its children in the call graph.
    unsigned int getMaxOccupancy(MCGNode *Node) const {
      return std::min(STC.getStackCacheSize(), getMinMaxOccupancy(Node, true));
    }

    /// getBytesReserved - Get the number of bytes reserved at the entry of a
    /// call graph node. Returns 0 for UNKNOWN nodes.
    unsigned int getBytesReserved(const MCGNode *Node)
    {
      if (Node->isUnknown())
        return 0;
      else {
        MachineFunction *MF = Node->getMF();
        PatmosMachineFunctionInfo *PMFI =
                                       MF->getInfo<PatmosMachineFunctionInfo>();

        // TODO a function might contain inline asm code that might use 
        // SRES/SFREE, we should check for that.

        return STC.getAlignedStackFrameSize(PMFI->getStackCacheReservedBytes());
      }
    }

    /// computeMinMaxOccupancy - Visit a call graph node and determine its 
    /// minimum/maximum stack occupancy, including all its children in the call 
    /// graph.
    void computeMinMaxOccupancy(MCGNodeSCC &SCCMap, MCGNode *Node,
                                MCGNodeUInt &succCount, MCGNodes &WL,
                                bool Maximize)
    {
      // keep track of the total occupancy of the node and its children
      unsigned int totalOccupancy;

      // get the local stack occupancy of the call graph node
      unsigned int nodeOccupancy = getBytesReserved(Node);

      // handle some cases:
      // (1) dead functions (2) SCCs, and (3) regular nodes
      if (Node->isDead()) {
        // ok, dead nodes don't do anything
        return;
      }
      else if (SCCMap[Node]->second) {
        // the node is in an SCC! -> make an ILP
        // note: we know here that all successors of the entire SCC have been
        // handled
        totalOccupancy = computeMinMaxOccupancyILP(SCCMap[Node]->first, Node,
                                                   Maximize);
        assert(totalOccupancy >= nodeOccupancy);
      }
      else {
        // keep track of the occupancy of children in the call graph
        unsigned int childOccupancy = (Maximize || IsCallFree[Node]) ? 0 :
                                       std::numeric_limits<unsigned int>::max();

        // check all called functions
        const MCGSites &callSites(Node->getSites());
        for(MCGSites::const_iterator i(callSites.begin()), ie(callSites.end());
            i != ie; i++) {
          // get the child's stack occupancy
          if (Maximize) {
            childOccupancy = std::max(childOccupancy,
                                      getMaxOccupancy((*i)->getCallee()));
          }
          else {
            childOccupancy = std::min(childOccupancy,
                                      getMinOccupancy((*i)->getCallee()));
          }
        }

        // include the current function's stack occupancy
        totalOccupancy = childOccupancy + nodeOccupancy;
      }

      // store the call graph node's stack occupancy
      if (Maximize)
        MaxOccupancy[Node] = totalOccupancy;
      else
        MinOccupancy[Node] = totalOccupancy;

      // get unique predecessors in the call graph, excluding dead functions and
      // predecessors within the same SCC
      MCGNodeSet preds;
      const MCGSites &callingSites(Node->getCallingSites());
      for(MCGSites::const_iterator i(callingSites.begin()),
          ie(callingSites.end()); i != ie; i++) {
        MCGNode *caller = (*i)->getCaller();
        // do not consider dead functions and functions in the same SCC here
        if (!caller->isDead() && SCCMap[caller] != SCCMap[Node]) {
          const MCGNodes &predSCC(SCCMap[caller]->first);
          preds.insert(predSCC.begin(), predSCC.end());
        }
      }

      // update the work list by checking the predecessor list
      for(MCGNodeSet::const_iterator i(preds.begin()), ie(preds.end()); i != ie;
          i++) {
        assert(succCount[*i] > 0);
        if (--succCount[*i] == 0)
          WL.push_back(*i);
      }
    }

    /// computeMinMaxOccupancy - Visit all nodes of the call graph and compute
    /// the minimum/maximum stack occupancy for each of them (including their
    /// respective children in the call graph).
    ///
    /// The algorithm uses a simple work list (and successor counters) to 
    /// traverse the call graph in some topological order. During the traversal 
    /// the minimum/maximum occupancy is propagated from children upwards to the 
    /// root(s) of the call graph.
    ///
    /// Note that SCCs are considered as if they were collapsed into a single 
    /// node. Within SCCs an ILP formulation is used to bound the occupancy.
    /// 
    /// \see computeMinMaxOccupancyILP
    void computeMinMaxOccupancy(const MCallGraph &G, bool Maximize)
    {
      // list of SCCs in the call graph and mapping to/from call graph nodes
      MCGNSCCs SCCs;
      MCGNodeSCC SCCMap;

      // get all call graph nodes
      const MCGNodes &nodes(G.getNodes());

      // make sure we have enough space for all SCCs without re-allocation
      SCCs.reserve(nodes.size());

      // get SCCs
      typedef scc_iterator<MCallGraph> PCGSCC_iterator;
      for(PCGSCC_iterator s(scc_begin(G)); !s.isAtEnd(); s++) {

        // keep track of call graph nodes and their SCCs.
        SCCs.push_back(std::make_pair(*s, s.hasLoop()));
        for(MCGNodes::iterator n((*s).begin()), ne((*s).end()); n != ne; n++) {
          SCCMap[*n] = &SCCs.back();
        }

      }

      // initialize the work list and successor counters
      MCGNodeUInt succCount;
      MCGNodes WL;
      for(MCGNSCCs::const_iterator i(SCCs.begin()), ie(SCCs.end()); i != ie;
          i++) {
        // get info of the current SCC
        const MCGNodes *SCC = &i->first;

        // get the number of successors of the SCC that are not in that SCC.
        MCGNodeSet succs;
        for(MCGNodes::const_iterator j(SCC->begin()), je(SCC->end()); j != je;
            j++) {
          for(MCGSites::const_iterator k((*j)->getSites().begin()),
              ke((*j)->getSites().end()); k != ke; k++) {
            // check if the two are in the same SCC
            if (&SCCMap[(*k)->getCallee()]->first != SCC) {
              succs.insert((*k)->getCallee());
            }
          }
        }

        // be sure to keep all nodes within an SCC off the WL for now
        for(MCGNodes::const_iterator j(SCC->begin()), je(SCC->end()); j != je;
            j++) {
          if (succs.size() == 0)
            WL.push_back(*j);
          else
            succCount[*j] = succs.size();
        }
      }

      // process nodes in topological order
      while(!WL.empty()) {
        // pop some node from the work list
        MCGNode *tmp = WL.back();
        WL.pop_back();

        // compute its stack occupancy
        computeMinMaxOccupancy(SCCMap, tmp, succCount, WL, Maximize);
      }

#ifdef PATMOS_TRACE_CG_OCCUPANCY
      DEBUG(dbgs() << (Maximize ? ">>>>>>>>>>>>>>> MAX >>>>>>>>>>>>>>>>\n" :
                                  "<<<<<<<<<<<<<<< MIN <<<<<<<<<<<<<<<<\n"););
      DEBUG(
        for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
            i++) {
          if (!(*i)->isDead()) {
            dbgs() << **i <<  ": " << getMinMaxOccupancy(*i, Maximize)
                   << " (" << getBytesReserved(*i) << ")\n";
          }
        }
      );
#endif // PATMOS_TRACE_CG_OCCUPANCY
    }

    /// getLiveAreaSize - Bound the address accessed by the instruction wrt.
    /// the stack cache, i.e., get the size of the area within the stack cache 
    /// that contains live data.
    unsigned int getLiveAreaSize(MachineInstr *MI)
    {
      unsigned int scale = 1;
      switch(MI->getOpcode())
      {
        case Patmos::SWS:
          scale = 2;
        case Patmos::SHS:
          scale <<= 1;
        case Patmos::SBS:
        {
          unsigned B = MI->getOperand(2).getReg();
          if (MI->getOperand(3).isImm() && B == Patmos::R0) {
            return scale * (MI->getOperand(3).getImm() + 1);
          }
          else return STC.getStackCacheSize();
        }
        case Patmos::LWS:
          scale = 2;
        case Patmos::LHS:
        case Patmos::LHUS:
          scale <<= 1;
        case Patmos::LBS:
        case Patmos::LBUS:
        {
          unsigned B = MI->getOperand(3).getReg();
          if (MI->getOperand(4).isImm() && B == Patmos::R0) {
            return scale * (MI->getOperand(4).getImm() + 1);
          }
          else return STC.getStackCacheSize();
        }
        default:
          return 0;
      }
    }

    /// propagateLiveArea - Propagate information on the live data within the 
    /// stack cache, e.g., accessed by loads and stores, upwards trough the CFG 
    /// to ensure instructions. This information can be used to downsize or 
    /// remove ensures.
    // TODO: check for STCr
    void propagateLiveArea(MBBs &WL, MBBUInt &INs, SIZEs &ENSs,
                           MachineBasicBlock *MBB)
    {
      // get the size of the live stack area from the CFG successors
      unsigned int liveAreaSize = INs[MBB];

      // propagate within the basic block
      for(MachineBasicBlock::reverse_instr_iterator i(MBB->instr_rbegin()),
          ie(MBB->instr_rend()); i != ie; i++) {

        // check for ensures
        if (i->getOpcode() == Patmos::SENSi) {
          assert(i->getOperand(2).isImm());

          // compute actual space to ensure here (in words)
          unsigned int ensure = STC.getAlignedStackFrameSize(liveAreaSize) / 4;

          // update the ensure to reserve only the space actually used.
          ENSs[&*i] = std::min(ensure, (unsigned int)i->getOperand(2).getImm());

          // If we encounter an ensure, we can reset the liveAreaSize to 0
          // since all following accesses will be served by this ensure. This
          // also applies when this ensure is eliminated.
          liveAreaSize = 0;
        }
        else {
          // compute the size of the live area before the current instruction
          liveAreaSize = std::max(liveAreaSize, getLiveAreaSize(&*i));
        }
      }

      // propagate to CFG predecessors
      for(MachineBasicBlock::pred_iterator i(MBB->pred_begin()),
          ie(MBB->pred_end()); i != ie; i++) {
        // check if the new live area size is larger than what was known 
        // previously for this predecessor
        if (INs[*i] < liveAreaSize) {
          // update the predecessor's live area size and put it on the work list
          INs[*i] = liveAreaSize;
          WL.insert(*i);
        }
      }
    }

    /// propagateLiveArea - Propagate information on the live data within the
    /// stack cache, e.g., accessed by loads and stores, upwards trough the CFG
    /// to ensure instructions. This information can be used to downsize or
    /// remove ensures.
    void propagateLiveArea(const MCallGraph &G)
    {
      const MCGNodes &nodes(G.getNodes());

      // visit all functions
      for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
          i++) {
        if (!(*i)->isUnknown() && !(*i)->isDead()) {
          MBBs WL;
          SIZEs ENSs;
          MBBUInt INs;
          MachineFunction *MF = (*i)->getMF();

          // initialize work list (yeah, reverse-reverse post order would be
          // optimal, but this works too).
          for(MachineFunction::iterator i(MF->begin()), ie(MF->end()); i != ie;
              i++) {
            WL.insert(i);
          }

          // process until the work list becomes empty
          while (!WL.empty()) {
            // get some basic block
            MachineBasicBlock *MBB = *WL.begin();
            WL.erase(WL.begin());

            // update the basic block's information, potentially putting any of
            // its predecessors on the work list.
            propagateLiveArea(WL, INs, ENSs, MBB);
          }

          // actually update the sizes of the ensure instructions.
          if (EnableEnsureOpt) {
            for(SIZEs::const_iterator i(ENSs.begin()), ie(ENSs.end()); i != ie;
                i++) {
              i->first->getOperand(2).setImm(i->second);
            }
          }

#ifdef PATMOS_TRACE_BB_LIVEAREA
          DEBUG(
            dbgs() << "*************************** "
                   << MF->getFunction()->getName() << "\n";
            for(MBBUInt::const_iterator i(INs.begin()), ie(INs.end()); i != ie;
                i++) {
              dbgs() << "  " << i->first->getName()
                    << "(" << i->first->getNumber() << ")"
                    << ": " << i->second << "\n";
            }
          );
#endif // PATMOS_TRACE_BB_LIVEAREA
        }
      }
    }

    /// analyzeEnsures - Does what it says. SENS instructions can be removed if
    /// the preceding calls plus the current frame on the stack cache fit into
    /// the stack cache.
    // TODO: take care of predication, i.e., predicated SENS/CALL instructions
    // might be mangled and they might not match one to one.
    // TODO: check for STCr
    void analyzeEnsures(MBBs &WL, MBBUInt &INs, SIZEs &ENSs, MCGNode *Node,
                        MachineBasicBlock *MBB)
    {
      // track maximum stack occupancy of children in the call graph -- 
      // initialize from predecessors in the CFG.
      unsigned int childOccupancy = INs[MBB];

      // propagate maximum stack occupancy through the basic block
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie; i++) {
        if (i->isCall()) {
          // find call site
          MCGSite *site = Node->findSite(i);
          assert(site);

          childOccupancy = std::max(childOccupancy,
                                    getMaxOccupancy(site->getCallee()));
        }
        else if (i->getOpcode() == Patmos::SENSi) {
          unsigned int ensure = i->getOperand(2).getImm() * 4;

          // does the content of the ensure and all the children in the call
          // graph fit into the stack cache?
          bool remove = (ensure + childOccupancy) <= STC.getStackCacheSize();

          if (ensure == 0) assert(remove);

          // if all fits, the SENS can be removed.
          unsigned int filling = remove ? 0u : ensure + childOccupancy -
                                               STC.getStackCacheSize();
          ENSs[i] = filling;

          assert(filling != 0 || remove);

          if (!remove && !TII.isPredicated(i)) {
            childOccupancy = std::min(childOccupancy,
                                      STC.getStackCacheSize() - ensure);
          }
        }
      }

      // propagate to CFG successors
      for(MachineBasicBlock::succ_iterator i(MBB->succ_begin()),
          ie(MBB->succ_end()); i != ie; i++) {
        // check if the new stack occupancy is larger than what was known 
        // previously for this successor
        if (INs[*i] < childOccupancy) {
          // update the successor’s stack occupancy and put it on the work list
          INs[*i] = childOccupancy;
          WL.insert(*i);
        }
      }
    }

    /// analyzeEnsures - Does what it says.
    /// SENS instructions can be removed if
    /// the preceding calls plus the current frame on the stack cache fit into
    /// the stack cache.
    void analyzeEnsures(const MCallGraph &G)
    {
      const MCGNodes &nodes(G.getNodes());

      // we store analysis results in a pseudo pass
      PatmosStackCacheAnalysisInfo *info =
       &getAnalysis<PatmosStackCacheAnalysisInfo>();
      info->setValid();

      // visit all functions
      for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
          i++) {
        if (!(*i)->isUnknown() && !(*i)->isDead()) {
          MBBs WL;
          SIZEs ENSs;
          MBBUInt INs;
          MachineFunction *MF = (*i)->getMF();

          // initialize work list (yeah, reverse post order would be optimal,
          // but this works too).
          for(MachineFunction::iterator j(MF->begin()), je(MF->end()); j != je;
              j++) {
            WL.insert(j);
          }

          // process until the work list becomes empty
          while (!WL.empty()) {
            // get some basic block
            MachineBasicBlock *MBB = *WL.begin();
            WL.erase(WL.begin());

            // update the basic block's information, potentially putting any of
            // its successors on the work list.
            analyzeEnsures(WL, INs, ENSs, *i, MBB);
          }

          // actually remove ensure instructions (if requested)
          for(SIZEs::const_iterator i(ENSs.begin()), ie(ENSs.end()); i != ie;
              i++) {
            unsigned int ensure = i->first->getOperand(2).getImm() * 4;
#ifdef PATMOS_TRACE_DETAILED_RESULTS
            MachineBasicBlock *MBB = i->first->getParent();
            MachineBasicBlock::instr_iterator MI(i->first);
            dbgs() << "ENS: " << MF->getFunction()->getName() << ":BB"
                   << MBB->getNumber() << ":"
                   << std::distance(MBB->instr_begin(), MI) << ": k=" 
                   << ensure << ", f=" << i->second << "\n";
#endif // PATMOS_TRACE_DETAILED_RESULTS

            if (i->second == 0) {
              if (EnableEnsureOpt) {
                i->first->getParent()->erase(i->first);
                RemovedSENS++;
              } else {
                NonFillingSENS++;
              }
            }
            else {
              if (i->second == ensure)
                FullyFillingSENS++;
              FillingSENS++;
            }

            // update the analysis info pseudo pass (convert bytes to blocks)
            assert(i->second % STC.getStackCacheBlockSize() == 0);
            info->Ensures[i->first] = i->second; // export in bytes
          }

#ifdef PATMOS_TRACE_SENS_REMOVAL
          DEBUG(
            dbgs() << "########################### "
                   << MF->getFunction()->getName() << "\n";
            for(MBBUInt::const_iterator i(INs.begin()), ie(INs.end()); i != ie;
                i++) {
              dbgs() << "  " << i->first->getName()
                    << "(" << i->first->getNumber() << ")"
                    << ": " << i->second << "\n";
            }
          );
#endif // PATMOS_TRACE_SENS_REMOVAL
        }
      }
    }

    /// ilp_name - make a name for a node suitable for the LP file.
    static std::string ilp_name(ilp_prefix Prefix, const MCGNode *N)
    {
      std::string tmps;
      raw_string_ostream tmp(tmps);
      tmp << Prefix;
      if (N->isUnknown()) {
        tmp << "U" << (void*)N;
      }
      else
        tmp << "X" << N->getMF()->getFunction()->getName();
      return tmp.str();
    }

    /// ilp_name - make a name for a call site suitable for the LP file.
    static std::string ilp_name(ilp_prefix Prefix, const MCGSite *S)
    {
      std::string tmps;
      raw_string_ostream tmp(tmps);
      tmp << Prefix << "S" << (void*)S;
      return tmp.str();
    }

    /// solve_ilp - solve the ILP problem.
    static unsigned int solve_ilp(const char *LPname, bool Maximize)
    {
      unsigned int result = Maximize ? std::numeric_limits<unsigned int>::max():
                                       std::numeric_limits<unsigned int>::min();

      std::vector<const char*> args;
      args.push_back(Solve_ilp.c_str());
      args.push_back(LPname);
      args.push_back(0);

      std::string ErrMsg;
      if (sys::ExecuteAndWait(sys::FindProgramByName(Solve_ilp),
                                       &args[0],0,0,0,0,&ErrMsg)) {
        report_fatal_error("calling ILP solver (" + Solve_ilp + "): " + ErrMsg);
      }
      else {
        // read solution
        // construct name of solution
        std::string SOLname(LPname);
        SOLname += ".sol";

        //SmallString<1024> SOLname(LPname);
        //sys::path::replace_extension(SOLname, ".sol");

        if (!sys::fs::exists(SOLname))
          report_fatal_error("Failed to read ILP solution");

        std::ifstream IS(SOLname.c_str());
        assert(IS.good());

        // read the result value
        double tmp;
        IS >> tmp;
        result = tmp;

        // don't go ahead when solving has failed
        if (tmp == -1.)
          assert(0 && "unbounded/infeasible ILP");

        sys::fs::remove(SOLname);
      }

      ILPs++;

      return result;
    }

    /// computeMinMaxOccupancyILP - Construct an ILP modeling the stack 
    /// occupancy of an SCC within the call graph, write it to an LP file, and 
    /// solve it.
    unsigned int computeMinMaxOccupancyILP(const MCGNodes &SCC,
                                           const MCGNode *N, bool Maximize)
    {
      assert(std::find(SCC.begin(), SCC.end(), N) != SCC.end());

      // get user-supplied bounds to solve the ILP.
      const SCCInfo &BInfo(BI.getInfo(SCC));

      // open LP file.
      SmallString<1024> LPname;
      error_code err = sys::fs::createUniqueDirectory("stack", LPname);
      if (err) {
        errs() << "Error creating temp .lp file: " << err.message() << "\n";
        return std::numeric_limits<unsigned int>::max();
      }

      sys::path::append(LPname, "scc.lp");

      std::string ErrMsg;
      raw_fd_ostream OS(LPname.c_str(), ErrMsg);
      if (!ErrMsg.empty()) {
        errs() << "Error: Failed to open file '" << LPname.str()
               << "' for writing!\n";
        return std::numeric_limits<unsigned int>::max();
      }

      // find entry and exit call sites
      typedef std::set<MCGSite*> MCGSiteSet;
      MCGSiteSet entries, exits;
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        // look for call sites exiting the SCC
        for(MCGSites::const_iterator cs((*n)->getSites().begin()),
          cse((*n)->getSites().end()); cs != cse; cs++) {
          MCGNode *d = (*cs)->getCallee();

          if (std::find(SCC.begin(), SCC.end(), d) == SCC.end())
            exits.insert(*cs);
        }

        // look for call sites entering the SCC
        for(MCGSites::const_iterator cs((*n)->getCallingSites().begin()),
          cse((*n)->getCallingSites().end()); cs != cse; cs++) {
          MCGNode *s = (*cs)->getCaller();

          if (std::find(SCC.begin(), SCC.end(), s) == SCC.end())
            entries.insert(*cs);
        }
      }

      //************************************************************************
      // objective function

      OS << (Maximize ? "Maximize" : "Minimize");

      // nodes in the SCC
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        OS << "\n + " << getBytesReserved(*n) << " " << ilp_name(W, *n);
      }

      // exit sites
      for(MCGSiteSet::iterator n(exits.begin()), ne(exits.end()); n != ne;
          n++) {
        OS << "\n + " << getMinMaxOccupancy((*n)->getCallee(), Maximize)
           << "\t" << ilp_name(W, *n);

        if (!(*n)->getCallee()->isUnknown())
          OS << "\t\\ " << (*n)->getCallee()->getMF()->getFunction()->getName();
      }

      // entries do not matter here

      // add user-defined parts of objective function
      OS << BInfo.ObjectiveFunction;

      //************************************************************************
      // constraints
      OS << "\nSubject To\n";
      unsigned int cnt = 0;

      // force path over the call graph node N
      OS << "path:\t" << ilp_name(W, N) << " >= 1\n";

      // constraints on in-flow of nodes in SCC
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        for(ilp_prefix p = Z; p <= W; p = (ilp_prefix)(p + 1)) {
          OS << "if" << p << cnt << ":\t";

          const MCGSites &CS((*n)->getCallingSites());
          for(MCGSites::const_iterator cs(CS.begin()), cse(CS.end()); cs != cse;
              cs++) {
            OS << " + " << ilp_name(p, *cs);

            // account for transition edges from Z to W version of node N
            if (*n == N && p == W)
              OS << " + " << ilp_name(T, *cs);
          }

          OS << " - " << ilp_name(p, *n) << " = 0\n";
        }
        cnt++;
      }

      // constraints on out-flow of nodes in SCC
      cnt = 0;
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        for(ilp_prefix p = Z; p <= W; p = (ilp_prefix)(p + 1)) {
          OS << "of" << p << cnt << ":\t";

          const MCGSites &CS((*n)->getSites());
          for(MCGSites::const_iterator cs(CS.begin()), cse(CS.end()); cs != cse;
              cs++) {
            OS << " + " << ilp_name(p, *cs);

            // account for transition edges from Z to W version of node N
            if ((*cs)->getCallee() == N && p == Z)
              OS << " + " << ilp_name(T, *cs);
          }

          // handle function containing at least one call-free path
          if (IsCallFree[*n] && p == W) {
            OS << " + " << ilp_name(OF, *n);
          }

          OS << " - " << ilp_name(p, *n) << " = 0\n";
        }
        cnt++;
      }

      // constraints on out-flow of nodes in SCC
      cnt = 0;
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        OS << "veq" << cnt << ":\t + "
           << ilp_name(Z, *n) << " + " << ilp_name(W, *n)
           << " - " << ilp_name(X, *n) << " = 0\n";
        cnt++;
      }

      // constraint on in-flow over transition edges to the W version of node N
      // (both, from within the SCC or from the outside)
      {
        const MCGSites &CS(N->getCallingSites());
        OS << "tran:\t";
        for(MCGSites::const_iterator cs(CS.begin()), cse(CS.end()); cs != cse;
            cs++) {
          OS << " + " << ilp_name(T, *cs);
        }
        OS << " = 1\n";
      }

      // constraint on out-flow of entry nodes
      OS << "en:\t";
      for(MCGSiteSet::const_iterator cs(entries.begin()), cse(entries.end());
          cs != cse; cs++) {
        OS << " + " << ilp_name(Z, *cs);;

        // account for transition edges from an entry to the W version of node N
        if ((*cs)->getCallee() == N)
          OS << " + " << ilp_name(T, *cs);
      }
      OS << " = 1\n";

      // constraint on in-flow of exit nodes
      OS << "ex:\t";
      // only used for assertion
      bool LLVM_ATTRIBUTE_UNUSED ex_printed = false;
      for(MCGSiteSet::const_iterator cs(exits.begin()), cse(exits.end());
          cs != cse; cs++) {
        OS << " + " << ilp_name(W, *cs);
        ex_printed = true;
      }

      // handle exits through functions containing at least one call-free path
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        if (IsCallFree[*n]) {
          OS << " + " << ilp_name(OF, *n);
          ex_printed = true;
        }
      }

      assert(ex_printed);
      OS << " = 1\n";

      // add user-defined constraints
      OS << BInfo.Constraints;

      //************************************************************************
      // variable domains
      OS << "Generals\n";

      // nodes and call sites in SCC
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        OS << ilp_name(Z, *n) << "\n";
        OS << ilp_name(W, *n) << "\n";

        // handle functions with at least one call-free path
        if (IsCallFree[*n]) {
          OS << ilp_name(OF, *n) << "\n";
        }

        const MCGSites &CS((*n)->getCallingSites());
        for(MCGSites::const_iterator cs(CS.begin()), cse(CS.end()); cs != cse;
            cs++) {
          // skip entry sites
          if(entries.find(*cs) == entries.end()) {
            OS << ilp_name(Z, *cs) << "\n";
            OS << ilp_name(W, *cs) << "\n";
          }
        }
      }

      // transition edges to W version of node N
      {
        const MCGSites &CS(N->getCallingSites());
        for(MCGSites::const_iterator cs(CS.begin()), cse(CS.end()); cs != cse;
            cs++) {
          OS << ilp_name(T, *cs) << "\n";
        }
      }

      // entries
      for(MCGSiteSet::const_iterator cs(entries.begin()), cse(entries.end());
          cs != cse; cs++) {
        OS << ilp_name(Z, *cs) << "\n";
      }

      // exits
      for(MCGSiteSet::const_iterator cs(exits.begin()), cse(exits.end()); 
          cs != cse; cs++) {
        OS << ilp_name(W, *cs) << "\n";
      }

      // add user-defined variable definitions
      OS << BInfo.Variables;

      OS << "End\n";

      OS.close();

      // solve the ILP
      unsigned int result = solve_ilp(LPname.c_str(), Maximize);

#ifdef PATMOS_TRACE_CG_OCCUPANCY_ILP
      dbgs() << "ILP: " << *N << ": " << result << "\n";
#endif // PATMOS_TRACE_CG_OCCUPANCY_ILP

      // remove LP file
      // TODO do we need to remove the temp dir??
      sys::fs::remove(LPname.str());

      return result;
    }

    /// checkCallFreePaths - Check whether functions have call free paths.
    /// see below.
    void checkCallFreePaths(MBBs &WL, MBBBool &OUTs, MachineBasicBlock *MBB)
    {
      // see if a successor contains a call-free path to a sink
      bool is_call_free = OUTs[MBB];

      for(MachineBasicBlock::succ_iterator i(MBB->succ_begin()),
          ie(MBB->succ_end()); i != ie && is_call_free; i++) {
        is_call_free |= OUTs[*i];
      }

      // see if the block contains a call
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie && is_call_free; i++) {
        if (i->isCall() && !TII.isPredicated(i))
          is_call_free = false;
      }

      // simply put all predecessors on the work list -- this may happen only
      // once.
      if (is_call_free != OUTs[MBB]) {
        WL.insert(MBB->pred_begin(), MBB->pred_end());
      }

      OUTs[MBB] = is_call_free;
    }

    /// checkCallFreePaths - Check whether functions have call free paths. In 
    /// the data flow problem, we propagate whether a basic block and some path 
    /// from it to a sink do not contain a call instruction.
    void checkCallFreePaths(const MCallGraph &G)
    {
      const MCGNodes &nodes(G.getNodes());

      // visit all functions
      for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
          i++) {

        bool is_call_free = false;

        // ignore unknown functions here
        if (!(*i)->isUnknown() && !(*i)->isDead()) {
          MBBs WL;
          MBBBool OUTs;
          MachineFunction *MF = (*i)->getMF();

          // initialize work list -- initially we assume
          for(MachineFunction::iterator i(MF->begin()), ie(MF->end()); i != ie;
              i++) {
            WL.insert(i);

            // initially assume the basic block does not contain a call and a
            // path to a sink exists without any calls.
            OUTs[i] = true;
          }

          // process until the work list becomes empty
          while (!WL.empty()) {
            // get some basic block
            MachineBasicBlock *MBB = *WL.begin();
            WL.erase(WL.begin());

            // update the basic block's information, potentially putting any of
            // its predecessors on the work list.
            checkCallFreePaths(WL, OUTs, MBB);
          }

          // see if the entry node contains a call-free path
          is_call_free = OUTs[MF->begin()];
        }

        IsCallFree[*i] = is_call_free;
      }
    }

    /// propagateWorstCaseOccupancyAtSite - Propagate, locally within a 
    /// function, the worst-case stack occupancy at call sites.
    ///
    /// We assume that the stack cache is full at function entry and then 
    /// propagate the worst-case occupancy at call sites through the function. 
    /// We use the minimum displacement caused by the functions called along 
    /// a path to get the worst-case occupancy.
    void propagateWorstCaseOccupancyAtSite(MBBs &WL, MBBUInt &INs,
                                           MCGNode *Node,
                                           MachineBasicBlock *MBB)
    {
      // track the worst-case stack occupancy across call sites accounting for
      // children in the call graph and fills due to ensures, assuming a full 
      // stack cache at function entry -- 
      // initialize from predecessors in the CFG.
      unsigned int worstOccupancy = INs[MBB];

      // propagate the worst-case stack occupancy through the basic block
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie; i++) {
        if (i->isCall()) {
          // find call site
          MCGSite *site = Node->findSite(i);
          assert(site);

          // store the worst-case occupancy before the call site, i.e., for the
          // functions potentially entered through calls from this site
          WorstCaseSiteOccupancy[site] = worstOccupancy;

          if (!TII.isPredicated(i)) {
            // get the worst-case occupancy after the call
            unsigned int worstCallOccupancy =
                   STC.getStackCacheSize() - getMinOccupancy(site->getCallee());

            // update the worst-case occupancy
            worstOccupancy = std::min(worstOccupancy, worstCallOccupancy);
          }
        }
        else if (i->getOpcode() == Patmos::SENSi && !TII.isPredicated(i)) {
          unsigned int ensure = i->getOperand(2).getImm() * 4;
          worstOccupancy = std::max(ensure, worstOccupancy);
        }
      }

      // propagate to CFG successors
      for(MachineBasicBlock::succ_iterator i(MBB->succ_begin()),
          ie(MBB->succ_end()); i != ie; i++) {
        // propagate the worst-case occupancy to the successors and put them on 
        // the work list
        if (INs[*i] < worstOccupancy) {
          INs[*i] = worstOccupancy;
          WL.insert(*i);
        }
      }
    }

    /// propagateWorstCaseOccupancyAtSite - Propagate, locally within a
    /// function, the worst-case stack occupancy at call sites.
    ///
    /// We assume that the stack cache is full at function entry and then
    /// propagate the worst-case occupancy at call sites through the function.
    /// We use the minimum displacement caused by the functions called along
    /// a path to get the worst-case occupancy.
    void propagateWorstCaseOccupancyAtSite(const MCallGraph &G)
    {
      const MCGNodes &nodes(G.getNodes());

      // visit all functions
      for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
          i++) {
        if ((*i)->isUnknown()) {
          // ensure that incoming values later, when the call graph is processed 
          // globally, are simply propagated onward through UNKNOWN nodes
          for(MCGSites::const_iterator j((*i)->getSites().begin()),
              je((*i)->getSites().end()); j != je; j++) {
            WorstCaseSiteOccupancy[*j] = STC.getStackCacheSize();
          }
        }
        else if (!(*i)->isDead()) {
          MBBs WL;
          MBBUInt INs;
          MachineFunction *MF = (*i)->getMF();

          // initialize work list.
          INs[MF->begin()] = STC.getStackCacheSize();
          WL.insert(MF->begin());

          // process until the work list becomes empty
          while (!WL.empty()) {
            // get some basic block
            MachineBasicBlock *MBB = *WL.begin();
            WL.erase(WL.begin());

            // update the basic block's information, potentially putting any of
            // its successors on the work list.
            propagateWorstCaseOccupancyAtSite(WL, INs, *i, MBB);
          }

#ifdef PATMOS_TRACE_WORST_SITE_OCCUPANCY
          DEBUG(
            dbgs() << "\\\\\\\\\\\\\\\\\\\\\\\\\\\\ "
                   << MF->getFunction()->getName()
                   << " (" << getBytesReserved(*i) << ")\n";
            for(MCGSiteUInt::const_iterator j(WorstCaseSiteOccupancy.begin()),
                je(WorstCaseSiteOccupancy.end()); j != je; j++) {
              dbgs() << "  " << *j->first << ": " << j->second
                     << " (" << getMinOccupancy(j->first->getCallee()) << ")\n";
            }
          );
#endif // PATMOS_TRACE_WORST_SITE_OCCUPANCY
        }
      }
    }

    /// pruneNodes - Find nodes that lead to impossible stack cache states.
    /// Nodes are marked valid during a depth first traversal whenever the node
    /// is reachable from the root node such that it remains in the maximum 
    /// stack occupancy computed before.
    /// \see computeMinMaxOccupancy
    void pruneNodes(SCANode *N, unsigned int parentOccupancy)
    {

      // This node is already valid, so skip it.
      if (!N->isValid()) {
        unsigned int nodeReserved = getBytesReserved(N->getMCGNode());
        bool isValid = (parentOccupancy >= nodeReserved);

        if (isValid) {
          // mark the node as valid.
          N->setValid();
          N->setRemainingOccupancy(parentOccupancy);

          // compute the stack occupancy remaining for the children.
          unsigned int remainingOccupancy =
                            std::min(parentOccupancy - nodeReserved,
                                     getMinMaxOccupancy(N->getMCGNode(), true));

          // visit the children in the graph
          for(SCAEdgeSet::iterator i(N->getChildren().begin()),
              ie(N->getChildren().end()); i != ie; i++) {
            pruneNodes(i->getCallee(), remainingOccupancy);
          }
        }
      }
    }

    /// pruneSCAGraph - Hide nodes in the graph that are not relevant for the
    /// analysis, i.e., those that neither spill nor lead to a context that
    /// spills, or that do not lead to a valid stack cache state.
    void pruneSCAGraph()
    {
      const MCGSCANodeMap &nodes(SCAGraph.getNodes());

      // keep statistics of the initial SCA graph size.
      TotalSCAGraphSize += nodes.size();

      // eliminate UNKNOWN nodes from the graph
      SCANodeSet cleanup;
      for(MCGSCANodeMap::const_iterator i(nodes.begin()), ie(nodes.end());
          i != ie; i++) {
        MCGNode *mcgNode = i->second->getMCGNode();
        if (mcgNode->isUnknown()) {
          cleanup.insert(i->second);
          // remove the node and redirect its parent/child relations
          for(SCAEdgeSet::iterator j(i->second->getChildren().begin()),
              je(i->second->getChildren().end()); j != je; j++) {

            SCANode *calleeNode = j->getCallee();

            for(SCAEdgeSet::const_iterator k(i->second->getParents().begin()),
                ke(i->second->getParents().end()); k != ke; k++) {

              SCANode *callerNode = k->getCaller();

              SCAEdge newEdge(callerNode, calleeNode, k->getSite());
              calleeNode->getParents().insert(newEdge);
              callerNode->getChildren().insert(newEdge);
            }
          }
        }
      }

      // remove the UNKNOWN nodes from the graph and free them
      std::for_each(cleanup.begin(), cleanup.end(),
                 std::bind1st(std::mem_fun(&SpillCostAnalysisGraph::deleteNode),
                             &SCAGraph));

      // eliminate nodes whose shortest path to a leaf is longer than the
      // previously analyzed maximum occupancy
      pruneNodes(SCAGraph.getRoot(),
                 getMinMaxOccupancy(SCAGraph.getRoot()->getMCGNode(), true));

      // mark only those nodes visible that have non-zero spill costs or have a
      // descendent with non-zero spill costs.
      for(MCGSCANodeMap::const_iterator i(nodes.begin()), ie(nodes.end());
          i != ie; i++) {
        if (i->second->getSpillCost()) {
          i->second->setVisible();
        }
      }
    }

    /// propagateMaxOccupancy - propagate the maximum stack occupancy on the
    /// call graph and analyze the worst-case spilling of reserves.
    ///
    /// The main idea is to associate each call graph node with a set of calling 
    /// contexts, which are in turn associated with a maximum stack occupancy.
    /// The occupancy is then propagated onwards to children in the call graph
    /// through calls sites, considering the worst-case occupancy computed 
    /// before.
    ///
    /// We know that there are only two options:
    /// (1) either we propagate the stack occupancy of the current context,
    ///     adding the stack space reserved by the current function or
    /// (2) we propagate the worst-case occupancy of the site onward.
    ///
    /// We only need to propagate the minimum of the two.
    ///
    /// \see propagateWorstCaseOccupancyAtSite
    void propagateMaxOccupancy(SCANode *Node, SCANodeSet &WL)
    {
      // get the call graph node and occupancy
      MCGNode *mcgNode = Node->getMCGNode();

      // get the stack occupancy of the current calling context and add the
      // space allocated by the current function to it.
      unsigned int nodeOccupancy = std::min(STC.getStackCacheSize(),
                              Node->getOccupancy() + getBytesReserved(mcgNode));

      // propagate to call sites
      for(MCGSites::const_iterator j(mcgNode->getSites().begin()),
          je(mcgNode->getSites().end()); j != je; j++) {
        // get the site's stack worst-case occupancy
        MCGSite *site = *j;
        MCGNode *callee = site->getCallee();
        unsigned int worstSiteOccupancy = STC.getStackCacheSize();
        if (WorstCaseSiteOccupancy.count(site))
          WorstCaseSiteOccupancy[site];

        // compute the occupancy and the call site
        unsigned int siteOccupancy = std::min(nodeOccupancy,
                                               worstSiteOccupancy);

        // compute the occupancy after the child's reserve
        unsigned int childOccupancy = getBytesReserved(callee) +
                                            siteOccupancy;

        // compute the spill caused by the child's reserve
        unsigned int spillCost =
            childOccupancy <= STC.getStackCacheSize() ? 0 :
                                  childOccupancy - STC.getStackCacheSize();

        // the occupancy before child's reserve (and spill cost) is propagated
        SCANode *calleeSCANode;
        bool isNewNode = SCAGraph.makeNode(callee, siteOccupancy, spillCost,
                                           getMaxOccupancy(callee),
                                           IsCallFree[callee], calleeSCANode);

        // make a link to the parent context
        calleeSCANode->addParent(Node, site);

        // if the node did not exist before, append it to the work list
        if (isNewNode) {
          WL.insert(calleeSCANode);
        }
      }
    }

    /// propagateMaxOccupancy - propagate the maximum stack occupancy on the
    /// call graph and analyze the worst-case spilling of reserves.
    ///
    /// The main idea is to associate each call graph node with a set of calling
    /// contexts, which are in turn associated with a maximum stack occupancy.
    /// The occupancy is then propagated onwards to children in the call graph
    /// through calls sites, considering the worst-case occupancy computed
    /// before.
    ///
    /// We know that there are only two options:
    /// (1) either we propagate the stack occupancy of the current context,
    ///     adding the stack space reserved by the current function or
    /// (2) we propagate the worst-case occupancy of the site onward.
    ///
    /// We only need to propagate the minimum of the two.
    ///
    /// \see propagateWorstCaseOccupancyAtSite
    void propagateMaxOccupancy(const MCallGraph &G, MCGNode *main)
    {
      // initialize the work list and calling context information
      SCANodeSet WL;
      WL.insert(SCAGraph.makeRoot(main, getMaxOccupancy(main),
                                  IsCallFree[main]));
      while (!WL.empty()) {
        // pop current call graph node
        SCANode *Node =  *WL.begin();
        WL.erase(Node);

        // propagate to callees through call sites
        if (!Node->getMCGNode()->isDead()) {
          propagateMaxOccupancy(Node, WL);
        }
      }

      // prune graph, i.e., hide nodes not relevant for the analysis
      pruneSCAGraph();

      const MCGSCANodeMap &nodes(SCAGraph.getNodes());
#ifdef PATMOS_TRACE_DETAILED_RESULTS
      for(MCGSCANodeMap::const_iterator i(nodes.begin()), ie(nodes.end());
          i != ie; i++) {
        if (i->second->isVisible()) {
          MCGNode *N = i->first.first;
          dbgs() << "CTXT: " << N->getMF()->getFunction()->getName()
                << ": k=" << getBytesReserved(N)
                << ", s=" << i->second->getSpillCost()
                << ", o=" << i->first.second << "\n";
        }
      }
#endif // PATMOS_TRACE_DETAILED_RESULTS

      // keep statistics of the pruned SCA graph size.
      MCGNodeUInt Spilling;
      for(MCGSCANodeMap::const_iterator i(nodes.begin()), ie(nodes.end());
          i != ie; i++) {
        if (i->second->isVisible()) {
          PrunedSCAGraphSize++;
          Spilling[i->first.first] = std::max(Spilling[i->first.first],
                                              i->second->getSpillCost());
        }
      }

      PatmosStackCacheAnalysisInfo *info =
       &getAnalysis<PatmosStackCacheAnalysisInfo>();

      // statistics of SRES instructions (functions for now)
      for(MCGNodes::const_iterator i(G.getNodes().begin()),
          ie(G.getNodes().end()); i != ie; i++) {
        unsigned int reserved = getBytesReserved(*i);
        if (!(*i)->isDead()) {
          Functions++;
          if (reserved != 0) {
            unsigned int tmp = Spilling[*i];
            if (tmp == 0)
              NonSpillingSRES++;
            else {
              SpillingSRES++;
              if (tmp == reserved)
                FullySpillingSRES++;
            }

            // update the analysis info pseudo pass
            // (needs to find the SRES instruction first)
            MachineBasicBlock &MBB = (*i)->getMF()->front();
            MachineBasicBlock::const_instr_iterator I, E;
            for (I = MBB.instr_begin(), E = MBB.instr_end(); I != E; ++I)
              if (I->getOpcode() == Patmos::SRESi)
                break;
            assert(I != MBB.instr_end());
            assert(tmp % STC.getStackCacheBlockSize() == 0);

            // convert bytes back to blocks
            info->Reserves[I] = tmp; // export in bytes
          }
        }
      }

      if (EnableViewSCAGraph)
        ViewGraph(SCAGraph, "sca");
    }

    /// runOnModule - determine the state of the stack cache for each call site.
    virtual bool runOnMachineModule(const Module &M)
    {
      PatmosCallGraphBuilder &PCGB(getAnalysis<PatmosCallGraphBuilder>());
      const MCallGraph &G(*PCGB.getCallGraph());
      MCGNode *main = PCGB.getMCGNode(M, "main");

      // find out whether a call free path exists in each function
      checkCallFreePaths(G);

      // find the amount of live stack content after each ensure instruction
      propagateLiveArea(G);

      // compute call-graph-level information on maximal stack cache occupancy
      computeMinMaxOccupancy(G, true);

      // compute ensure behavior and optionally remove useless SENS instructions
      analyzeEnsures(G);

      // compute call-graph-level information on minimal stack cache occupancy
      computeMinMaxOccupancy(G, false);

      // propagate the worst-case stack occupancy at call sites locally within 
      // functions, assuming a full stack cache at function entry.
      propagateWorstCaseOccupancyAtSite(G);

      // propagate the maximum stack occupancy on the call graph and analyze 
      // the worst-case spilling at reserves.
      propagateMaxOccupancy(G, main);

      if (!SCAPMLExport.empty())
        exportPML(G, SCAGraph);

      return false;
    }

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Stack Cache Analysis";
    }

    void mapIndices(MachineFunction &MF) {
      for (MachineFunction::iterator BB = MF.begin(), E = MF.end(); BB != E; ++BB)
      {
        unsigned Index = 0;
        for (MachineBasicBlock::instr_iterator Ins = BB->instr_begin(),
             E = BB->instr_end(); Ins != E; ++Ins)
        {
          if (Ins->isPseudo()) continue;
          MiMap[Ins] = std::make_pair(BB, Index++);
        }
      }
    }

    void exportPML(const MCallGraph &G, const SpillCostAnalysisGraph &scag) {
      yaml::SCADoc YDoc;

      std::set<MachineFunction*> Seen;
      for (MCGSCANodeMap::const_iterator I = scag.getNodes().begin(),
          E = scag.getNodes().end(); I != E; ++I) {
        SCANode *n = I->second;
        n->initYAML(STC);
        YDoc.SCAG.N.push_back(n);

        MachineFunction *mf = n->getMCGNode()->getMF();
        if (!Seen.count(mf)) {
          mapIndices(*mf);
          Seen.insert(mf);
        }

        SCAEdgeSet &e = n->getChildren();
        for (SCAEdgeSet::iterator I = e.begin(), E = e.end(); I != E; ++I) {
          MInstrIndex::iterator it = MiMap.find(I->getSite()->getMI());
          assert(it != MiMap.end());
          YDoc.SCAG.addEdge(I->getCaller(), I->getCallee(),
              it->second.first, it->second.second);
        }

      }

      yaml::Output *Output;
      assert(!SCAPMLExport.empty());
      StringRef OutFileName(SCAPMLExport);
      tool_output_file *OutFile;
      std::string ErrorInfo;
      OutFile = new tool_output_file(OutFileName.str().c_str(), ErrorInfo);
      if (!ErrorInfo.empty()) {
        delete OutFile;
        errs() << "[mc2yml] Opening Export File failed: " << OutFileName << "\n";
        errs() << "[mc2yml] Reason: " << ErrorInfo;
        report_fatal_error("Exporting stack analysis results to PML failed!");
      }
      else {
        Output = new yaml::Output(OutFile->os());
      }
      *Output << YDoc;
      if (OutFile) {
        OutFile->keep();
        delete Output;
        delete OutFile;
      }
    }
  };

  char PatmosStackCacheAnalysis::ID = 0;
}

/// createPatmosStackCacheAnalysis - Returns a new PatmosStackCacheAnalysis.
ModulePass *llvm::createPatmosStackCacheAnalysis(const PatmosTargetMachine &tm){
  return new PatmosStackCacheAnalysis(tm);
}

namespace llvm {
  llvm::raw_ostream &operator <<(llvm::raw_ostream &O, ilp_prefix Prefix)
  {
    switch(Prefix)
    {
      case W:
        O << "W"; break;
      case Z:
        O << "Z"; break;
      case T:
        O << "T"; break;
      case OF:
        O << "OF"; break;
      case X:
        // do not print anything
        break;
    }

    return O;
  }

  bool operator <(const SCAEdge &a, const SCAEdge &b)
  {
    if (a.getCaller() != b.getCaller())
      return (a.getCaller() < b.getCaller());
    else if (a.getCallee() != b.getCallee())
      return (a.getCallee() < b.getCallee());
    else
      return (a.getSite() < b.getSite());
  }

  template <> struct GraphTraits<SpillCostAnalysisGraph> {
    typedef SCANode NodeType;
    class ChildIteratorType
    {
      SCAEdgeSet::const_iterator I;

    public:
      typedef SCAEdgeSet::const_iterator::iterator_category iterator_category;
      typedef SCAEdgeSet::const_iterator::difference_type difference_type;
      typedef SCAEdgeSet::const_iterator::pointer pointer;
      typedef SCAEdgeSet::const_iterator::reference reference;
      typedef NodeType value_type;

      ChildIteratorType(SCAEdgeSet::const_iterator i) : I(i)
      {
      }

      bool operator!=(ChildIteratorType a) 
      {
        return I != a.I;
      }

      ChildIteratorType operator++() 
      {
        ChildIteratorType tmp(I);
        I++;
        return tmp;
      }

      NodeType *operator*() 
      {
        return I->getCallee();
      }

      MachineInstr *getMI()
      {
        return I->getSite()->getMI();
      }
    };

    static inline ChildIteratorType child_begin(NodeType *N)
    {
      return N->getChildren().begin();
    }

    static inline ChildIteratorType child_end(NodeType *N)
    {
      return N->getChildren().end();
    }

    static NodeType *getEntryNode(const SpillCostAnalysisGraph &G)
    {
      return G.getRoot();
    }

    class nodes_iterator
    {
      MCGSCANodeMap::const_iterator I;

    public:
      typedef MCGSCANodeMap::const_iterator::iterator_category iterator_category;
      typedef MCGSCANodeMap::const_iterator::difference_type difference_type;
      typedef MCGSCANodeMap::const_iterator::pointer pointer;
      typedef MCGSCANodeMap::const_iterator::reference reference;

      nodes_iterator(MCGSCANodeMap::const_iterator i) : I(i) 
      {
      }

      bool operator!=(nodes_iterator a) 
      {
        return I != a.I;
      }

      nodes_iterator operator++() 
      {
        nodes_iterator tmp(I);
        I++;
        return tmp;
      }

      NodeType *operator*() 
      {
        return I->second;
      }
    };

    static nodes_iterator nodes_begin(const SpillCostAnalysisGraph &G) 
    {
      return G.getNodes().begin();
    }
    static nodes_iterator nodes_end (const SpillCostAnalysisGraph &G)
    {
      return G.getNodes().end();
    }
    static unsigned size (const SpillCostAnalysisGraph &G)
    {
      return G.getNodes().size();
    }
  };

  template<>
  struct DOTGraphTraits<SpillCostAnalysisGraph> : public DefaultDOTGraphTraits {
    DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

    static std::string getGraphName(const SpillCostAnalysisGraph &G) 
    {
      return "scagraph";
    }

    static bool isNodeHidden(const SCANode *N, const SpillCostAnalysisGraph &G)
    {
      return !N->isVisible();
    }

    static std::string getEdgeAttributes(const void *,
                       GraphTraits<SpillCostAnalysisGraph>::ChildIteratorType e,
                       const SpillCostAnalysisGraph &G)
    {
      std::string tmp;
      raw_string_ostream s(tmp);

      MachineInstr *MI = e.getMI();

      s << "label=\"";

      if (MI) {
        MachineBasicBlock *MBB = MI->getParent();
        s << "BB#" << MBB->getNumber() << ":"
          << std::distance(MBB->instr_begin(),
                           MachineBasicBlock::instr_iterator(MI));
      }

      s << "\"";

      return s.str();
    }

    std::string getNodeLabel(const SCANode *N,
                             const SpillCostAnalysisGraph &G) 
    {
      MCGNode *mcgNode = N->getMCGNode();
      std::string tmp;
      raw_string_ostream s(tmp);

      if (mcgNode->isUnknown())
        s << "<UNKNOWN-" << *mcgNode->getType() << ">";
      else {
        s << mcgNode->getMF()->getFunction()->getName();
      }

      s << (N->hasCallFreePath() ? "* (occ:" : " (occ:")
        << N->getOccupancy() << ", spill:" << N->getSpillCost() << ")\n"
        << "[rem:" << N->getRemainingOccupancy()
        << ", maxocc:" << N->getMaxOccupancy() << "]";

      return s.str();
    }

    static std::string getNodeAttributes(const SCANode *N,
                                         const SpillCostAnalysisGraph &G)
    {
      if (!N->isValid())
        return "style=filled, fillcolor=\"violet\"";
      else
        return N->getSpillCost() ? "style=filled, fillcolor=\"red\"" : "";
    }
  };
} // end namespace llvm
