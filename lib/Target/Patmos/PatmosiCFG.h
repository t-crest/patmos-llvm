//===- PatmosiCFG.h - Interprocedural Control-flow graph. -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Provide a (possibly context-sensitive) interprocedural CFG.
//
//===----------------------------------------------------------------------===//

#ifndef _LLVM_TARGET_PATMOSICFG_H_
#define _LLVM_TARGET_PATMOSICFG_H_

#include <cstddef>
#include <cassert>
#include <list>
#include <set>

namespace llvm {
  // forward declarations
  class PatmosCallGraphBuilder;
  class PatmosLP;
  class MachineInstr;
  class iCFG;

  // A node in the interprocedural CFG.
  class iCFGNode {
  public:
    typedef std::list<iCFGNode> List;

  public:
    iCFG &G;

    // First instruction of the CFG node
    const MachineInstr *FirstMI;

    // Last instruction of the CFG node
    const MachineInstr *LastMI;

    iCFGNode(iCFG &g, const MachineInstr *firstMI) :
        G(g), FirstMI(firstMI), LastMI(NULL) {
    }

    bool operator<(const iCFGNode &n) const {
      if (FirstMI == n.FirstMI)
        return LastMI < n.LastMI;
      else
        return FirstMI < n.FirstMI;
    }

    void dump() const;
  };

  /// An edge in the interprocedural CFG.
  class iCFGEdge {
  public:
    typedef std::set<iCFGEdge> Set;

    enum Flags {
      // The edge represents a function call
      CALL = 1,
      // The edge represents a return from a function call
      RETURN = 3,
      // The edge connects two nodes belonging to the same basic block
      BBLINK = 7
    };
  private:
    const iCFGNode *Src;
    const iCFGNode *Dest;
    unsigned int EdgeFlags;
  public:
    iCFGEdge(const iCFGNode &src, const iCFGNode &dest, unsigned int flags) :
        Src(&src), Dest(&dest), EdgeFlags(flags) {
    }

    const iCFGNode *getSrc() const {
      return Src;
    }

    const iCFGNode *getDest() const {
      return Dest;
    }

    bool isCall() const {
      return EdgeFlags == CALL;
    }

    bool isReturn() const {
      return EdgeFlags == RETURN;
    }

    bool isFlow() const {
      return EdgeFlags == 0 || isCall() || isReturn();
    }

    bool isBBLink() const {
      return EdgeFlags == BBLINK;
    }

    bool operator<(const iCFGEdge &e) const {
      if (Src == e.Src) {
        if (Dest == e.Dest)
          return EdgeFlags < e.EdgeFlags;
        else
          return Dest < e.Dest;
      }
      else
        return Src < e.Src;
    }

    void dump() const;
  };

  // An interprocedural CFG.
  class iCFG {
    friend class iCFGAnalysisContext;
  private:
    iCFGNode::List Nodes;
    iCFGEdge::Set Edges;

    const iCFGNode *RootNode;

  public:
    const iCFGNode &getRootNode() const {
      assert(RootNode);
      return *RootNode;
    }

    const iCFGNode::List &getNodes() const {
      return Nodes;
    }

    const iCFGEdge::Set &getEdges() const {
      return Edges;
    }

    void view() const;

    void view(PatmosLP &LP) const;

    void write() const;
  };

  iCFG *computeiCFG(PatmosCallGraphBuilder &pcgb);
}

#endif //_LLVM_TARGET_PATMOSICFG_H_
