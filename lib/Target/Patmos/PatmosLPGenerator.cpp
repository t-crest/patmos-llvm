//===-- PatmosLPGenerator.cpp - Building and solving linear programs. -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Infrastructure to build and solve linear programs, additional classes to
// model execution flow and other flows in programs.
//
// see PatmosCallGraphBuilder
//
//===----------------------------------------------------------------------===//

#include "PatmosLPGenerator.h"

#include "llvm/Support/Program.h"

using namespace llvm;

namespace llvm {
  /// Order on LPVar objects (needed for std::map)
  bool operator <(const LPVar &a, const LPVar &b) {
    if (a.Prefix != b.Prefix)
      return (a.Prefix < b.Prefix);
    else
      return (a.Object < b.Object);
  }

  /// Order on LPTy objects (needed for std::map)
  bool operator <(const LPTy &a, const LPTy &b) {
    if (a.Type != b.Type)
      return (a.Type < b.Type);
    else
      return (a.Var < b.Var);
  }

  /// Order on LPWgt objects (needed for std::map)
  bool operator <(const LPWgt &a, const LPWgt &b) {
    if (a.Weight != b.Weight)
      return (a.Weight < b.Weight);
    else
      return (a.Var < b.Var);
  }

  LPWgt LPVar::operator *(double weight) const
  {
    return LPWgt(*this, weight);
  }

  PatmosLP &PatmosLP::operator +(const LPVar &var) {
    IsConstrainedClosed = false;

    // add to cost-function (with weight 0 if it does not yet exist)
    CostFunction[var] += 0.0;

    // append to constraint
    Constraints << " + " << var.Prefix << var.Object;
    return *this;
  }

  PatmosLP &PatmosLP::operator +(const LPWgt &wgt) {
    IsConstrainedClosed = false;
    Constraints << " + " << wgt.Weight << ' '  << wgt.Var.Prefix
                << wgt.Var.Object;
    return *this;
  }

  PatmosLP &PatmosLP::operator +(double d) {
    IsConstrainedClosed = false;
    Constraints << " + " << d;
    return *this;
  }

  PatmosLP &PatmosLP::operator -(const LPVar &var) {
    IsConstrainedClosed = false;

    // add to cost-function (with weight 0 if it does not yet exist)
    CostFunction[var] += 0.0;

    // append to constraint
    Constraints << " - " << var.Prefix << var.Object;
    return *this;
  }

  PatmosLP &PatmosLP::operator -(const LPWgt &wgt) {
    IsConstrainedClosed = false;
    Constraints << " - " << wgt.Weight << ' ' << wgt.Var.Prefix
                << wgt.Var.Object;
    return *this;
  }

  PatmosLP &PatmosLP::operator -(double d) {
    IsConstrainedClosed = false;
    Constraints << " - " << d;
    return *this;
  }

  PatmosLP &PatmosLP::operator <(double d) {
    IsConstrainedClosed = true;
    Constraints << " < " << d << '\n';
    return *this;
  }

  PatmosLP &PatmosLP::operator <<(PatmosLP &lp)
  {
    // copy weights of cost function
    CostFunction.insert(lp.CostFunction.begin(), lp.CostFunction.end());
    lp.CostFunction.clear();

    // copy types
    VarTypes.insert(lp.VarTypes.begin(), lp.VarTypes.end());
    lp.VarTypes.clear();

    // copy constraints
    Constraints << lp.Constraints.str();
    lp.Buf.clear();

    return *this;
  }

  PatmosLP &PatmosLP::operator ==(double d) {
    IsConstrainedClosed = true;
    Constraints << " = " << d << '\n';
    return *this;
  }

  PatmosLP &PatmosLP::operator <=(double d) {
    IsConstrainedClosed = true;
    Constraints << " <= " << d << '\n';
    return *this;
  }

  PatmosLP &PatmosLP::operator <<(const LPTypes &ty) {
    DefaultType = ty;
    return *this;
  }

  PatmosLP &PatmosLP::operator <<(const LPTy &ty) {
    VarTypes[ty.Var] = ty.Type;
    return *this;
  }

  PatmosLP &PatmosLP::operator <<(const LPWgt &wgt) {
    CostFunction[wgt.Var] = wgt.Weight;
    return *this;
  }

  PatmosLP &PatmosLP::operator <<(const LPDirectives &dir) {
    switch(dir)
    {
      case LP_ENDC:
        Constraints << '\n';
        IsConstrainedClosed = true;
        break;
      case LP_MAX:
        Maximize = true;
        break;
      case LP_MIN:
        Maximize = false;
        break;
    }
    return *this;
  }

  PatmosLP::~PatmosLP() {
    if (LPName != "") {
      sys::fs::remove(LPName.str());
    }
  }

  bool PatmosLP::generateLP(const char *fileName) {
    assert(IsConstrainedClosed);

    // open LP file.
    error_code err = sys::fs::createUniqueDirectory("PatmosLP", LPName);
    if (err) {
      errs() << "Error creating temp .lp file: " << err.message() << "\n";
      return false;
    }

    sys::path::append(LPName, fileName);

    std::string ErrMsg;
    raw_fd_ostream OS(LPName.c_str(), ErrMsg);
    if (!ErrMsg.empty()) {
      errs() << "Error: Failed to open file '" << LPName.str()
             << "' for writing!\n";
      return false;
    }

    // start objective function
    OS << (Maximize ? "Maximize\n" : "Minimize\n");

    // Terms of objective function
    unsigned int cnt = 0;
    for(LPVarWeights::const_iterator i(CostFunction.begin()),
        ie(CostFunction.end()); i != ie; i++)
    {
      OS << " + " << i->second << ' ' << i->first.Prefix << i->first.Object
         << (++cnt % 5 == 0 ? "\n" : "");
    }

    // Constraints
    OS << "\nSubject To\n";
    OS << Constraints.str();

    // Declare variables
    OS << "\nGenerals\n";
    for(LPVarWeights::const_iterator i(CostFunction.begin()),
        ie(CostFunction.end()); i != ie; i++)
    {
      LPVarTypes::const_iterator tmp(VarTypes.find(i->first));
      LPTypes ty = (tmp == VarTypes.end()) ? DefaultType : tmp->second;
      if (ty == LP_INTEGER)
        OS << i->first.Prefix << i->first.Object
           << (cnt % 5 == 0 ? '\n' : '\t');
    }

    OS << "\nBinaries\n";
    for(LPVarWeights::const_iterator i(CostFunction.begin()),
        ie(CostFunction.end()); i != ie; i++)
    {
      LPVarTypes::const_iterator tmp(VarTypes.find(i->first));
      LPTypes ty = (tmp == VarTypes.end()) ? DefaultType : tmp->second;
      if (ty == LP_BINARY)
        OS << i->first.Prefix << i->first.Object
           << (cnt % 5 == 0 ? '\n' : '\t');
    }

    OS << "\nEnd\n";

    return true;
  }
}



