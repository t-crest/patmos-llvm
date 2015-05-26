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

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Program.h"

#include <fstream>
#include <iomanip>

using namespace llvm;

typedef enum { LP_GLPK, LP_COINS, LP_SCIP } LPSolver;

static cl::opt<LPSolver> PatmosLPSolver("mpatmos-lp-solver",
                        cl::init(LP_SCIP),
                        cl::desc("Supported solvers for linear problems."),
                        cl::values(
                            clEnumValN(LP_GLPK,
                                       "glpk",
                                       "use glpsol of GLPK"),
                            clEnumValN(LP_COINS,
                                       "coins",
                                       "use cbcsol script for cbc of COINS-OR"),
                            clEnumValEnd));

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
    assert(LPName.empty() && lp.LPName.empty());

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
    SmallString<1024> Path(LPName);
    sys::path::remove_filename(Path);
    if (Path != "") {
      // TODO: reenable
//       sys::fs::remove_all(Path.str());
    }
  }

  bool PatmosLP::generateLP() {
    assert(IsConstrainedClosed && LPName.empty());

    // open LP file.
    error_code err = sys::fs::createUniqueDirectory("PatmosLP", LPName);
    if (err) {
      errs() << "Error creating temp .lp file: " << err.message() << "\n";
      return false;
    }

    sys::path::append(LPName, Name);
    LPName += ".lp";

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
           << (++cnt % 5 == 0 ? '\n' : '\t');
    }

    OS << "\nBinaries\n";
    for(LPVarWeights::const_iterator i(CostFunction.begin()),
        ie(CostFunction.end()); i != ie; i++)
    {
      LPVarTypes::const_iterator tmp(VarTypes.find(i->first));
      LPTypes ty = (tmp == VarTypes.end()) ? DefaultType : tmp->second;
      if (ty == LP_BINARY)
        OS << i->first.Prefix << i->first.Object
           << (++cnt % 5 == 0 ? '\n' : '\t');
    }

    OS << "\nEnd\n";

    return true;
  }

  LPStatus PatmosLP::parseSol(const char *solName) {
    switch (PatmosLPSolver) {
      case LP_GLPK:
        return parseSolGLPK(solName);
      case LP_COINS:
        return parseSolCOINS(solName);
      case LP_SCIP:
        return parseSolSCIP(solName);
    }
    llvm_unreachable("praseSol: unknown LP solver kind.");
  }

  LPStatus PatmosLP::parseSolSCIP(const char *solName) {
    std::ifstream IS(solName);
    if (!IS) {
      errs() << "Error: Failed to open file '" << solName << "' for reading!\n";
      return LP_FAIL;
    }

    IS >> std::setw(100);

    // read from file
    char state[100] = {0,};
    IS.ignore(17);
    IS >> state;
    IS.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    if ((!strcmp(state, "infeasible")) || (!strcmp(state, "unbounded")))
      return LP_NOSOLUTION;
    else if (strcmp(state, "optimal"))
      return LP_FAIL;

    IS.ignore(17);
    IS >> ObjectiveValue;

    // sort variables by name
    typedef std::map<std::string, LPVar> LPVarNames;
    LPVarNames SortedLPVarNames;
    for(LPVarWeights::iterator i(CostFunction.begin()), ie(CostFunction.end());
        i != ie; i++) {
      std::string tmp;
      raw_string_ostream s(tmp);
      s << i->first.Prefix << i->first.Object;
      SortedLPVarNames.insert(std::make_pair(s.str(), i->first));
    }

    // read non-zero variables from the file (not sorted)
    while(IS.good()) {
      char name[100];
      double tmpVal;
      IS >> name;

      if (!IS.good())
        break;

      IS >> tmpVal;

      if (!IS.good())
        break;

      IS.ignore(std::numeric_limits<std::streamsize>::max(), '\n');

      LPVarNames::iterator v = SortedLPVarNames.find(name);
      assert(v != SortedLPVarNames.end());
      Solution.insert(LPWgt(v->second, tmpVal));
      SortedLPVarNames.erase(v);
    }

    // explicitly set other variables to zero
    for(LPVarNames::iterator i(SortedLPVarNames.begin()),
      ie(SortedLPVarNames.end()); i != ie; i++) {
      Solution.insert(LPWgt(i->second, 0));
    }

    return LP_OPTIMAL;
  }

  LPStatus PatmosLP::parseSolCOINS(const char *solName) {
    std::ifstream IS(solName);
    if (!IS) {
      errs() << "Error: Failed to open file '" << solName << "' for reading!\n";
      return LP_FAIL;
    }

    IS >> std::setw(100);

    // read from file
    char state[100] = {0,};
    IS >> state;
    if (!strcmp(state, "Infeasible") || !strcmp(state, "Unbounded"))
      return LP_NOSOLUTION;
    else if (!strcmp(state, "optimal"))
      return LP_FAIL;

    IS.ignore(20);
    IS >> ObjectiveValue;

    // sort variables by name
    typedef std::map<std::string, LPVar> LPVarNames;
    LPVarNames SortedLPVarNames;
    for(LPVarWeights::iterator i(CostFunction.begin()), ie(CostFunction.end());
        i != ie; i++) {
      std::string tmp;
      raw_string_ostream s(tmp);
      s << i->first.Prefix << i->first.Object;
      SortedLPVarNames.insert(std::make_pair(s.str(), i->first));
    }

    // read variable by variable from the file (sorted by variable name)
    for(LPVarNames::iterator i(SortedLPVarNames.begin()),
      ie(SortedLPVarNames.end()); i != ie; i++) {
      unsigned int idx;
      char name[100];
      double tmpVal;
      IS >> idx;
      IS >> name;
      IS >> tmpVal;
      IS.ignore(std::numeric_limits<std::streamsize>::max(), '\n');

      assert(i->first == name);
      Solution.insert(LPWgt(i->second, tmpVal));
    }

    return LP_OPTIMAL;
  }

  LPStatus PatmosLP::parseSolGLPK(const char *solName) {
    std::ifstream IS(solName);
    if (!IS) {
      errs() << "Error: Failed to open file '" << solName << "' for reading!\n";
      return LP_FAIL;
    }

    unsigned int numRows;
    unsigned int numCols;
    enum solStatus_t {UNDEF = 1, FEAS = 2, NOFEAS = 4, OPT = 5};
    unsigned int solStatus;

    // read from file
    IS >> numRows;
    IS >> numCols;
    IS >> solStatus;
    IS >> ObjectiveValue;

    // skip auxiliary variables
    for(unsigned int i = 0; i < numRows; i++) {
      double tmpVal;
      IS >> tmpVal;
    }

    assert(CostFunction.size() == numCols);
    LPVarWeights::iterator v(CostFunction.begin());
    for(unsigned int i = 0; i < numCols; i++) {
      double tmpVal;
      IS >> tmpVal;
      Solution.insert(LPWgt(v->first, tmpVal));
      v++;
    }

    switch (solStatus) {
      case FEAS:
        return LP_SOLVED;
      case OPT:
        return LP_OPTIMAL;
      default:
        return LP_FAIL;
    }
  }

  bool PatmosLP::runSolver(const char *solName, const char *logName) {
    switch (PatmosLPSolver) {
      case LP_GLPK:
        return runSolverGLPK(solName, logName);
      case LP_COINS:
        return runSolverCOINS(solName, logName);
      case LP_SCIP:
        return runSolverSCIP(solName, logName);
    }
    llvm_unreachable("praseSol: unknown LP solver kind.");
  }

  bool PatmosLP::runSolverGLPK(const char *solName, const char *logName) {
    const std::string LPSolver("glpsol");
    std::vector<const char*> args;
    args.push_back(LPSolver.c_str());
    args.push_back("--lp");
    args.push_back(LPName.c_str());
    args.push_back("--write");
    args.push_back(solName);
    args.push_back("--log");
    args.push_back(logName);
    args.push_back(0);

    StringRef empty("");
    const StringRef *redirects[] = {&empty, &empty, &empty};

    std::string ErrMsg;
    if (sys::ExecuteAndWait(sys::FindProgramByName(LPSolver),
                            &args[0],0,redirects,0,0,&ErrMsg)) {
      report_fatal_error("calling LP solver (" + LPSolver + "): " + ErrMsg);
      return false;
    }

    return true;
  }

  bool PatmosLP::runSolverCOINS(const char *solName, const char *logName) {
    const std::string LPSolver("cbcsol");
    std::vector<const char*> args;
    args.push_back(LPSolver.c_str());
    args.push_back(LPName.c_str());
    args.push_back(solName);
    args.push_back(logName);
    args.push_back(0);

    StringRef empty("");
    const StringRef *redirects[] = {&empty, &empty, &empty};

    std::string ErrMsg;
    if (sys::ExecuteAndWait(sys::FindProgramByName(LPSolver),
                            &args[0],0,redirects,0,0,&ErrMsg)) {
      report_fatal_error("calling LP solver (" + LPSolver + "): " + ErrMsg);
      return false;
    }
    while(true);
    return true;
  }

  bool PatmosLP::runSolverSCIP(const char *solName, const char *logName) {
    // TODO: replace script by "-c \"read xxx.lp\""-style arguments
    const std::string LPSolver("scipsol");
    std::vector<const char*> args;
    args.push_back(LPSolver.c_str());
    args.push_back(LPName.c_str());
    args.push_back(solName);
    args.push_back(logName);
    args.push_back(0);

    StringRef empty("");
    const StringRef *redirects[] = {&empty, &empty, &empty};

    std::string ErrMsg;
    if (sys::ExecuteAndWait(sys::FindProgramByName(LPSolver),
                            &args[0],0,redirects,0,0,&ErrMsg)) {
      report_fatal_error("calling LP solver (" + LPSolver + "): " + ErrMsg);
      return false;
    }
    return true;
  }

  LPStatus PatmosLP::solve() {
    // write the .lp file to the disk
    if (!generateLP())
      return LP_FAIL;

    SmallString<1024> logName = LPName;
    sys::path::replace_extension(logName, ".log");

    SmallString<1024> solName = LPName;
    sys::path::replace_extension(solName, ".sol");

    // invoke the LP solver
    if (!runSolver(solName.c_str(), logName.c_str()))
      return LP_FAIL;

    // parse the solution file
    return parseSol(solName.c_str());
  }

  const PatmosLP::LPSolution &PatmosLP::getSolution() const {
    return Solution;
  }

  PatmosLP::LPSolution PatmosLP::getSolution(const void *object) const {
    assert(object);

    LPSolution result;
    for(LPSolution::const_iterator i(Solution.begin()), ie(Solution.end());
        i != ie; i++) {
      if (i->Var.Object == object)
        result.insert(*i);
    }

    return result;
  }
}



