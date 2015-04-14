//===-- PatmosLPGenerator.h - Building and solving linear programs. -------===//
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
//===----------------------------------------------------------------------===//

#ifndef _LLVM_TARGET_PATMOSLPGENERATOR_H_
#define _LLVM_TARGET_PATMOSLPGENERATOR_H_

#include "llvm/Support/raw_ostream.h"

#include <map>

using namespace llvm;

namespace llvm {
  class PatmosLP;
  class LPWgt;

  /// Class to associate arbitrary objects with an LP variable.
  ///
  /// The object's address and an optional prefix constitute a unique identifier
  /// used as an LP variable.
  class LPVar
  {
    friend class PatmosLP;
    friend   bool operator <(const LPVar &a, const LPVar &b);
  private:
    /// The object that the LP variable represents
    const void *Object;

    /// Associate a prefix with the object's LP variable
    std::string Prefix;
  public:
    LPVar(const void *object, const char *prefix = "") :
        Object(object), Prefix(prefix) {
    }

    /// Make a weighted LP variable on multiplication.
    LPWgt operator *(double weight) const;
  };

  /// Associate an LP variable with a weight for the cost function.
  class LPWgt
  {
    friend class PatmosLP;
    friend bool operator <(const LPWgt &a, const LPWgt &b);
  private:
    /// An LP variable
    LPVar Var;

    /// The variable's type.
    double Weight;
  public:
    LPWgt(const LPVar &var, double weight) : Var(var), Weight(weight) {
    }
  };

  /// Available types of LP variables
  enum LPTypes
  {
    /// A binary 0-1 variable
    LP_BINARY,
    /// An integer variable
    LP_INTEGER,
    /// A general variable (float)
    LP_DEFAULT
  };

  /// Associate an LP variable with a "type" (binary, integer, general)
  class LPTy
  {
    friend class PatmosLP;
    friend bool operator <(const LPTy &a, const LPTy &b);
  private:
    /// An LP variable
    LPVar Var;

    /// The variable's type.
    LPTypes Type;
  public:
    LPTy(const LPVar &var, LPTypes type) : Var(var), Type(type) {
    }
  };

  /// A kind of a directive for the LP builder or solver.
  enum LPDirectives
  {
    /// End a constraint.
    LP_ENDC,
    // Maximize the LP cost function during solving.
    LP_MAX,
    // Minimize the LP cost function during solving.
    LP_MIN
  };

  class PatmosLP
  {
  private:
    typedef std::map<LPVar, double> LPVarWeights;
    typedef std::map<LPVar, LPTypes> LPVarTypes;

    /// Flag indicating whether the solver should maximize or minimize the cost
    /// function.
    bool Maximize;

    /// Default type of variables
    LPTypes DefaultType;

    /// Associate weight with LP variables in the cost function.
    LPVarWeights CostFunction;

    /// Types associated with LP variables
    LPVarTypes VarTypes;

    std::string Buf;
    raw_string_ostream Constraints;
    bool IsConstrainedClosed;

    /// Name of the .lp file generated from the LP instance.
    SmallString<1024> LPName;

    /// Write a complete LP problem to a file.
    /// Returns true if the file was created successfully.
    bool generateLP(const char *fileName);
  public:
    PatmosLP(bool maximize = true, LPTypes defaulttype = LP_INTEGER) :
        Maximize(maximize), DefaultType(defaulttype), Buf(""), Constraints(Buf),
        IsConstrainedClosed(true) {
    }

    ~PatmosLP();

    ////////////////////////////////////////////////////////////////////////////
    /// Add a variable to a constraint.
    PatmosLP &operator +(const LPVar &var);

    /// Define the weight in the cost function of an LP variable and add it to
    /// the constraint.
    PatmosLP &operator +(const LPWgt &wgt);

    /// Add a constant to a constraint.
    PatmosLP &operator +(double d);

    /// Add a variable to a constraint.
    PatmosLP &operator -(const LPVar &var);

    /// Define the weight in the cost function of an LP variable and add it to
    /// the constraint.
    PatmosLP &operator -(const LPWgt &wgt);

    /// Add a constant to a constraint.
    PatmosLP &operator -(double d);

    /// Close a constraint.
    PatmosLP &operator <(double d);

    /// Close a constraint.
    PatmosLP &operator ==(double d);

    /// Close a constraint.
    PatmosLP &operator <=(double d);

    ////////////////////////////////////////////////////////////////////////////
    // Add the constraints and variables of an other LP.
    // The other LP is wiped clean.
    PatmosLP &operator <<(PatmosLP &lp);

    ////////////////////////////////////////////////////////////////////////////
    /// Declare the default type LP variables.
    PatmosLP &operator <<(const LPTypes &ty);

    /// Declare the type of an LP variable.
    PatmosLP &operator <<(const LPTy &ty);

    /// Define the weight of an LP variable.
    PatmosLP &operator <<(const LPWgt &wgt);

    /// Add a directive to the problem.
    PatmosLP &operator <<(const LPDirectives &dir);

  };

}


#endif // _LLVM_TARGET_PATMOSLPGENERATOR_H_
