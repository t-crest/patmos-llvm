//===-- PredicateDefinition.cpp - Patmos LLVM single-path predicate register allocator -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the PredicateDefinition class that handles
// predicate register allocation for single-path code.
//
//===----------------------------------------------------------------------===//


#ifndef TARGET_PATMOS_SINGLEPATH_PREDICATE_DEFINITION_H_
#define TARGET_PATMOS_SINGLEPATH_PREDICATE_DEFINITION_H_

#define DEBUG_TYPE "patmos-singlepath"

#include "spimpl.h"
#include "RAInfo.h"

namespace llvm {

/// Definition of a singlepath predicate, i.e. a calculation of its
/// boolean value prior to being used as a guard for some instructions.
/// A predicate's value is is derived from some condition and is guarded
/// by some other guard predicate.
///
/// There are two kinds of definitions:
/// 1. Simple: these definitions simply define a single predicate value
///            using some other predicate as a guard and a condition.
/// 2. Swap:   This is a special kind of definition, where 2 predicates
///            are defined, with each predicate having the other predicate
///            as its guard. E.g. the following two simple definition 
///            are cumulatively a swap definition:
///                   PAND p1 = P2, p3
///                   PAND p2 = p1, p4
///            Each predicate has its own condition.
///
/// is_swap() and is_simple() can be used to check which kind of definition
/// a given instance of this class is.
/// To access the right predicates and guards, use the .simple or .swap 
/// members after checking the kind of the definition.
///
/// Predicates and guards are unsigned integers, however, this class
/// doesn't dictate what they mean. It is the responsibility of the user
/// to keep track whether these are abstract predicate numbers or
/// physical predicates registers or something else.
class PredicateDefinition {
public:
  union {
    /// If is_wap() is true, this is valid.
    struct {
      /// The first predicate to define
      unsigned predicate_1;
      /// The second predicate register to define.
      unsigned predicate_2;
    } swap;

    /// If is_simple() is true, this is valid.
    struct {
      /// Predicate to define
      unsigned predicate;
      /// The guard of the definition
      unsigned guard;
      /// Whether the predicate location is a register or stack location
      RAInfo::LocType type;
    } simple;
  };

  /// Conditions of the predicates to be defined.
  /// If this is a simple definition, only indeces 0 and 1 are occupied
  /// where the first is the condition predicate register and the second
  /// is whether the negate flag is set.
  ///
  /// If this is a swap definition, the 3rd and 4th indices are also
  /// occupied. The first two are then the condition for the first predicate
  /// and the second two are the condition for the second predicate.
  SmallVector<MachineOperand, 4> conditions;

  /// Constructs a definition that is identical to the one given.
  PredicateDefinition(const PredicateDefinition& from);

  /// Constructs a simple definition
  PredicateDefinition(RAInfo::LocType type, unsigned predicate, unsigned guard, 
    SmallVector<MachineOperand, 2> condition);

  void operator=(const PredicateDefinition& rhs);

  /// Whether this instance is a swap definition.
  bool is_swap();

  /// Whether this instance is a simple definition.
  bool is_simple();

  /// Whether this definition defines a predicate that is
  /// identical to the guard predicate of the given definition.
  bool overwrites_guard_of(PredicateDefinition &other);

  /// Whether this definition and the one given define the same predicate.
  bool share_predicate_target(PredicateDefinition &other);

  /// Whether this definition and the one given can be merged into
  /// a combined swap definition.
  bool can_merge_into_swap(PredicateDefinition &other);

  /// Tries to merge the given definition into this one
  /// to make this one into a swap definition.
  ///
  /// If successful this definition becomes a swap definition and true is returned.
  /// If not, nothing happens and false is returned.
  bool merge_into_swap(PredicateDefinition &other);

private:
  class Impl;
  /// We use the PIMPL pattern to implement the private
  /// members of this instance.
  spimpl::unique_impl_ptr<Impl> Priv;

};
}

#endif /* TARGET_PATMOS_SINGLEPATH_PREDICATE_DEFINITION_H_ */
