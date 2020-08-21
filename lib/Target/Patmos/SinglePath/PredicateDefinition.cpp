//===-- PredicateDefinition.cpp - Patmos LLVM single-path predicate register allocator -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The PredicateDefinition class handles predicate register allocation for single-path code.
//
//===----------------------------------------------------------------------===//
#include "PredicateDefinition.h"

using namespace llvm;

// The private implementation of PredicateDefinition using the PIMPL pattern.
class PredicateDefinition::Impl {
public:
  // A reference to the PredicateDefinition that uses this instance
  // to implement its private members. (I.e. the public part
  // of the implementation)
  PredicateDefinition &Pub;

  enum DefinitionType{ Simple, Swap } type;

  Impl(PredicateDefinition *pub, const PredicateDefinition &from):
    Pub(*pub), type(from.Priv->type)
  {}
  Impl(PredicateDefinition *pub): Pub(*pub), type(DefinitionType::Simple)
  {}
};

///////////////////////////////////////////////////////////////////////////////
//  PredicateDefinition methods
///////////////////////////////////////////////////////////////////////////////

PredicateDefinition::PredicateDefinition(const PredicateDefinition& from):
  Priv(spimpl::make_unique_impl<Impl>(this, from))
{
  conditions.push_back(from.conditions[0]);
  conditions.push_back(from.conditions[1]);
  if( is_swap() ){
    swap.predicate_1 = from.swap.predicate_1;
    swap.predicate_2 = from.swap.predicate_2;
    conditions.push_back(from.conditions[2]);
    conditions.push_back(from.conditions[3]);
  }else if( is_simple() ){
    simple.type = from.simple.type;
    simple.predicate = from.simple.predicate;
    simple.guard = from.simple.guard;
  }else{
    report_fatal_error("PredicateDefinition::PredicateDefinition(PredicateDefinition&) called on unsupported Definition type.");
  }
}

PredicateDefinition::PredicateDefinition(RAInfo::LocType type,
  unsigned predicate, unsigned guard, SmallVector<MachineOperand, 2> condition):
  Priv(spimpl::make_unique_impl<Impl>(this))
{
  simple.type = type;
  simple.predicate = predicate;
  simple.guard = guard;
  conditions.push_back(condition[0]);
  conditions.push_back(condition[1]);
}

void PredicateDefinition::operator=(const PredicateDefinition& rhs) {
  Priv->type = rhs.Priv->type;
  conditions.clear();
  conditions.push_back(rhs.conditions[0]);
  conditions.push_back(rhs.conditions[1]);
  if( is_swap() ){
    swap.predicate_1 = rhs.swap.predicate_1;
    swap.predicate_2 = rhs.swap.predicate_2;
    conditions.push_back(rhs.conditions[2]);
    conditions.push_back(rhs.conditions[3]);
  }else if( is_simple() ){
    simple.type = rhs.simple.type;
    simple.predicate = rhs.simple.predicate;
    simple.guard = rhs.simple.guard;
  }else{
    report_fatal_error("PredicateDefinition::operator= called on unsupported Definition type.");
  }
}

bool PredicateDefinition::is_swap(){
  return Priv->type == Impl::Swap;
}

bool PredicateDefinition::is_simple(){
  return Priv->type == Impl::Simple;
}

bool PredicateDefinition::overwrites_guard_of(PredicateDefinition &other) {
  if( is_swap() ){
    if( other.is_swap() ) {
      return swap.predicate_1 == other.swap.predicate_1 ||
             swap.predicate_1 == other.swap.predicate_2 ||
             swap.predicate_2 == other.swap.predicate_1 ||
             swap.predicate_2 == other.swap.predicate_2;
    } else if( other.is_simple() ) {
      return swap.predicate_1 == other.simple.guard ||
             swap.predicate_2 == other.simple.guard;
    }
  } else if( is_simple() ){
    if(simple.type == RAInfo::Register){
      if( other.is_swap() ) {
        return simple.predicate == other.swap.predicate_1 ||
               simple.predicate == other.swap.predicate_2;
      } else if( other.is_simple() ) {
        return simple.predicate == other.simple.guard ;
      }
    } else {
      return false;
    }
  }
  report_fatal_error("PredicateDefinition::overwrite_guard_of called on unsupported Definition type.");
}

bool PredicateDefinition::share_predicate_target(PredicateDefinition &other){
  if( is_swap() ){
    if( other.is_swap() ) {
      return overwrites_guard_of(other);
    } else if( other.is_simple() ) {
      return other.overwrites_guard_of(*this);
    }
  } else if( is_simple() ){
    if(simple.type == RAInfo::Register){
      if( other.is_swap() ) {
        return overwrites_guard_of(other);
      } else if( other.is_simple() ) {
        return simple.predicate == other.simple.predicate ;
      }
    } else {
      return false;
    }
  }
  report_fatal_error("PredicateDefinition::share_predicate_register_target called on unsupported definition type.");
}

bool PredicateDefinition::can_merge_into_swap(PredicateDefinition &other) {
  return is_simple() && other.is_simple() &&
         simple.type == RAInfo::Register &&
		 other.simple.type == RAInfo::Register &&
         simple.predicate == other.simple.guard &&
         simple.guard == other.simple.predicate;
}

bool PredicateDefinition::merge_into_swap(PredicateDefinition &other) {
  if( can_merge_into_swap(other) ) {
    auto pred1 = simple.predicate, pred2 = simple.guard;

    Priv->type = Impl::DefinitionType::Swap;
    swap.predicate_1 = pred1;
    swap.predicate_2 = pred2;
    conditions.push_back(other.conditions[0]);
    conditions.push_back(other.conditions[1]);

    return true;
  } else {
    return false;
  }
}