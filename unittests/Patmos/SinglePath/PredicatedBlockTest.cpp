
#include "../lib/Target/Patmos/SinglePath/PredicatedBlock.h"
#include "gtest/gtest.h"
#include "gmock/gmock.h"

using namespace llvm;

namespace llvm{

  class MockMBB{
  public:
    MOCK_CONST_METHOD0(begin, MachineInstr*());
  };

}

namespace {

/// For testing, we mock 'MachineBasicBlock' with 'MockMBB'
typedef _PredicatedBlock<MockMBB> PredicatedBlock;

TEST(PredicatedBlockTest, SinglePredicateTest){
  /*
   * We test that if a block is only predicated by one predicates,
   * 'getBlockPredicates()' only returns that single predicate
   */
  MockMBB mockMBB;
  PredicatedBlock b(& mockMBB, 1);

  auto preds = b.getBlockPredicates();

  ASSERT_EQ(1,preds.size());
	ASSERT_EQ(1,preds[0]);
}

}
