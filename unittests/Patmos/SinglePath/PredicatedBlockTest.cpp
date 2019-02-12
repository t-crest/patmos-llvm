#include "gtest/gtest.h"
#include "gmock/gmock.h"
#include "SinglePath/PredicatedBlock.h"
#include "SinglePath/RAInfo.h"

using namespace llvm;

using ::testing::Return;

namespace llvm{

  class MockInstr {
  };

  class MockMBB{
  public:
    MockMBB(unsigned length):
      instr(length, MockInstr())
    {}

    std::vector<MockInstr> instr;

    std::vector<MockInstr>::iterator begin()
    {
      return instr.begin();
    }

    std::vector<MockInstr>::iterator end()
    {
      return instr.end();
    }
  };

}

namespace {

/// For testing, we mock 'MachineBasicBlock' with 'MockMBB'
typedef _PredicatedBlock<MockMBB, MockInstr> PredicatedBlock;

TEST(PredicatedBlockTest, SameMBBTest){
  /*
   * We test that the MBB given to PredicateBlock is also returned
   * by getMBB();
   */
  MockMBB mockMBB(0);

  PredicatedBlock b(&mockMBB, 1);

  ASSERT_EQ(b.getMBB(),&mockMBB);
}

TEST(PredicatedBlockTest, EmptyBlockTest){
  /*
   * We test that setting the predicate for an empty block
   * makes the block be unpredicated
   */
  MockMBB mockMBB(0);

  PredicatedBlock b(&mockMBB, 1);

  auto preds = b.getBlockPredicates();

  ASSERT_EQ((unsigned)0,preds.size());
}

TEST(PredicatedBlockTest, SinglePredicateTest){
  /*
   * We test that if a block is only predicated by one predicate,
   * 'getBlockPredicates()' only returns that single predicate
   */
  MockMBB mockMBB(5);

  PredicatedBlock b(&mockMBB, 1);

  auto preds = b.getBlockPredicates();

  ASSERT_EQ((unsigned)1,preds.size());
	ASSERT_EQ((unsigned)1,*preds.begin());
}

TEST(PredicatedBlockTest, SetPredicateTest){
  /*
   * We test that we can give the whole block a new predicate
   * using 'setPredicate()'.
   */
  MockMBB mockMBB(5);

  PredicatedBlock b(&mockMBB, 1);
  b.setPredicate(2);

  auto preds = b.getBlockPredicates();

  ASSERT_EQ((unsigned)1,preds.size());
  ASSERT_EQ((unsigned)2,*preds.begin());
}

}
