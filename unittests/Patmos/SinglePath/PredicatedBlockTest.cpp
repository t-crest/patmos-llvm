#include "gtest/gtest.h"
#include "gmock/gmock.h"
#include "SinglePath/PredicatedBlock.h"
#include "SinglePath/RAInfo.h"

using namespace llvm;

using ::testing::Return;
using ::testing::Contains;
using ::testing::SizeIs;
using ::testing::UnorderedElementsAreArray;
using ::testing::Eq;

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

    MOCK_METHOD0(succ_begin, MockMBB**());
    MOCK_METHOD0(succ_end, MockMBB**());

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

  PredicatedBlock b(&mockMBB);

  ASSERT_EQ(b.getMBB(),&mockMBB);
}

TEST(PredicatedBlockTest, EmptyBlockTest){
  /*
   * We test that setting the predicate for an empty block
   * makes the block be unpredicated
   */
  MockMBB mockMBB(0);

  PredicatedBlock b(&mockMBB);

  auto preds = b.getBlockPredicates();

  ASSERT_EQ((unsigned)0,preds.size());
}

TEST(PredicatedBlockTest, NoPredsAtInitTest){
  /*
   * We test that no instructions are predicated at init
   */
  MockMBB mockMBB(5);

  PredicatedBlock b(&mockMBB);

  auto preds = b.getBlockPredicates();

  EXPECT_THAT(preds, SizeIs(0));
}

TEST(PredicatedBlockTest, SinglePredicateTest){
  /*
   * We test that if a block is only predicated by one predicate,
   * 'getBlockPredicates()' only returns that single predicate
   */
  MockMBB mockMBB(5);

  PredicatedBlock b(&mockMBB);
  b.setPredicate(2);

  auto preds = b.getBlockPredicates();

  EXPECT_THAT(preds, SizeIs(1));
  EXPECT_THAT(preds, Contains(2));
}

TEST(PredicatedBlockTest, NoDefsAtInitTest){
  /*
   * We test that initially a block does not define any predicate
   * definitions
   */
  MockMBB mockMBB(5);

  PredicatedBlock b(&mockMBB);

  auto defs = b.getDefinitions();

  ASSERT_THAT(defs, SizeIs(0));
}

TEST(PredicatedBlockTest, AddDefinitionTest){
  /*
   * We test that we can add definitions to a block
   */
  MockMBB mockMBB1(1);
  MockMBB mockMBB2(1);
  MockMBB mockMBB3(1);

  PredicatedBlock b1(&mockMBB1);
  PredicatedBlock b2(&mockMBB2);
  PredicatedBlock b3(&mockMBB3);

  b1.addDefinition(2, &b2, 4);
  b1.addDefinition(3, &b3, 5);

  auto defs = b1.getDefinitions();

  EXPECT_THAT(defs, UnorderedElementsAreArray({
    std::make_tuple(2, &b2, 4),
    std::make_tuple(3, &b3, 5)
  }));
}

TEST(PredicatedBlockTest, NoExitAtInitTest){
  /*
   * We test that initially a block does not define any single-path
   * scope exit targets
   */
  MockMBB mockMBB1(1);

  PredicatedBlock b1(&mockMBB1);

  auto exits = b1.getExitTargets();

  EXPECT_THAT(exits, SizeIs(0));
}

TEST(PredicatedBlockTest, AddExitTest){
  /*
   * We test that we can add exit targets
   */
  MockMBB mockMBB1(1);
  MockMBB mockMBB2(1);
  MockMBB mockMBB3(1);

  // We allow the implementation to assert that the added blocks actually
  // are successors of the main block.
  MockMBB* succs[3] = {&mockMBB2, &mockMBB3, NULL};
  EXPECT_CALL(mockMBB1, succ_begin()).WillRepeatedly(Return(succs));
  EXPECT_CALL(mockMBB1, succ_end()).WillRepeatedly(Return(succs+2));

  PredicatedBlock b1(&mockMBB1);
  PredicatedBlock b2(&mockMBB2);
  PredicatedBlock b3(&mockMBB3);

  b1.addExitTarget(&b2);
  b1.addExitTarget(&b3);

  auto exits = b1.getExitTargets();

  EXPECT_THAT(exits, UnorderedElementsAreArray({
      &b2,
      &b3
    }));
}

}
