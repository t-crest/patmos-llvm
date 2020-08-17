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

  class MockInstr {};

  class MockMBB{
  public:

    MockMBB(unsigned nrInstr):
      instr(nrInstr, MockInstr()), firstTerm(nrInstr)
    {}

    MockMBB(unsigned nrInstr, unsigned nrTerm):
      instr(nrInstr + nrTerm, MockInstr()), firstTerm(nrInstr)
    {}

    std::vector<MockInstr> instr;

    unsigned firstTerm;

    std::vector<MockInstr>::iterator instr_begin()
    {
      return instr.begin();
    }

    std::vector<MockInstr>::iterator instr_end()
    {
      return instr.end();
    }

    std::vector<MockInstr>::iterator getFirstInstrTerminator()
    {
      auto result = instr.begin();
      for(unsigned i = 0; i < firstTerm; i++){
        result++;
      }
      return result;
    }

    MOCK_METHOD0(succ_begin, MockMBB**());
    MOCK_METHOD0(succ_end, MockMBB**());

  };
}

namespace {

/// For testing, we mock 'MachineBasicBlock' with 'MockMBB'
typedef _PredicatedBlock<MockMBB, MockInstr, int> PredicatedBlock;

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

TEST(PredicatedBlockTest, OnlyTerminatorPredicateTest){
  /*
   * We test that if a block only has one instruction, which is a terminator,
   * 'getBlockPredicates()' still returns the predicate set for the block.
   *
   * We have to support blocks with only terminators, such that the block still has
   * a predicate assigned, even through is has only terminators.
   */

  MockMBB mockMBB(0,1);

  PredicatedBlock b(&mockMBB);
  b.setPredicate(1);

  auto preds = b.getBlockPredicates();

  EXPECT_THAT(preds, SizeIs(1));
  EXPECT_THAT(preds, Contains(1));
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

  b1.addDefinition(PredicatedBlock::Definition{2, 4, &b2, 1, 2});
  b1.addDefinition(PredicatedBlock::Definition{3, 5, &b3, 3, 4});

  auto defs = b1.getDefinitions();

  // We don't care about the ordering in this case, because they don't
  // share any predicates or guards
  EXPECT_THAT(defs, UnorderedElementsAreArray({
    PredicatedBlock::Definition{2, 4, &b2, 1, 2},
    PredicatedBlock::Definition{3, 5, &b3, 3, 4}
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

TEST(PredicatedBlockTest, InitialSuccTest){
  /*
   * We test that a new block's successor are the same as the
   */
  MockMBB mockMBB1(1);

  PredicatedBlock b1(&mockMBB1);

  auto succs = b1.getSuccessors();

  EXPECT_THAT(succs.size(), Eq(0));
}

TEST(PredicatedBlockTest, AddSuccTest){
  /*
   * We test that a new block's successor are the same as the
   */
  MockMBB mockMBB1(1);
  MockMBB mockMBB2(1);
  MockMBB mockMBB3(1);

  PredicatedBlock b1(&mockMBB1);
  PredicatedBlock b2(&mockMBB2);
  PredicatedBlock b3(&mockMBB3);

  b1.addSuccessor(&b2, 2);
  b1.addSuccessor(&b3, 2);

  auto succs = b1.getSuccessors();

  EXPECT_THAT(succs, UnorderedElementsAreArray({
    std::make_pair(&b2, 2),
    std::make_pair(&b3, 2)
  }));
}

TEST(PredicatedBlockTest, MergeTest){
  /*
   * We test that merging two blocks
   */

  MockMBB mockMBB1(2,1);
  MockMBB mockMBB2(2,2);
  MockMBB mockMBB3(1);
  MockMBB mockMBB4(1);

  PredicatedBlock b1(&mockMBB1);
  PredicatedBlock b2(&mockMBB2);
  PredicatedBlock b3(&mockMBB3);
  PredicatedBlock b4(&mockMBB4);

  // We allow the implementation to assert that the added blocks actually
  // are successors of the main block.
  MockMBB* succs[3] = {&mockMBB3, &mockMBB4, NULL};
  EXPECT_CALL(mockMBB1, succ_begin()).WillRepeatedly(Return(succs));
  EXPECT_CALL(mockMBB1, succ_end()).WillRepeatedly(Return(succs+1));
  EXPECT_CALL(mockMBB2, succ_begin()).WillRepeatedly(Return(succs+1));
  EXPECT_CALL(mockMBB2, succ_end()).WillRepeatedly(Return(succs+2));

  b1.setPredicate(1);
  b2.setPredicate(2);

  b1.addDefinition(PredicatedBlock::Definition{3,1,&b3, 3, 1});
  b2.addDefinition(PredicatedBlock::Definition{4,2,&b4, 4, 2});

  b1.addExitTarget(&b3);
  b2.addExitTarget(&b4);

  b1.addSuccessor(&b3, 3);
  b2.addSuccessor(&b4, 4);

  b1.merge(&b2);

  auto instr1Iter = mockMBB1.instr_begin();
  auto instr2Iter = mockMBB2.instr_begin();

  EXPECT_THAT(b1.getInstructionPredicates(), UnorderedElementsAreArray({
    std::make_pair(&(*instr1Iter), 1),
    std::make_pair(&(*(instr1Iter+1)), 1),
    std::make_pair(&(*(instr1Iter+2)), 1),
    std::make_pair(&(*instr2Iter), 2),
    std::make_pair(&(*(instr2Iter+1)), 2),
    std::make_pair(&(*(instr2Iter+2)), 2),
    std::make_pair(&(*(instr2Iter+3)), 2)
  }));


  EXPECT_THAT(b1.getDefinitions() , UnorderedElementsAreArray({
    PredicatedBlock::Definition{3,1,&b3, 3, 1},
    PredicatedBlock::Definition{4,2,&b4, 4, 2}
  }));

  EXPECT_THAT(b1.getExitTargets(), UnorderedElementsAreArray({
    &b3,
    &b4
  }));

  EXPECT_THAT(b1.getSuccessors(), UnorderedElementsAreArray({
    std::make_pair(&b3, 3),
    std::make_pair(&b4, 4)
  }));

  EXPECT_THAT(b1.bundled(), Eq(true));
}

TEST(PredicatedBlockTest, NotBundledAtInitTest){
  /*
   * We test that initially a block is not bundled
   */
  MockMBB mockMBB(5);

  PredicatedBlock b(&mockMBB);

  ASSERT_THAT(b.bundled(), Eq(false));
}

}
