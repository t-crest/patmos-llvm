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



}
