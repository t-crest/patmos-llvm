
#include "../lib/Target/Patmos/SinglePath/PredicatedBlock.h"
#include "gtest/gtest.h"

using namespace llvm;

namespace {

TEST(PredicatedBlockTest, SomeTest){
	PredicatedBlock b((MachineBasicBlock *) 123);
	ASSERT_EQ((MachineBasicBlock *)123,b.getMBB());
}

TEST(PredicatedBlockTest, SomeTest2){
	PredicatedBlock b((MachineBasicBlock *) 123);
	ASSERT_EQ((MachineBasicBlock *)123,b.getMBB());
}
}
