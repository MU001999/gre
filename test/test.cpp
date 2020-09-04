#include "../src/gre.hpp"
#include <string>
#include <gtest/gtest.h>

using namespace std;
using namespace gre;

TEST(Simple, SingleChar)
{
    ASSERT_EQ(GRE::full_match(" "s, " ").value(), " ");
}

int main(int argc, char *argv[])
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
