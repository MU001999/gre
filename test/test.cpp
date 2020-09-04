#include "../src/gre.hpp"
#include <string>
#include <gtest/gtest.h>

using namespace std;
using namespace gre;

TEST(Simple, SingleChar)
{
    ASSERT_EQ(GRE::full_match("a"s, "a").value().self(), "a");
}

TEST(Capture, Multi)
{
    auto re = GRE("(a)*");
    auto result = re.match("aaaaa").value();

    ASSERT_EQ(result.self(), "aaaaa");
    ASSERT_EQ(result.subs().size(), 5);

    for (const auto &sub : result[0])
    {
        ASSERT_EQ(sub, "a");
    }
}

TEST(Capture, Named)
{
    auto re = GRE("(?<test>a)*");
    auto result = re.match("aaaaa").value();

    ASSERT_EQ(result.self(), "aaaaa");
    ASSERT_EQ(result.subs().size(), 5);

    for (const auto &sub : result["test"])
    {
        ASSERT_EQ(sub, "a");
    }
}

TEST(Capture, SimpleNested)
{
    auto re = GRE("(?<1>(?<2>a)*(?<3>b))*");
    auto result = re.match("babaabaaab").value();

    ASSERT_EQ(result.self(), "babaabaaab");
    ASSERT_EQ(result.subs().size(), 4);

    int i = 0;
    for (auto &sub : result.subs("1"))
    {
        ASSERT_EQ(sub.get().self(), string(i, 'a') + "b");

        auto subs2 = sub.get()["2"];
        ASSERT_EQ(subs2.size(), i++);
        for (const auto &str : subs2)
        {
            ASSERT_EQ(str, "a");
        }

        auto subs3 = sub.get()["3"];
        ASSERT_EQ(subs3.size(), 1);
        ASSERT_EQ(subs3[0], "b");
    }
}

int main(int argc, char *argv[])
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
