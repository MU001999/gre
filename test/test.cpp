#include "../src/gre.hpp"
#include <string>
#include <gtest/gtest.h>

using namespace std;
using namespace gre;

TEST(SingleChar, Alpha)
{
    ASSERT_EQ(GRE::full_match("a"s, "a").value(), "a"s);
}

TEST(SingleChar, Blank)
{
    ASSERT_EQ(GRE::full_match(" "s, "  ").value(), " "s);
}

TEST(Cat, Literal)
{
    ASSERT_EQ(GRE::full_match("abcd"s, "abcde").value(), "abcd"s);
}

TEST(Cat, WithCapture)
{
    ASSERT_EQ(GRE::full_match("ab(cd)"s, "abcde").value(), "abcd"s);
}

TEST(Select, Single)
{
    auto re = GRE("a|b|c");
    ASSERT_EQ(re.match("abc").value(), "a"s);
    ASSERT_EQ(re.match("bca").value(), "b"s);
    ASSERT_EQ(re.match("cab").value(), "c"s);
    ASSERT_EQ(re.match("d").has_value(), false);
}

TEST(Capture, Multi)
{
    auto re = GRE("(a)*");
    auto result = re.match("aaaaa").value();

    ASSERT_EQ(result, "aaaaa"s);
    ASSERT_EQ(result.subs().size(), 5);

    for (const auto &sub : result[0])
    {
        ASSERT_EQ(sub, "a"s);
    }
}

TEST(Capture, Named)
{
    auto re = GRE("(?<test>a)*");
    auto result = re.match("aaaaa").value();

    ASSERT_EQ(result, "aaaaa"s);
    ASSERT_EQ(result.subs().size(), 5);

    for (const auto &sub : result["test"])
    {
        ASSERT_EQ(sub, "a"s);
    }
}

TEST(Capture, SimpleNested)
{
    auto re = GRE("(?<1>(?<2>a)*(?<3>b))*");
    auto result = re.match("babaabaaab").value();

    ASSERT_EQ(result, "babaabaaab"s);
    ASSERT_EQ(result.subs().size(), 4);

    int i = 0;
    for (auto &sub : result["1"])
    {
        ASSERT_EQ(sub, string(i, 'a') + "b");

        auto subs2 = sub["2"];
        ASSERT_EQ(subs2.size(), i++);
        for (const auto &str : subs2)
        {
            ASSERT_EQ(str, "a"s);
        }

        auto subs3 = sub["3"];
        ASSERT_EQ(subs3.size(), 1);
        ASSERT_EQ(subs3[0], "b"s);
    }
}

TEST(Failure, Main)
{
    auto re = GRE("abc");
    auto result = re.match("b");
    ASSERT_EQ(result.has_value(), false);
}

int main(int argc, char *argv[])
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
