#include "../src/gre.hpp"
#include <set>
#include <string>
#include <gtest/gtest.h>

using namespace std;
using namespace gre;

TEST(SingleChar, Alpha)
{
    ASSERT_EQ(GRE::full_match("a", "a").value(), "a"s);
}

TEST(SingleChar, Blank)
{
    ASSERT_EQ(GRE::full_match(" ", "  ").value(), " "s);
}

TEST(PreDef, Space)
{
    set<char> spaces{' ', '\r', '\f', '\v', '\n', '\t'};
    for (char chr : spaces)
    {
        ASSERT_EQ(GRE::full_match("\\s", string(1, chr)).value(), string(1, chr));
    }

    // 0x80 = -128
    char chr = 0x80;
    for (int i = 0; i <= 0xFF; ++i, ++chr)
    {
        if (!spaces.count(chr))
        {
            ASSERT_EQ(GRE::full_match("\\s", string(1, chr)).has_value(), false);
        }
    }
}

TEST(PreDef, Digit)
{
    for (char n = '0'; n <= '9'; ++n)
    {
        ASSERT_EQ(GRE::full_match("\\d", string(1, n)).value(), string(1, n));
    }

    char chr = 0x80;
    for (int i = 0; i <= 0xFF; ++i, ++chr)
    {
        if (!isdigit(chr))
        {
            ASSERT_EQ(GRE::full_match("\\d", string(1, chr)).has_value(), false);
        }
    }
}

TEST(PreDef, LowerWord)
{
    for (char chr = 'a'; chr <= 'z'; ++chr)
    {
        ASSERT_EQ(GRE::full_match("\\l", string(1, chr)).value(), string(1, chr));
    }

    char chr = 0x80;
    for (int i = 0; i <= 0xFF; ++i, ++chr)
    {
        if (!islower(chr))
        {
            ASSERT_EQ(GRE::full_match("\\l", string(1, chr)).has_value(), false);
        }
    }
}

TEST(PreDef, UpperWord)
{
    for (char chr = 'A'; chr <= 'Z'; ++chr)
    {
        ASSERT_EQ(GRE::full_match("\\u", string(1, chr)).value(), string(1, chr));
    }

    char chr = 0x80;
    for (int i = 0; i <= 0xFF; ++i, ++chr)
    {
        if (!isupper(chr))
        {
            ASSERT_EQ(GRE::full_match("\\u", string(1, chr)).has_value(), false);
        }
    }
}

TEST(PreDef, Word)
{
    ASSERT_EQ(GRE::full_match("\\w", "_").value(), "_"s);

    for (char chr = 'a'; chr <= 'z'; ++chr)
    {
        ASSERT_EQ(GRE::full_match("\\w", string(1, chr)).value(), string(1, chr));
    }

    for (char chr = 'A'; chr <= 'Z'; ++chr)
    {
        ASSERT_EQ(GRE::full_match("\\w", string(1, chr)).value(), string(1, chr));
    }

    for (char n = '0'; n <= '9'; ++n)
    {
        ASSERT_EQ(GRE::full_match("\\w", string(1, n)).value(), string(1, n));
    }

    char chr = 0x80;
    for (int i = 0; i <= 0xFF; ++i, ++chr)
    {
        if (!isalpha(chr) && !isdigit(chr) && chr != '_')
        {
            ASSERT_EQ(GRE::full_match("\\w", string(1, chr)).has_value(), false);
        }
    }
}

TEST(Cat, Literal)
{
    ASSERT_EQ(GRE::full_match("abcd", "abcde").value(), "abcd"s);
}

TEST(Cat, WithCapture)
{
    ASSERT_EQ(GRE::full_match("ab(cd)", "abcde").value(), "abcd"s);
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
    auto re = GRE("((a)*(b))*");
    auto result = re.match("babaabaaab").value();

    ASSERT_EQ(result, "babaabaaab"s);
    ASSERT_EQ(result.subs().size(), 4);

    int i = 0;
    for (auto &sub : result[0])
    {
        ASSERT_EQ(sub, string(i, 'a') + "b");

        auto subs2 = sub[1];
        ASSERT_EQ(subs2.size(), i++);
        for (const auto &str : subs2)
        {
            ASSERT_EQ(str, "a"s);
        }

        auto subs3 = sub[2];
        ASSERT_EQ(subs3.size(), 1);
        ASSERT_EQ(subs3[0], "b"s);
    }
}

TEST(Capture, NameRef)
{
    auto re = GRE("(?<test>a)(?<test>)*");
    auto result = re.match("aaaaa").value();

    ASSERT_EQ(result, "aaaaa"s);
    ASSERT_EQ(result.subs().size(), 5);

    for (const auto &sub : result["test"])
    {
        ASSERT_EQ(sub, "a"s);
    }
}

TEST(Qualifier, NTimes)
{
    ASSERT_EQ(GRE::full_match("a{5}", "aaaaa").value(), "aaaaa"s);
}

TEST(Qualifier, AtLeastNTimes)
{
    ASSERT_EQ(GRE::full_match("a{5,}", "aaaaaa").value(), "aaaaaa"s);
}

TEST(Range, Normal)
{
    auto re = GRE("[abc]");
    ASSERT_EQ(re.match("abc").value(), "a"s);
    ASSERT_EQ(re.match("bca").value(), "b"s);
    ASSERT_EQ(re.match("cab").value(), "c"s);
    ASSERT_EQ(re.match("d").has_value(), false);
}

TEST(Failure, Main)
{
    ASSERT_EQ(GRE::full_match("abc", "b").has_value(), false);
}

int main(int argc, char *argv[])
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
