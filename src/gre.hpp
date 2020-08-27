#pragma once

#include <list>
#include <string>
#include <bitset>
#include <memory>
#include <vector>
#include <cstdint>
#include <optional>
#include <type_traits>

namespace gre
{
namespace details
{
class State
{
  public:
    enum EdgeType
    {
        EPSILON,
        CCL,
        EMPTY,
    };

  public:
    explicit State(EdgeType edge_type)
      : edge_type_(edge_type)
      , next1_(nullptr)
      , next2_(nullptr)
    {
        // ...
    }

    std::bitset<256> &accept()
    {
        return accept_;
    }

    void set_next1(State *next)
    {
        next1_ = next;
    }

    void set_next2(State *next)
    {
        next2_ = next;
    }

    State *get_next1() const
    {
        return next1_;
    }

    State *get_next2() const
    {
        return next2_;
    }

  private:
    EdgeType edge_type_;
    std::bitset<256> accept_;
    State *next1_;
    State *next2_;
};

class Allocator
{
  public:
    ~Allocator()
    {
        for (auto ptr : allocated_)
        {
            delete ptr;
        }
    }

    template<typename ...Args>
    State *allocate(Args &&...args)
    {
        auto ptr = new State(std::forward<Args>(args)...);
        allocated_.push_back(ptr);
        return ptr;
    }

  private:
    std::list<State *> allocated_;
};

class Pair
{
  public:
    Pair(Allocator &allocator)
      : start_(allocator.allocate())
      , end_(allocator.allocate())
    {
        // ...
    }

    Pair(State *start, State *end)
      : start_(start), end_(end)
    {
        // ...
    }

    State *start() const
    {
        return start_;
    }

    State *end() const
    {
        return end_;
    }

  private:
    State *start_;
    State *end_;
};

struct AST
{
    AST(Allocator &allocator)
      : allocator_(allocator)
    {
        // ...
    }
    virtual ~AST() = default;

    virtual Pair compile() = 0;

  protected:
    Allocator &allocator_;
};

struct SingleChar : AST
{
    char chr;

    SingleChar(Allocator &allocator, char chr)
      : AST(allocator), chr(chr)
    {
        // ...
    }
};

struct Token
{
    enum Type
    {
        Normal,
        Digit,

        Or,     // |

        End,
    };

    Type type;
    char value;

    Token() = default;
};

class Parser
{
  public:
    Parser(Allocator &allocator,
        const std::string &pattern)
      : allocator_(allocator)
      , pattern_(pattern), ind_(0)
    {
        get_next_token();
    }

    std::unique_ptr<AST> parse()
    {

    }

  private:
    Token &token(Token::Type type)
    {
        cur_tok_.type = type;
        return cur_tok_;
    }
    Token &token(Token::Type type, char value)
    {
        cur_tok_.type = type;
        cur_tok_.value = value;
        return cur_tok_;
    }

    Token &get_next_token()
    {
        if (ind_ >= pattern_.size())
        {
            return token(Token::End);
        }

        auto chr = pattern_[ind_++];
        switch (chr)
        {
        case '|':
            return token(Token::Or);

        case '\\':
        {
            auto chr = pattern_[ind_++];
            switch (chr)
            {
            case 'd':
                return token(Token::Digit);

            default:
                return token(Token::Normal, chr);
            }
        }

        default:
            return token(Token::Normal, chr);
        }
    }

    std::unique_ptr<AST>
    gen_single_char()
    {
        auto sc = std::make_unique<SingleChar>(allocator_, cur_tok_.value);
        get_next_token();
        return sc;
    }

  private:
    Allocator &allocator_;
    const std::string &pattern_;
    std::size_t ind_;
    Token cur_tok_;
};
} // namespace details

class Options
{
  public:
    Options() = default;
};

/**
 * "((pattern)*)"
 *  will generate Group{Group{}, ...}
*/
class Group
{
  public:
    Group(std::string name)
      : name_(std::move(name))
    {
        // ...
    }

    void set_self(std::string self)
    {
        self_.swap(self);
    }

    const std::string &name() const
    {
        return name_;
    }

    const std::string &self() const
    {
        return self_;
    }

    const std::vector<Group> &subs() const
    {
        return subs_;
    }

  private:
    std::string name_;
    std::string self_;
    std::vector<Group> subs_;
};

class GRE
{
  public:
    static std::optional<Group>
    full_match(const GRE &re,
        const std::string &text)
    {
        return re.match(text);
    }

  public:
    GRE(std::string pattern)
      : pattern_(std::move(pattern))
    {
        auto ast = details::Parser(allocator_, pattern_).parse();
        entry_ = ast->compile().start();
    }

    /**
     * @options: unused now
    */
    GRE(std::string pattern, const Options &options)
      : pattern_(std::move(pattern))
    {
        auto ast = details::Parser(allocator_, pattern_).parse();
        entry_ = ast->compile().start();
    }

    const std::string &pattern() const
    {
        return pattern_;
    }

    std::optional<Group>
    match(const std::string &text) const;

  private:
    std::string pattern_;
    details::Allocator allocator_;
    details::State *entry_;
};
} // namespace gre;
