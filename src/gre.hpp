#pragma once

#include <map>
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
inline const std::bitset<256> SPACES(0X100003e00ULL);
inline const std::bitset<256> DIGITS(287948901175001088ULL);
inline const std::bitset<256> LWORDS("111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
inline const std::bitset<256> UWORDS("1111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000");
inline const std::bitset<256> WORD_S("111111111111111111111111110100001111111111111111111111111100000001111111111000000000000000000000000000000000000000000000000");
inline const std::bitset<256> NON_SPACES = ~SPACES;
inline const std::bitset<256> NON_DIGITS = ~DIGITS;
inline const std::bitset<256> NON_LWORDS = ~LWORDS;
inline const std::bitset<256> NON_UWORDS = ~UWORDS;
inline const std::bitset<256> NON_WORD_S = ~WORD_S;

inline const std::map<char, std::bitset<256>> ECMAP
{
    {'s', SPACES}, {'S', NON_SPACES},
    {'d', DIGITS}, {'D', NON_DIGITS},
    {'l', LWORDS}, {'L', NON_LWORDS},
    {'u', UWORDS}, {'U', NON_UWORDS},
    {'w', WORD_S}, {'W', NON_WORD_S}
};

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

struct SingleCharExpr : AST
{
    char chr;

    SingleCharExpr(Allocator &allocator, char chr)
      : AST(allocator), chr(chr)
    {
        // ...
    }

    Pair compile() override;
};

struct DotExpr : AST
{
    DotExpr(Allocator &allocator)
      : AST(allocator)
    {
        // ...
    }

    Pair compile() override;
};

struct PredefExpr : AST
{
    char sym;

    PredefExpr(Allocator &allocator, char sym)
      : AST(allocator), sym(sym)
    {
        // ...
    }

    Pair compile() override;
};

struct CatExpr : AST
{
    std::unique_ptr<AST> lhs, rhs;

    CatExpr(Allocator &allocator,
        std::unique_ptr<AST> &&lhs,
        std::unique_ptr<AST> &&rhs)
      : AST(allocator)
      , lhs(std::move(lhs))
      , rhs(std::move(rhs))
    {
        // ...
    }

    Pair compile() override;
};

struct SelectExpr : AST
{
    std::unique_ptr<AST> lhs, rhs;

    SelectExpr(Allocator &allocator,
        std::unique_ptr<AST> &&lhs,
        std::unique_ptr<AST> &&rhs)
      : AST(allocator)
      , lhs(std::move(lhs))
      , rhs(std::move(rhs))
    {
        // ...
    }

    Pair compile() override;
};

struct QualifierExpr : AST
{
    enum Mode : int
    {
        NTimes = -1,
        AtLeastNTimes = -2,
    };

    std::unique_ptr<AST> expr;
    int n, m;

    QualifierExpr(Allocator &allocator,
        std::unique_ptr<AST> &&expr,
        int n, int m = Mode::NTimes)
      : AST(allocator)
      , expr(std::move(expr))
      , n(n), m(m)
    {
        // ...
    }

    Pair compile() override;
};

struct RangeExpr : AST
{
    std::bitset<256> accept;

    RangeExpr(Allocator &allocator,
        std::bitset<256> accept)
      : AST(allocator), accept(accept)
    {
        // ...
    }

    Pair compile() override;
};

struct SubExpr : AST
{
    std::size_t index;
    std::string name;
    std::unique_ptr<AST> expr;

    SubExpr(Allocator &allocator,
        std::size_t index,
        std::string name,
        std::unique_ptr<AST> &&expr)
      : AST(allocator)
      , index(index)
      , name(std::move(name))
      , expr(std::move(expr))
    {
        // ...
    }

    Pair compile() override;
};

struct Token
{
    enum Type
    {
        Normal,
        Escape,

        Dot,

        Predef,

        Star,
        Plus,
        Ques,

        Or,
        Caret,
        LParen,
        RParen,
        LBracket,
        RBracket,

        End,
    };

    Type type;
    char value;

    Token() = default;

    bool is_normal(char val)
    {
        return type == Normal && value == val;
    }
};

class Parser
{
  public:
    Parser(Allocator &allocator,
        const std::string &pattern)
      : allocator_(allocator)
      , pattern_(pattern)
      , ind_of_pattern_(0)
      , ind_of_subexpr_(0)
    {
        get_next_token();
    }

    std::unique_ptr<AST> parse()
    {
        return gen_select_expr();
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
        if (ind_of_pattern_ >= pattern_.size())
        {
            return token(Token::End);
        }

        auto chr = pattern_[ind_of_pattern_++];
        switch (chr)
        {
        default:
            return token(Token::Normal, chr);

        case '.':
            return token(Token::Dot);

        case '\\':
        {
            auto chr = pattern_[ind_of_pattern_++];
            switch (chr)
            {
            /**
             * for predefined set
            */
            case 's': case 'd': case 'l': case 'u': case 'w':
            case 'S': case 'D': case 'L': case 'U': case 'W':
                return token(Token::Predef, chr);

            /**
             * for escape characters
            */
            case 'b':
                return token(Token::Escape, '\b');

            default:
                return token(Token::Escape, chr);
            }
        }

        case '*':
            return token(Token::Star);
        case '+':
            return token(Token::Plus);
        case '?':
            return token(Token::Ques);

        case '|':
            return token(Token::Or);
        case '^':
            return token(Token::Caret);
        case '(':
            return token(Token::LParen);
        case ')':
            return token(Token::RParen);
        case '[':
            return token(Token::LBracket);
        case ']':
            return token(Token::RBracket);
        }
    }

    std::unique_ptr<AST>
    gen_cat_expr()
    {
        auto lhs = gen_term();
        /**
         * that rhs is nullptr is all right
        */
        while (auto rhs = gen_term())
        {
            lhs = std::make_unique<CatExpr>(allocator_,
                std::move(lhs), std::move(rhs)
            );
        }

        /**
         * TODO: throw here
        */
        if (!lhs)
        {
            // ...
        }

        return lhs;
    }

    std::unique_ptr<AST>
    gen_select_expr()
    {
        auto lhs = gen_cat_expr();
        while (cur_tok_.type == Token::Or)
        {
            get_next_token();
            lhs = std::make_unique<SelectExpr>(allocator_,
                std::move(lhs), gen_cat_expr()
            );
        }
        return lhs;
    }

    std::unique_ptr<AST>
    gen_term()
    {
        std::unique_ptr<AST> term;
        switch (cur_tok_.type)
        {
        case Token::LParen:
            term = gen_sub_expr();
            break;

        case Token::LBracket:
            term = gen_range_expr();
            break;

        case Token::Normal:
        case Token::Escape:
            term = gen_single_char_expr();
            break;

        case Token::Dot:
            term = gen_dot_expr();
            break;

        case Token::Predef:
            term = gen_predef_expr();
            break;

        default:
            return nullptr;
        }

        for (bool cond = true; cond;)
        {
            switch (cur_tok_.type)
            {
            case Token::Star:
                term = std::make_unique<QualifierExpr>(
                    allocator_, move(term),
                    0, QualifierExpr::AtLeastNTimes
                );
                get_next_token();
                break;

            case Token::Plus:
                term = std::make_unique<QualifierExpr>(
                    allocator_, move(term),
                    1, QualifierExpr::AtLeastNTimes
                );
                get_next_token();
                break;

            case Token::Ques:
                term = std::make_unique<QualifierExpr>(
                    allocator_, move(term), 0, 1
                );
                get_next_token();
                break;

            default:
                cond = false;
                break;
            }
        }

    }

    std::unique_ptr<AST>
    gen_sub_expr()
    {
        // eat '('
        get_next_token();
        if (cur_tok_.type != Token::Ques)
        {
            auto sub_expr = std::make_unique<SubExpr>(allocator_,
                ind_of_subexpr_, std::to_string(ind_of_subexpr_), gen_select_expr()
            );
            ++ind_of_subexpr_;
            get_next_token();
            return sub_expr;
        }

        // eat '?'
        get_next_token();

        /**
         * ?: not ?\:
         *  use is_normal instead of making ':' reserved
        */
        if (cur_tok_.is_normal(':'))
        {
            // eat ':'
            get_next_token();
            auto sub_expr = gen_select_expr();
            get_next_token();
            return sub_expr;
        }
        // ?< not ?\<
        else if (cur_tok_.is_normal('<'))
        {
            // eat '<'
            get_next_token();
            std::string name;
            /**
             * only normal characters can occur in name
            */
            while (cur_tok_.type == Token::Normal
                and cur_tok_.value != '>')
            {
                name += cur_tok_.value;
                get_next_token();
            }

            if (cur_tok_.value != '>')
            {
                /**
                 * TODO: throw here
                */
            }

            // eat '>'
            get_next_token();
            if (cur_tok_.type == Token::RParen)
            {
                return std::make_unique<SubExpr>(allocator_,
                    ind_of_subexpr_++, std::move(name), nullptr
                );
            }
            else
            {
                return std::make_unique<SubExpr>(allocator_,
                    ind_of_subexpr_++, std::move(name), gen_select_expr()
                );
            }
        }
        else
        {
            /**
             * TODO: throw here
            */
        }
    }

    /**
     * in [...] no special characters
     *  except the first '^', '\' and '-'
    */
    std::unique_ptr<AST>
    gen_range_expr()
    {
        std::bitset<256> accept;
        bool exclude = false;

        // eat '['
        get_next_token();

        if (cur_tok_.type == Token::Caret)
        {
            exclude = true;
            // eat '^'
            get_next_token();
        }

        while (cur_tok_.type != Token::RBracket)
        {
            // meet \s ...
            if (cur_tok_.type == Token::Predef)
            {
                accept |= ECMAP.at(cur_tok_.value);
                get_next_token();
                continue;
            }

            if (cur_tok_.is_normal('-'))
            {
                /**
                 * TODO: throw here
                */
            }

            auto start = cur_tok_.value;
            get_next_token();
            if (cur_tok_.is_normal('-'))
            {
                get_next_token();
                if (cur_tok_.type == Token::Predef
                    || cur_tok_.type == Token::RBracket)
                {
                    /**
                     * TODO: throw here
                    */
                }

                auto end = cur_tok_.value;
                for (; start <= end; ++start)
                {
                    accept.set(start, true);
                }

                get_next_token();
            }
            else
            {
                accept.set(start, true);
            }
        }
        get_next_token();

        if (exclude)
        {
            accept.flip();
        }

        return std::make_unique<RangeExpr>(allocator_, accept);
    }

    std::unique_ptr<AST>
    gen_single_char_expr()
    {
        auto expr = std::make_unique<SingleCharExpr>(
            allocator_, cur_tok_.value
        );
        get_next_token();
        return expr;
    }

    std::unique_ptr<AST>
    gen_dot_expr()
    {
        get_next_token();
        return std::make_unique<DotExpr>(allocator_);
    }

    std::unique_ptr<AST>
    gen_predef_expr()
    {
        auto expr = std::make_unique<PredefExpr>(
            allocator_, cur_tok_.value
        );
        get_next_token();
        return expr;
    }

  private:
    Allocator &allocator_;
    const std::string &pattern_;
    std::size_t ind_of_pattern_;
    Token cur_tok_;
    std::size_t ind_of_subexpr_;
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
