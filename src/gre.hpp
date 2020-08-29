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
// ---------------------------------------*************************\******************\\\\\*********-----
// ---------------------------------------************************* ******************rfvnt*********-----
inline const std::bitset<256> theSpaces(0b0000000000000000000000000100000000000000000011111000000000ULL);
// ---------------------------------------9876543210                                                -----
inline const std::bitset<256> theDigits(0b1111111111000000000000000000000000000000000000000000000000ULL);
// --------------------------------------zyxwvutsrqponmlkjihgfedcba                                                                                                 ---
inline const std::bitset<256> theLWords("111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
// --------------------------------------                                ZYXWVUTSRQPONMLKJIHGFEDCBA                                                                 ---
inline const std::bitset<256> theUWords("000000000000000000000000000000001111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000");
// --------------------------------------zyxwvutsrqponmlkjihgfedcba _    ZYXWVUTSRQPONMLKJIHGFEDCBA       9876543210                                                ---
inline const std::bitset<256> theWord_s("111111111111111111111111110100001111111111111111111111111100000001111111111000000000000000000000000000000000000000000000000");

inline const std::bitset<256> theNonSpaces = ~theSpaces;
inline const std::bitset<256> theNonDigits = ~theDigits;
inline const std::bitset<256> theNonLWords = ~theLWords;
inline const std::bitset<256> theNonUWords = ~theUWords;
inline const std::bitset<256> theNonWord_s = ~theWord_s;

inline const std::map<char, const std::bitset<256> &> thePredefMap
{
    { 's', theSpaces }, { 'S', theNonSpaces },
    { 'd', theDigits }, { 'D', theNonDigits },
    { 'l', theLWords }, { 'L', theNonLWords },
    { 'u', theUWords }, { 'U', theNonUWords },
    { 'w', theWord_s }, { 'W', theNonWord_s },
};

struct State
{
    enum EdgeType
    {
        EPSILON,
        CCL,
        EMPTY,
    };

    // common data
    EdgeType edge_type;
    std::bitset<256> accept;
    State *next1;
    State *next2;

    // special for sub-expr
    bool is_sub_expr;
    std::size_t index;
    std::string name;

    State()
      : edge_type(EMPTY)
      , next1(nullptr)
      , next2(nullptr)
      , is_sub_expr(false)
    {
        // ...
    }
};

template<typename T>
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

    void deallocate()
    {
        for (auto ptr : allocated_)
        {
            delete ptr;
        }
        allocated_.clear();
    }

    template<typename R = T, typename ...Args>
    T *allocate(Args &&...args)
    {
        auto ptr = new R(std::forward<Args>(args)...);
        allocated_.push_back(ptr);
        return ptr;
    }

  private:
    std::list<T *> allocated_;
};

struct Pair
{
    State *start;
    State *end;

  public:
    Pair(Allocator<State> &allocator)
      : start(allocator.allocate())
      , end(allocator.allocate())
    {
        // ...
    }

    Pair(State *start, State *end)
      : start(start), end(end)
    {
        // ...
    }
};

struct AST
{
    AST(Allocator<State> &allocator)
      : allocator_(allocator)
    {
        // ...
    }
    virtual ~AST() = default;

    virtual Pair compile() = 0;

  protected:
    Allocator<State> &allocator_;
};

struct SingleCharExpr : AST
{
    char chr;

    SingleCharExpr(Allocator<State> &allocator, char chr)
      : AST(allocator), chr(chr)
    {
        // ...
    }

    Pair compile() override
    {
        Pair res(allocator_);

        res.start->edge_type = State::CCL;
        res.start->next1 = res.end;
        res.start->accept.set(chr);

        return res;
    }
};

struct CatExpr : AST
{
    AST *lhs, *rhs;

    CatExpr(Allocator<State> &allocator, AST *lhs, AST *rhs)
      : AST(allocator), lhs(lhs), rhs(rhs)
    {
        // ...
    }

    Pair compile() override
    {
        auto lhs = this->lhs->compile();
        auto rhs = this->rhs->compile();

        Pair res(lhs.start, rhs.end);

        lhs.end->edge_type = State::EPSILON;
        lhs.end->next1 = rhs.start;

        return res;
    }
};

struct SelectExpr : AST
{
    AST *lhs, *rhs;

    SelectExpr(Allocator<State> &allocator, AST *lhs, AST *rhs)
      : AST(allocator), lhs(lhs), rhs(rhs)
    {
        // ...
    }

    Pair compile() override
    {
        auto lhs = this->lhs->compile();
        auto rhs = this->rhs->compile();

        Pair res(allocator_);

        res.start->edge_type = State::EPSILON;
        res.start->next1 = lhs.start;
        res.start->next2 = rhs.start;

        lhs.end->edge_type = State::EPSILON;
        lhs.end->next1 = res.end;
        rhs.end->edge_type = State::EPSILON;
        rhs.end->next1 = res.end;

        return res;
    }
};

struct QualifierExpr : AST
{
    enum Mode : int
    {
        NTimes        = -1,
        AtLeastNTimes = -2,
    };

    AST *expr;
    int n, m;

    QualifierExpr(Allocator<State> &allocator, AST *expr,
        int n, int m = Mode::NTimes)
      : AST(allocator), expr(expr), n(n), m(m)
    {
        // ...
    }

    Pair compile() override
    {
        // {n}
        if (m == NTimes)
        {
            // check of n < 0 is in parsing
            if (n == 0)
            {
                Pair res(allocator_);

                res.start->edge_type = State::EPSILON;
                res.start->next1 = res.end;

                return res;
            }

            // assert n > 0
            Allocator<AST> temp_allocator;
            auto ast = expr;
            for (int i = 1; i < n; ++i)
            {
                ast = temp_allocator.allocate<CatExpr>(allocator_, ast, expr);
            }
            return ast->compile();
        }
    }
};

struct RangeExpr : AST
{
    std::bitset<256> accept;

    RangeExpr(Allocator<State> &allocator,
        std::bitset<256> accept)
      : AST(allocator), accept(accept)
    {
        // ...
    }

    Pair compile() override
    {
        Pair res(allocator_);

        res.start->edge_type = State::CCL;
        res.start->next1 = res.end;
        res.start->accept = accept;

        return res;
    }
};

struct SubExpr : AST
{
    std::size_t index;
    std::string name;
    AST *expr;

    SubExpr(Allocator<State> &allocator,
        std::size_t index, std::string name, AST *expr)
      : AST(allocator), index(index), name(std::move(name)), expr(expr)
    {
        // ...
    }

    Pair compile() override
    {
        auto res = expr->compile();

        res.start->is_sub_expr = true;
        res.start->index = index;
        res.start->name = name;

        return res;
    }
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
    Parser(Allocator<State> &allocator,
        Allocator<AST> &allocator4ast,
        const std::string &pattern)
      : allocator_(allocator)
      , allocator4ast_(allocator4ast)
      , pattern_(pattern)
      , ind_of_pattern_(0)
      , ind_of_subexpr_(0)
    {
        get_next_token();
    }

    AST *parse()
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

    AST *gen_cat_expr()
    {
        auto lhs = gen_term();
        /**
         * that rhs is nullptr is all right
        */
        while (auto rhs = gen_term())
        {
            lhs = allocator4ast_.allocate<CatExpr>(
                allocator_, lhs, rhs
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

    AST *gen_select_expr()
    {
        auto lhs = gen_cat_expr();
        while (cur_tok_.type == Token::Or)
        {
            get_next_token();
            lhs = allocator4ast_.allocate<SelectExpr>(
                allocator_, lhs, gen_cat_expr()
            );
        }
        return lhs;
    }

    AST *gen_term()
    {
        AST *term;
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
                term = allocator4ast_.allocate<QualifierExpr>(
                    allocator_, term,
                    0, QualifierExpr::AtLeastNTimes
                );
                get_next_token();
                break;

            case Token::Plus:
                term = allocator4ast_.allocate<QualifierExpr>(
                    allocator_, term,
                    1, QualifierExpr::AtLeastNTimes
                );
                get_next_token();
                break;

            case Token::Ques:
                term = allocator4ast_.allocate<QualifierExpr>(
                    allocator_, term, 0, 1
                );
                get_next_token();
                break;

            default:
                cond = false;
                break;
            }
        }

    }

    AST *gen_sub_expr()
    {
        // eat '('
        get_next_token();
        if (cur_tok_.type != Token::Ques)
        {
            auto sub_expr = allocator4ast_.allocate<SubExpr>(allocator_,
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
                return allocator4ast_.allocate<SubExpr>(allocator_,
                    ind_of_subexpr_++, std::move(name), nullptr
                );
            }
            else
            {
                return allocator4ast_.allocate<SubExpr>(allocator_,
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
    AST *gen_range_expr()
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
                accept |= thePredefMap.at(cur_tok_.value);
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
                    accept.set(start);
                }

                get_next_token();
            }
            else
            {
                accept.set(start);
            }
        }
        get_next_token();

        return allocator4ast_.allocate<RangeExpr>(
            allocator_, exclude ? accept.flip() : accept
        );
    }

    AST *gen_single_char_expr()
    {
        auto expr = allocator4ast_.allocate<SingleCharExpr>(
            allocator_, cur_tok_.value
        );
        get_next_token();
        return expr;
    }

    AST *gen_dot_expr()
    {
        get_next_token();
        std::bitset<256> accept(1024UL);
        return allocator4ast_.allocate<RangeExpr>(allocator_, accept.flip());
    }

    AST *gen_predef_expr()
    {
        auto expr = allocator4ast_.allocate<RangeExpr>(
            allocator_, thePredefMap.at(cur_tok_.value)
        );
        get_next_token();
        return expr;
    }

  private:
    Allocator<State> &allocator_;
    Allocator<AST> &allocator4ast_;

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
        details::Allocator<details::AST> allocator4ast;
        auto ast = details::Parser(allocator_, allocator4ast, pattern_).parse();
        entry_ = ast->compile().start;
    }

    /**
     * @options: unused now
    */
    GRE(std::string pattern, const Options &options)
      : pattern_(std::move(pattern))
    {
        details::Allocator<details::AST> allocator4ast;
        auto ast = details::Parser(allocator_, allocator4ast, pattern_).parse();
        entry_ = ast->compile().start;
    }

    const std::string &pattern() const
    {
        return pattern_;
    }

    std::optional<Group>
    match(const std::string &text) const;

  private:
    std::string pattern_;
    details::Allocator<details::State> allocator_;
    details::State *entry_;
};
} // namespace gre;
