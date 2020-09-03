#pragma once

#include <map>
#include <set>
#include <list>
#include <string>
#include <bitset>
#include <memory>
#include <vector>
#include <cctype>
#include <utility>
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

struct Node
{
    // for common data
    enum EdgeType
    {
        Epsilon,
        CharSet,
        Empty,
    };

    EdgeType edge_type;
    std::bitset<256> accept;
    Node *next1;
    // next2 will be not nullptr only when edge_type is Epsilon
    Node *next2;

    // for capture-expr
    enum CaptureState
    {
        NoGo,
        Begin,
        End,
    };

    CaptureState state;
    std::size_t index;
    std::string name;

    Node()
      : edge_type(Empty)
      , next1(nullptr)
      , next2(nullptr)
      , state(NoGo)
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
    Node *start;
    Node *end;

  public:
    Pair(Allocator<Node> &allocator)
      : start(allocator.allocate())
      , end(allocator.allocate())
    {
        // ...
    }

    Pair(Node *start, Node *end)
      : start(start), end(end)
    {
        // ...
    }
};

struct AST
{
    AST(Allocator<Node> &allocator)
      : allocator_(allocator)
    {
        // ...
    }
    virtual ~AST() = default;

    virtual Pair compile() = 0;

  protected:
    Allocator<Node> &allocator_;
};

struct SingleCharExpr : AST
{
    char chr;

    SingleCharExpr(Allocator<Node> &allocator, char chr)
      : AST(allocator), chr(chr)
    {
        // ...
    }

    Pair compile() override
    {
        Pair res(allocator_);

        res.start->edge_type = Node::CharSet;
        res.start->next1 = res.end;
        res.start->accept.set(chr);

        return res;
    }
};

struct CatExpr : AST
{
    AST *lhs, *rhs;

    CatExpr(Allocator<Node> &allocator, AST *lhs, AST *rhs)
      : AST(allocator), lhs(lhs), rhs(rhs)
    {
        // ...
    }

    Pair compile() override
    {
        auto lhs = this->lhs->compile();
        auto rhs = this->rhs->compile();

        Pair res(lhs.start, rhs.end);

        lhs.end->edge_type = Node::Epsilon;
        lhs.end->next1 = rhs.start;

        return res;
    }
};

struct SelectExpr : AST
{
    AST *lhs, *rhs;

    SelectExpr(Allocator<Node> &allocator, AST *lhs, AST *rhs)
      : AST(allocator), lhs(lhs), rhs(rhs)
    {
        // ...
    }

    Pair compile() override
    {
        auto lhs = this->lhs->compile();
        auto rhs = this->rhs->compile();

        Pair res(allocator_);

        res.start->edge_type = Node::Epsilon;
        res.start->next1 = lhs.start;
        res.start->next2 = rhs.start;

        lhs.end->edge_type = Node::Epsilon;
        lhs.end->next1 = res.end;
        rhs.end->edge_type = Node::Epsilon;
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

    QualifierExpr(Allocator<Node> &allocator, AST *expr,
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

                res.start->edge_type = Node::Epsilon;
                res.start->next1 = res.end;

                return res;
            }

            // assert n > 0
            Allocator<AST> temp_allocator;
            auto ast = expr;
            for (int i = 1; i < n; ++i)
            {
                ast = temp_allocator.allocate<CatExpr>(
                    allocator_, ast, expr
                );
            }
            return ast->compile();
        }
        // {n, }
        else if (m == AtLeastNTimes)
        {
            if (n == 0)
            {
                auto expr = this->expr->compile();

                Pair res(allocator_);

                res.start->edge_type = Node::Epsilon;
                res.start->next1 = expr.start;
                res.start->next2 = res.end;

                expr.end->edge_type = Node::Epsilon;
                expr.end->next1 = expr.start;
                expr.end->next2 = res.end;

                return res;
            }

            // assert n > 0
            Allocator<AST> temp_allocator;
            return temp_allocator.allocate<CatExpr>(allocator_,
                temp_allocator.allocate<QualifierExpr>(
                    allocator_, expr, n, NTimes),
                temp_allocator.allocate<QualifierExpr>(
                    allocator_, expr, 0, AtLeastNTimes)
            )->compile();
        }
        // {n, m}
        else
        {
            // assert n < m and n >= 0
            Allocator<AST> temp_allocator;
            auto ast = temp_allocator.allocate<QualifierExpr>(
                allocator_, expr, n, NTimes
            );
            for (int i = n + 1; i <= m; ++i)
            {
                ast = temp_allocator.allocate<SelectExpr>(
                    allocator_, ast,
                    temp_allocator.allocate<QualifierExpr>(
                        allocator_, expr, i, NTimes)
                );
            }
            return ast->compile();
        }
    }
};

struct RangeExpr : AST
{
    std::bitset<256> accept;

    RangeExpr(Allocator<Node> &allocator,
        std::bitset<256> accept)
      : AST(allocator), accept(accept)
    {
        // ...
    }

    Pair compile() override
    {
        Pair res(allocator_);

        res.start->edge_type = Node::CharSet;
        res.start->next1 = res.end;
        res.start->accept = accept;

        return res;
    }
};

struct CaptureExpr : AST
{
    std::size_t index;
    std::string name;
    AST *expr;

    CaptureExpr(Allocator<Node> &allocator,
        std::size_t index, std::string name, AST *expr)
      : AST(allocator), index(index), name(std::move(name)), expr(expr)
    {
        // ...
    }

    Pair compile() override
    {
        auto res = expr->compile();

        res.start->state = Node::Begin;
        res.start->index = index;
        res.start->name = name;

        res.end->state = Node::Begin;
        res.end->index = index;
        res.end->name = name;

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
        LBrace,
        RBrace,

        End,
    };

    Type type;
    char value;

    Token() = default;

    bool is_normal(char val)
    {
        return type == Normal && value == val;
    }

    bool is_digit()
    {
        return type == Normal && std::isdigit(value);
    }
};

class Parser
{
  public:
    Parser(Allocator<Node> &allocator,
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

    std::pair<int, int> gen_qualifier()
    {
        // eat '{'
        get_next_token();

        int n, m;

        if (cur_tok_.is_digit())
        {
            int n = cur_tok_.value - '0';
            get_next_token();

            while (cur_tok_.is_digit())
            {
                n = n * 10 + cur_tok_.value - '0';
                get_next_token();
            }

            if (cur_tok_.is_normal(','))
            {
                // eat ','
                get_next_token();

                // {n,m}
                if (cur_tok_.is_digit())
                {
                    m = cur_tok_.value - '0';
                    get_next_token();

                    while (cur_tok_.is_digit())
                    {
                        m = m * 10 + cur_tok_.value - '0';
                        get_next_token();
                    }

                    if (cur_tok_.type != Token::RBrace)
                    {
                        throw;
                    }

                    // eat '}'
                    get_next_token();
                    return {n, m};
                }
                // {n,}
                else if (cur_tok_.type == Token::RBrace)
                {
                    get_next_token();
                    return {n, QualifierExpr::AtLeastNTimes};
                }
                else
                {
                    throw;
                }
            }
            // {n}
            else if (cur_tok_.type == Token::RBrace)
            {
                get_next_token();
                return {n, QualifierExpr::NTimes};
            }
            else
            {
                throw;
            }
        }
        else
        {
            throw;
        }
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

            case Token::LBrace:
            {
                auto qualifier = gen_qualifier();
                term = allocator4ast_.allocate<QualifierExpr>(
                    allocator_, term, qualifier.first, qualifier.second
                );
            }
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
            auto sub_expr = allocator4ast_.allocate<CaptureExpr>(allocator_,
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
                return allocator4ast_.allocate<CaptureExpr>(allocator_,
                    ind_of_subexpr_++, std::move(name), nullptr
                );
            }
            else
            {
                return allocator4ast_.allocate<CaptureExpr>(allocator_,
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
        return allocator4ast_.allocate<RangeExpr>(
            allocator_, accept.flip()
        );
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
    Allocator<Node> &allocator_;
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

    Group &operator[](std::size_t i)
    {
        return subs_[i];
    }

    std::vector<std::string>
    operator[](const std::string &name)
    {
        std::vector<std::string> res;

        if (name_ == name)
        {
            res.push_back(self_);
        }

        for (auto &group : subs_)
        {
            auto temp = group[name];
            for (auto &str : temp)
            {
                res.push_back(std::move(str));
            }
        }

        return res;
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
        auto node = ast->compile();
        start_ = node.start; end_ = node.end;
    }

    /**
     * @options: unused now
    */
    GRE(std::string pattern, const Options &options)
      : pattern_(std::move(pattern))
    {
        details::Allocator<details::AST> allocator4ast;
        auto ast = details::Parser(allocator_, allocator4ast, pattern_).parse();
        auto node = ast->compile();
        start_ = node.start; end_ = node.end;
    }

    const std::string &pattern() const
    {
        return pattern_;
    }

    std::optional<Group>
    match(const std::string &text) const
    {
        Group res(pattern_);

        if (match_impl(text, 0, res))
        {
            return res;
        }

        return std::optional<Group>();
    }

  private:
    std::optional<Group> match_impl(const std::string &text,
        std::size_t i, Group &group) const
    {
        return std::optional<Group>();
    }

  private:
    std::string pattern_;
    details::Allocator<details::Node> allocator_;
    details::Node *start_;
    details::Node *end_;
};
} // namespace gre;
