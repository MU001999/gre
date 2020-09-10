#pragma once

#include <map>
#include <set>
#include <list>
#include <tuple>
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

    void record(const std::string &name, T *expr)
    {
        if (mapping_named_captures_.count(name))
        {
            throw;
        }
        mapping_named_captures_[name] = expr;
    }

    T *find(const std::string &name)
    {
        if (mapping_named_captures_.count(name))
        {
            return mapping_named_captures_[name];
        }
        return nullptr;
    }

  private:
    std::list<T *> allocated_;
    std::map<std::string, T *> mapping_named_captures_;
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
    Allocator<AST> &allocator4ast;
    std::size_t index;
    std::string name;
    AST *expr;

    CaptureExpr(Allocator<Node> &allocator,
        Allocator<AST> &allocator4ast,
        std::size_t index, std::string name, AST *expr)
      : AST(allocator), allocator4ast(allocator4ast)
      , index(index), name(std::move(name)), expr(expr)
    {
        // ...
    }

    Pair compile() override
    {
        Pair res(allocator_);

        auto expr = this->expr
                  ? this->expr->compile()
                  : allocator4ast.find(name)->compile();

        res.start->edge_type = Node::Epsilon;
        res.start->next1 = expr.start;
        expr.end->edge_type = Node::Epsilon;
        expr.end->next1 = res.end;

        res.start->state = Node::Begin;
        res.start->index = index;
        res.start->name = name;

        res.end->state = Node::End;
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
      , ind_of_subexpr_(1)
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

        if (!lhs)
        {
            throw;
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

        return term;
    }

    AST *gen_sub_expr()
    {
        // eat '('
        get_next_token();
        if (cur_tok_.type != Token::Ques)
        {
            /**
             * capture without name will has default name which is the literal of its index
            */
            auto name = std::to_string(ind_of_subexpr_);
            auto capture = allocator4ast_.allocate<CaptureExpr>(
                allocator_, allocator4ast_,
                ind_of_subexpr_, name, gen_select_expr()
            );
            allocator4ast_.record(name, capture);

            ++ind_of_subexpr_;
            get_next_token();
            return capture;
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
             *
             * TODO: enable (?<NUMERAL>) such as (?<1>)
             *  but not support (?<1>...) because this may influence
             *  other captures without name
            */
            while (cur_tok_.type == Token::Normal
                and cur_tok_.value != '>')
            {
                name += cur_tok_.value;
                get_next_token();
            }

            if (cur_tok_.value != '>')
            {
                throw;
            }

            // eat '>'
            get_next_token();
            if (cur_tok_.type == Token::RParen)
            {
                get_next_token();
                auto capture = allocator4ast_.allocate<CaptureExpr>(
                    allocator_, allocator4ast_,
                    ind_of_subexpr_++, std::move(name), nullptr
                );
                return capture;
            }
            else
            {
                auto capture = allocator4ast_.allocate<CaptureExpr>(
                    allocator_, allocator4ast_,
                    ind_of_subexpr_++, name, gen_select_expr()
                );
                allocator4ast_.record(name, capture);
                // eat ')'
                get_next_token();
                return capture;
            }
        }
        else
        {
            throw;
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
                throw;
            }

            auto start = cur_tok_.value;
            get_next_token();
            if (cur_tok_.is_normal('-'))
            {
                get_next_token();
                if (cur_tok_.type == Token::Predef
                    || cur_tok_.type == Token::RBracket)
                {
                    throw;
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

class Group;
class GroupRef
{
  public:
    GroupRef(Group &group)
      : group_(group)
    {
        // ...
    }

    GroupRef(const GroupRef &other)
      : group_(other.group_)
    {
        // ...
    }

    Group &get()
    {
        return group_;
    }

  private:
    Group &group_;
};
class GroupRefsIterator
{
    using RealIterator = std::vector<GroupRef>::iterator;

  public:
    GroupRefsIterator(RealIterator it)
      : it_(std::move(it))
    {
        // ...
    }

    Group &operator*()
    {
        return it_->get();
    }

    Group *operator->()
    {
        return &it_->get();
    }

    bool operator==(const GroupRefsIterator &rhs)
    {
        return it_ == rhs.it_;
    }

    bool operator!=(const GroupRefsIterator &rhs)
    {
        return it_ != rhs.it_;
    }

    GroupRefsIterator &operator++()
    {
        ++it_;
        return *this;
    }

  private:
    RealIterator it_;
};
class GroupRefs
{
  public:
    GroupRefs() = default;
    GroupRefs(const GroupRefs &other)
      : refs_(other.refs_)
    {
        // ...
    }

    void push_back(Group &group)
    {
        refs_.emplace_back(group);
    }

    Group &operator[](std::size_t i)
    {
        return refs_[i].get();
    }

    GroupRefsIterator begin()
    {
        return GroupRefsIterator(refs_.begin());
    }

    GroupRefsIterator end()
    {
        return GroupRefsIterator(refs_.end());
    }

    std::size_t size()
    {
        return refs_.size();
    }

  private:
    std::vector<GroupRef> refs_;
};

/**
 * "((pattern)*)"
 *  will generate Group{Group{}, ...}
*/
class Group
{
    friend class GRE;

  public:
    Group(std::size_t index,
        std::string name)
      : index_(index)
      , name_(std::move(name))
    {
        // ...
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

    /**
     * captures will return all captures
    */
    GroupRefs captures(std::size_t index)
    {
        if (cache4captures1_.count(index))
        {
            return cache4captures1_[index];
        }

        GroupRefs res;

        if (index_ == index + 1)
        {
            res.push_back(*this);
        }

        for (auto &sub : subs_)
        {
            for (auto &group : sub.captures(index))
            {
                res.push_back(group);
            }
        }

        return res;
    }

    GroupRefs captures(const std::string &name)
    {
        if (cache4captures2_.count(name))
        {
            return cache4captures2_[name];
        }

        GroupRefs res;

        if (name_ == name)
        {
            res.push_back(*this);
        }

        for (auto &sub : subs_)
        {
            for (auto &group : sub.captures(name))
            {
                res.push_back(group);
            }
        }

        return res;
    }

    /**
     * different from captures
     *  operator[] will only return groups non-nested
    */
    GroupRefs
    operator[](std::size_t index)
    {
        if (cache4astree1_.count(index))
        {
            return cache4astree1_[index];
        }

        GroupRefs res;

        for (auto &group : subs_)
        {
            if (group.index_ == index)
            {
                res.push_back(group);
            }
        }

        return res;
    }

    GroupRefs
    operator[](const std::string &name)
    {
        if (cache4astree2_.count(name))
        {
            return cache4astree2_[name];
        }

        GroupRefs res;

        for (auto &group : subs_)
        {
            if (group.name_ == name)
            {
                res.push_back(group);
            }
        }

        return res;
    }

    bool operator==(const std::string &rhs) const
    {
        return self_ == rhs;
    }

    operator std::string() const
    {
        return self_;
    }

  private:
    std::size_t index_;
    std::string name_;
    std::string self_;
    std::vector<Group> subs_;

    // caches
    std::map<std::size_t, GroupRefs>
    cache4astree1_; // when calling [] with std::size_t
    std::map<std::string, GroupRefs>
    cache4astree2_; // when calling [] with std::string
    std::map<std::size_t, GroupRefs>
    cache4captures1_; // when calling captures with std::size_t
    std::map<std::string, GroupRefs>
    cache4captures2_; // when calling captures with std::string
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
        /**
         * find the pleased path by bfs
         *  and then construct group from the path
        */

        struct Chain
        {
            /**
             * pre->node points to the node which decides the path
            */
            Chain *pre;
            details::Node *node;
            std::size_t pos;

            Chain(Chain *pre, details::Node *node, std::size_t pos)
              : pre(pre), node(node), pos(pos)
            {
                // ...
            }
        };

        details::Allocator<Chain> allocator4chain;

        Chain *result = nullptr;
        std::set<Chain *> current{allocator4chain.allocate(nullptr, start_, 0)};

        std::set<std::tuple<details::Node *, details::Node *, std::size_t>> meeted_paths;

        /**
         * simple bfs to find path
        */
        while (!current.empty())
        {
            std::set<Chain *> temp;

            for (auto chain : current)
            {
                /**
                 * make sure that each cycle eats one input
                */

                auto node = chain->node;
                auto pos = chain->pos;

                for (bool cond = true; cond;)
                {
                    switch (node->edge_type)
                    {
                    case details::Node::Epsilon:
                    {
                        if (!node->next2)
                        {
                            node = node->next1;
                        }
                        else
                        {
                            /**
                             * for (expr*)*
                             *  consider the next1 to expr's start
                             *  discard this chain if the cycle has beed traveled once
                            */
                            auto path = std::make_tuple(chain->node, node, pos);
                            if (!meeted_paths.count(path))
                            {
                                temp.insert(allocator4chain.allocate(chain, node->next1, pos));
                                temp.insert(allocator4chain.allocate(chain, node->next2, pos));

                                meeted_paths.insert(path);
                            }
                            cond = false;
                        }
                    }
                        break;
                    case details::Node::CharSet:
                    {
                        if (pos >= text.size())
                        {
                            cond = false;
                        }
                        else
                        {
                            const auto input = text[pos];
                            if (node->accept[input])
                            {
                                ++pos;
                                node = node->next1;
                            }
                            else
                            {
                                cond = false;
                            }
                        }
                    }
                        break;
                    case details::Node::Empty:
                        if (!result || result->pos < pos)
                        {
                            result = allocator4chain.allocate(chain, node, pos);
                        }
                        cond = false;
                        break;
                    }
                }
            }

            current.swap(temp);
        }

        if (!result)
        {
            return std::optional<Group>();
        }

        std::list<details::Node *> trunks;
        while (result)
        {
            trunks.push_front(result->node);
            result = result->pre;
        }
        // pop start
        trunks.pop_front();

        Group res_group(0, pattern_);
        std::list<Group *> groups_chain{&res_group};

        auto node = start_;
        std::size_t pos = 0;
        while (!trunks.empty())
        {
            while (node->next1 && !node->next2)
            {
                if (node->state == details::Node::Begin)
                {
                    groups_chain.back()->subs_.emplace_back(node->index, node->name);
                    groups_chain.push_back(&groups_chain.back()->subs_.back());
                }
                else if (node->state == details::Node::End)
                {
                    groups_chain.pop_back();
                }

                if (node->edge_type == details::Node::CharSet)
                {
                    const auto input = text[pos++];
                    for (auto group : groups_chain)
                    {
                        group->self_ += input;
                    }
                }

                node = node->next1;
            }

            if (node->state == details::Node::Begin)
            {
                groups_chain.back()->subs_.emplace_back(node->index, node->name);
                groups_chain.push_back(&groups_chain.back()->subs_.back());
            }
            else if (node->state == details::Node::End)
            {
                groups_chain.pop_back();
            }

            node = trunks.front();
            trunks.pop_front();
        }

        return res_group;
    }

  private:
    std::string pattern_;
    details::Allocator<details::Node> allocator_;
    details::Node *start_;
    details::Node *end_;
};
} // namespace gre
