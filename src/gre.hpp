#pragma once

#include <list>
#include <string>
#include <bitset>
#include <memory>
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
      : start(allocator.allocate())
      , end(allocator.allocate())
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

class AST
{
  public:
    AST(Allocator &allocator)
      : allocator_(allocator)
    {
        // ...
    }
    virtual ~ASR() = default;

    virtual Pair compile() = 0;

  protected:
    Allocator &allocator_;
};

class Parser
{
  public:
    Parser(Allocator &allocator)
      : allocator_(allocator)
    {
        // ...
    }

    std::unique_ptr<AST> parse(const std::string &pattern);

  private:
    Allocator &allocator_;
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
    Group() = default;

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
        auto ast = details::Parser(allocator_).parse(pattern_);
        entry_ = ast->compile().start;
    }

    /**
     * @options: unused now
    */
    GRE(std::string pattern, const Options &options)
      : pattern_(std::move(pattern))
    {
        auto ast = details::Parser(allocator_).parse(pattern_);
        entry_ = ast->compile().start;
    }

    const std::string &pattern() const
    {
        return pattern_;
    }

    std::optional<Group>
    match(const std::string &text);

  private:
    std::string pattern_;
    details::Allocator allocator_;
    details::State *entry_;
};
} // namespace gre;
