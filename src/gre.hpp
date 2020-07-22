#pragma once

#include <string>

namespace gre
{
namespace details
{

} // namespace details

class Options
{
  public:
    Options() = default;
  private:
};

class GRE
{
  public:
    GRE(const std::string &pattern);
    GRE(const std::string &pattern, const Options &Options);

    const std::string &pattern() const;

    template<typename ...Args>
    static bool
    full_match(const std::string &text,
        const GRE &re, Args &...args);

  private:
};
} // namespace gre;
