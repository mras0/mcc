#ifndef MCC_UTIL_H
#define MCC_UTIL_H

#include <string>
#include <string_view>
#include <sstream>
#include <stdexcept>
#include <cassert>

#define NOT_IMPLEMENTED(stuff) do { std::ostringstream oss_; oss_ << __FILE__ << ":" << __LINE__ << ": " << __func__ << " Not implemented: " << stuff; throw std::runtime_error(oss_.str()); } while (0)

namespace mcc {

std::string quoted(std::string_view s);
std::pair<unsigned, size_t> unescape_char(const std::string_view s);
std::string unescape(const std::string_view s);

}

#endif