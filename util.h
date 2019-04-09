#ifndef MCC_UTIL_H
#define MCC_UTIL_H

#include <string>
#include <string_view>
#include <sstream>
#include <stdexcept>
#include <cassert>

#define NOT_IMPLEMENTED(stuff) do { std::ostringstream oss_; oss_ << __FILE__ << ":" << __LINE__ << ": " << __func__ << " Not implemented: " << stuff; throw std::runtime_error(oss_.str()); } while (0)

#define ENUM_BIT_OPS(T) \
T& operator|=(T& t, T q) { \
    return t = static_cast<T>(static_cast<std::underlying_type_t<T>>(t)|static_cast<std::underlying_type_t<T>>(q)); \
} \
T& operator&=(T& t, T q) { \
    return t = static_cast<T>(static_cast<std::underlying_type_t<T>>(t)&static_cast<std::underlying_type_t<T>>(q)); \
} \
T operator|(T l, T r) {\
    return l |= r;\
}\
T operator&(T l, T r) {\
    return l &= r;\
}\
T operator~(T x) {\
    return static_cast<T>(~static_cast<std::underlying_type_t<T>>(x));\
}\
bool operator!(T x) {\
    return !static_cast<std::underlying_type_t<T>>(x);\
}



namespace mcc {

std::string quoted(std::string_view s);
std::pair<unsigned, size_t> unescape_char(const std::string_view s);
std::string unescape(const std::string_view s);

}

#endif