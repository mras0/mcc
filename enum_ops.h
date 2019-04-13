#ifndef MCC_ENUM_OPS_H
#define MCC_ENUM_OPS_H

#include <type_traits>

#define ENUM_BIT_OPS(T) \
constexpr T& operator|=(T& t, T q) { \
    return t = static_cast<T>(static_cast<std::underlying_type_t<T>>(t)|static_cast<std::underlying_type_t<T>>(q)); \
} \
constexpr T& operator&=(T& t, T q) { \
    return t = static_cast<T>(static_cast<std::underlying_type_t<T>>(t)&static_cast<std::underlying_type_t<T>>(q)); \
} \
constexpr T& operator^=(T& t, T q) { \
    return t = static_cast<T>(static_cast<std::underlying_type_t<T>>(t)^static_cast<std::underlying_type_t<T>>(q)); \
} \
constexpr T operator|(T l, T r) {\
    return l |= r;\
}\
constexpr T operator&(T l, T r) {\
    return l &= r;\
}\
constexpr T operator^(T l, T r) {\
    return l ^= r;\
}\
constexpr T operator~(T x) {\
    return static_cast<T>(~static_cast<std::underlying_type_t<T>>(x));\
}\
constexpr bool operator!(T x) {\
    return !static_cast<std::underlying_type_t<T>>(x);\
}

#endif