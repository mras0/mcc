#ifndef MCC_TOKEN_H
#define MCC_TOKEN_H

#include <iosfwd>
#include <string_view>
#include <variant>
#include <string>
#include <cassert>
#include "type.h"

namespace mcc {

#define TOK_KEWORDS(X) \
    X(auto)            \
    X(break)           \
    X(case)            \
    X(char)            \
    X(const)           \
    X(continue)        \
    X(default)         \
    X(do)              \
    X(double)          \
    X(else)            \
    X(enum)            \
    X(extern)          \
    X(float)           \
    X(for)             \
    X(goto)            \
    X(if)              \
    X(inline)          \
    X(int)             \
    X(long)            \
    X(register)        \
    X(restrict)        \
    X(return)          \
    X(short)           \
    X(signed)          \
    X(sizeof)          \
    X(static)          \
    X(struct)          \
    X(switch)          \
    X(typedef)         \
    X(union)           \
    X(unsigned)        \
    X(void)            \
    X(volatile)        \
    X(while)           \
    X(__attribute__)

#define TOK_OPERATORS(X)  \
    X("!"   , not_)       \
    X("!="  , noteq)      \
    X("%"   , mod)        \
    X("%="  , modeq)      \
    X("&"   , and_)       \
    X("&&"  , andand)     \
    X("&="  , andeq)      \
    X("("   , lparen)     \
    X(")"   , rparen)     \
    X("*"   , star)       \
    X("*="  , stareq)     \
    X("+"   , plus)       \
    X("++"  , plusplus)   \
    X("+="  , pluseq)     \
    X(","   , comma)      \
    X("-"   , minus)      \
    X("--"  , minusminus) \
    X("-="  , minuseq)    \
    X("->"  , arrow)      \
    X("."   , dot)        \
    X("..." , ellipsis)   \
    X("/"   , div)        \
    X("/="  , diveq)      \
    X(":"   , colon)      \
    X(";"   , semicolon)  \
    X("<"   , lt)         \
    X("<<"  , lshift)     \
    X("<<=" , lshifteq)   \
    X("<="  , lteq)       \
    X("="   , eq)         \
    X("=="  , eqeq)       \
    X(">"   , gt)         \
    X(">="  , gteq)       \
    X(">>"  , rshift)     \
    X(">>=" , rshifteq)   \
    X("?"   , question)   \
    X("["   , lbracket)   \
    X("]"   , rbracket)   \
    X("^"   , xor_)       \
    X("^="  , xoreq)      \
    X("{"   , lbrace)     \
    X("|"   , or_)        \
    X("|="  , oreq)       \
    X("||"  , oror)       \
    X("}"   , rbrace)     \
    X("~"   , bnot)

enum class token_type {
    eof,
    id,
    const_int,
    const_float,
    char_lit,
    string_lit,

#define DEF_TOK_KEYWORD(name) name ## _ ,
#define DEF_TOK_OPERATOR(sym, name) name,
    TOK_KEWORDS(DEF_TOK_KEYWORD)
    TOK_OPERATORS(DEF_TOK_OPERATOR)
#undef DEF_TOK_KEYWORD
#undef DEF_TOK_OPERATOR
};

std::ostream& operator<<(std::ostream& os, token_type tt);
token_type keyword_token_from_text(const std::string_view s);
token_type op_token_from(const std::string_view s);
int operator_precedence(token_type t);
bool is_assignment_op(token_type t);
token_type without_assignment(token_type t);

struct const_int_val {
    uint64_t val;
    ctype type;
};

std::ostream& operator<<(std::ostream& os, const_int_val civ);
const_int_val cast(const const_int_val& val, ctype new_type);

class token {
public:
    explicit token() : type_{token_type::eof}, value_{} {
    }
    explicit token(token_type tt) : type_{tt}, value_{} {
        assert(tt != token_type::id && tt != token_type::const_int && tt != token_type::const_float && tt != token_type::char_lit && tt != token_type::string_lit);
    }
    explicit token(token_type tt, const std::string& s) : type_{tt}, value_{s} {
        assert(tt == token_type::id || tt == token_type::string_lit);
    }
    explicit token(const const_int_val& v) : type_{token_type::const_int}, value_{v} {
    }
    explicit token(unsigned char_val) : type_{token_type::char_lit}, value_{char_val} {
    }
    explicit token(double val) : type_{token_type::const_float}, value_{val} {
    }

    token_type type() const { return type_; }

    const std::string& text() const {
        assert(type_ == token_type::id || type_ == token_type::string_lit);
        return std::get<std::string>(value_);
    }

    const_int_val int_val() const {
        assert(type_ == token_type::const_int);
        return std::get<const_int_val>(value_);
    }

    unsigned char_val() const {
        assert(type_ == token_type::char_lit);
        return std::get<unsigned>(value_);
    }

    double float_val() const {
        assert(type_ == token_type::const_float);
        return std::get<double>(value_);
    }

private:
    token_type type_;
    std::variant<std::monostate, std::string, const_int_val, unsigned, double> value_;
};

std::ostream& operator<<(std::ostream& os, const token& t);


inline bool is_storage_class_specifier(token_type t) {
    return t == token_type::typedef_
        || t == token_type::extern_
        || t == token_type::static_
        || t == token_type::auto_
        || t == token_type::register_;
}

inline bool is_simple_type_specifier(token_type t) {
    return t == token_type::void_
        || t == token_type::char_
        || t == token_type::short_
        || t == token_type::int_
        || t == token_type::long_
        || t == token_type::float_
        || t == token_type::double_
        || t == token_type::signed_
        || t == token_type::unsigned_;
}

inline bool is_type_qualifier(token_type t) {
    return t == token_type::const_
        || t == token_type::restrict_
        || t == token_type::volatile_;
}

inline bool is_literal(token_type t) {
    return t == token_type::const_int
        || t == token_type::const_float
        || t == token_type::char_lit
        || t == token_type::string_lit;
}

ctype ctype_from_storage_class_token(token_type t);
ctype ctype_from_type_qualifier_token(token_type t);

} // namespace mcc
#endif