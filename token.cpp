#include "token.h"
#include "util.h"
#include <ostream>

namespace mcc {

std::ostream& operator<<(std::ostream& os, token_type tt) {
    switch (tt) {
    case token_type::eof:           return os << "eof";
    case token_type::id:            return os << "id";
    case token_type::const_int:     return os << "const_int";
    case token_type::const_float:   return os << "const_float";
    case token_type::char_lit:      return os << "char_lit";
    case token_type::string_lit:    return os << "string_lit";
#define CASE_KEYWORD(name) case token_type::name##_: return os << #name;
#define CASE_OP(sym, name) case token_type::name:    return os << sym;
    TOK_KEWORDS(CASE_KEYWORD)
    TOK_OPERATORS(CASE_OP)
#undef CASE_KEYWORD
#undef CASE_OP
    }
    NOT_IMPLEMENTED("token_type " << (int)tt);
}

token_type keyword_token_from_text(const std::string_view s) {
#define X(name) if (s == #name) return token_type::name##_;
    TOK_KEWORDS(X)
#undef X
    return token_type::eof;
}

token_type op_token_from(const std::string_view s) {
#define X(sym, name) if (s == sym) return token_type::name;
    TOK_OPERATORS(X)
#undef X
    return token_type::eof;
}

int operator_precedence(token_type t) {
    switch (t) {
    case token_type::star:
    case token_type::div:
    case token_type::mod:
        return 5;
    case token_type::plus:
    case token_type::minus:
        return 6;
    case token_type::lshift:
    case token_type::rshift:
        return 7;
    case token_type::lt:
    case token_type::lteq:
    case token_type::gteq:
    case token_type::gt:
        return 9;
    case token_type::eqeq:
    case token_type::noteq:
        return 10;
    case token_type::and_:
        return 11;
    case token_type::xor_:
        return 12;
    case token_type::or_:
        return 13;
    case token_type::andand:
        return 14;
    case token_type::oror:
        return 15;
    case token_type::question:
    case token_type::eq:
    case token_type::pluseq:
    case token_type::minuseq:
    case token_type::stareq:
    case token_type::diveq:
    case token_type::modeq:
    case token_type::lshifteq:
    case token_type::rshifteq:
    case token_type::andeq:
    case token_type::xoreq:
    case token_type::oreq:
        return 16;
    case token_type::comma:
        return 17;
    default:
        return 1000;
    }
}

std::ostream& operator<<(std::ostream& os, const_int_val civ) {
    os << civ.val;
    if (!!(civ.type & ctype::unsigned_f)) {
        os << 'U';
    }
    if (base_type(civ.type) == ctype::long_t) {
        os << 'L';
    } else if (base_type(civ.type) == ctype::long_long_t) {
        os << "LL";
    }
    return os;
}

const_int_val cast(const const_int_val& val, ctype new_type) {
    if (val.type == new_type) {
        return val;
    }
    NOT_IMPLEMENTED(val << " " << new_type);
}

std::ostream& operator<<(std::ostream& os, const token& t) {
    switch (t.type()) {
    case token_type::id: return os << "id:" << t.text();
    case token_type::const_int: return os << "int:" << t.int_val();
    case token_type::const_float: return os << "float:" << t.float_val();
    case token_type::char_lit: return os << "char:" << quoted(std::string(1, (char)t.char_val()));
    case token_type::string_lit: return os << "str:" << quoted(t.text()) << "\n";
    default:
        return os << t.type();
    }
}

ctype ctype_from_storage_class_token(token_type t) {
    switch (t) {
    case token_type::typedef_:  return ctype::typedef_f;
    case token_type::extern_:   return ctype::extern_f;
    case token_type::static_:   return ctype::static_f;
    case token_type::auto_:     return ctype::none;
    case token_type::register_: return ctype::register_f;
    default: NOT_IMPLEMENTED(t);
    }
}

ctype ctype_from_type_qualifier_token(token_type t) {
    switch (t) {
    case token_type::const_:     return ctype::const_f;
    case token_type::restrict_:  return ctype::restrict_f;
    case token_type::volatile_:  return ctype::volatile_f;
    default: NOT_IMPLEMENTED(t);
    }
}

} // namespace mcc