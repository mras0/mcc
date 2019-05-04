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

bool is_right_associative(token_type t) {
    // HACK
    return operator_precedence(t) == operator_precedence(token_type::question);
}

bool is_assignment_op(token_type t) {
    switch (t) {
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
        return true;
    default:
        return false;
    }
}

token_type without_assignment(token_type t) {
    assert(is_assignment_op(t) && t != token_type::eq);
    switch (t) {
    case token_type::pluseq:     return token_type::plus;
    case token_type::minuseq:    return token_type::minus;
    case token_type::stareq:     return token_type::star;
    case token_type::diveq:      return token_type::div;
    case token_type::modeq:      return token_type::mod;
    case token_type::lshifteq:   return token_type::lshift;
    case token_type::rshifteq:   return token_type::rshift;
    case token_type::andeq:      return token_type::and_;  
    case token_type::xoreq:      return token_type::xor_;
    case token_type::oreq:       return token_type::or_;    
    default:
        NOT_IMPLEMENTED(t);
    }
}

bool is_comparison_op(token_type op) {
    return op == token_type::lt
        || op == token_type::lteq
        || op == token_type::eqeq
        || op == token_type::noteq
        || op == token_type::gt
        || op == token_type::gteq;
}

template<typename T>
void print_as(std::ostream& os, uint64_t v) {
    if constexpr (sizeof(T) == 1) {
        os << static_cast<int>(static_cast<T>(v));
    } else {
        os << static_cast<T>(v);
    }
}

std::ostream& operator<<(std::ostream& os, const_int_val civ) {
    switch (static_cast<uint32_t>(civ.type)) {
#define CT(ct, rt) case static_cast<uint32_t>(ctype::ct): print_as<signed rt>(os, civ.val); break; \
                   case static_cast<uint32_t>(ctype::ct|ctype::unsigned_f): print_as<unsigned rt>(os, civ.val); break
        CT(char_t, char);
        CT(plain_char_t, char);
        CT(short_t, short);
        CT(int_t, int);
        CT(long_t, int);
        CT(long_long_t, long long);
#undef CT
    default:
        NOT_IMPLEMENTED(civ.type);
    }
    return os;
}

template<typename T>
const_int_val chop(uint64_t v, ctype new_type) {
    if (!(new_type & ctype::unsigned_f)) {
        v = static_cast<uint64_t>(static_cast<int64_t>(static_cast<std::make_signed_t<T>>(static_cast<T>(v))));
    } else {
        v = static_cast<T>(v);
    }
    return const_int_val{v, new_type};
}

const_int_val cast(const const_int_val& val, ctype new_type) {
    if (!!(new_type & ctype::storage_f)) {
        NOT_IMPLEMENTED(new_type);
    }
    if (val.type == new_type) {
        return val;
    }
    if (!is_integral(val.type) || !is_integral(new_type)) {
        NOT_IMPLEMENTED(val << " " << new_type);
    }
    const_int_val res{val.val, new_type};
    if (base_type(val.type) > base_type(new_type)) {
        switch (base_type(new_type)) {
        case ctype::char_t:
        case ctype::plain_char_t:
            return chop<uint8_t>(val.val, new_type);
        case ctype::short_t:
            return chop<uint16_t>(val.val, new_type);
        case ctype::int_t:
        case ctype::long_t:
            return chop<uint32_t>(val.val, new_type);
        default:
            NOT_IMPLEMENTED(val << " " << new_type);
        }
    }
    return res;
}

bool operator<(const const_int_val& l, const const_int_val& r) {
    const auto ct = common_type(l.type, r.type);
    return cast(l, ct).val < cast(r, ct).val;
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