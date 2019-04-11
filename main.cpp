#include <iostream>
#include <variant>
#include <map>

#include "util.h"
#include "source.h"
#include "preprocessor.h"

using namespace mcc;

std::ostream& operator<<(std::ostream& os, const std::vector<pp_token>& v) {
    os << "{";
    for (size_t i = 0, s = v.size(); i < s; ++i) {
        os << (i ? ", ": " ") << v[i];
    }
    return os << " }";
}

#define PP_NUM(n)   pp_token{pp_token_type::number, #n}
#define PP_FNUM(n)   pp_token{pp_token_type::float_number, #n}
#define PP_PUNCT(p) pp_token{pp_token_type::punctuation, p}
#define PP_STR(s)   pp_token{pp_token_type::string_literal, #s}
#define PP_ID(id)   pp_token{pp_token_type::identifier, #id}

void test_preprocessor() {
    auto do_pp = [](const std::string& text) {
            const source_file source{"test", text};
            source_manager sm;
            preprocessor pp{sm, source};
            std::vector<pp_token> res;
            for (pp_token tok; !!(tok = pp.current()); pp.next()) {
                res.push_back(tok);
            }
            return res;
    };

    const struct {
        const char* text;
        std::vector<pp_token> expected;
    } test_cases[] = {
        { "//10\n42" , { PP_NUM(42) } },
        { "/*****/42\n/*****/" , { PP_NUM(42) } },
        { "2 \n + 3  \t\v\f\n", { PP_NUM(2), PP_PUNCT("+"), PP_NUM(3) } },
        { R"(#define A 2
#define B ((A)+1)
#define X A + B
X // 2 + ((2)+1)
)",  { PP_NUM(2), PP_PUNCT("+"), PP_PUNCT("("), PP_PUNCT("("), PP_NUM(2), PP_PUNCT(")"), PP_PUNCT("+"), PP_NUM(1), PP_PUNCT(")"), } },

        // Example from https://gcc.gnu.org/onlinedocs/cpp/Self-Referential-Macros.html
        { "#define x (4 + y)\n#define y (2 * x)\nx y", {
            PP_PUNCT("("), PP_NUM(4), PP_PUNCT("+"), PP_PUNCT("("), PP_NUM(2), PP_PUNCT("*"), PP_ID(x), PP_PUNCT(")"), PP_PUNCT(")"), 
            PP_PUNCT("("), PP_NUM(2), PP_PUNCT("*"), PP_PUNCT("("), PP_NUM(4), PP_PUNCT("+"), PP_ID(y), PP_PUNCT(")"), PP_PUNCT(")"), 
        } },
        { "#define X(a) a\n#define Y X(2)\nY", {
            PP_NUM(2)
        } },
        { "#define twice(x) 2*(x)\ntwice(twice(1))\n", {
            PP_NUM(2), PP_PUNCT("*"), PP_PUNCT("("), PP_NUM(2), PP_PUNCT("*"), PP_PUNCT("("), PP_NUM(1), PP_PUNCT(")"), PP_PUNCT(")")
        } } ,

        // Examples from https://gcc.gnu.org/onlinedocs/cpp/Macro-Arguments.html
        { "#define min(X, Y)  ((X) < (Y) ? (X) : (Y))\nmin (min (a, b), c)", do_pp("((((a) < (b) ? (a) : (b))) < (c) ? (((a) < (b) ? (a) : (b))) : (c))") },
        { "#define min(X, Y)  ((X) < (Y) ? (X) : (Y))\nmin(, b)\n",     do_pp("((   ) < (b) ? (   ) : (b))") },
        { "#define min(X, Y)  ((X) < (Y) ? (X) : (Y))\nmin(a, )\n",     do_pp("((a  ) < ( ) ? (a  ) : ( ))") },
        { "#define min(X, Y)  ((X) < (Y) ? (X) : (Y))\nmin(,)\n",       do_pp("((   ) < ( ) ? (   ) : ( ))") },
        { "#define min(X, Y)  ((X) < (Y) ? (X) : (Y))\nmin((,),)\n",    do_pp("(((,)) < ( ) ? ((,)) : ( ))") },
        { "#define foo(x) x, \"x\"\nfoo(bar)", do_pp("bar, \"x\"")},
        // Examples from https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html
        { "#define xstr(s) str(s)\n#define str(s) #s\n#define foo 4\nstr(foo)", { PP_STR("foo") } },
        { "#define xstr(s) str(s)\n#define str(s) #s\n#define foo 4\nxstr(foo)", { PP_STR("4") } },
        // Paste
        { "#define X(a) a ## _bar\nX(foo)" , { PP_ID(foo_bar) }},
        { "#define X(a) a ## 1\nX(foo)"    , { PP_ID(foo1) }},
        { "#define M(a) x ## a ## 1\nM(x)" , { PP_ID(xx1) }},
        { "#define P(a,b) a##b\nP(1 , 2)"  , { PP_NUM(12) } },
        { "#define X(n) {n}\n#define Y(n) X(n), X(n)\nY(foo)", do_pp("{foo}, {foo}") },
        { R"(
#define DEF(id, str) id,
#define DEF_BWL(x) \
 DEF(TOK_ASM_ ## x ## b, #x "b") \
 DEF(TOK_ASM_ ## x ## w, #x "w") \
 DEF(TOK_ASM_ ## x ## l, #x "l") \
 DEF(TOK_ASM_ ## x, #x)
# define DEF_BWLX DEF_BWL
 DEF_BWLX(mov)
        )", { do_pp("TOK_ASM_movb,TOK_ASM_movw,TOK_ASM_movl,TOK_ASM_mov,") } },
        { R"(
#define hash_hash # ## #
#define mkstr(a) # a
#define in_between(a) mkstr(a)
#define join(c, d) in_between(c hash_hash d)
char p[] = join(x, y);
        )", { do_pp("char p[] = \"x ## y\";") } },

        // Floating point numbers
        { "42.0", {PP_FNUM(42.0)}}, 
        { "1.234", {PP_FNUM(1.234)}}, 
        { "5e-10", {PP_FNUM(5e-10)}}, 
        // Suffixed numbers
        { "100ulLU", {PP_NUM(100ulLU)}},
        // Variadic macro
        { "#define X(...) __VA_ARGS__\nX(42, 43, 60)", { PP_NUM(42), PP_PUNCT(","), PP_NUM(43), PP_PUNCT(","), PP_NUM(60) }},
        { "#define Y( a, ... ) __VA_ARGS__ a\nY( 1, 2, 3)", { PP_NUM(2), PP_PUNCT(","), PP_NUM(3), PP_NUM(1) }}, // "2, 3 1"

        // https://github.com/pfultz2/Cloak/wiki/C-Preprocessor-tricks,-tips,-and-idioms
        { R"(
#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)
#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__
#define IIF(c) PRIMITIVE_CAT(IIF_, c)
#define IIF_0(t, ...) __VA_ARGS__
#define IIF_1(t, ...) t
#define COMPL(b) PRIMITIVE_CAT(COMPL_, b)
#define COMPL_0 1
#define COMPL_1 0
#define BITAND(x) PRIMITIVE_CAT(BITAND_, x)
#define BITAND_0(y) 0
#define BITAND_1(y) y
#define INC(x) PRIMITIVE_CAT(INC_, x)
#define INC_0 1
#define INC_1 2
#define INC_2 3
#define INC_3 4
#define INC_4 5
#define INC_5 6
#define INC_6 7
#define INC_7 8
#define INC_8 9
#define INC_9 9
#define DEC(x) PRIMITIVE_CAT(DEC_, x)
#define DEC_0 0
#define DEC_1 0
#define DEC_2 1
#define DEC_3 2
#define DEC_4 3
#define DEC_5 4
#define DEC_6 5
#define DEC_7 6
#define DEC_8 7
#define DEC_9 8
#define CHECK_N(x, n, ...) n
#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)
#define PROBE(x) x, 1,

CHECK(PROBE(~)) // Expands to 1
CHECK(xxx) // Expands to 0
#define IS_PAREN(x) CHECK(IS_PAREN_PROBE x)
#define IS_PAREN_PROBE(...) PROBE(~)
IS_PAREN(()) // Expands to 1
IS_PAREN(xxx) // Expands to 0
)",     { PP_NUM(1), PP_NUM(0), PP_NUM(1), PP_NUM(0) } },

    { "#define X 42\n#define Y (X+X)\n#if Y == 84\n1\n#else\n0\n#endif\n", {PP_NUM(1)}},
    { "#if X\n42\n#else\n60\n#endif\n", {PP_NUM(60)}},
    { "#define X 1\n#if defined(X)\n42\n#endif", {PP_NUM(42)}},
    { "#define X\n#ifdef X\n1\n#endif\n#ifdef Y\n2\n#endif\n#ifndef Y\n3\n#endif", {PP_NUM(1),PP_NUM(3)}},
    { "#if X\n42\n#endif", {}},
    { "#if 0x2A==42\n1\n#endif", {PP_NUM(1)}},
    { "#if 2==(1?2:3)\n1\n#endif", {PP_NUM(1)}},
    { "#define Y 42\n#if X\n#elif Y\n50\n#else\n2\n#endif", {PP_NUM(50)}},
    { R"(
#if 0
    000
    #if 1
        111
    #elif 1
        222
    #endif
#elif 0
    333
#elif 1
    #if 0
        444
    #else
        123
    #endif
#else
    666
#endif
)", { PP_NUM(123) }},

    { "#define BLAH(pf,sf) pf ## cc ## sf\nBLAH(,s)\nBLAH(p,)\n", {PP_ID(ccs), PP_ID(pcc)}},
    };

    const char* delim = "-----------------------------------\n";
    for (const auto& t: test_cases) {
        try {
            const auto res = do_pp(t.text);
            if (res != t.expected) {
                std::ostringstream oss;
                oss << "Got\n" << res << "\nExpecting\n" << t.expected << "\n" << delim;
                for (const auto& t2: res) {
                    oss << t2.text() << " ";
                }
                oss << "\n" << delim;
                for (const auto& t2: t.expected) {
                    oss << t2.text() << " ";
                }
                throw std::runtime_error(oss.str());
            }
        } catch (...) {
            std::cout << "Failure while processing\n" << delim << t.text << "\n" << delim << "\n";
            throw;
        }
    }
}

#undef PP_NUM
#undef PP_PUNCT
#undef PP_STR
#undef PP_ID

void define_standard_headers(source_manager& sm) {
    sm.define_standard_headers("assert.h", "");
    sm.define_standard_headers("complex.h", "");
    sm.define_standard_headers("ctype.h", "");
    sm.define_standard_headers("errno.h", "");
    sm.define_standard_headers("fenv.h", "");
    sm.define_standard_headers("float.h", "");
    sm.define_standard_headers("inttypes.h", R"(
#include <stdint.h>
)");
    sm.define_standard_headers("iso646.h", "");
    sm.define_standard_headers("limits.h", "");
    sm.define_standard_headers("locale.h", "");
    sm.define_standard_headers("math.h", "");
    sm.define_standard_headers("setjmp.h", R"(
#ifndef _SETJMP_H
#define _SETJMP_H
struct __jmp_buf;
typedef struct __jmp_buf jmp_buf[1];
#endif
)");
    sm.define_standard_headers("signal.h", "");
    sm.define_standard_headers("stdalign.h", "");
    sm.define_standard_headers("stdarg.h", "");
    sm.define_standard_headers("stdatomic.h", "");
    sm.define_standard_headers("stdbool.h", "");
    sm.define_standard_headers("stddef.h", R"(
#ifndef _STDDEF_H
#define _STDDEF_H
typedef signed long long ptrdiff_t;
typedef unsigned long long size_t;
typedef unsigned short wchar_t;
#define NULL ((void*)0)
#define offsetof(type,member) ((size_t)&((type*)0)->member))
#endif
)");
    sm.define_standard_headers("stdint.h", R"(
#ifndef _STDINT_H
#define _STDINT_H
typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef long long int int64_t;
typedef unsigned char           uint8_t;
typedef unsigned short int      uint16_t;
typedef unsigned int            uint32_t;
typedef unsigned long long int  uint64_t;
#endif
)");
    sm.define_standard_headers("stdio.h", R"(
#ifndef _STDIO_H
#define _STDIO_H
#include <stddef.h>
struct _FILE;
typedef struct _FILE FILE;
#endif
)");
    sm.define_standard_headers("stdlib.h", "");
    sm.define_standard_headers("stdnoreturn.h", "");
    sm.define_standard_headers("string.h", "");
    sm.define_standard_headers("tgmath.h", "");
    sm.define_standard_headers("threads.h", "");
    sm.define_standard_headers("time.h", "");
    sm.define_standard_headers("uchar.h", "");
    sm.define_standard_headers("wchar.h", "");
    sm.define_standard_headers("wctype.h", "");
}

void define_posix_headers(source_manager& sm) {
    sm.define_standard_headers("dlfcn.h", "");
    sm.define_standard_headers("fcntl.h", "");
    sm.define_standard_headers("unistd.h", "");
    sm.define_standard_headers("sys/stat.h", "");
    sm.define_standard_headers("sys/time.h", "");
}

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

enum class ctype {
    none,

    void_t,
    plain_char_t,
    char_t,
    short_t,
    int_t,
    long_t,
    long_long_t,
    float_t,
    double_t,
    long_double_t,
    pointer_t, // Must be first non-basic type in enum
    array_t,
    struct_t,
    union_t,
    enum_t,
    function_t,

    base_f     = 0xff,

    unsigned_f = 1<<8,
    extern_f   = 1<<9,
    static_f   = 1<<10,
    typedef_f  = 1<<11,
    register_f = 1<<12,
    const_f    = 1<<13,
    restrict_f = 1<<14,
    volatile_f = 1<<15,

    storage_f = extern_f | static_f | typedef_f | register_f,
};

ENUM_BIT_OPS(ctype)

bool is_storage_class_specifier(token_type t) {
    return t == token_type::typedef_
        || t == token_type::extern_
        || t == token_type::static_
        || t == token_type::auto_
        || t == token_type::register_;
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

bool is_simple_type_specifier(token_type t) {
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

bool is_type_qualifier(token_type t) {
    return t == token_type::const_
        || t == token_type::restrict_
        || t == token_type::volatile_;
}

ctype ctype_from_type_qualifier_token(token_type t) {
    switch (t) {
    case token_type::const_:     return ctype::const_f;
    case token_type::restrict_:  return ctype::restrict_f;
    case token_type::volatile_:  return ctype::volatile_f;
    default: NOT_IMPLEMENTED(t);
    }
}

bool is_function_specifier(token_type t) {
    return t == token_type::inline_;
}

bool is_literal(token_type t) {
    return t == token_type::const_int
        || t == token_type::const_float
        || t == token_type::char_lit
        || t == token_type::string_lit;
}

void output_flags(std::ostream& os, ctype t) {
#define CHECK_FLAG(f) if (!!(t & ctype::f##_f)) os << #f " "
    CHECK_FLAG(extern);
    CHECK_FLAG(static);
    CHECK_FLAG(typedef);
    CHECK_FLAG(register);
    CHECK_FLAG(const);
    CHECK_FLAG(restrict);
    CHECK_FLAG(volatile);
    CHECK_FLAG(unsigned);
#undef CHECK_FLAG
    if ((t & ctype::base_f) == ctype::char_t && !(t & ctype::unsigned_f)) {
        os << "signed ";
    }
}

std::ostream& operator<<(std::ostream& os, ctype t) {
    output_flags(os, t);
    switch (t & ctype::base_f) {
    case ctype::none:          return os << "none";
    case ctype::void_t:        return os << "void";
    case ctype::plain_char_t:  return os << "char";
    case ctype::char_t:        return os << "char";
    case ctype::short_t:       return os << "short";
    case ctype::int_t:         return os << "int";
    case ctype::long_t:        return os << "long";
    case ctype::long_long_t:   return os << "long long";
    case ctype::float_t:       return os << "float";
    case ctype::double_t:      return os << "double";
    case ctype::long_double_t: return os << "long double";
    case ctype::pointer_t:     return os << "pointer";
    case ctype::array_t:       return os << "array";
    case ctype::struct_t:      return os << "struct";
    case ctype::union_t:       return os << "union";
    case ctype::enum_t:        return os << "enum";
    case ctype::function_t:    return os << "function";
    }
    NOT_IMPLEMENTED(static_cast<uint32_t>(t & ctype::base_f));
}

ctype base_type(ctype t) {
    return t & ctype::base_f;
}

ctype modified_base_type(ctype t, ctype new_base) {
    return new_base | (t & ~ctype::base_f);
}

struct const_int_val {
    uint64_t val;
    ctype type;
};

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

class lexer {
public:
    explicit lexer(source_manager& sm, const source_file& source) : pp_{sm, source} {
        next();
    }

    auto position() const {
        return pp_.position();
    }

    const token& current() const {
        return current_;
    }

    void next() {
        const auto tok = pp_.current();
        if (!tok) {
            current_ = token{token_type::eof};
            return;
        }
        pp_.next();

        if (tok.type() == pp_token_type::identifier) {
            const auto kt = keyword_token_from_text(tok.text());
            if (kt == token_type::eof) {
                current_ = token{token_type::id, tok.text()};
            } else {
                current_ = token{kt};
            }
        } else if(tok.type() == pp_token_type::punctuation) {
            const auto pt = op_token_from(tok.text());
            if (pt == token_type::eof) {
                NOT_IMPLEMENTED(tok);
            }
            current_ = token{pt};
        } else if (tok.type() == pp_token_type::number) {
            char* end = nullptr;
            errno = 0;
            const auto n = strtoull(tok.text().c_str(), &end, 0);
            // FIXME: Use correct type
            ctype t = ctype::int_t;
            for (; end && *end; ++end) {
                const auto ch = static_cast<uint8_t>(*end | 0x20);
                if (ch == 'u') {
                    t |= ctype::unsigned_f;
                } else if (ch == 'l') {
                    if (base_type(t) == ctype::long_t) {
                        t = modified_base_type(t, ctype::long_long_t);
                    }
                }
                else break;
            }
            if (!end || *end || (n == ULLONG_MAX && errno == ERANGE)) {
                NOT_IMPLEMENTED("Invalid number " << tok.text());
            }
            current_ = token{const_int_val{n, t}};
        } else if (tok.type() == pp_token_type::float_number) {
            char* end = nullptr;
            errno = 0;
            double v = std::strtod(tok.text().c_str(), &end);
            if (!end || *end) {
                NOT_IMPLEMENTED("Invalid float " << tok.text());
            }
            if (errno == ERANGE) {
                if (v == HUGE_VAL) {
                    v = INFINITY;
                }
                errno = 0;
            }
            current_ = token{v};
        } else if (tok.type() == pp_token_type::character_constant) {
            assert(tok.text().size() >= 3 && tok.text().front() == '\'' && tok.text().back() == '\'');
            const auto [ch, len] = unescape_char(std::string_view{tok.text().c_str()+1,tok.text().size()-2});
            if (len + 2 != tok.text().size() || ch > 255) {
                NOT_IMPLEMENTED(tok << " ch = " << ch);
            }
            current_ = token{ch};
        } else if (tok.type() == pp_token_type::string_literal) {
            auto t = tok.text();
            std::string lit;
            for (;;) {
                assert(t.size() >= 2 && t.front() == '\"' && t.back() == '\"');
                lit += unescape(std::string_view{t.c_str() + 1, t.size()-2});
                if (pp_.current().type() != pp_token_type::string_literal) {
                    break;
                }
                t = pp_.current().text();
                pp_.next();
            }
            current_ = token{token_type::string_lit, lit};
        } else {
            NOT_IMPLEMENTED(tok);
        }
    }

private:
    preprocessor pp_;
    token current_;
};

class array_info;
class struct_info;
class union_info;
class enum_info;
class function_info;
class type;

class type {
public:
    explicit type() : t_{ctype::none}, val_{} {
    }

    explicit type(ctype t) : t_{t}, val_{} {
        assert(base_type(t_) < ctype::pointer_t);
    }

    explicit type(ctype t, const std::shared_ptr<const type>& pointee) : t_{t}, val_{pointee} {
        assert(base_type(t_) == ctype::pointer_t);
    }

    explicit type(ctype t, const std::shared_ptr<const array_info>& array_inf) : t_{t}, val_{array_inf} {
        assert(base_type(t_) == ctype::array_t);
    }

    explicit type(ctype t, const std::shared_ptr<const struct_info>& struct_inf) : t_{t}, val_{struct_inf} {
        assert(base_type(t_) == ctype::struct_t);
    }

    explicit type(ctype t, const std::shared_ptr<const union_info>& union_inf) : t_{t}, val_{union_inf} {
        assert(base_type(t_) == ctype::union_t);
    }

    explicit type(ctype t, const std::shared_ptr<const enum_info>& enum_inf) : t_{t}, val_{enum_inf} {
        assert(base_type(t_) == ctype::enum_t);
    }

    explicit type(ctype t, const std::shared_ptr<const function_info>& function_inf) : t_{t}, val_{function_inf} {
        assert(base_type(t_) == ctype::function_t);
    }

    ctype ct() const { return t_; }

    ctype base() const { return t_ & ctype::base_f; }

    void set_base_type(ctype new_base) {
        assert(!(new_base & ~ctype::base_f) && new_base <= ctype::long_double_t);
        t_ = modified_base_type(t_, new_base);
    }

    void add_flags(ctype flags) {
        assert(!(flags & ctype::base_f));
        t_ |= flags;
    }

    void remove_flags(ctype flags) {
        assert(!(flags & ctype::base_f));
        t_ &= ~flags;
    }

    void modify_inner(const std::shared_ptr<type>& t) {
        if (base() == ctype::pointer_t) {
            auto& pointed_type = std::get<1>(val_);
            if (pointed_type->ct() != ctype::none) {
                NOT_IMPLEMENTED(pointed_type->ct());
            }
            val_ = t;
        } else {
            NOT_IMPLEMENTED(ct());
        }        
    }

    const type& pointer_val() const {
        assert(base_type(t_) == ctype::pointer_t);
        return *std::get<1>(val_);
    }

    const array_info& array_val() const {
        assert(base_type(t_) == ctype::array_t);
        return *std::get<2>(val_);
    }

    const struct_info& struct_val() const {
        assert(base_type(t_) == ctype::struct_t);
        return *std::get<3>(val_);
    }

    const union_info& union_val() const {
        assert(base_type(t_) == ctype::union_t);
        return *std::get<4>(val_);
    }

    const enum_info& enum_val() const {
        assert(base_type(t_) == ctype::enum_t);
        return *std::get<5>(val_);
    }

    const function_info& function_val() const {
        assert(base_type(t_) == ctype::function_t);
        return *std::get<6>(val_);
    }

private:
    ctype t_;
    std::variant<std::monostate,
                 std::shared_ptr<const type>,
                 std::shared_ptr<const array_info>,
                 std::shared_ptr<const struct_info>,
                 std::shared_ptr<const union_info>,
                 std::shared_ptr<const enum_info>,
                 std::shared_ptr<const function_info>
        > val_;
};

std::ostream& operator<<(std::ostream& os, type t);

class decl {
public:
    explicit decl(const std::shared_ptr<const type>& t, const std::string& id) : type_{t}, id_{id} {}

    const std::shared_ptr<const type>& t() const { return type_; }
    const std::string& id() const { return id_; }

private:
    std::shared_ptr<const type> type_;
    std::string id_;
};

std::ostream& operator<<(std::ostream& os, const decl& d) {
    return os << *d.t() << " " << d.id();
}

class array_info {
public:
    static constexpr uint64_t unbounded = UINT64_MAX;
    explicit array_info(const std::shared_ptr<const type>& t, uint64_t bound) : t_{t}, bound_{bound} {}

    const std::shared_ptr<const type>& t() const { return t_;}
    uint64_t bound() const { return bound_; }

private:
    std::shared_ptr<const type> t_;
    uint64_t bound_;
};

std::ostream& operator<<(std::ostream& os, const array_info& ai) {
    os << *ai.t();
    if (ai.bound() != array_info::unbounded) {
        return os << "[" << ai.bound() << "]";
    } else {
        return os << "[]";
    }
}

class tag_info_type {
public:
    explicit tag_info_type(const std::string& id) : id_{id} {}
    virtual ~tag_info_type() {}

    const std::string& id() const { return id_; }
    virtual ctype base_type() const = 0;
private:
    std::string id_;
};

std::ostream& operator<<(std::ostream& os, const tag_info_type& tit) {
    return os << tit.base_type()  << " " << tit.id();
}

class struct_info : public tag_info_type {
public:
    explicit struct_info(const std::string& id) : tag_info_type{id} {}
    ctype base_type() const override { return ctype::struct_t; }
};

class union_info : public tag_info_type {
public:
    explicit union_info(const std::string& id) : tag_info_type{id} {}
    ctype base_type() const override { return ctype::union_t; }
};

class enum_info : public tag_info_type {
public:
    explicit enum_info(const std::string& id) : tag_info_type{id} {}
    ctype base_type() const override { return ctype::enum_t; }
};

std::shared_ptr<type> make_tag_type(const std::shared_ptr<tag_info_type>& tag_type, ctype flags) {
    assert(!(flags & ctype::base_f));
    const auto bt = tag_type->base_type();
    if (bt == ctype::struct_t) {
        return std::make_shared<type>(bt | flags, std::static_pointer_cast<struct_info>(tag_type));
    } else if (bt == ctype::union_t) {
        return std::make_shared<type>(bt | flags, std::static_pointer_cast<union_info>(tag_type));
    } else if (bt == ctype::enum_t) {
        return std::make_shared<type>(bt | flags, std::static_pointer_cast<enum_info>(tag_type));
    } else {
        NOT_IMPLEMENTED(flags << " " << *tag_type);
    }
}

class function_info {
public:
    explicit function_info(const std::shared_ptr<const type>& ret_type, const std::vector<decl>& params) : ret_type_{ret_type}, params_{params} {
    }

    const std::shared_ptr<const type>& ret_type() const { return ret_type_; }
    const std::vector<decl>& params() const { return params_; }

private:
    std::shared_ptr<const type> ret_type_;
    std::vector<decl> params_;
};

std::ostream& operator<<(std::ostream& os, const function_info& fi) {
    os << *fi.ret_type() << "(";
    for (size_t i = 0; i < fi.params().size(); ++i) {
        if (i) os << ", ";
        os << fi.params()[i];
    }
    return os << ")";
}

std::ostream& operator<<(std::ostream& os, type t) {
    switch (t.base()) {
    case ctype::pointer_t: 
        os << t.pointer_val();
        os << " * ";
        output_flags(os, t.ct());
        return os;
    case ctype::array_t:
        output_flags(os, t.ct());
        return os << t.array_val();
    case ctype::struct_t:
        output_flags(os, t.ct());
        return os << "struct " << t.struct_val().id();
    case ctype::union_t:
        output_flags(os, t.ct());
        return os << "union " << t.union_val().id();
    case ctype::enum_t:
        output_flags(os, t.ct());
        return os << "enum " << t.enum_val().id();
    case ctype::function_t:
        output_flags(os, t.ct());
        return os << t.function_val();
    default:
        return os << t.ct();
    }
}

#define EXPECT(tok) do { if (current().type() != token_type::tok) NOT_IMPLEMENTED("Expected " << token_type::tok << " got " << current()); next(); } while (0)
#define TRACE(msg) std::cout << __FILE__ << ":" << __LINE__ << ": " << __func__ <<  " Current: " << current() << " " << msg << "\n"

class parser {
public:
    explicit parser(source_manager& sm, const source_file& source) : lex_{sm, source} {
    }

    auto position() const {
        return lex_.position();
    }

    void parse() {
        for (;;) {
            parse_declaration();
        }
    }

private:
    lexer lex_;
    int unnamed_cnt_ = 0;
    std::vector<std::shared_ptr<tag_info_type>> tag_types_;
    std::map<std::string, std::shared_ptr<type>> typedefs_;

    std::shared_ptr<tag_info_type> find_tag_type(const std::string_view id) {
        for (auto& s: tag_types_) {
            if (s->id() == id) {
                return s;
            }
        }
        return nullptr;
    }

    std::shared_ptr<type> find_typedef(const std::string& id) const {
        if (auto it = typedefs_.find(id); it != typedefs_.end()) {
            assert(!(it->second->ct() & ctype::typedef_f));
            return it->second;
        }
        return nullptr;
    }

    void next() {
        assert(current().type() != token_type::eof);
        lex_.next();
    }

    const token& current() const {
        return lex_.current();
    }

    void parse_declaration() {
        // declaration
        //    declaration_specifiers init-declarator-list? ';'
        const auto ds = parse_declaration_specifiers();

        if (current().type() == token_type::semicolon) {
            // Type decl.
            if (!!(ds->ct() & ctype::typedef_f)) {
                NOT_IMPLEMENTED(ds);
            }
            std::cout << *ds << "\n";
            next();
            return;
        }

        for (;;) {
            auto d = parse_declarator(ds);

            if (!!(d.t()->ct() & ctype::typedef_f)) {
                std::cout << d << "\n";
                if (d.id().empty()) {
                    NOT_IMPLEMENTED(d);
                }
                if (auto it = typedefs_.find(d.id()); it != typedefs_.end()) {
                    NOT_IMPLEMENTED(d << " Already defined as " << it->first << " " << it->second);
                }
                auto t = std::make_shared<type>(*d.t());
                t->remove_flags(ctype::typedef_f);
                typedefs_[d.id()] = t;
                break;
            }

            if (current().type() == token_type::colon) {
                // TODO: Bitfield
                next();
                parse_constant_expression();
                std::cout << "Ignoring bitfield\n";
            }

            std::cout << d << "\n";

            if (current().type() != token_type::comma) {
                break;
            }
            next();
        }

        EXPECT(semicolon);

        // init-declarator-list
        //    init-declarator
        //    init-declarator-list , init-declarator
        
        // init-declarator
        //     declarator
        //     declarator = initializer
    }

    std::shared_ptr<type> parse_declaration_specifiers() {
        auto res_type = std::make_shared<type>();
        int long_ = 0;
        int int_  = 0;
        int sign  = -1;

        for (bool stop = false; !stop;) {
            const auto t = current().type();
            // declaration_specifiers
            //     storage_class_specifier
            //     type-specifier
            //     type-qualifier
            //     function-specifier

            if (is_storage_class_specifier(t)) {
                res_type->add_flags(ctype_from_storage_class_token(t));
                next();
                continue;
            }

            if (is_type_qualifier(t)) {
                res_type->add_flags(ctype_from_type_qualifier_token(t));
                next();
                continue;
            }

            if (is_simple_type_specifier(t)) {
                if (t == token_type::int_) {
                    ++int_;
                } else if (t == token_type::long_) {
                    ++long_;
                } else if (t == token_type::signed_) {
                    if (sign != -1) NOT_IMPLEMENTED(sign);
                    sign = 1;
                } else if (t == token_type::unsigned_) {
                    if (sign != -1) NOT_IMPLEMENTED(sign);
                    sign = 0;
                } else {
                    if (res_type->base() != ctype::none) {
                        NOT_IMPLEMENTED(t << " in addition to " << *res_type);
                    }
                    switch (t) {
                    case token_type::void_:   res_type->set_base_type(ctype::void_t); break;
                    case token_type::char_:   res_type->set_base_type(ctype::plain_char_t); break;
                    case token_type::short_:  res_type->set_base_type(ctype::short_t); break;
                    case token_type::float_:  res_type->set_base_type(ctype::float_t); break;
                    case token_type::double_: res_type->set_base_type(ctype::double_t); break;
                    default:
                        NOT_IMPLEMENTED(t);
                    }
                }
                next();
                continue;
            }

            if (is_function_specifier(t)) {
                std::cout << "Ignoring " << t << "\n";
                next();
                continue;
            }

            if (t == token_type::__attribute___) {
                next();
                EXPECT(lparen);
                EXPECT(lparen);
                if (current().type() != token_type::id) {
                    NOT_IMPLEMENTED("__attribute__ " << current());
                }
                const auto id = current().text();
                next();
                EXPECT(rparen);
                EXPECT(rparen);
                std::cout << "Ignoring __attribute__((" << id << "))\n";
                continue;
            }

            if (t == token_type::struct_
                || t == token_type::union_
                || t == token_type::enum_) {
                next();

                if (res_type->base() != ctype::none) {
                    NOT_IMPLEMENTED("Invalid decl " << *res_type << " and " << t);
                }

                std::shared_ptr<tag_info_type> tag_type;
                std::string id;

                auto make_tag_info = [&]() {
                    assert(!id.empty());
                    assert(!find_tag_type(id));
                    if (t == token_type::struct_)  {
                        tag_type = std::make_shared<struct_info>(id);
                    } else if (t == token_type::union_) {
                        tag_type = std::make_shared<union_info>(id);
                    } else if (t == token_type::enum_) {
                        tag_type = std::make_shared<enum_info>(id);
                    } else {
                        NOT_IMPLEMENTED(t);
                    }
                    tag_types_.push_back(tag_type);
                };

                if (current().type() == token_type::id) {
                    id = current().text();
                    next();

                    tag_type = find_tag_type(id);
                    if (!tag_type) {
                        make_tag_info();
                    }
                }
                if (current().type() == token_type::lbrace) {
                    next();
                    if (t == token_type::enum_) {
                        parse_enum_list();
                    } else {
                        parse_struct_declaration_list();
                    }
                    EXPECT(rbrace);
                    TRACE("Not using " << t << " definition");
                    if (!tag_type) {
                        assert(id.empty());
                        id = "__unnamed" + std::to_string(unnamed_cnt_++);
                        make_tag_info();
                    } else {
                        // TODO: Check that the struct hasn't been defined before
                    }
                } else if (id.empty()) {
                    NOT_IMPLEMENTED(current());
                }

                assert(tag_type);
                res_type = make_tag_type(tag_type, res_type->ct());
                continue;
            }

            if (t == token_type::id) {
                if (auto td = find_typedef(current().text())) {
                    if (res_type->base() != ctype::none) {
                        NOT_IMPLEMENTED("typedef " << *td << " combine with " << *res_type);
                    }
                    auto saved_flags = res_type->ct() & ~ctype::base_f;
                    res_type = std::make_shared<type>(*td);
                    res_type->add_flags(saved_flags);
                    next();
                    continue;
                }
            }

            break;
        }

        if (!long_ && !int_ && sign == -1) {
            return res_type;
        }

        // Long double
        if (long_ == 1 && !int_ && sign == -1 && res_type->base() == ctype::double_t) {
            res_type->set_base_type(ctype::long_double_t);
            return res_type;
        }

        if (sign == 0) {
            res_type->add_flags(ctype::unsigned_f);
        }

        // short int/int/long int/long long int
        if (int_ == 0 || int_ == 1) {
            if (res_type->base() == ctype::none) {
                if (long_ == 0 && (int_ || sign != -1)) {
                    res_type->set_base_type(ctype::int_t);
                    return res_type;
                } else if (long_ == 1) {
                    res_type->set_base_type(ctype::long_t);
                    return res_type;
                } else if (long_ == 2) {
                    res_type->set_base_type(ctype::long_long_t);
                    return res_type;
                }
            } else if (res_type->base() == ctype::short_t) {
                return res_type;
            }
        }

        // signed char/ unsigned char
        if (!long_ && !int_ && sign != -1 && res_type->base() == ctype::plain_char_t) {
            res_type->set_base_type(ctype::char_t);
            return res_type;
        }

        NOT_IMPLEMENTED(*res_type << " long: " << long_ << " int: " << int_ << " sign: " << sign << " current: " << current());
    }

    decl parse_declarator(std::shared_ptr<type> t) {
        // '*' type-qualifier-list? pointer?
        while (current().type() == token_type::star) {
            next();
            ctype pt = ctype::pointer_t;
            while (is_type_qualifier(current().type())) {
                pt |=  ctype_from_type_qualifier_token(current().type());
                next();
            }
            t = std::make_shared<type>(pt, t);
        }
        return parse_direct_declarator(t);
    }

    decl parse_direct_declarator(std::shared_ptr<type> t) {
        std::shared_ptr<type> inner_type{};
        std::string id;
        const auto storage_flags = t->ct() & ctype::storage_f;
        if (current().type() == token_type::lparen) {
            next();
            auto decl = parse_declarator(std::make_shared<type>());
            EXPECT(rparen);
            inner_type = std::make_shared<type>(*decl.t());
            id = decl.id();
        } else if (current().type() == token_type::id) {
            id = current().text();
            next();
        }
        if (current().type() == token_type::lbracket) {
            next();
            parse_assignment_expression(); // FIXME: Use result
            EXPECT(rbracket);
            t->remove_flags(ctype::storage_f);
            t = std::make_shared<type>(ctype::array_t | storage_flags, std::make_unique<array_info>(t, array_info::unbounded));
        } else if (current().type() == token_type::lparen) {
            next();
            auto arg_types = parse_parameter_type_list();
            EXPECT(rparen);
            t->remove_flags(ctype::storage_f);
            t = std::make_shared<type>(ctype::function_t | storage_flags, std::make_unique<function_info>(t, arg_types));
        }
        if (inner_type) {
            inner_type->modify_inner(t);
            t = inner_type;
        }
        return decl{t, id};
    }

    std::vector<decl> parse_parameter_type_list() {
        std::vector<decl> arg_types;
        for (size_t cnt = 0; current().type() != token_type::rparen; ++cnt) {
            if (cnt) {
                EXPECT(comma);
            }
            if (current().type() == token_type::ellipsis) {
                std::cout << "TODO: Handle ellipsis\n";
                arg_types.push_back(decl{std::make_shared<type>(), "..."});
                next();
            } else {
                const auto ds = parse_declaration_specifiers();
                const auto d = parse_declarator(ds);
                if (d.t()->base() == ctype::none) {
                    NOT_IMPLEMENTED(d);
                }
                arg_types.push_back(d);
            }
        }
        return arg_types;
    }

    void parse_struct_declaration_list() {
        while (current().type() != token_type::rbrace) {
            parse_declaration();
        }
    }

    void parse_enum_list() {
        while (current().type() != token_type::rbrace) {
            if (current().type() != token_type::id) {
                NOT_IMPLEMENTED("Expected identifier got " << current());
            }
            const auto id = current().text();
            next();
            if (current().type() == token_type::eq) {
                next();
                parse_constant_expression();
            }
            if (current().type() != token_type::comma) {
                break;
            }
            next();
        }
    }

    //
    // Expression
    //

    void parse_primary_expression() {
        // identifier
        // constant
        // string-literal
        // ( expression )
        const auto t = current().type();
        if (t == token_type::id) {
            NOT_IMPLEMENTED(current());
        } else if (is_literal(t)) {
            TRACE("Consuming");
            next();
            return;
        } else if (t == token_type::lparen) {
            next();
            parse_expression();
            EXPECT(rparen);
            return;
        }
        NOT_IMPLEMENTED("Expected primary expression got " << current());
    }

    void parse_postfix_expression() {
        parse_primary_expression();
        const auto t = current().type();
        if (t == token_type::lbracket
            || t == token_type::lparen
            || t == token_type::dot
            || t == token_type::arrow
            || t == token_type::plusplus
            || t == token_type::minusminus) {
            NOT_IMPLEMENTED(t);
        }
    }

    void parse_unary_expression() {
        const auto t = current().type();
        if (t == token_type::plusplus
            || t == token_type::minusminus) {
            NOT_IMPLEMENTED(t);
        }
        if (t == token_type::and_
            || t == token_type::star
            || t == token_type::plus
            || t == token_type::minus
            || t == token_type::bnot
            || t == token_type::not_) {
            NOT_IMPLEMENTED(t);
            // parse_cast_expression
        }
        if (t == token_type::sizeof_) {
            NOT_IMPLEMENTED(t);
            // parse_unary_expression or if parenthesis: parse typename
        }
        return parse_postfix_expression();
    }

    void parse_expression1(/*expression_ptr&& lhs, */int outer_precedence) {
        for (;;) {
            const auto op = current().type();
            const auto precedence = operator_precedence(op);
            if (precedence > outer_precedence) {
                break;
            }
            next();
            if (op == token_type::question) {
                NOT_IMPLEMENTED(current());
                //auto l = parse_assignment_expression();
                //EXPECT(token_type::colon);
                //lhs = make_expression<conditional_expression>(std::move(lhs), std::move(l), parse_assignment_expression());
                continue;
            }

            /*auto rhs = */parse_unary_expression();
            for (;;) {
                const auto look_ahead = current().type();
                const auto look_ahead_precedence = operator_precedence(look_ahead);
                if (look_ahead_precedence > precedence /*|| (look_ahead_precedence == precedence && !is_right_to_left(look_ahead))*/) {
                    break;
                }
                /*rhs = */parse_expression1(/*std::move(rhs), */look_ahead_precedence);
            }

            //lhs = make_expression<binary_expression>(op, std::move(lhs), std::move(rhs));
        }
        //return std::move(lhs);
    }

    void parse_expression() {
        /*lhs=*/ parse_unary_expression();
        return parse_expression1(operator_precedence(token_type::comma));
    }

    void parse_assignment_expression() {
        /*lhs=*/ parse_unary_expression();
        return parse_expression1(operator_precedence(token_type::eq));
    }

    void parse_constant_expression() {
        /*lhs=*/ parse_unary_expression();
        return parse_expression1(operator_precedence(token_type::oror));
    }
};

int main(int argc, char* argv[]) {
    try {
        test_preprocessor();
        if (argc < 2) {
            return 0;
        }
        std::cout << "Compile " << argv[1] << "\n";

        source_manager sm;
        define_standard_headers(sm);
        define_posix_headers(sm);
        parser ps{sm, sm.load(argv[1])};
        try {
            ps.parse();
        } catch (...) {
            std::cerr << "At\n";
            for (const auto& p : ps.position()) {
                std::cerr << p << "\n";
            }
            std::cerr << "\n";
            throw;
        }

    } catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
}