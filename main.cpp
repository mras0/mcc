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
    sm.define_standard_headers("inttypes.h", "");
    sm.define_standard_headers("iso646.h", "");
    sm.define_standard_headers("limits.h", "");
    sm.define_standard_headers("locale.h", "");
    sm.define_standard_headers("math.h", "");
    sm.define_standard_headers("setjmp.h", "");
    sm.define_standard_headers("signal.h", "");
    sm.define_standard_headers("stdalign.h", "");
    sm.define_standard_headers("stdarg.h", "");
    sm.define_standard_headers("stdatomic.h", "");
    sm.define_standard_headers("stdbool.h", "");
    sm.define_standard_headers("stddef.h", "");
    sm.define_standard_headers("stdint.h", "");
    sm.define_standard_headers("stdio.h", "");
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
    X(while)

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
    X("|="  , oreq_)      \
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

class struct_info;

class type {
public:
    explicit type(ctype t) : t_{t}, val_{} {
        assert(base_type(t_) < ctype::pointer_t);
    }

    explicit type(ctype t, const std::shared_ptr<const type>& pointee) : t_{t}, val_{pointee} {
        assert(base_type(t_) == ctype::pointer_t);
    }

    explicit type(ctype t, const std::shared_ptr<const struct_info>& struct_inf) : t_{t}, val_{struct_inf} {
        assert(base_type(t_) == ctype::struct_t);
    }

    ctype ct() const { return t_; }
    ctype base() const { return t_ & ctype::base_f; }

    const type& pointee() const {
        assert(base_type(t_) == ctype::pointer_t);
        return *std::get<1>(val_);
    }

    const struct_info& struct_val() const {
        assert(base_type(t_) == ctype::struct_t);
        return *std::get<2>(val_);
    }

    void add_flags(ctype flags) {
        assert(!(flags & ctype::base_f));
        t_ |= flags;
    }

private:
    ctype t_;
    std::variant<std::monostate, std::shared_ptr<const type>, std::shared_ptr<const struct_info>> val_;
};

class struct_info {
public:
    explicit struct_info(const std::string& id) : id_(id) {}

    const std::string& id() const { return id_; }

private:
    std::string id_;
};

std::ostream& operator<<(std::ostream& os, const struct_info& si) {
    return os << "struct " << si.id();
}

std::ostream& operator<<(std::ostream& os, type t) {
    switch (t.base()) {
    case ctype::pointer_t: 
        os << t.pointee();
        os << " * ";
        output_flags(os, t.ct());
        return os;
    case ctype::struct_t:
        return os << "struct " << t.struct_val().id();
    default:
        return os << t.ct();
    }
}

#define EXPECT(tok) do { if (current().type() != token_type::tok) NOT_IMPLEMENTED("Expected " << token_type::tok << " got " << current()); next(); } while (0)

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
    std::vector<std::shared_ptr<struct_info>> structs_;

    std::shared_ptr<struct_info> find_struct(const std::string_view id) {
        for (auto& s: structs_) {
            if (s->id() == id) {
                return s;
            }
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

    bool is_typedef_name(const std::string& id) const {
        std::cout << "TODO: Check if typedef name: " << id << "\n";
        return false;
    }

    void parse_declaration() {
        // declaration
        //    declaration_specifiers init-declarator-list? ';'
        const auto ds = parse_declaration_specifiers();

        if (current().type() == token_type::semicolon) {
            NOT_IMPLEMENTED("type(def) decl");
        }

        parse_declarator(ds);
        EXPECT(semicolon);

        // init-declarator-list
        //    init-declarator
        //    init-declarator-list , init-declarator
        
        // init-declarator
        //     declarator
        //     declarator = initializer
    }

    type parse_declaration_specifiers() {
        type res_type{ctype::none};
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
                res_type.add_flags(ctype_from_storage_class_token(t));
                next();
                continue;
            }

            if (is_type_qualifier(t)) {
                res_type.add_flags(ctype_from_type_qualifier_token(t));
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
                    if (res_type.base() != ctype::none) {
                        NOT_IMPLEMENTED(t << " in addition to " << res_type);
                    }
                    switch (t) {
                    case token_type::void_:   res_type = type{modified_base_type(res_type.ct(), ctype::void_t)}; break;
                    case token_type::char_:   res_type = type{modified_base_type(res_type.ct(), ctype::plain_char_t)}; break;
                    case token_type::short_:  res_type = type{modified_base_type(res_type.ct(), ctype::short_t)}; break;
                    case token_type::float_:  res_type = type{modified_base_type(res_type.ct(), ctype::float_t)}; break;
                    case token_type::double_: res_type = type{modified_base_type(res_type.ct(), ctype::double_t)}; break;
                    default:
                        NOT_IMPLEMENTED(t);
                    }
                }
                next();
                continue;
            }

            if (is_function_specifier(t)) {
                NOT_IMPLEMENTED(t);
            }

            if (t == token_type::struct_
                || t == token_type::union_
                || t == token_type::enum_) {
                next();

                if (res_type.base() != ctype::none) {
                    NOT_IMPLEMENTED("Invalid decl " << res_type << " and " << t);
                }

                std::string id;
                if (current().type() == token_type::id) {
                    id = current().text();
                    next();
                }
                if (current().type() == token_type::lbrace) {
                    NOT_IMPLEMENTED(current());
                } else if (id.empty()) {
                    NOT_IMPLEMENTED(current());
                }

                assert(!find_struct(id));
                structs_.push_back(std::make_shared<struct_info>(id));
                res_type = type{ctype::struct_t | res_type.ct(), structs_.back()};

                continue;
            }

            if (t == token_type::id && is_typedef_name(current().text())) {
                NOT_IMPLEMENTED("typedef " << current());
            }

            break;
        }

        if (!long_ && !int_ && sign == -1) {
            return res_type;
        }

        if (long_ == 1 && !int_ && sign == -1 && res_type.base() == ctype::double_t) {
            return type{modified_base_type(res_type.ct(), ctype::long_double_t)};
        }

        NOT_IMPLEMENTED(res_type << " long: " << long_ << " int: " << int_ << " sign: " << sign << " current: " << current());
    }

    void parse_declarator(type t) {
        // '*' type-qualifier-list? pointer?
        while (current().type() == token_type::star) {
            next();
            ctype pt = ctype::pointer_t;
            while (is_type_qualifier(current().type())) {
                pt |=  ctype_from_type_qualifier_token(current().type());
                next();
            }
            t = type{pt, std::make_shared<type>(t)};
        }
        parse_direct_declarator(t);
    }

    void parse_direct_declarator(type t) {
        std::string id;
        if (current().type() == token_type::lparen) {
            NOT_IMPLEMENTED(t << " (declarator)");
        } else if (current().type() == token_type::id) {
            id = current().text();
            next();
            std::cout << t << " " << id << "\n";
            if (current().type() == token_type::lbracket) {
                NOT_IMPLEMENTED(current());
            } else if (current().type() == token_type::lparen) {
                next();
                parse_parameter_type_list();
                EXPECT(rparen);
            }
        } else {
            NOT_IMPLEMENTED(current());
        }
    }

    void parse_parameter_type_list() {
        for (size_t cnt = 0;; ++cnt) {
            const auto t = current().type();
            if (t == token_type::rparen) {
                break;
            }
            if (cnt) {
                EXPECT(comma);
            }
            const auto ds = parse_declaration_specifiers();
            parse_declarator(ds);
        }
        NOT_IMPLEMENTED("FUNCTION");
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