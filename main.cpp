#include <iostream>
#include <variant>

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

struct const_int_val {
    uint64_t val;
    bool unsigned_;
    uint8_t long_;
};

std::ostream& operator<<(std::ostream& os, const_int_val civ) {
    os << civ.val;
    if (civ.unsigned_) os << "U";
    if (civ.long_>0) os << "L";
    if (civ.long_>1) os << "L";
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
            int num_l = 0, num_u = 0;
            for (; end && *end; ++end) {
                const auto ch = static_cast<uint8_t>(*end | 0x20);
                if (ch == 'u') ++num_u;
                else if (ch == 'l') ++num_l;
                else break;
            }
            if (!end || *end || (n == ULLONG_MAX && errno == ERANGE)) {
                NOT_IMPLEMENTED("Invalid number " << tok.text());
            }
            current_ = token{const_int_val{n, !!num_u, static_cast<uint8_t>(num_l>2?2:num_l)}};
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
        lexer l{sm, sm.load(argv[1])};
        try {
            for (; l.current().type() != token_type::eof; l.next()) {
                std::cout << l.current() << "\n";
            }
        } catch (...) {
            std::cerr << "At\n";
            for (const auto& p : l.position()) {
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