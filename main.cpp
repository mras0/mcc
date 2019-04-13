#include <iostream>
#include <variant>
#include <map>

#include "util.h"
#include "source.h"
#include "preprocessor.h"
#include "parser.h"

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
            source_manager sm{""};
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
    { "#define X(name) table_ ## name\nX(286_0f)\n", {PP_ID(table_286_0f)}},
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

void process_one(source_manager& sm, const std::string& filename) {
    std::cout << filename << "\n";
    auto decls = parse(sm, sm.load(filename));
    for (const auto& d: decls) {
        if (d->d().t()->base() == ctype::function_t) {
            if (d->has_init_val()) {
                std::cout << *d << "\n";
                std::cout << d->body() << "\n\n";
            }
        }
    }
}

int main(int argc, char* argv[]) {
    try {
        test_preprocessor();
        if (argc < 2) {
            return 0;
        }

        source_manager sm{standard_builtin_text()};
        std::vector<std::string> files;
        for (int i = 1; i < argc; ++i) {
            const auto a = std::string_view{argv[i]};
            if (a == "-I") {
                if (++i == argc) NOT_IMPLEMENTED("Missing argument for -I");
                sm.add_include_directory(argv[i]);
            } else {
                auto fs = process_wild_cards(a);
                files.insert(files.end(), fs.begin(), fs.end());
            }
        }
        if (files.empty()) {
            NOT_IMPLEMENTED("No files found");
        }
        define_standard_headers(sm);
        define_posix_headers(sm);
        for (const auto& f: files) {
            process_one(sm, f);
        }
    } catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
}