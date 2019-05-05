#include <iostream>
#include <variant>
#include <map>
#include <set>

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
#if 1
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
    { R"(
# define ELFW(type) ELF##32##_##type
#define ELF32_ST_BIND(val)		(((unsigned char) (val)) >> 4)
#define ELF32_ST_TYPE(val)		((val) & 0xf)
#define ELF32_ST_INFO(bind, type)	(((bind) << 4) + ((type) & 0xf))

ELFW(ST_INFO)(sym_bind, ELFW(ST_TYPE)(esym->st_info));
)",     {
        // (((sym_bind) << 4) + ((((esym->st_info) & 0xf)) & 0xf));
        PP_PUNCT("("),
            PP_PUNCT("("),
                PP_PUNCT("("),PP_ID(sym_bind),PP_PUNCT(")"),
                PP_PUNCT("<<"),PP_NUM(4),
            PP_PUNCT(")"),
            PP_PUNCT("+"),
            PP_PUNCT("("),
                PP_PUNCT("("),
                    PP_PUNCT("("),
                        PP_PUNCT("("),
                            PP_ID(esym),
                            PP_PUNCT("->"),
                            PP_ID(st_info),
                        PP_PUNCT(")"),
                        PP_PUNCT("&"),
                        PP_NUM(0xf),
                    PP_PUNCT(")"),
                PP_PUNCT(")"),
                PP_PUNCT("&"),
                PP_NUM(0xf),
            PP_PUNCT(")"),
        PP_PUNCT(")"),
        PP_PUNCT(";"),
        }
    },
    { "#define X(a) X(a)\nX(42)", { PP_ID(X), PP_PUNCT("("), PP_NUM(42), PP_PUNCT(")"), } },
    // GCC allows a macro expansion to include defined
    { R"(
#define W
#define V 0x0502
#define X (V >= 0x0502 || !defined (W))
#if X && !defined (__C)
1
#endif)", { PP_NUM(1) } },
#endif
    { R"(
# define __MINGW_NAME_AW(func) func##A
# define __MINGW_NAME_AW_EXT(func,ext) func##A##ext
# define __MINGW_NAME_UAW(func) func##_A
# define __MINGW_NAME_UAW_EXT(func,ext) func##_A_##ext
# define __MINGW_STRING_AW(str) str	/* same as TEXT() from winnt.h */
# define __MINGW_PROCNAMEEXT_AW "A"
#define RPC_CALL_ATTRIBUTES_V1 __MINGW_NAME_UAW(RPC_CALL_ATTRIBUTES_V1)
typedef RPC_CALL_ATTRIBUTES_V1 RPC_CALL_ATTRIBUTES;
)", {  PP_ID(typedef), PP_ID(RPC_CALL_ATTRIBUTES_V1_A), PP_ID(RPC_CALL_ATTRIBUTES), PP_PUNCT(";") } },
    { "#define __inline inline\n#define inline __inline\ninline\n", { PP_ID(inline) } },
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
            std::cerr << "Failure while processing\n" << delim << t.text << "\n" << delim << "\n";
            throw;
        }
    }
}

#undef PP_NUM
#undef PP_PUNCT
#undef PP_STR
#undef PP_ID

enum class reg_name {
    RAX, RCX, RDX, RBX,
    RSP, RBP, RSI, RDI,
    R8,  R9,  R10, R11,
    R12, R13, R14, R15,

    XMM0 = 0, XMM1, XMM2, XMM3,
    XMM4, XMM5, XMM6, XMM7,

    NONE = 0xff
};

enum class reg_type {
    b, w, d, q, x
};

class reg {
public:
    constexpr explicit reg(reg_name name, reg_type type) : name_{name}, type_{type} {}
    constexpr reg_name name() const { return name_; }
    constexpr reg_type type() const { return type_; }
private:
    reg_name name_;
    reg_type type_;
};

constexpr const reg AL{reg_name::RAX, reg_type::b};
constexpr const reg CL{reg_name::RCX, reg_type::b};

constexpr const reg EAX{reg_name::RAX, reg_type::d};

constexpr const reg RAX{reg_name::RAX, reg_type::q};
constexpr const reg RCX{reg_name::RCX, reg_type::q};
constexpr const reg RDX{reg_name::RDX, reg_type::q};
constexpr const reg RBX{reg_name::RBX, reg_type::q};
constexpr const reg RSP{reg_name::RSP, reg_type::q};
constexpr const reg RBP{reg_name::RBP, reg_type::q};
constexpr const reg RSI{reg_name::RSI, reg_type::q};
constexpr const reg RDI{reg_name::RDI, reg_type::q};
constexpr const reg R8{reg_name::R8, reg_type::q};
constexpr const reg R9{reg_name::R9, reg_type::q};

constexpr const reg XMM0{reg_name::XMM0, reg_type::x};
constexpr const reg XMM1{reg_name::XMM1, reg_type::x};
constexpr const reg XMM2{reg_name::XMM2, reg_type::x};
constexpr const reg XMM3{reg_name::XMM3, reg_type::x};

const char* const reg_names_8[16]     = { "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b" };
const char* const reg_names_8_rex[16] = { "al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b" };
const char* const reg_names_16[16]    = { "ax", "cx", "dx", "bx", "sp", "bp", "si", "di" , "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w" };
const char* const reg_names_32[16]    = { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"  };
const char* const reg_names_64[16]    = { "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15" };

std::ostream& operator<<(std::ostream& os, const reg& r) {
    if (r.name() == reg_name::NONE) {
        return os << "NONE-REGISTER";
    }
    os << "%";
    const auto ri = static_cast<int>(r.name());
    switch (r.type()) {
    case reg_type::b: return os << reg_names_8_rex[ri];
    case reg_type::w: return os << reg_names_16[ri];
    case reg_type::d: return os << reg_names_32[ri];
    case reg_type::q: return os << reg_names_64[ri];
    case reg_type::x: return os << "xmm" << ri;
    default: NOT_IMPLEMENTED(static_cast<int>(r.type()));
    }
}

reg reg_for_type(ctype t, reg_name r = reg_name::RAX) {
    switch (sizeof_type(t)) {
    case 1: return reg{r, reg_type::b};
    case 2: return reg{r, reg_type::w};
    case 4: return reg{r, reg_type::d};
    case 8: return reg{r, reg_type::q};
    default:
        NOT_IMPLEMENTED(t << " " << reg_names_64[static_cast<int>(r)]);
    }
}

class memory_ref_sib {
public:
    constexpr explicit memory_ref_sib(reg_name base, reg_name index, int scale, int disp) : base_{base}, index_{index}, scale_{scale}, disp_{disp} {
        assert(scale_ == 1 || scale_ == 2 || scale_ == 4 || scale_ == 8);
        assert(base != reg_name::NONE);
    }
    friend std::ostream& operator<<(std::ostream& os, const memory_ref_sib& m) {
        os << "[" << reg{m.base_, reg_type::q};
        if (m.index_ != reg_name::NONE) {
            os << "+" << reg{m.index_, reg_type::q};
            if (m.scale_ > 1) os << "*" << m.scale_;
        }
        if (m.disp_) {
            if (m.disp_ > 0) os << "+";
            os << m.disp_;
        }
        return os << "]";
    }
private:
    reg_name base_;
    reg_name index_;
    int scale_;
    int disp_;
};


std::string current_section;
std::string next_comment_;

#define NEXT_COMMENT(args) do { std::ostringstream oss_; oss_<<args; if (!next_comment_.empty()) next_comment_ += "; "; next_comment_ += oss_.str(); } while (0)

template<typename... Args>
void emit(const std::string& inst, Args&&... args) {
    std::cout << "\t" << inst;
    int n = 0;
    auto output_one = [&](const auto& a) { std::cout << (n++ ? ", ": "\t") << a; };
    (output_one(args), ...);
    if (!next_comment_.empty()) {
        std::cout << "\t\t# " << next_comment_;
        next_comment_.clear();
    }
    std::cout << "\n";
}

void emit_start() {
    emit(".intel_syntax", "prefix");
}

void emit_zeros(size_t num_bytes) {
    assert(current_section != "bss");
    emit(".zero", num_bytes);
}

void emit_section_change(const std::string& s) {
    assert(s == "text" || s == "data" || s == "rodata" || s == "bss");

    if (s != current_section) {
        emit(".section", "." + s);
        current_section = s;
    }
}

void emit_extern_decl(const std::string& id) {
    emit(".extern", id);
}

void emit_global_decl(const std::string& id) {
    emit(".global", id);
}

void emit_label(const std::string& l) {
    std::cout << l << ":";
    if (!next_comment_.empty()) {
        std::cout << "\t\t# " << next_comment_;
        next_comment_.clear();
    }
    std::cout << "\n";
}

void emit_copy(size_t sz) {
    std::string suffix = "B";
    if (sz % 8 == 0) {
        sz /= 8;
        suffix = "Q";
    } else if (sz % 4 == 0) {
        sz /= 4;
        suffix = "D";
    } else if (sz % 2 == 0) {
        sz /= 2;
        suffix = "W";
    }
    if (sz > 1) {
        emit("MOV", RCX, sz);
        emit("REP", "MOVS" + suffix);
    } else {
        emit("MOVS" + suffix);
    }
}

const char* compare_cond(token_type op, bool unsigned_) {
    switch (op) {
    case token_type::lt:    return unsigned_ ? "B" : "L";
    case token_type::lteq:  return unsigned_ ? "BE" : "LE";
    case token_type::eqeq:  return "E";
    case token_type::noteq: return "NE";
    case token_type::gt:    return unsigned_ ? "A" : "G";
    case token_type::gteq:  return unsigned_ ? "AE" : "GE";
    default: NOT_IMPLEMENTED(op);
    }
}

memory_ref_sib reg_off_str(reg_name r, int offset) {
    return memory_ref_sib{r, reg_name::NONE, 1, offset};
}

memory_ref_sib rbp_str(int offset) {
    return reg_off_str(reg_name::RBP, offset);
}

std::string name_mangle(const std::string& id) { return id; }

std::string op_size_str(ctype t) {
    switch (t) {
    case ctype::int_t:
    case ctype::long_t:
        return "DWORD PTR ";
    case ctype::pointer_t:
    case ctype::long_long_t:
        return "QWORD PTR ";
    }
    NOT_IMPLEMENTED(t);
}

bool unsigned_arit(ctype t)  {
    assert(base_type(t) <= ctype::pointer_t);
    return base_type(t) == ctype::pointer_t || !!(t & ctype::unsigned_f);
}

std::string arit_op_name(token_type t, bool is_unsigned) {
    switch (t) {
    case token_type::plus:   return "ADD";
    case token_type::minus:  return "SUB";
    case token_type::star:   return is_unsigned ? "MUL" : "IMUL";
    case token_type::mod:    [[fallthrough]];
    case token_type::div:    return is_unsigned ? "DIV" : "IDIV";
    case token_type::and_:   return "AND";
    case token_type::xor_:   return "XOR";
    case token_type::or_:    return "OR";
    case token_type::lshift: return is_unsigned ? "SHL" : "SAL";
    case token_type::rshift: return is_unsigned ? "SHR" : "SAR";
    }
    NOT_IMPLEMENTED(t);
}

std::string data_decl(ctype ct) {
    switch (base_type(ct)) {
    case ctype::plain_char_t:
    case ctype::char_t:
        return ".byte";
    case ctype::short_t:
        return ".short";
    case ctype::int_t:
    case ctype::long_t:
        return ".int";
    case ctype::long_long_t:
    case ctype::pointer_t:
        return ".quad";
    }
    NOT_IMPLEMENTED(ct);
}

reg arg_reg(int n, bool integral) {
    const reg iarg_regs[4] = { RCX, RDX, R8, R9 };
    const reg farg_regs[4] = { XMM0, XMM1, XMM2, XMM3 };
    assert(n < 4);
    return integral ? iarg_regs[n] : farg_regs[n];
}

constexpr const memory_ref_sib ref_RAX{reg_name::RAX, reg_name::NONE, 1, 0};
constexpr const memory_ref_sib ref_RCX{reg_name::RCX, reg_name::NONE, 1, 0};
constexpr const memory_ref_sib ref_RAX_RCX{reg_name::RAX, reg_name::RCX, 1, 0};
constexpr const memory_ref_sib ref_RSP{reg_name::RSP, reg_name::NONE, 1, 0};

class test_visitor {
public:
    explicit test_visitor() {}
    ~test_visitor() {
        assert(!break_labels_);
        assert(!continue_labels_);
        assert(!switches_);
    }

    void do_all(const parse_result& pr) {
        emit_start();
        std::vector<const symbol*> syms;
        for (const auto& s : scope_symbols(pr.global_scope())) {
            const auto& dt = s->decl_type();
            if (s->has_const_int_def() || !dt || !!(dt->ct() & ctype::typedef_f)) {
                continue;
            }
            if (!s->referenced() && !s->definition()) {
                continue;
            }

            if (!s->definition() && (!!(dt->ct() & ctype::extern_f) || dt->base() == ctype::function_t)) {
                add_extern(*s);
            } else {
                if (!(dt->ct() & ctype::static_f)) {
                    emit_global_decl(s->id());
                }
                syms_.push_back(sym_info{s.get(), 0});
                syms.push_back(s.get());
            }
        }
        // Data first
        std::sort(syms.begin(), syms.end(), [](const symbol* l, const symbol* r) {
            return (l->decl_type()->base() == ctype::function_t) < (r->decl_type()->base() == ctype::function_t);
        });
        //bool skip = true;
        for (const auto s: syms) {
            if (s->decl_type()->base() == ctype::function_t) {
                handle_function(*s->definition());
            } else {
                handle_global_data(*s);
            }
        }

        if (!string_literals_.empty()) {
            emit_section_change("rodata");
            for (const auto& sl: string_literals_) {
                emit_label(sl.second);
                emit_string_literal(sl.first, 0);
            }
        }
    }

    void handle(const expression& e) {
        return visit(*this, e);
    }

    void handle(const statement& s) {
        try {
            visit(*this, s);
        } catch (...) {
            std::cerr << "At " << s.pos() << "\n";
            throw;
        }
    }

    //
    // Expression
    //
    void operator()(const identifier_expression& e) {
        const auto& si = find_sym(e.sym());
        NEXT_COMMENT(e.sym().id());
        if (!si.offset) {
            emit("MOV", RAX, name_mangle(si.sym->id()));
        } else {
            emit("LEA", RAX, rbp_str(si.offset));
        }
    }
    void operator()(const const_int_expression& e) {
        emit("MOV", RAX, e.val().val);
    }
    void operator()(const const_float_expression& e) {
        const double dval = e.val();
        if (dval == 0.0) {
            NEXT_COMMENT("0.0");
            emit("PXOR", XMM0, XMM0);
            return;
        }

        emit_section_change("rodata");
        uint64_t uval;
        std::memcpy(&uval, &dval, sizeof(double));
        const auto l = make_label();
        emit_label(l);
        NEXT_COMMENT(dval);
        emit(data_decl(ctype::long_long_t), uval);
        emit_section_change("text");
        emit("MOVSD", XMM0, "[" + l + "]");
    }
    void operator()(const string_lit_expression& e) {
        emit("MOV", RAX, add_string_lit(e.text()));
    }
    void operator()(const initializer_expression& e) {
        if (e.et()->base() != ctype::reference_t || e.et()->reference_val()->base() != ctype::array_t) {
            NOT_IMPLEMENTED(e << " : " << *e.et());
        }
        const auto elem_t = e.et()->reference_val()->array_val().t();
        if (!is_integral(elem_t->base())) {
            NOT_IMPLEMENTED(e << " : " << *e.et() << " elem type: " << *elem_t);
        }

        std::ostringstream oss;
        for (const auto& i: e.es()) {
            if (!oss.str().empty()) oss << ", ";
            oss << const_int_eval(*i);
        }

        emit_section_change("data");
        const auto l = make_label();
        emit_label(l);
        emit(data_decl(elem_t->ct()), oss.str());
        emit_section_change("text");
        emit("MOV", RAX, l);
    }
    void operator()(const array_access_expression& e) {
        const auto& at = decay(e.a().et());
        handle_and_convert(e.a(), at);
        emit("PUSH", RAX);
        handle_and_convert(e.i(), at, true);
        emit("POP", RCX);
        emit("LEA", RAX, ref_RAX_RCX);
    }
    void operator()(const function_call_expression& e) {
        struct stack_arg {
            type_ptr t;
            int      offset;
        };
        std::vector<stack_arg> args;

        size_t stack_adj_size = 0;

        auto ft = to_rvalue(e.f().et());
        if (ft->base() == ctype::pointer_t) {
            ft = ft->pointer_val();
        }
        if (ft->base() != ctype::function_t) {
            NOT_IMPLEMENTED(e << " : " << *ft);
        }

        const auto& ptypes = ft->function_val().params();
        assert(e.args().size() == ptypes.size() || to_rvalue(e.f().et())->function_val().variadic());
        for (size_t i = 0, sz = e.args().size(); i < sz; ++i) {
            const auto& aa = *e.args()[i];
            type_ptr t = decay(aa.et());
            if (i < ptypes.size()) {
                t = ptypes[i].t();
            } else {
                t = var_arg_type(t);
            }
            const auto align = alignof_type(*t);
            stack_adj_size = round_up(stack_adj_size, align);
            args.push_back(stack_arg{t, static_cast<int>(stack_adj_size)});
            stack_adj_size = stack_adj_size + round_up(sizeof_type(*t), 8);
        }
        stack_adj_size = round_up(std::max(32ULL, stack_adj_size), 16);
        emit("SUB", RSP, stack_adj_size);

        for (size_t i = e.args().size(); i--; ) {
            const auto bt = args[i].t->base();
            if (!is_arithmetic(bt) && bt != ctype::pointer_t) {
                NOT_IMPLEMENTED(*args[i].t);
            }
            handle_and_convert(*e.args()[i], args[i].t);
            const bool int_arg = is_integral(bt);
            emit(int_arg ? "MOV" : "MOVSD", reg_off_str(reg_name::RSP, args[i].offset), int_arg ? RAX : XMM0);
        }

        handle(e.f());
        if(!e.args().empty()){
            for (size_t i = 0; i < std::min(4ULL, e.args().size()); ++i) {
                const bool int_arg = is_integral(args[i].t->base());
                emit(int_arg ? "MOV" : "MOVSD", arg_reg(static_cast<int>(i), int_arg), reg_off_str(reg_name::RSP, args[i].offset));
            }
        }
        emit("CALL", RAX);
        emit("ADD", RSP, stack_adj_size);
    }
    void operator()(const access_expression& e) {
        if (e.op() == token_type::arrow) {
            auto t = decay(e.e().et());
            if (t->base() == ctype::pointer_t) {
                handle_and_convert(e.e(), t);
                NEXT_COMMENT(*t->pointer_val() << "->" << e.m().id());
                emit("ADD", RAX, e.m().pos());
                return;
            }
        } else if (e.op() == token_type::dot) {
            if (e.e().et()->base() == ctype::reference_t) {
                const auto& rt = *e.e().et()->reference_val();
                if (rt.base() == ctype::struct_t || rt.base() == ctype::union_t) {
                    handle(e.e());
                    NEXT_COMMENT((rt.base() == ctype::struct_t ? rt.struct_val().id() : rt.union_val().id()) << "." << e.m().id());
                    emit("ADD", RAX, e.m().pos());
                }
                return;
            }
        }
        NOT_IMPLEMENTED(e << ", " << *e.e().et());
    }
    void operator()(const sizeof_expression& e) { NOT_IMPLEMENTED(e); }
    void operator()(const prefix_expression& e) {
        const auto op = e.op();
        if (op == token_type::plusplus || op == token_type::minusminus) {
            handle(e.e());
            handle_incr_decr(op, e.e().et()->reference_val(), true);
            return;
        } else if (op == token_type::star || op == token_type::and_) {
            handle(e.e());
            return;
        } else if (op == token_type::not_) {
            handle_and_convert(e.e(), e.et());
            if (e.et()->ct() != ctype::bool_t) {
                NOT_IMPLEMENTED(e << " : " << *e.et());
            }
            emit("NOT", AL);
            return;
        } else if (op == token_type::minus) {
            const auto& dt = e.et();
            handle_and_convert(e.e(), dt);
            if (is_integral(dt->base())) {
                emit("NEG", reg_for_type(dt->base()));
                return;
            } else if (is_floating_point(dt->base())) {
                if (dt->base() == ctype::double_t || dt->base() == ctype::long_double_t) {
                    static struct sign_const {
                        sign_const() {
                            emit_section_change("rodata");
                            emit_label("__xmm_sign64");
                            NEXT_COMMENT("64-bit signbit");
                            emit(data_decl(ctype::long_long_t), "0x8000000000000000");
                            emit_section_change("text");
                        }
                    } sign_const_;
                    emit("MOVSD", XMM1, "[__xmm_sign64]");
                    emit("PXOR", XMM0, XMM1);
                    return;
                }
            }
        } else if (op == token_type::bnot) {
            const auto& dt = e.et();
            assert(is_integral(dt->base()));
            handle_and_convert(e.e(), dt);
            emit("NOT", reg_for_type(dt->base()));
            return;
        }
        NOT_IMPLEMENTED(e << " : " << *e.e().et() << " --> " << *e.et());
    }
    void operator()(const postfix_expression& e) {
        const auto op = e.op();
        handle(e.e());
        if (op == token_type::plusplus || op == token_type::minusminus) {
            handle_incr_decr(op, e.e().et()->reference_val(), false);
            return;
        }
        NOT_IMPLEMENTED(e << " : " << *e.et());
    }
    void operator()(const cast_expression& e) {
        handle_and_convert(e.e(), e.et());
    }

    void push_fp(const reg& r = XMM0) {
        emit("SUB", RSP, 8);
        emit("MOVSD", ref_RSP, r);
    }

    void pop_fp() {
        emit("ADD", RSP, 8);
    }

    void pop_fp(const reg& r) {
        emit("MOVSD", r, ref_RSP);
        pop_fp();
    }

    void handle_fp_op(const binary_expression& e) {
        const auto& ct = e.common_t();
        handle_and_convert(e.r(), ct);
        push_fp();

        const auto suffix = ct->base() == ctype::float_t ? "SS" : "SD";

        if (is_assignment_op(e.op())) {
            handle(e.l());

            if (e.op() == token_type::eq) {
                pop_fp(XMM0);
            } else {
                handle_load(ct);
                emit(arit_op_name(without_assignment(e.op()), true) + suffix, XMM0, ref_RSP);
                pop_fp();
            }
            handle_store(ref_RAX, ct);
            return;
        }


        handle_and_convert(e.l(), ct);

        std::string inst;
        if (is_comparison_op(e.op())) {
            inst = "UCOMI";
        } else {
            inst = arit_op_name(e.op(), true);
        }
        emit(inst + suffix, XMM0, ref_RSP);
        if (is_comparison_op(e.op())) {
            const auto l = make_label();
            // Return false if parity set (result unordered)
            emit("XOR", EAX, EAX);
            emit("JP", l);
            emit("SET" + std::string(compare_cond(e.op(), true)), AL);
            emit_label(l);
        }
        pop_fp();
    }

    void operator()(const binary_expression& e) {
        const auto op = e.op();

        if (op == token_type::comma) {
            handle(e.l());
            handle(e.r());
            return;
        }

        const auto& ct = e.common_t();
        if (op == token_type::andand || op == token_type::oror) {
            const bool is_and = op == token_type::andand;
            const auto l_end  = make_label();
            NEXT_COMMENT(op << " start");
            handle_and_convert(e.l(), ct);
            emit("TEST", AL, AL);
            emit(is_and ? "JZ" : "JNZ", l_end);
            handle_and_convert(e.r(), ct);
            NEXT_COMMENT(op << " end");
            emit_label(l_end);
            return;
        }

        if (is_floating_point(ct->ct())) {
            handle_fp_op(e);
            return;
        }

        if ((ct->base() == ctype::struct_t || ct->base() == ctype::union_t) && e.op() == token_type::eq) {
            handle(e.r());
            emit("PUSH", RAX);
            handle(e.l());
            emit("MOV", RDI, RAX);
            emit("POP", RSI);
            NEXT_COMMENT("Copy " << *ct);
            emit_copy(sizeof_type(*ct));
            return;
        }

        if (ct->base() != ctype::pointer_t && !is_integral(ct->base())) {
            NOT_IMPLEMENTED(e << " : " << *ct);
        }

        if (is_comparison_op(op)) {
            handle_and_convert(e.r(), ct);
            emit("PUSH", RAX);
            handle_and_convert(e.l(), ct);
            emit("POP", RDX);
            handle_compare(op, ct);
            return;
        }

        if (is_assignment_op(op)) {
            handle_and_convert(e.r(), ct, op != token_type::eq);
            emit("PUSH", RAX);
            handle(e.l());
            emit("MOV", RCX, RAX);

            if (op == token_type::eq) {
                emit("POP", RAX);
            } else {
                handle_load(ct);
                emit("POP", RDX);
                handle_arit_op(without_assignment(op), ct);
            }
            handle_store(ref_RCX, e.l().et()->reference_val());
            return;
        }

        handle_and_convert(e.r(), ct, true);
        emit("PUSH", RAX);
        handle_and_convert(e.l(), ct, true);
        emit("POP", RDX);
        handle_arit_op(op, ct);
    }

    void operator()(const conditional_expression& e) {
        const auto l_rhs = make_label();
        const auto l_end = make_label();
        NEXT_COMMENT("? begin");
        handle(e.cond());
        emit("TEST", AL, AL);
        emit("JZ", l_rhs);
        handle(e.l());
        emit("JMP", l_end);
        NEXT_COMMENT("? rhs");
        emit_label(l_rhs);
        handle(e.r());
        NEXT_COMMENT("? end");
        emit_label(l_end);
    }

    //
    // Statement
    //
    void operator()(const empty_statement&) {
    }

    void operator()(const declaration_statement& s) {
        for (const auto& ds: s.ds()) {
            if (!ds->has_init_val()) {
                continue;
            }
            NEXT_COMMENT(ds->sym().id());
            const auto& si = find_sym(ds->sym());
            const auto& t = si.sym->decl_type();
            if (is_integral(t->base()) || t->base() == ctype::pointer_t) {
                handle_and_convert(ds->init_expr(), t);
                handle_store(rbp_str(si.offset), t);
            } else {
                if (dynamic_cast<const initializer_expression*>(&ds->init_expr())) {
                    emit_section_change("rodata");
                    const auto l = make_label();
                    emit_label(l);
                    emit_initializer(*t, ds->init_expr());
                    emit_section_change("text");
                    emit("MOV", RSI, l);
                } else {
                    handle(ds->init_expr());
                    emit("MOV", RSI, RAX);
                }
                emit("LEA", RDI, rbp_str(si.offset));
                emit_copy(sizeof_type(*t));
            }
        }
    }
    void operator()(const labeled_statement& s) {
        if (s.is_case_label()) {
            const auto l = make_label();
            const auto v = const_int_eval(s.e());
            NEXT_COMMENT("case " << v);
            emit_label(l);
            if (!switches_) NOT_IMPLEMENTED("case outside switch");
            assert(break_labels_);
            switches_->add_case(v, l);
            handle(s.s());
            emit("JMP", break_labels_->label());
            return;
        } else if (s.is_default_label()) {
            const auto l = make_label();
            NEXT_COMMENT("default");
            emit_label(l);
            if (!switches_) NOT_IMPLEMENTED("default outside switch");
            assert(break_labels_);
            switches_->add_default(l);
            handle(s.s());
            emit("JMP", break_labels_->label());
            return;
        } else {
            assert(s.is_normal_label());
            const auto& id = s.label().id();
            NEXT_COMMENT(id);
            assert(local_labels_.find(id) != local_labels_.end());
            emit_label(local_labels_[id]);
            handle(s.s());
            return;
        }
    }
    void operator()(const compound_statement& s) {
        for (const auto& s2: s.ss()) {
            handle(*s2);
        }
    }
    void operator()(const expression_statement& s) {
        handle(s.e());
    }
    void operator()(const if_statement& s) {
        const auto l_end   = make_label();
        const auto l_else  = s.else_s() ? make_label() : l_end;
        NEXT_COMMENT("if start");
        handle(s.cond());
        emit("TEST", AL, AL);
        emit("JZ", l_else);
        NEXT_COMMENT("if part");
        handle(s.if_s());
        if (s.else_s()) {
            emit("JMP", l_end);
            NEXT_COMMENT("if else");
            emit_label(l_else);
            handle(*s.else_s());
        }
        NEXT_COMMENT("if end");
        emit_label(l_end);
    }
    void operator()(const switch_statement& s) {
        const auto l_end   = make_label();
        const auto l_e    = make_label();
        switch_stack_node sn{switches_};
        label_stack_node bl{break_labels_, l_end};
        const auto t = decay(s.e().et());
        NEXT_COMMENT("switch begin");
        emit("JMP", l_e);
        handle(s.s());
        NEXT_COMMENT("switch e");
        emit_label(l_e);
        handle_and_convert(s.e(), t);
        const auto r = reg_for_type(t->ct());
        for (const auto& c: sn.cases()) {
            emit("CMP", r, cast(c.first, t->ct()));
            emit("JE", c.second);
        }

        if (auto l = sn.default_label(); !l.empty()) {
            emit("JMP", l);
        }

        NEXT_COMMENT("switch end");
        emit_label(l_end);
    }
    void operator()(const while_statement& s) {
        const auto l_start = make_label();
        const auto l_end   = make_label();
        label_stack_node bl{break_labels_, l_end};
        label_stack_node cl{continue_labels_, l_end};
        NEXT_COMMENT("while cond");
        emit_label(l_start);
        handle(s.cond());
        emit("TEST", AL, AL);
        emit("JZ", l_end);
        NEXT_COMMENT("while body");
        handle(s.s());
        emit("JMP", l_start);
        NEXT_COMMENT("while end");
        emit_label(l_end);
    }
    void operator()(const do_statement& s) {
        const auto l_start = make_label();
        const auto l_cond = make_label();
        const auto l_end   = make_label();
        label_stack_node bl{break_labels_, l_end};
        label_stack_node cl{continue_labels_, l_cond};
        NEXT_COMMENT("do start");
        emit_label(l_start);
        handle(s.s());
        NEXT_COMMENT("do cond");
        emit_label(l_cond);
        handle(s.cond());
        emit("TEST", AL, AL);
        emit("JNZ", l_start);
        NEXT_COMMENT("do end");
        emit_label(l_end);
    }
    void operator()(const for_statement& s) {
        handle(s.init());
        const auto l_start = make_label();
        const auto l_end   = make_label();
        label_stack_node bl{break_labels_, l_end};
        label_stack_node cl{continue_labels_, l_end};
        NEXT_COMMENT("for cond");
        emit_label(l_start);
        if (s.cond()) {
            handle(*s.cond());
            emit("TEST", AL, AL);
            emit("JZ", l_end);
        }
        NEXT_COMMENT("for body");
        handle(s.body());
        if (s.iter()) {
            NEXT_COMMENT("for iter");
            handle(*s.iter());
        }
        emit("JMP", l_start);
        NEXT_COMMENT("for end");
        emit_label(l_end);
    }
    void operator()(const goto_statement& s) {
        auto it = local_labels_.find(s.target().id());
        if (it == local_labels_.end()) {
            NOT_IMPLEMENTED(s);
        }
        NEXT_COMMENT("goto " << it->first);
        emit("JMP", it->second);
    }
    void operator()(const continue_statement&) {
        if (!continue_labels_) {
            NOT_IMPLEMENTED("continue outside loop");
        }
        emit("JMP", continue_labels_->label());
    }
    void operator()(const break_statement&) {
        if (!break_labels_) {
            NOT_IMPLEMENTED("break outside loop");
        }
        emit("JMP", break_labels_->label());
    }
    void operator()(const return_statement& s) {
        assert(!end_label_.empty());
        if (s.e()) {
            handle_and_convert(*s.e(), func_ret_type_);
        }
        emit("JMP", end_label_);
    }

private:
    class label_stack_node {
    public:
        explicit label_stack_node(label_stack_node*& s, const std::string& label) : s_{s}, prev_{s_}, label_{label} {
            s_ = this;
        }
        label_stack_node(const label_stack_node&) = delete;
        label_stack_node& operator=(const label_stack_node&) = delete;
        ~label_stack_node() {
            assert(s_ == this);
            s_ = prev_;
        }
        const std::string& label() const { return label_; }
    private:
        label_stack_node*& s_;
        label_stack_node*  prev_;
        std::string label_;
    };
    class switch_stack_node {
    public:
        explicit switch_stack_node(switch_stack_node*& s) : s_{s}, prev_{s_} {
            s_ = this;
        }
        switch_stack_node(const switch_stack_node&) = delete;
        switch_stack_node& operator=(const switch_stack_node&) = delete;
        ~switch_stack_node() {
            assert(s_ == this);
            s_ = prev_;
        }

        const std::string& default_label() const { return default_; }
        const std::map<const_int_val, std::string>& cases() const { return cases_; }

        void add_case(const const_int_val& val, const std::string& label) {
            assert(!label.empty());
            if (auto [it, inserted] = cases_.emplace(val, label); !inserted) {
                NOT_IMPLEMENTED("Duplicate case label for " << val << " " << it->second << " and " << label);
            }
        }
        void add_default(const std::string& label) {
            assert(!label.empty());
            if (!default_.empty()) {
                NOT_IMPLEMENTED("Duplicate default labels");
            }
            default_ = label;
        }

    private:
        switch_stack_node*& s_;
        switch_stack_node*  prev_;
        std::map<const_int_val, std::string> cases_;
        std::string default_;
    };
    struct sym_info {
        const symbol* sym;
        int offset;
    };
    std::vector<sym_info> syms_;
    int next_label_ = 0;
    std::string end_label_;
    std::map<std::string, std::string> local_labels_;
    type_ptr func_ret_type_;
    std::map<std::string, std::string> string_literals_;
    label_stack_node* break_labels_ = nullptr;
    label_stack_node* continue_labels_ = nullptr;
    switch_stack_node* switches_ = nullptr;

    std::string add_string_lit(const std::string& text) {
        if (auto it = string_literals_.find(text); it != string_literals_.end()) {
            return it->second;
        }
        const auto l = make_label();
        string_literals_.emplace(text, l);
        return l;
    }

    void add_sym(const symbol& sym, int offset) {
        assert(!try_find_sym(sym));
        assert(offset);
        syms_.push_back(sym_info{&sym, offset});
    }

    void add_extern(const symbol& sym) {
        assert(!try_find_sym(sym));
        emit_extern_decl(sym.id());
        syms_.push_back(sym_info{&sym, 0});
    }

    std::string make_label() { return "L" + std::to_string(next_label_++); }

    const sym_info* try_find_sym(const symbol& s) const {
        auto it = std::find_if(syms_.crbegin(), syms_.crend(), [&s](const auto& si) { return si.sym == &s; });
        if (it == syms_.crend()) {
            return nullptr;
        }
        return &*it;
    }

    const sym_info& find_sym(const symbol& s) const {
        auto sym = try_find_sym(s);
        if (!sym) {
            NOT_IMPLEMENTED("Unknown symbol " << s.id());
        }
        return *sym;
    }

   size_t handle_locals(const scope& sc, size_t current_size) {
        for (const auto& s: scope_symbols(sc)) {
            if (!s->decl_type()) continue;
            const auto size  = sizeof_type(*s->decl_type());
            const auto align = alignof_type(*s->decl_type());
            current_size = round_up(current_size + size, align);
            add_sym(*s, -static_cast<int>(current_size));
        }
        for (const auto& c: scope_children(sc)) {
            current_size = handle_locals(*c, current_size);
        }
        return current_size;
    }

   void handle_function(const init_decl& d)  {
       syms_.push_back(sym_info{&d.sym(), 0});

        emit_section_change("text");
        NEXT_COMMENT(d.d());
        emit_label(name_mangle(d.d().id()));
        emit("PUSH", RBP);
        emit("MOV", RBP, RSP);

        assert(local_labels_.empty());
        int offset = 0x10;
        int arg_cnt = 0;
        for (const auto& s: scope_symbols(d.local_scope())) {
            if (!s->decl_type()) {
                assert(local_labels_.find(s->id()) == local_labels_.end());
                local_labels_.emplace(s->id(), make_label());
                continue;
            }
            const auto& t = s->decl_type();
            const auto align = alignof_type(*t);
            const auto size  = sizeof_type(*t);
            offset = static_cast<int>(round_up(offset, align));
            add_sym(*s, offset);

            if (arg_cnt < 4) {
                if (!is_arithmetic(t->base()) && t->base() != ctype::pointer_t && t->base() != ctype::array_t) NOT_IMPLEMENTED(decl(t, s->id()));
                NEXT_COMMENT(s->id());
                const bool int_arg = is_integral(t->base());
                emit(int_arg ? "MOV" : "MOVSD", rbp_str(offset), arg_reg(arg_cnt, int_arg));
            }

            offset += static_cast<int>(round_up(size, 8));
            ++arg_cnt;
        }
        const auto& ls = scope_children(d.local_scope());
        assert(ls.size() == 1);
        const auto local_size = handle_locals(*ls[0], 16);
        emit("SUB", RSP, round_up(local_size, 16));
        assert(end_label_.empty());
        end_label_ = make_label();
        func_ret_type_ = d.d().t()->function_val().ret_type();
        handle(d.body());
        assert(!end_label_.empty());
        emit_label(end_label_);
        end_label_.clear();
        func_ret_type_.reset();
        local_labels_.clear();
        emit("MOV", RSP, RBP);
        emit("POP", RBP);
        emit("RET");
    }

    void emit_string_literal(const std::string& text, size_t min_size) {
        std::ostringstream oss;
        oss << "\"";
        for (const auto c : text) {
            if (c < 32 || c > 127 || c == '\"') {
                const char* od = "01234567";
                oss << "\\" << od[(c>>6)&7] << od[(c>>3)&7] << od[c&7];
            } else {
                oss << c;
            }
        }
        for (size_t i = text.size() + 1; i < min_size; ++i) {
            oss << "\000";
        }
        oss << "\"";

        emit(".asciz", oss.str());
    }

    void emit_initializer(const type& t, const expression& init) {
        if (t.base() == ctype::array_t) {
           const auto& av = t.array_val();
           if (av.bound() == array_info::unbounded) {
               NOT_IMPLEMENTED(t << " " << init);
           }
           if (auto sl = dynamic_cast<const string_lit_expression*>(&init)) {
               if (sizeof_type(*av.t()) != 1) {
                   NOT_IMPLEMENTED(t << " " << init);
               }
               emit_string_literal(sl->text(), av.bound());
               return;
           } else if (auto il = dynamic_cast<const initializer_expression*>(&init)) {
               const int64_t missing = static_cast<int64_t>(av.bound()) - static_cast<int64_t>(il->es().size());
               if (missing < 0) {
                   NOT_IMPLEMENTED(t << " " << init << " missing: " << missing);
               }
               for (const auto& e: il->es()) {
                   emit_initializer(*av.t(), *e);
               }
               if (missing) {
                   NEXT_COMMENT(missing << " x " << *av.t());
                   emit_zeros(sizeof_type(*av.t())*missing);
               }
               return;
           }
           NOT_IMPLEMENTED(t << " " << init);
        } else if (t.base() == ctype::struct_t) {
            const auto& sv = t.struct_val();
            auto il = dynamic_cast<const initializer_expression*>(&init);
            if (!il) {
                NOT_IMPLEMENTED(t << " " << init);
            }
            if (sv.members().empty() || il->es().size() > sv.members().size()) {
                NOT_IMPLEMENTED(t << " " << init);
            }
            size_t offset = 0;
            for (size_t i = 0; i < il->es().size(); ++i) {
                const auto& m = sv.members()[i];
                if (const auto pad = m.pos() - offset) {
                    NEXT_COMMENT(pad << " bytes of padding");
                    emit_zeros(pad);
                    offset += pad;
                }
                NEXT_COMMENT("pos " << m.pos() << " offset " << offset << " " << m.id());
                emit_initializer(*m.t(), *il->es()[i]);
                offset += sizeof_type(*m.t());
            }

            const auto& last_initialized_member = sv.members()[il->es().size()-1];
            const auto extra = sv.size() - (last_initialized_member.pos() + sizeof_type(*last_initialized_member.t()));

            if (extra) {
                NEXT_COMMENT(extra << " bytes of padding" << (il->es().size() == sv.members().size() ? "" : " and missing initializers"));
                emit_zeros(extra);
            }
            return;
        } else if (t.base() == ctype::pointer_t) {
            const auto decl = data_decl(t.ct());
            if (auto sl = dynamic_cast<const string_lit_expression*>(&init)) {
                if (sizeof_type(*t.pointer_val()) != 1) {
                    NOT_IMPLEMENTED(t << " " << init);
                }
                emit(decl, add_string_lit(sl->text()));
                return;
            } else if (auto ce = dynamic_cast<const cast_expression*>(&init); ce && ce->et()->base() == ctype::pointer_t) {
                // HACK HACK
                const auto ival = const_int_eval(ce->e());
                emit(decl, ival);
                return;
            }
            emit(decl, const_int_eval(init).val);
            return;
        } else if (!is_integral(t.base())) {
            NOT_IMPLEMENTED(t << " " << init);
        }

        emit(data_decl(t.ct()), const_int_eval(init).val);
    }

    static bool is_const_init_type(const type& t) {
        if (t.base() == ctype::array_t) {
            return is_const_init_type(*t.array_val().t());
        }
        return !!(t.ct() & ctype::const_f);
    }

    void handle_global_data(const symbol& sym) {
        const auto& t = *sym.decl_type();
        const auto l = name_mangle(sym.id());
        if (!sym.definition() || !sym.definition()->has_init_val()) {
            emit_section_change("bss");
            emit_label(l);
            emit(".skip", sizeof_type(t));
            return;
        }

        emit_section_change(is_const_init_type(t) ? "rodata" : "data");
        NEXT_COMMENT(sym.definition()->d());
        emit_label(l);
        emit_initializer(t, sym.definition()->init_expr());
    }

   void handle_conversion(const type_ptr& dst, const type_ptr& src, bool scale_pointers = false) {
        if (dst->base() == ctype::pointer_t && src->base() == ctype::pointer_t) {
            // Assume types have been checked elsewhere
            return;
        }
        if (types_equal(*dst, *src)) {
            return;
        }
        assert(dst->base() != ctype::reference_t);
        if (src->base() == ctype::reference_t) {
            const auto& rt = src->reference_val();
            if (rt->base() == ctype::array_t) {
                if (dst->base() != ctype::pointer_t) {
                    NOT_IMPLEMENTED("Conversion from " << *rt << " (" << *src << ") to " << *dst);
                }
            } else if (rt->base() == ctype::function_t) {
                if (dst->base() != ctype::pointer_t) {
                    NOT_IMPLEMENTED("Conversion from " << *rt << " (" << *src << ") to " << *dst);
                }
            } else {
                handle_load(rt);
                handle_conversion(dst, rt, scale_pointers);
            }
            return;
        }
        NEXT_COMMENT("Conversion from " << *src << " to " << *dst);

        if (dst->base() == ctype::bool_t) {
            const auto r = reg_for_type(src->ct());
            emit("TEST", r, r);
            emit("SETNZ", AL);
            return;
        }

        auto extend_int = [&src](bool zero_ext) {
            if (zero_ext && src->base() >= ctype::int_t) {
                // MOVZX RAX, EAX isn't valid
                return;
            }
            emit(zero_ext ? "MOVZX" : "MOVSX", RAX, reg_for_type(src->ct()));
        };

        if (is_integral(src->base()) && is_integral(dst->base())) {
            if (src->base() < dst->base()) {
                extend_int(unsigned_arit(dst->ct()));
            }
            return;
        }

        if (src->base() == ctype::pointer_t && is_integral(dst->base())) {
            return;
        }

        if (dst->base() == ctype::pointer_t && is_integral(src->base())) {
            if (src->base() < ctype::long_long_t) {
                extend_int(true);
            }
            // Must be pointer arithmetic
            if (scale_pointers) {
                const auto element_size = sizeof_type(*dst->pointer_val());
                if (element_size > 1) {
                    emit("IMUL", RAX, element_size);
                }
            }
            return;
        }

        if (is_floating_point(dst->ct())) {
            const std::string suffix = dst->base() == ctype::float_t ? "S" : "D";
            if (is_integral(src->base())) {
                if (src->base() < ctype::int_t) {
                    extend_int(!!(src->base() & ctype::unsigned_f));
                }
                emit("CVTSI2S" + suffix, XMM0, src->base() <= ctype::int_t ? EAX : RAX);
                return;
            } else if (src->base() == ctype::float_t) {
                assert(suffix == "D");
                emit("CVTSD2SS", XMM0, XMM0);
                return;
            } else if (src->base() == ctype::double_t || src->base() == ctype::long_double_t) {
                if (dst->base() == ctype::float_t) {
                    emit("CVTSS2SD", XMM0, XMM0);
                }
                return;
            }
        }

        if (is_floating_point(src->ct()) && is_integral(dst->ct())) {
            const std::string suffix = src->base() == ctype::float_t ? "S" : "D";
            emit("CVTS" + suffix + "2SI", dst->base() <= ctype::int_t ? EAX : RAX, XMM0);
            return;
        }


        NOT_IMPLEMENTED("conversion from " << *src << " to " << *dst << " scale_pointers = " << scale_pointers);
   }

    void handle_and_convert(const expression& e, const type_ptr& t, bool scale_pointers = false) {
        handle(e);
        handle_conversion(t, e.et(), scale_pointers);
    }

    template<typename Addr>
    void handle_load_store(const Addr& addr, const type_ptr& t, bool is_store) {
        if (is_store) {
            NEXT_COMMENT("Store as " << *t);
        } else {
            NEXT_COMMENT("Load as " << *t);
        }
        if (is_floating_point(t->base())) {
            const auto inst = t->base() == ctype::float_t ? "MOVSS" : "MOVSD";
            if (is_store) {
                emit(inst, addr, XMM0);
            } else {
                emit(inst, XMM0, addr);
            }
            return;
        }

        const auto r = reg_for_type(t->base());
        if (is_store) {
            emit("MOV", addr, r);
        } else {
            emit("MOV", r, addr);
        }
    }

    template<typename Addr>
    void handle_store(const Addr& addr, const type_ptr& t) {
        handle_load_store(addr, t, true);
    }

    template<typename Addr>
    void handle_load(const Addr& addr, const type_ptr& t) {
        handle_load_store(addr, t, false);
    }

    void handle_load(const type_ptr& t) {
        handle_load(ref_RAX, t);
    }

    // Assumes ops in RAX,RDX (for int/pointer values)
    void handle_compare(token_type op, const type_ptr& t) {
        const auto b = t->base();
        if (!is_integral(b) && b != ctype::pointer_t) {
            NOT_IMPLEMENTED(op << " " << *t);
        }
        emit("CMP", reg_for_type(b, reg_name::RAX), reg_for_type(b, reg_name::RDX));
        emit("SET" + std::string(compare_cond(op, unsigned_arit(t->ct()))), AL);
    }
    // Assumes ops in RAX,RDX (for int/pointer values)
    void handle_arit_op(token_type op, const type_ptr& t) {
        const auto b = t->base();
        if (!is_integral(b) && b != ctype::pointer_t) {
            NOT_IMPLEMENTED(op << " " << *t);
        }
        const auto inst = arit_op_name(op, unsigned_arit(t->ct()));
        if (op == token_type::star || op == token_type::div || op == token_type::mod) {
            emit(inst, reg_for_type(b, reg_name::RDX));
            if (op == token_type::mod) {
                emit("MOV", RAX, RDX);
            }
        } else if (op == token_type::lshift || op == token_type::rshift) {
            emit("MOV", RCX, RDX);
            emit(inst, reg_for_type(b, reg_name::RAX), CL);
        } else {
            emit(inst, reg_for_type(b, reg_name::RAX), reg_for_type(b, reg_name::RDX));
        }
    }

    void handle_incr_decr(token_type op, const type_ptr& t, bool is_prefix) {
        assert(op == token_type::plusplus || op == token_type::minusminus);
        const auto plus = op == token_type::plusplus;

        if (is_integral(t->base()) || t->base() == ctype::pointer_t) {
            auto e = [&]() {
                std::ostringstream oss;
                oss << op_size_str(t->base()) << ref_RAX;
                if (t->base() == ctype::pointer_t) {
                    emit(plus ? "ADD" : "SUB", oss.str(), sizeof_type(*t->pointer_val()));
                } else {
                    emit(plus ? "INC" : "DEC", oss.str());
                }
            };
            if (is_prefix) {
                e();
                handle_load(t);
            } else {
                emit("PUSH", RAX);
                handle_load(t);
                emit("MOV", RCX, RAX);
                emit("POP", RAX);
                e();
                emit("MOV", RAX, RCX);
            }
            return;
        }
        NOT_IMPLEMENTED(op << " " << *t << " " << is_prefix);
    }
};

void process_one(source_manager& sm, const std::string& filename) {
    test_visitor vis{};
    std::cerr << "Parsing " << filename << "\n";
    const auto pr = parse(sm, sm.load(filename));
    std::cerr << "Generating code...\n";
    vis.do_all(pr);
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
        sm.define_standard_headers("x86intrin.h", "");
        sm.define_standard_headers("emmintrin.h", "");
        sm.define_standard_headers("psdk_inc/intrin-impl.h", "");

        for (const auto& f: files) {
            process_one(sm, f);
        }
    } catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
}
