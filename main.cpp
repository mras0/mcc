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
#endif
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

std::string current_section;
std::string next_comment_;

#define NEXT_COMMENT(args) do { std::ostringstream oss_; oss_<<args; if (!next_comment_.empty()) next_comment_ += "; "; next_comment_ += oss_.str(); } while (0)

void emit_section_change(const std::string& s) {
    assert(s == "code" || s == "data" || s == "rodata" || s == "bss");
    if (s != current_section) {
        std::cout << "\tSECTION ." << s << "\n";
        current_section = s;
    }
}

void emit_extern_decl(const std::string& id) {
    std::cout << "\tEXTERN " << id << "\n";
}

void emit_label(const std::string& l) {
    std::cout << l << ":";
    if (!next_comment_.empty()) {
        std::cout << "\t\t; " << next_comment_;
        next_comment_.clear();
    }
    std::cout << "\n";
}

template<typename... Args>
void emit(const std::string& inst, Args&&... args) {
    std::cout << "\t" << inst;
    int n = 0;
    auto output_one = [&](const auto& a) { std::cout << (n++ ? ", ": "\t") << a; };
    (output_one(args), ...);
    if (!next_comment_.empty()) {
        std::cout << "\t\t; " << next_comment_;
        next_comment_.clear();
    }
    std::cout << "\n";
}

void emit_zeros(size_t num_bytes) {
    assert(current_section != "bss");
    emit("TIMES " + std::to_string(num_bytes) + " DB 0");
}

const char* compare_cond(token_type op, bool unsigned_) {
    switch (op) {
    case token_type::lt:    return unsigned_ ? "B" : "L";
    case token_type::lteq:  return unsigned_ ? "BE" : "LE";
    case token_type::eqeq:  return "EQ";
    case token_type::noteq: return "NE";
    case token_type::gt:    return unsigned_ ? "A" : "G";
    case token_type::gteq:  return unsigned_ ? "AE" : "GE";
    default: NOT_IMPLEMENTED(op);
    }
}

std::string reg_off_str(const std::string& reg, int offset) {
    return "[" + reg + (offset < 0 ? "" : "+") + std::to_string(offset) + "]";
}

std::string rbp_str(int offset) {
    return reg_off_str("RBP", offset);
}

std::string name_mangle(const std::string& id) { return id; }

std::string op_size_str(ctype t) {
    switch (t) {
    case ctype::int_t:
        return "DWORD ";
    }
    NOT_IMPLEMENTED(t);
}

enum r64_names {
    RAX, RCX, RDX, RBX,
    RSP, RBP, RSI, RDI,
    R8,  R9,  R10, R11,
    R12, R13, R14, R15
};

std::string reg_name_for_type(ctype t, r64_names r = RAX) {
    assert(r == RAX || r == RDX);
    switch (base_type(t)) {
    case ctype::bool_t:         [[fallthrough]];
    case ctype::plain_char_t:   [[fallthrough]];
    case ctype::char_t:         return r == RAX ? "AL"  : "DL";
    case ctype::short_t:        return r == RAX ? "AX"  : "DX";
    case ctype::long_t:         [[fallthrough]];
    case ctype::int_t:          return r == RAX ? "EAX" : "EDX";
    case ctype::pointer_t:      [[fallthrough]];
    case ctype::long_long_t:    return r == RAX ? "RAX" : "RDX";
    }
    NOT_IMPLEMENTED(t);
}

bool unsigned_arit(const type& t)  {
    assert(t.base() <= ctype::pointer_t);
    return t.base() == ctype::pointer_t || !!(t.ct() & ctype::unsigned_f);
}

std::string arit_op_name(token_type t, bool is_unsigned) {
    switch (t) {
    case token_type::plus:   return "ADD";
    case token_type::minus:  return "SUB";
    case token_type::star:   return is_unsigned ? "MUL" : "IMUL";
    case token_type::and_:   return "AND";
    case token_type::xor_:   return "XOR";
    case token_type::or_:    return "OR";
    case token_type::lshift: return is_unsigned ? "LSL" : "ASL";
    case token_type::rshift: return is_unsigned ? "LSR" : "ASR";
    }
    NOT_IMPLEMENTED(t);
}

std::string data_decl_suffix(ctype ct) {
    switch (base_type(ct)) {
    case ctype::plain_char_t:
    case ctype::char_t:
        return "B";
    case ctype::short_t:
        return "W";
    case ctype::int_t:
    case ctype::long_t:
        return "D";
    case ctype::long_long_t:
    case ctype::pointer_t:
        return "Q";
    }
    NOT_IMPLEMENTED(ct);
}

const char* const arg_regs[4] = {"RCX","RDX","R8","R9"};

class test_visitor {
public:
    explicit test_visitor() {}

    void do_all(const parse_result& pr) {
        std::vector<const symbol*> syms;
        for (const auto& s : scope_symbols(pr.global_scope())) {
            if (!s->definition()) {
                if (s->decl_type() && !(s->decl_type()->ct() & ctype::typedef_f) && s->referenced() && !s->has_const_int_def()) {
                    add_extern(*s);
                }
            } else {
                if (!(s->decl_type()->ct() & ctype::static_f)) {
                    std::cout << "\tGLOBAL " << s->id() << "\n";
                }
                syms.push_back(s.get());
            }
        }
        // Data first
        std::sort(syms.begin(), syms.end(), [](const symbol* l, const symbol* r) {
            return (l->decl_type()->base() == ctype::function_t) < (r->decl_type()->base() == ctype::function_t);
        });
        for (const auto s: syms) {
            //if (s->id() != "options_W") {
            //    std::cout << "Line " << __LINE__ << " Skipping " << s->id() << "\n";
            //    continue;
            //}

            if (s->decl_type()->base() == ctype::function_t) {
                handle_function(*s->definition());
            } else {
                handle_global_data(*s->definition());
            }
        }

#if 0
        for (const auto& d: pr.decls()) {
            const auto& t = d->d().t();
            if (!!(t->ct() & ctype::typedef_f) || d->d().id().empty()) {
                continue;
            }
            if (t->base() == ctype::function_t) {
                //if (d->has_init_val()) {
                //    handle_function(*d);
                //}
            } else {
                std::cout << d->d() << "\n";
                //if (!(t->ct() & ctype::extern_f)) {
                //    handle_global_data(*d);
                //}
            }
        }
#endif

        if (!string_literals_.empty()) {
            emit_section_change("rodata");
            for (const auto& sl: string_literals_) {
                emit_label(sl.second);
                emit_string_literal(sl.first);
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
            emit("MOV", "RAX", name_mangle(si.sym->id()));
        } else {
            emit("LEA", "RAX", rbp_str(si.offset));
        }
    }
    void operator()(const const_int_expression& e) {
        emit("MOV", "RAX", e.val().val);
    }
    void operator()(const const_float_expression& e) { NOT_IMPLEMENTED(e); }
    void operator()(const string_lit_expression& e) {
        emit("MOV", "RAX", add_string_lit(e.text()));
    }
    void operator()(const initializer_expression& e) {
        if (e.et()->base() != ctype::reference_t || e.et()->reference_val()->base() != ctype::array_t) {
            NOT_IMPLEMENTED(e << " : " << *e.et());
        }
        const auto elem_t = e.et()->reference_val()->array_val().t();
        if (!is_integral(elem_t->base()) || sizeof_type(*elem_t) != 1) {
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
        emit("DB", oss.str());
        emit_section_change("code");
        emit("MOV", "RAX", l);
    }
    void operator()(const array_access_expression& e) {
        const auto& at = decay(e.a().et());
        handle_and_convert(e.a(), at);
        emit("PUSH", "RAX");
        handle_and_convert(e.i(), at);
        emit("POP", "RCX");
        emit("LEA", "RAX", "[RAX+RCX]");
    }
    void operator()(const function_call_expression& e) {
        struct stack_arg {
            type_ptr t;
            int      offset;
        };
        std::vector<stack_arg> args;

        size_t stack_adj_size = 0;
        const auto& ptypes = to_rvalue(e.f().et())->function_val().params();
        assert(e.args().size() == ptypes.size() || to_rvalue(e.f().et())->function_val().variadic());
        for (size_t i = 0, sz = e.args().size(); i < sz; ++i) {
            const auto& aa = *e.args()[i];
            type_ptr t = decay(aa.et());
            if (!is_integral(t->base()) && t->base() != ctype::pointer_t) {
                NOT_IMPLEMENTED(aa << ":" << *t);
            }
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
        emit("SUB", "RSP", stack_adj_size);

        for (size_t i = e.args().size(); i--; ) {
            handle_and_convert(*e.args()[i], args[i].t);
            emit("MOV", reg_off_str("RSP", args[i].offset), "RAX");
        }

        handle(e.f());
        if(!e.args().empty()){
            for (size_t i = 0; i < std::min(4ULL, e.args().size()); ++i) {
                emit("MOV", arg_regs[i], reg_off_str("RSP", args[i].offset));
            }
        }
        emit("CALL", "RAX");
        emit("ADD", "RSP", stack_adj_size);        
    }
    void operator()(const access_expression& e) { NOT_IMPLEMENTED(e); }
    void operator()(const sizeof_expression& e) { NOT_IMPLEMENTED(e); }
    void operator()(const prefix_expression& e) {
        const auto op = e.op();
        handle(e.e());
        if (op == token_type::plusplus || op == token_type::minusminus) {
            handle_incr_decr(op, e.e().et()->reference_val(), true);
            return;
        }
        if (op == token_type::star) {
            return;
        }
        NOT_IMPLEMENTED(e << " : " << *e.et());
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

    void operator()(const binary_expression& e) {
        const auto op = e.op();

        if (op == token_type::comma) {
            NOT_IMPLEMENTED(e);
        }

        const auto& ct = e.common_t();
        if (op == token_type::andand || op == token_type::oror) {
            const bool is_and = op == token_type::andand;
            const auto l_end  = make_label();
            NEXT_COMMENT(op << " start");
            handle_and_convert(e.l(), ct);
            emit("TEST", "AL", "AL");
            emit(is_and ? "JZ" : "JNZ", l_end);
            handle_and_convert(e.r(), ct);
            NEXT_COMMENT(op << " end");
            emit_label(l_end);
            return;
        }

        if (is_comparison_op(op)) {
            if (!is_integral(ct->base())) NOT_IMPLEMENTED(e);
            handle_and_convert(e.r(), ct);
            emit("PUSH", "RAX");
            handle_and_convert(e.l(), ct);
            emit("POP", "RDX");
            handle_compare(op, ct);
            return;
        }

        if (is_assignment_op(op)) {
            if (!is_integral(ct->base())) NOT_IMPLEMENTED(e);

            handle_and_convert(e.r(), ct);
            emit("PUSH", "RAX");
            handle(e.l());
            emit("MOV", "RCX", "RAX");

            if (op == token_type::eq) {
                emit("POP", "RAX");
            } else {
                handle_load(ct);
                emit("POP", "RDX");
                handle_arit_op(without_assignment(op), ct);
            }
            handle_store("[RCX]", e.l().et()->reference_val());
            return;
        }

        handle_and_convert(e.r(), ct);
        emit("PUSH", "RAX");
        handle_and_convert(e.l(), ct);
        emit("POP", "RDX");
        handle_arit_op(op, ct);
    }

    void operator()(const conditional_expression& e) {
        const auto l_rhs = make_label();
        const auto l_end = make_label();
        NEXT_COMMENT("? begin");
        handle(e.cond());
        emit("TEST", "AL", "AL");
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
    void operator()(const empty_statement& s) {
        (void)s;
    }
    void operator()(const declaration_statement& s) {
        for (const auto& ds: s.ds()) {
            if (!ds->has_init_val()) {
                continue;
            }
            NEXT_COMMENT(ds->sym().id());
            const auto& si = find_sym(ds->sym());
            const auto& t = si.sym->decl_type();
            if (is_integral(t->base())) {
                handle_and_convert(ds->init_expr(), t);
                handle_store(rbp_str(si.offset), t);
            } else if (t->base() == ctype::array_t) {
                assert(ds->init_expr().et()->base() == ctype::reference_t && ds->init_expr().et()->reference_val()->base() == ctype::array_t);
                if (ds->init_expr().et()->reference_val()->array_val().bound() == array_info::unbounded) {
                    NOT_IMPLEMENTED(*ds);
                }
                const auto size = sizeof_type(*ds->init_expr().et()->reference_val());
                handle(ds->init_expr());
                emit("MOV", "RSI", "RAX");
                emit("LEA", "RDI", rbp_str(si.offset));
                emit("MOV", "RCX", size);
                emit("REP", "MOVSB");
            } else {
                NOT_IMPLEMENTED(*t);
            }
        }
    }
    void operator()(const labeled_statement& s) { NOT_IMPLEMENTED(s); }
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
        emit("TEST", "AL", "AL");
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
    void operator()(const switch_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const while_statement& s) {
        const auto l_start = make_label();
        const auto l_end   = make_label();
        NEXT_COMMENT("while cond");
        emit_label(l_start);
        handle(s.cond());
        emit("TEST", "AL", "AL");
        emit("JZ", l_end);
        NEXT_COMMENT("while body");
        handle(s.s());
        emit("JMP", l_start);
        NEXT_COMMENT("while end");
        emit_label(l_end);
    }
    void operator()(const do_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const for_statement& s) {
        handle(s.init());
        const auto l_start = make_label();
        const auto l_end   = make_label();
        NEXT_COMMENT("for cond");
        emit_label(l_start);
        if (s.cond()) {
            handle(*s.cond());
            emit("TEST", "AL", "AL");
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
    void operator()(const goto_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const continue_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const break_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const return_statement& s) {
        assert(!end_label_.empty());
        if (s.e()) {
            handle_and_convert(*s.e(), func_ret_type_);
        }
        emit("JMP", end_label_);
    }

private:
    struct sym_info {
        const symbol* sym;
        int offset;
    };
    std::vector<sym_info> syms_;
    int next_label_ = 0;
    std::string end_label_;
    type_ptr func_ret_type_;
    std::map<std::string, std::string> string_literals_;

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

        emit_section_change("code");
        NEXT_COMMENT(d.d());
        emit_label(name_mangle(d.d().id()));
        emit("PUSH", "RBP");
        emit("MOV", "RBP", "RSP");

        int offset = 0x10;
        int arg_cnt = 0;
        for (const auto& s: scope_symbols(d.local_scope())) {
            const auto& t = s->decl_type();
            const auto align = alignof_type(*t);
            const auto size  = sizeof_type(*t);
            offset = static_cast<int>(round_up(offset, align));
            add_sym(*s, offset);

            if (arg_cnt < 4) {
                if (!is_integral(t->base()) && t->base() != ctype::pointer_t && t->base() != ctype::array_t) NOT_IMPLEMENTED(decl(t, s->id()));
                NEXT_COMMENT(s->id());
                emit("MOV", rbp_str(offset), arg_regs[arg_cnt]);
            }

            offset += static_cast<int>(round_up(size, 8));
            ++arg_cnt;
        }
        const auto& ls = scope_children(d.local_scope());
        assert(ls.size() == 1);
        const auto local_size = handle_locals(*ls[0], 16);
        emit("SUB","RSP", round_up(local_size, 16));
        assert(end_label_.empty());
        end_label_ = make_label();
        func_ret_type_ = d.d().t()->function_val().ret_type();
        handle(d.body());
        assert(!end_label_.empty());
        emit_label(end_label_);
        end_label_.clear();
        func_ret_type_.reset();
        emit("MOV", "RSP", "RBP");
        emit("POP", "RBP");
        emit("RET");                
    }

    void emit_string_literal(const std::string& text) {
        std::ostringstream oss;
        std::string accum;
        auto dump_str = [&]() {
            if (!accum.empty()) {
                if (!oss.str().empty())
                    oss << ", ";
                oss << "'" << accum << "'";
                accum.clear();
            }
        };
        for (const auto c : text) {
            if (c < 32 || c > 127 || c == '\'') {
                dump_str();
                if (!oss.str().empty())
                    oss << ", ";
                oss << static_cast<int>(c);
            } else {
                accum += c;
            }
        }
        dump_str();
        if (!oss.str().empty())
            oss << ", ";
        oss << "0";

        emit("DB", oss.str());
    }

    void emit_initializer(const type& t, const expression& init) {
        if (t.base() == ctype::array_t) {
           const auto& av = t.array_val();
           if (av.bound() == array_info::unbounded) {
               NOT_IMPLEMENTED(t << " " << init);
           }
           if (auto sl = dynamic_cast<const string_lit_expression*>(&init)) {
               if (sizeof_type(*av.t()) != 1 || av.bound() != sl->text().size() + 1) {
                   NOT_IMPLEMENTED(t << " " << init);
               }
               emit_string_literal(sl->text());
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
        } else if (t.base() >= ctype::pointer_t) {
           if (auto sl = dynamic_cast<const string_lit_expression*>(&init)) {
               if (sizeof_type(*t.pointer_val()) != 1) {
                   NOT_IMPLEMENTED(t << " " << init);
               }
               emit("D"+data_decl_suffix(t.ct()), add_string_lit(sl->text()));
               return;
           } else if (auto ce = dynamic_cast<const cast_expression*>(&init); ce && ce->et()->base() == ctype::pointer_t) {
               // HACK HACK
               const auto ival = const_int_eval(ce->e());
               emit("D"+data_decl_suffix(t.ct()), ival);
               return;
           }
           NOT_IMPLEMENTED(t << " " << init);
        }

        emit("D"+data_decl_suffix(t.ct()), const_int_eval(init).val);
    }

    static bool is_const_init_type(const type& t) {
        if (t.base() == ctype::array_t) {
            return is_const_init_type(*t.array_val().t());
        }
        return !!(t.ct() & ctype::const_f);
    }

    void handle_global_data(const init_decl& id) {
       const auto& t = *id.d().t();
       const auto l = name_mangle(id.d().id());
       if (!id.has_init_val()) {
            emit_section_change("bss");
            emit_label(l);
            emit("RESB", sizeof_type(t));
            return;
       }       
       
       emit_section_change(is_const_init_type(t) ? "rodata" : "data");
       NEXT_COMMENT(id.d());
       emit_label(l);
       emit_initializer(t, id.init_expr());
       /*

       if (t->base() == ctype::array_t) {
           const auto& av = t->array_val();
           if (av.bound() == array_info::unbounded || av.t()->base() >= ctype::pointer_t) {
               NOT_IMPLEMENTED(id.d());
           }
           if (auto sl = dynamic_cast<const string_lit_expression*>(&id.init_expr())) {
               assert(sizeof_type(av.t()) == 1);
               emit_string_literal(sl->text(), l);
               return;
           } else if (auto il = dynamic_cast<const initializer_expression*>(&id.init_expr())) {
               std::cout << il->es().size() << " is the size\n";
           }

           std::cout << "TODO: D" << data_decl_suffix(av.t()->ct()) << "\n";
           std::cout << id.init_expr() << "\n";
           NOT_IMPLEMENTED("FIXME");
           return;
       }

       if (t->base() >= ctype::pointer_t) {
           NOT_IMPLEMENTED(id.d());
       }*/
   }

   void handle_conversion(const type_ptr& dst, const type_ptr& src) {
        if (types_equal(*dst, *src)) {
            return;
        }
        if (dst->base() == ctype::pointer_t && src->base() == ctype::pointer_t && is_compatible_pointer_type(dst->pointer_val(), src->pointer_val())) {
            return;
        }
        assert(dst->base() != ctype::reference_t);
        if (src->base() == ctype::reference_t) {
            const auto& rt = src->reference_val();
            if (rt->base() == ctype::array_t) {
                if (dst->base() != ctype::pointer_t) {
                    NOT_IMPLEMENTED("Conversion from " << *rt << " (" << *src << ") to " << *dst);
                }
            } else {
                handle_load(rt);
                handle_conversion(dst, rt);
            }
            return;
        }
        NEXT_COMMENT("Conversion from " << *src << " to " << *dst);

        if (is_arithmetic(src->base()) && is_arithmetic(dst->base())) {
            if (src->base() < dst->base()) {
                emit(unsigned_arit(*src) ? "MOVZX" : "MOVSX", "RAX", reg_name_for_type(src->ct()));
            }
            return;
        }

        if (dst->base() == ctype::pointer_t && is_integral(src->base())) {
            // Must be pointer arithmetic
            const auto element_size = sizeof_type(*dst->pointer_val());
            if (element_size > 1) {
                emit("IMUL", "RAX", element_size);
            }
            return;
        }
        

        NOT_IMPLEMENTED("conversion from " << *src << " to " << *dst);
   }

    void handle_and_convert(const expression& e, const type_ptr& t) {
        handle(e);
        handle_conversion(t, e.et());
    }

    void handle_load_store(const std::string& addr, const type_ptr& t, bool is_store) {
        std::string r = reg_name_for_type(t->base());
        if (is_store) {
            NEXT_COMMENT("Store as " << *t);
            emit("MOV", addr, r);
        } else {
            NEXT_COMMENT("Load as " << *t);
            emit("MOV", r, addr);
        }
    }
    
    void handle_store(const std::string& addr, const type_ptr& t) {
        handle_load_store(addr, t, true);
    }

    void handle_load(const std::string& addr, const type_ptr& t) {
        handle_load_store(addr, t, false);
    }

    void handle_load(const type_ptr& t) {
        handle_load("[RAX]", t);
    }

    // Assumes ops in RAX,RDX (for int/pointer values)
    void handle_compare(token_type op, const type_ptr& t) {
        const auto b = t->base();
        if (!is_integral(b)) {
            NOT_IMPLEMENTED(op << " " << *t);
        }
        emit("CMP", reg_name_for_type(b, RAX), reg_name_for_type(b, RDX));
        emit("SET" + std::string(compare_cond(op, unsigned_arit(*t))), "AL");
    }
    // Assumes ops in RAX,RDX (for int/pointer values)
    void handle_arit_op(token_type op, const type_ptr& t) {
        const auto b = t->base();
        if (!is_integral(b) && b != ctype::pointer_t) {
            NOT_IMPLEMENTED(op << " " << *t);
        }
        emit(arit_op_name(op, unsigned_arit(*t)), reg_name_for_type(b, RAX), reg_name_for_type(b, RDX));
    }

    void handle_incr_decr(token_type op, const type_ptr& t, bool is_prefix) {
        assert(op == token_type::plusplus || op == token_type::minusminus);
        const auto plus = op == token_type::plusplus;

        if (is_integral(t->base()) || t->base() == ctype::pointer_t) {
            auto e = [&]() {
                if (t->base() == ctype::pointer_t) {
                    emit(plus ? "ADD" : "SUB", "qword [RAX]", sizeof_type(*t->pointer_val()));
                } else {
                    emit(plus ? "INC" : "DEC", op_size_str(t->base()) + "[RAX]");
                }
            };
            if (is_prefix) {
                e();
                handle_load(t);
            } else {
                emit("PUSH", "RAX");
                handle_load(t);
                emit("MOV", "RCX", "RAX");
                emit("POP", "RAX");
                e();
                emit("MOV", "RAX", "RCX");
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