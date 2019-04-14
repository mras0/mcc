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

// Unary:
//    { token_type::and_        , "" }       // &
//    { token_type::star        , "" }       // *
//    { token_type::plus        , "" }       // +
//    { token_type::minus       , "" }       // -
//    { token_type::bnot        , "" }       // ~
//    { token_type::not_        , "" }       // !
//    { token_type::plusplus    , "" }       // ++
//    { token_type::minusminus  , "" }       // --
//
// Binary (arithmetic):
//    { token_type::mod         , "" }       // %
//    { token_type::and_        , "" }       // &
//    { token_type::star        , "" }       // *
//    { token_type::plus        , "" }       // +
//    { token_type::minus       , "" }       // -
//    { token_type::div         , "" }       // /
//    { token_type::lshift      , "" }       // <<
//    { token_type::rshift      , "" }       // >>
//    { token_type::xor_        , "" }       // ^
//    { token_type::or_         , "" }       // |
//
// Sequencing:
//    { token_type::comma       , "" }       // ,
//
// Access:
//    { token_type::arrow       , "" }       // ->
//    { token_type::dot         , "" }       // .
//
// Assignment:
//    { token_type::eq          , "" }       // =
//
// Logical
//    { token_type::andand      , "" }       // &&
//    { token_type::oror        , "" }       // ||
//
// Comparison
//    { token_type::lt          , "" }       // <
//    { token_type::lteq        , "" }       // <=
//    { token_type::eqeq        , "" }       // ==
//    { token_type::noteq       , "" }       // !=
//    { token_type::gt          , "" }       // >
//    { token_type::gteq        , "" }       // >=

bool is_comparison_op(token_type op) {
    
    return op == token_type::lt
        || op == token_type::lteq
        || op == token_type::eqeq
        || op == token_type::noteq
        || op == token_type::gt
        || op == token_type::gteq;
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

class test_visitor {
public:
    explicit test_visitor() : global_scope_{*this} {}

    class expr_res {
    public:
        explicit expr_res() : t_{nullptr}, s_{} {
        }

        explicit expr_res(const std::shared_ptr<const type>& t, const std::string& s) : t_{t}, s_{s} {
        }

        const std::shared_ptr<const type>& t() const { return t_; }
        const std::string& s() const { return s_; }
        void s(const std::string& s) { s_ = s; }

        friend std::ostream& operator<<(std::ostream& os, const expr_res& er) {
            return os << *er.t_ << ":" << er.s_;
        }
    private:
        std::shared_ptr<const type> t_;
        std::string s_;
    };

    auto handle(const expression& e) {
        return visit(*this, e);
    }

    auto handle(const statement& s) {
        return visit(*this, s);
    }

    template<typename... T>
    void comment(T&&... args) {
        std::cout << "\t; ";
        ((std::cout << args << " "), ...);
        std::cout << "\n";
    }

    template<typename... T>
    void output(const std::string& instruction, T&&... args) {
        std::cout << "\t" << instruction;
        bool first = true;
        auto process = [&](const auto& a) {
            if (first) {
                first = false;
                std::cout << "\t";
            } else {
                std::cout << ", ";
            }
            std::cout << a;
        };
        (process(args), ...);
        std::cout << "\n";
    }

    void put_label(const std::string& l) {
        std::cout << l << ":\n";
    }

    auto value_type(const std::shared_ptr<const type>& t) {
        if (t->base() == ctype::reference_t) {
            return t->reference_val();
        }
        return t;
    }

    expr_res to_rvalue(const expr_res& r) {
        if (r.t()->base() == ctype::reference_t) {
            auto t = temp_reg();
            output("MOV", t, "[" + r.s() + "]");
            return expr_res{r.t()->reference_val(), t};
        }
        return r;
    }

    expr_res cast(const expr_res& r, ctype new_type) {
        if (r.t()->ct() == new_type) {
            return r;
        }
        NOT_IMPLEMENTED("cast " << r << " to " << new_type);
    }

    expr_res test_value(const std::string& res_r, const expr_res& r) {
        auto v = to_rvalue(r);
        if (v.t()->ct() != ctype::int_t) {
            NOT_IMPLEMENTED(v);
        }
        clear_reg(res_r);
        output("TEST", v.s(), v.s());
        output("SETNZ", res_r);
        return expr_res{std::make_shared<type>(ctype::int_t), res_r};
    }

    void clear_reg(const std::string& r) {
        output("XOR", r, r);
    }

    //
    // Expression
    //
    expr_res operator()(const identifier_expression& e) {
        if (auto er = lookup(e.id())) {
            auto r = temp_reg();
            comment(r,"=",e.id());
            output("LEA", r, er->s());
            return expr_res{std::make_shared<type>(ctype::reference_t, er->t()), r};
        }
        NOT_IMPLEMENTED("lookup failed for " << e);
    }
    expr_res operator()(const const_int_expression& e) {
        auto r = temp_reg();
        output("MOV", r, e.val().val);
        return expr_res{std::make_shared<type>(e.val().type), r};
    }
    expr_res operator()(const const_float_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const string_lit_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const initializer_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const array_access_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const function_call_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const access_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const sizeof_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const unary_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const cast_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const binary_expression& e) {
        auto op = e.op();
        auto l = handle(e.l());
        if (op == token_type::oror || op == token_type::andand) {
            const bool is_or = op == token_type::oror;
            auto lend  = label();
            auto res_reg = temp_reg();
            comment(res_reg,"= !!"+l.s());
            auto lres = test_value(res_reg, l);
            output("TEST", lres.s(), lres.s());
            output(is_or ? "JNZ" : "JZ", lend);
            auto rval = handle(e.r());
            comment(res_reg,"= !!"+rval.s());
            (void)test_value(lres.s(), rval);
            put_label(lend);
            return lres;
        }


        auto rval = to_rvalue(handle(e.r()));
        const auto lt = value_type(l.t())->ct();
        const auto res_t = common_type(lt, rval.t()->ct());

        rval = cast(rval, res_t);
        if (is_assignment_op(op)) {
            NOT_IMPLEMENTED(e.op() << " " << l << " " << rval << " -> " << res_t);
        }

        auto lval = cast(to_rvalue(l), res_t);

        auto res_r = temp_reg();

        if (is_comparison_op(op)) {
            comment(res_r,"=",lval.s(),op,rval.s());
            clear_reg(res_r);
            output("CMP", lval.s(), rval.s());
            output(std::string("SET") + compare_cond(op, !!(res_t & ctype::unsigned_f)), res_r + "b");
        } else {
            NOT_IMPLEMENTED(lval << op << rval);
        }
        return expr_res{std::make_shared<type>(res_t), res_r};
    }
    expr_res operator()(const conditional_expression& e) { NOT_IMPLEMENTED(e); }
    expr_res operator()(const expression& e) { NOT_IMPLEMENTED(e); }

    //
    // Statement
    //
    void operator()(const empty_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const declaration_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const labeled_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const compound_statement& s) {
        for (const auto& s2: s.ss()) {
            visit(*this, *s2);
        }
    }
    void operator()(const expression_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const if_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const switch_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const while_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const do_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const for_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const goto_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const continue_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const break_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const return_statement& s) {
        if (s.e()) {
            auto retval = visit(*this, *s.e());
            output("MOV","EAX",retval.s());
        }
        output("JMP", current_scope_->return_label());
    }
    void operator()(const statement& s) { NOT_IMPLEMENTED(s); }

    void do_decl(const init_decl& decl_and_val) {
        const auto& d = decl_and_val.d();

        auto er = current_scope_->lookup(d.id());
        if (er) {
            if (!!((er->t()->ct() ^ d.t()->ct()) & ~ctype::storage_f)) {
                NOT_IMPLEMENTED("Invalid redefinition of " << *er << " as " << d);
            }
        } else {
            er = current_scope_->add(d);
        }

        if (decl_and_val.has_init_val()) {
            if (!er->s().empty()) {
                NOT_IMPLEMENTED(*er << " already initialized! " << decl_and_val);
            }

            if (er->t()->base() == ctype::function_t) {
                handle_function(d, decl_and_val.body());
                er->s(d.id() + "_now_initialized");
            } else {
                std::cout << "TODO: Initialize global " << d << "\n";
            }
        }
    }

    void handle_function(const decl& d, const compound_statement& s) {
        //
        // cdecl stack layout after call to foo(1,2,3) and standard prologue:
        //
        //  +-----------------+----------------------------+      ^
        //  | 3               | [EBP+0x10]                 |      |
        //  | 2               | [EBP+0x0C]                 |      |
        //  | 1               | [EBP+0x08]                 |    Higher addresses
        //  | Return address  | [EBP+0x04]                 |
        //  | Old EBP         | [EBP]                      |    
        //  | Locals...       | [EBP-0x04]                 |    Lower addresses
        //  | ...             | [EBP-....]                 |      |
        //  | Stack top       | [EBP-locals_size] = [ESP]  |      |
        //  +-----------------+----------------------------+      v

        assert(d.t()->base() == ctype::function_t);
        const auto& fi = d.t()->function_val();
        scope function_scope{*this};
        function_scope.return_label("__" + d.id() + "_end");
        put_label("_" + d.id());
        output("PUSH", "EBP");
        output("MOV", "EBP", "ESP");
        size_t offset = 8;
        for (const auto& a: fi.params()) {
            auto er = function_scope.add(a);
            er->s("[EBP+" + std::to_string(offset) + "]");
            offset += 4;
        }
        handle(s);
        put_label(function_scope.return_label());
        output("MOV", "ESP", "EBP");
        output("POP", "EBP");
        output("RET");
    }

private:
    class scope {
    public:
        explicit scope(test_visitor& parent) : parent_(parent), prev_scope_{parent_.current_scope_} {
            parent_.current_scope_ = this;
        }
        ~scope() {
            assert(parent_.current_scope_ == this);
            parent_.current_scope_ = prev_scope_;
        }
        scope(const scope&) = delete;
        scope& operator=(const scope&) = delete;

        scope* prev() {
            return prev_scope_;
        }

        std::string return_label() const {
            if (!return_label_.empty()) {
                return return_label_;
            } else if (prev_scope_) {
                return prev_scope_->return_label();
            }
            NOT_IMPLEMENTED("no return label?");
        }

        void return_label(const std::string& label) {
            assert(return_label_.empty());
            return_label_ = label;
        }

        expr_res* lookup(const std::string& id) {
            if (auto it = syms_.find(id); it != syms_.end()) {
                return &it->second;
            }
            return nullptr;
        }

        expr_res* add(const decl& d) {
            assert(!d.id().empty());
            auto [it, inserted] = syms_.insert({d.id(), expr_res{d.t(), ""}});
            if (!inserted) {
                NOT_IMPLEMENTED(d << " already defined");
            }
            return &it->second;
        }
    private:
        test_visitor&                   parent_;
        scope*                          prev_scope_;
        std::string                     return_label_;
        std::map<std::string, expr_res> syms_;
    };
    scope* current_scope_ = nullptr;
    scope global_scope_;
    int reg_counter_ = 0;
    int label_counter_ = 0;

    expr_res* lookup(const std::string& id) {
        for (auto scope = current_scope_; scope; scope = scope->prev()) {
            if (auto er = scope->lookup(id)) {
                return er;
            }
        }
        return nullptr;
    }

    std::string temp_reg() {
        return "r" + std::to_string(reg_counter_++);
    }

    std::string label() {
        return "l" + std::to_string(label_counter_++);
    }
};

void process_one(source_manager& sm, const std::string& filename) {
    std::cout << filename << "\n";
    auto decls = parse(sm, sm.load(filename));
    test_visitor vis{};
    for (const auto& d: decls) {
        vis.do_decl(*d);
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