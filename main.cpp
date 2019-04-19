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

namespace mcc {
#if 0
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

bool types_equal(const type_ptr& l, const type_ptr& r) {
    if (l->base() != r->base()) return false;
    const auto b = l->base();

    if (l->ct() != r->ct()) {
        return false;
    }
    if (is_arithmetic(b) || b == ctype::void_t || b == ctype::bool_t) {
        return true;
    } else if (b == ctype::pointer_t) {
        return types_equal(l->pointer_val(), r->pointer_val());
    } else if (b == ctype::struct_t) {
        return &l->struct_val() == &r->struct_val();
    } else if (b == ctype::union_t) {
        return &l->union_val() == &r->union_val();
    } else if (b == ctype::function_t) {
        const auto& lf = l->function_val();
        const auto& rf = r->function_val();
        if (lf.variadic() ^ rf.variadic()) {
            return false;
        }
        if (!types_equal(lf.ret_type() , rf.ret_type())) {
            return false;
        }
        if (lf.params().size() != rf.params().size()) {
            return false;
        }
        for (size_t i = 0, sz = lf.params().size(); i < sz; ++i) {
            if (!types_equal(lf.params()[i].t(), rf.params()[i].t())) {
                return false;
            }
        }
        return true;
    }
    NOT_IMPLEMENTED(b << " " << *l <<  " " << *r);
}

bool is_convertible(const type_ptr& target_type, const type_ptr& source_type) {
    const auto bt = target_type->base();
    const auto bs = source_type->base();
    if (bt == ctype::bool_t) {
        return bs == ctype::bool_t || bs == ctype::pointer_t || is_arithmetic(bs);
    } else if (bs == ctype::bool_t) {
        return is_arithmetic(bt);
    } if (is_arithmetic(bt) && is_arithmetic(bs)) {
        return true;
    } else if (bt == ctype::pointer_t && bs == ctype::pointer_t) {
        const auto pt = target_type->pointer_val();
        const auto ps = source_type->pointer_val();
        if (pt->ct() == (ctype::const_f|ctype::void_t) || pt->ct() == ctype::void_t || ps->ct() == ctype::void_t) {
            return true;
        }
        return types_equal(pt, ps);
    } else if (bt == ctype::pointer_t && bs == ctype::int_t) {
        // Eww, 0 -> null pointer
        return true;
    }
    return false;
}

// Namespces: Labels, tags, struction/union members, other
class symbol_info {
public:
    explicit symbol_info(const std::string& id) : id_{id} {}

    const std::string& id() const { return id_; }
    const std::shared_ptr<const type>& t() const { return type_; }
    void* value() { return value_; }

    void set_type(const std::shared_ptr<const type>& type) {
        if (type_) {
            if (!types_equal(type_, type)) {
                NOT_IMPLEMENTED(id_ << " already declared as " << *type_ << " redeclared as " << *type);
            }
            return;
        }
        type_ = type;
    }

    void set_value(void* val) {
        assert(val);
        if (value_) {
            NOT_IMPLEMENTED(id_ << " already defined");
        }
        value_ = val;
    }

    void set_use_as_label() {
        label_flags_ |= 1;
    }
    void set_defined_as_label() {
        if (label_flags_ & 2) {
            NOT_IMPLEMENTED("Multiple definitions of label " << id_);
        }
        label_flags_ |= 2;
    }

private:
    std::string id_;
    std::shared_ptr<const type> type_;
    void* value_ = nullptr;
    int label_flags_ = 0;
};

class symbol_table {
public:
    explicit symbol_table() {}

    template<typename... Args>
    symbol_info* add_symbol(const std::string& id, Args&&... args) {
        assert(!id.empty());
        assert(!lookup(id));
        symbols_.push_back(std::make_unique<symbol_info>(id, std::forward<Args>(args)...));
        return symbols_.back().get();
    }

    symbol_info* lookup(const std::string_view id) {
        auto it = std::find_if(symbols_.begin(), symbols_.end(), [&id](const auto& s) { return s->id() == id; });
        return it != symbols_.end() ? it->get() : nullptr;
    }

private:
    std::vector<std::unique_ptr<symbol_info>> symbols_;
};

class scope_info {
public:
    explicit scope_info(scope_info*& prev, symbol_info* func_sym = nullptr) : prev_{prev}, prev_val_{prev}, func_sym_{func_sym} {
        prev = this;
    }
    ~scope_info() {
        assert(prev_ == this);
        prev_ = prev_val_;
    }

    scope_info(const scope_info&) = delete;
    scope_info& operator=(const scope_info&) = delete;

    scope_info& global_scope() {
        return prev_ ? prev_->global_scope() : *this;
    }

    scope_info& function_scope() {
        if (func_sym_) {
            return *this;
        } else if (prev_val_) {
            return prev_val_->function_scope();
        } else {
            NOT_IMPLEMENTED("called outside function");
        }
    }

    symbol_info& current_function() {
        return *function_scope().func_sym_;
    }

    template<typename... Args>
    symbol_info* add_symbol(Args&&... args) {
        return syms_.add_symbol(std::forward<Args>(args)...);
    }

    symbol_info* local_lookup(const std::string_view id) {
        return syms_.lookup(id);
    }

    symbol_info* get_or_add(const std::string_view id) {
        if (auto sym = syms_.lookup(id)) {
            return sym;
        }
        return add_symbol(std::string(id));
    }

    std::pair<symbol_info*, scope_info*> global_lookup(const std::string_view id) {
        if (auto sym = syms_.lookup(id)) {
            return {sym, this};
        } else if (prev_val_) {
            return prev_val_->global_lookup(id);
        } else {
            return {nullptr, nullptr};
        }
    }

private:
    scope_info*& prev_;
    scope_info* const prev_val_;
    symbol_info* func_sym_;
    symbol_table syms_;
};

class test_visitor {
public:
    explicit test_visitor() : global_scope_{current_scope_, false} {}

    type_ptr handle(const expression& e) {
        //std::cout << e << "\n";
        auto t = visit(*this, e);
        assert(t);
        e.set_expression_type(t);
        return t;
    }

    auto handle(const statement& s) {
        return visit(*this, s);
    }

    //
    // Expression
    //
    type_ptr operator()(const identifier_expression& e) {
        auto sym = current_scope_->global_lookup(e.id()).first;
        if (!sym || !sym->t()) {
            NOT_IMPLEMENTED("Reference to undefined symbol " << e.id());
        }
        e.set_symbol(*sym);
        return make_ref(remove_flags(sym->t(), ctype::storage_f));
    }
    type_ptr operator()(const const_int_expression& e) {
        return std::make_shared<type>(e.val().type);
    }
    type_ptr operator()(const const_float_expression& e) { NOT_IMPLEMENTED(e); }
    type_ptr operator()(const string_lit_expression& e) {
        auto ai = std::make_shared<array_info>(t_const_char, e.text().size()+1);
        return std::make_shared<type>(ctype::array_t, ai);
    }
    type_ptr operator()(const initializer_expression& e) { NOT_IMPLEMENTED(e); }
    type_ptr operator()(const array_access_expression& e) {
        auto ptr = decay(handle(e.a()));
        if (ptr->base() != ctype::pointer_t) {
            NOT_IMPLEMENTED("Not a pointer " << *ptr << " in " << e);
        }
        auto idx = to_rvalue(handle(e.i()));
        if (!is_integral(idx->ct())) {
            NOT_IMPLEMENTED("Invalid array index " << *idx << " in " << e);
        }
        return make_ref(ptr->pointer_val());
    }
    type_ptr operator()(const function_call_expression& e) {
        auto ft = decay(handle(e.f()));
        if (ft->base() != ctype::pointer_t || ft->pointer_val()->base() != ctype::function_t) {
            NOT_IMPLEMENTED("Expected function in " << e << " got " << *ft);
        }
        const auto& fi = ft->pointer_val()->function_val();
        std::vector<type_ptr> arg_types;
        for (const auto& a: e.args()) {
            arg_types.push_back(decay(handle(*a)));
        }

        if (arg_types.size() != fi.params().size() && (!fi.variadic() || arg_types.size() < fi.params().size())) {
            NOT_IMPLEMENTED(e << " " << fi);
        }

        for (size_t i = 0; i < fi.params().size(); ++i) {
            const auto& at = arg_types[i];
            const auto& p = fi.params()[i];
            if (!is_convertible(p.t(), at)) {
                NOT_IMPLEMENTED("Invalid argument for " << p.id() << " in " << e << " got " << *at << " expected "<< *p.t());
            }
        }

        return decay(fi.ret_type());
    }
    type_ptr operator()(const access_expression& e) {
        assert(e.op() == token_type::arrow || e.op() == token_type::dot);
        auto t = decay(handle(e.e()));
        if (e.op() == token_type::arrow) {
            if (t->base() != ctype::pointer_t) {
                NOT_IMPLEMENTED("Expected pointer in " << e << " got " << *t);
            }
            t = t->pointer_val();
        }
        if (t->base() == ctype::struct_t || t->base() == ctype::union_t) {
            if (!!(t->ct() & ctype::cvr_f)) {
                NOT_IMPLEMENTED(*t);
            }
            for (const auto& m : struct_union_members(*t)) {
                if (m.id() == e.id()) {
                    return make_ref(m.t());
                }
            }
            NOT_IMPLEMENTED(e.id() << " not found in " << *t);
        }

        NOT_IMPLEMENTED(e << " -> " << *t);
    }
    type_ptr operator()(const sizeof_expression&) {
        return t_size_t;
    }
    type_ptr operator()(const prefix_expression& e) {
        auto et = handle(e.e());
        switch (e.op()) {
        case token_type::and_:
            if (et->base() != ctype::reference_t) {
                NOT_IMPLEMENTED("Expected lvalue in " << e << " got " << *et);
            }
            return make_ptr(et->reference_val());
        case token_type::star:
            {
                et = decay(et);
                if (et->base() != ctype::pointer_t) {
                    NOT_IMPLEMENTED("Expected pointer in " << e << " got " << *et);
                }
                if (!!(et->ct() & ctype::cvr_f)) {
                    NOT_IMPLEMENTED(*et);
                }
                return make_ref(et->pointer_val());
            }
        case token_type::plus:
        case token_type::minus:
        case token_type::bnot:
            {
                et = decay(et);
                if (!is_arithmetic(et->base())) {
                    NOT_IMPLEMENTED("Invalid argument in " << e << ": " << *et);
                }
                return std::make_shared<type>(integral_promotion(et->ct()));
            }
        case token_type::not_:
            if (!is_convertible(t_bool, decay(et))) {
                NOT_IMPLEMENTED("Invalid argument in " << e << ": " << *et);
            }
            return t_bool;
        case token_type::plusplus:
        case token_type::minusminus:
            {
                if (et->base() != ctype::reference_t) {
                    NOT_IMPLEMENTED("Expected lvalue in " << e << " got " << *et);
                }
                if (!is_arithmetic(et->reference_val()->base()) && et->reference_val()->base() != ctype::pointer_t) {
                    NOT_IMPLEMENTED("Invalid type in " << e << " got " << *et->reference_val());
                }
                return et->reference_val();
            }
        default:
            NOT_IMPLEMENTED(e.op() << " " << *et);
        }
    }
    type_ptr operator()(const postfix_expression& e) {
        const auto op = e.op();
        auto et = handle(e.e());
        if (op == token_type::plusplus || op == token_type::minusminus) {
            if (et->base() != ctype::reference_t) {
                NOT_IMPLEMENTED("Expected lvalue in " << e << " got " << *et);
            }
            return et;
        }
        NOT_IMPLEMENTED(e << " " << *et);
    }
    type_ptr operator()(const cast_expression& e) {
        auto et = decay(handle(e.e()));
        if ((et->base() == ctype::pointer_t && e.t()->base() == ctype::pointer_t) || is_convertible(e.t(), et)) {
            return e.t();
        }
        NOT_IMPLEMENTED("cast " << *et << " to " << *e.t());
    }
    type_ptr operator()(const binary_expression& e) {
        const auto op = e.op();
        if (op == token_type::comma) {
            NOT_IMPLEMENTED(e);
        }
        if (op == token_type::andand || op == token_type::oror) {
            const auto lt = decay(handle(e.l()));
            if (!is_convertible(t_bool, lt)) {
                NOT_IMPLEMENTED("In " << e << " " << *lt << " is not convertible to bool");
            }
            const auto rt = decay(handle(e.r()));
            if (!is_convertible(t_bool, rt)) {
                NOT_IMPLEMENTED("In " << e << " " << *rt << " is not convertible to bool");
            }
            return t_bool;
        }

        auto lt = handle(e.l());
        auto lvalt = decay(lt);
        auto rt = decay(handle(e.r()));

        if (is_assignment_op(op) && lt->base() != ctype::reference_t) {
            NOT_IMPLEMENTED("Expected lvalue in " << e << " got " << *lt);
        }

        if (op == token_type::plus || op == token_type::minus) {
            if (lvalt->base() == ctype::pointer_t) {
                if (is_integral(rt->base())) {
                    return lvalt;
                } else if (op == token_type::minus && rt->base() == ctype::pointer_t) {
                    NOT_IMPLEMENTED("Pointer - pointer in " << e);
                }
            }
            if (is_integral(lvalt->base()) && rt->base() == ctype::pointer_t) {
                return rt;
            }
        } else if (op == token_type::pluseq || op == token_type::minuseq) {
            if (lvalt->base() == ctype::pointer_t && is_integral(rt->base())) {
                return lt;
            }
        }

        const auto ct = common_type(lvalt->ct(), rt->ct());
        if (is_comparison_op(op)) {
            return t_bool;
        } else if (is_assignment_op(op)) {
            if (lt->base() != ctype::reference_t) {
                NOT_IMPLEMENTED("Expected lvalue in " << e << " got " << *lt);
            }
            return lt;
        } else {
            return std::make_shared<type>(ct);
        }
    }
    type_ptr operator()(const conditional_expression& e) {
        auto cond_t = decay(handle(e.cond()));
        if (!is_convertible(t_bool, cond_t)) {
            NOT_IMPLEMENTED("Condition in " << e << " is not convertible to bool");
        }
        auto lt = decay(handle(e.l()));
        auto rt = decay(handle(e.r()));
        auto ct = common_type(lt->ct(), rt->ct());
        if (lt->ct() == ct) {
            return lt;
        }
        return std::make_shared<type>(common_type(lt->ct(), rt->ct()));
    }
    type_ptr operator()(const expression& e) { NOT_IMPLEMENTED(e); }

    //
    // Statement
    //
    void operator()(const empty_statement&) {
    }
    void operator()(const declaration_statement& s) {
        for (const auto& d: s.ds()) {
            const auto& id = d->d().id();
            auto dsym = current_scope_->get_or_add(id);
            const auto& t = d->d().t();
            if (t->base() == ctype::function_t) {
                NOT_IMPLEMENTED(*d);
            }
            dsym->set_type(t);

            if (d->has_init_val()) {
                auto init_type = decay(handle(d->init_expr()));
                if (!is_convertible(dsym->t(), init_type)) {
                    NOT_IMPLEMENTED(*dsym->t() << " vs " << *init_type << " for " << id);
                }
            }

            dsym->set_value((void*)1);
        }
    }
    void operator()(const labeled_statement& s) {
        if (!s.is_normal_label()) {
            // TODO: Handle case/default labels
            return;
        }
        const auto& id = s.label();
        auto& fs = current_scope_->function_scope();
        auto lsym = fs.get_or_add(id);
        lsym->set_defined_as_label();
        
    }
    void operator()(const compound_statement& s) {
        scope_info scope{current_scope_};
        for (const auto& s2: s.ss()) {
            handle(*s2);
        }
    }
    void operator()(const expression_statement& s) {
        (void)handle(s.e());
    }
    void operator()(const if_statement& s) {
        (void)handle(s.cond());
        {
            scope_info scope{current_scope_};
            handle(s.if_s());
        }
        if (s.else_s()) {
            scope_info scope{current_scope_};
            handle(*s.else_s());
        }
    }
    void operator()(const switch_statement& s) {
        (void)handle(s.e());
        scope_info scope{current_scope_};
        handle(s.s());
    }
    void operator()(const while_statement& s) {
        (void)handle(s.cond());
        scope_info scope{current_scope_};
        handle(s.s());
    }
    void operator()(const do_statement& s) { NOT_IMPLEMENTED(s); }
    void operator()(const for_statement& s) {
        scope_info scope{current_scope_};
        handle(s.init());
        if (s.cond()) (void)handle(*s.cond());
        if (s.iter()) (void)handle(*s.iter());
        handle(s.body());
    }
    void operator()(const goto_statement& s) {
        auto lsym = current_scope_->function_scope().get_or_add(s.target());
        lsym->set_use_as_label();
    }
    void operator()(const continue_statement&) { }
    void operator()(const break_statement&) { }
    void operator()(const return_statement& s) {
        auto ret_type = decay(current_scope_->current_function().t()->function_val().ret_type());
        if (!s.e()) {
            if (ret_type->ct() != ctype::void_t) {
                NOT_IMPLEMENTED("Missing return value in " << s << " expecting " << *ret_type);
            }
            return;
        }
        auto et = decay(handle(*s.e()));
        if (!is_convertible(ret_type, et)) {
            NOT_IMPLEMENTED("Invalid return type in " << s << " " << *et << " expecting " << *ret_type);
        }
    }
    void operator()(const statement& s) { NOT_IMPLEMENTED(s); }

    void do_function_decl(symbol_info& sym, const compound_statement& ss) {
        assert(sym.t() && sym.t()->base() == ctype::function_t);
        // TODO: Preserve scope?
        scope_info func_scope{current_scope_, &sym};
        sym.set_value((void*)1);
        auto& fi = sym.t()->function_val();
        if (fi.variadic()) {
            NOT_IMPLEMENTED(sym.id() << " " << *sym.t());
        }
        for (const auto& a: fi.params()) {
            if (a.t()->ct() == ctype::void_t) {
                NOT_IMPLEMENTED(*a.t());
            }
            auto asym = func_scope.add_symbol(a.id());
            asym->set_type(a.t());
            asym->set_value((void*)1);
        }
        // Don't use `handle(ss)` to avoid scope from compund statement
        for (const auto& s2: ss.ss()) {
            handle(*s2);
        }
    }

    void do_top_level_decl(const init_decl& decl_and_val) {
        const auto& d = decl_and_val.d();

        auto sym = global_scope_.get_or_add(d.id());
        sym->set_type(d.t());
        if (decl_and_val.has_init_val()) {
            if (d.t()->base() == ctype::function_t) {
                try {
                    do_function_decl(*sym, decl_and_val.body());
                } catch (...) {
                    std::cerr << "Error while processing " << d << "\n";
                    throw;
                }
            } else {
                sym->set_value((void*)1);
                std::cout << "TODO: Initialize global " << d << "\n";
            }
        }
    }
private:
    scope_info* current_scope_ = nullptr;
    scope_info global_scope_;

    const type_ptr t_bool       = std::make_shared<type>(ctype::bool_t);
    const type_ptr t_int        = std::make_shared<type>(ctype::int_t);
    const type_ptr t_const_char = std::make_shared<type>(ctype::const_f | ctype::plain_char_t);
    const type_ptr t_size_t     = std::make_shared<type>(ctype::unsigned_f | ctype::long_long_t);
};

#endif

} // namespace mcc

class empty_visitor {
public:
    explicit empty_visitor() {}

    auto handle(const expression& e) {
        return visit(*this, e);
    }

    auto handle(const statement& s) {
        return visit(*this, s);
    }

    void handle(const init_decl& id) {
        const auto& d = id.d();
        if (id.has_init_val() && d.t()->base() == ctype::function_t) {
            std::cout << "TODO: Handle " << d << "\n";
            handle(id.body());
        } else {
            // TODO: Handle variables and extern definitions
        }
    }

    //
    // Expression
    //
    void operator()(const identifier_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const const_int_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const const_float_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const string_lit_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const initializer_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const array_access_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const function_call_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const access_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const sizeof_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const prefix_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const postfix_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const cast_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const binary_expression& e) {
        NOT_IMPLEMENTED(e);
    }
    void operator()(const conditional_expression& e) {
        NOT_IMPLEMENTED(e);
    }

    //
    // Statement
    //
    void operator()(const empty_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const declaration_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const labeled_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const compound_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const expression_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const if_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const switch_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const while_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const do_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const for_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const goto_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const continue_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const break_statement& s) {
        NOT_IMPLEMENTED(s);
    }
    void operator()(const return_statement& s) {
        NOT_IMPLEMENTED(s);
    }
};


class test_visitor {
public:
    explicit test_visitor() {}

    auto handle(const expression& e) {
        return visit(*this, e);
    }

    auto handle(const statement& s) {
        return visit(*this, s);
    }

    void handle(const init_decl& id) {
        if (!id.has_init_val()) {
            return;
        }
        const auto& d = id.d();
        if (d.t()->base() == ctype::function_t) {
            std::cout << "TODO: Handle " << d << "\n";
            const auto& fi = d.t()->function_val();
            std::cout << fi << "\n";
            const auto& sc = id.local_scope();
            foo(sc);
            handle(id.body());
        } else {
            std::cout << "TODO: Handle " << d << "\n";
        }
    }

    static void foo(const scope& sc) {
        for (const auto& s: scope_symbols(sc)) {
            if (!s->decl_type()) continue;
            std::cout << indent{} << decl{s->decl_type(), s->id()} << "\n";
        }
        source_formatter sf{std::cout, default_indent};
        for (const auto& c: scope_children(sc)) {
            foo(*c);
        }
    }

    //
    // Expression
    //
    void operator()(const identifier_expression& e) {
        (void)e;
    }
    void operator()(const const_int_expression& e) {
        (void)e;
    }
    void operator()(const const_float_expression& e) {
        (void)e;
    }
    void operator()(const string_lit_expression& e) {
        (void)e;
    }
    void operator()(const initializer_expression& e) {
        for (const auto& e2: e.es()) {
            handle(*e2);
        }
    }
    void operator()(const array_access_expression& e) {
        handle(e.a());
        handle(e.i());
    }
    void operator()(const function_call_expression& e) {
        handle(e.f());
        for (const auto& a: e.args()) {
            handle(*a);
        }
    }
    void operator()(const access_expression& e) {
        handle(e.e());
    }
    void operator()(const sizeof_expression& e) {
        if (!e.arg_is_type()) {
            handle(e.e());
        }
    }
    void operator()(const prefix_expression& e) {
        handle(e.e());
    }
    void operator()(const postfix_expression& e) {
        handle(e.e());
    }
    void operator()(const cast_expression& e) {
        handle(e.e());
    }
    void operator()(const binary_expression& e) {
        handle(e.l());
        handle(e.r());
    }
    void operator()(const conditional_expression& e) {
        handle(e.cond());
        handle(e.l());
        handle(e.r());
    }

    //
    // Statement
    //
    void operator()(const empty_statement&) {
    }
    void operator()(const declaration_statement& s) {
        for (const auto& ds: s.ds()) {
            if (ds->has_init_val()) {
                assert(ds->d().t()->base() != ctype::function_t);
                handle(ds->init_expr());
            }
        }
    }
    void operator()(const labeled_statement& s) {
        handle(s.s());
    }
    void operator()(const compound_statement& s) {
        for (const auto& s2 : s.ss()) {
            handle(*s2);
        }
    }
    void operator()(const expression_statement& s) {
        handle(s.e());
    }
    void operator()(const if_statement& s) {
        handle(s.cond());
        handle(s.if_s());
        if (s.else_s()) {
            handle(*s.else_s());
        }
    }
    void operator()(const switch_statement& s) {
        handle(s.e());
        handle(s.s());
    }
    void operator()(const while_statement& s) {
        handle(s.cond());
        handle(s.s());
    }
    void operator()(const do_statement& s) {
        handle(s.s());
        handle(s.cond());
    }
    void operator()(const for_statement& s) {
        handle(s.init());
        if (s.cond()) handle(*s.cond());
        if (s.iter()) handle(*s.iter());
        handle(s.body());
    }
    void operator()(const goto_statement& s) {
        (void)s;
    }
    void operator()(const continue_statement& s) {
        (void)s;
    }
    void operator()(const break_statement& s) {
        (void)s;
    }
    void operator()(const return_statement& s) {
        if (s.e()) {
            handle(*s.e());
        }
    }
};

void process_one(source_manager& sm, const std::string& filename) {
    std::cout << filename << "\n";
    auto pr = parse(sm, sm.load(filename));
    test_visitor vis{};
    for (const auto& d: pr.decls()) {
        vis.handle(*d);
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