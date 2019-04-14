#ifndef MCC_PARSER_H
#define MCC_PARSER_H

#include "source.h"
#include "token.h"
#include "type.h"
#include <ostream>
#include <algorithm>

namespace mcc {

class expression;
class statement;
using expression_ptr = std::unique_ptr<expression>;
using statement_ptr = std::unique_ptr<statement>;

class symbol_info;

//
// Expression
//

class expression {
public:
    virtual ~expression() {}

    const std::shared_ptr<const type>& expression_type() const { return expression_type_; }
    void set_expression_type(const std::shared_ptr<const type>& t) const {
        assert(!expression_type_ && t);
        const_cast<expression&>(*this).expression_type_ = t;
    }

    friend std::ostream& operator<<(std::ostream& os, const expression& e) {
        e.do_print(os);
        return os;
    }
private:
    virtual void do_print(std::ostream& os) const = 0;
    std::shared_ptr<const type> expression_type_;
};

class identifier_expression : public expression {
public:
    explicit identifier_expression(const std::string& id) : id_{id}, sym_{nullptr} {
        assert(!id_.empty());
    }

    const std::string& id() const { return id_; }

    symbol_info* symbol() const { return const_cast<symbol_info*>(sym_); }

    void set_symbol(symbol_info& sym) const {
        assert(!sym_);
        const_cast<identifier_expression&>(*this).sym_ = &sym;
    }

private:
    std::string id_;
    symbol_info* sym_;

    void do_print(std::ostream& os) const override {
        os << id_;
    }
};

class const_int_expression : public expression {
public:
    explicit const_int_expression(const const_int_val& val) : val_{val} {
    }

    const const_int_val& val() const { return val_; }

private:
    const_int_val val_;

    void do_print(std::ostream& os) const override {
        os << val_;
    }
};

class const_float_expression : public expression {
public:
    explicit const_float_expression(double val, ctype t) : val_{val}, t_{t} {
        assert(t == ctype::float_t || t == ctype::double_t || t == ctype::long_double_t);
    }

    double val() const { return val_; }
    ctype t() const { return t_; }

private:
    double val_;
    ctype t_;

    void do_print(std::ostream& os) const override {
        os << val_;
    }
};

class string_lit_expression : public expression {
public:
    explicit string_lit_expression(const std::string& text) : text_(text) {
    }

    const std::string& text() const { return text_; }

private:
    std::string text_;
    void do_print(std::ostream& os) const override;
};

class initializer_expression : public expression {
public:
    explicit initializer_expression(std::vector<expression_ptr>&& es) : es_{std::move(es)} {
    }
    const std::vector<expression_ptr>& es() const { return es_; }
private:
    std::vector<expression_ptr> es_;
    void do_print(std::ostream& os) const override;
};

class array_access_expression : public expression {
public:
    explicit array_access_expression(expression_ptr&& a, expression_ptr&& i) : a_{std::move(a)}, i_{std::move(i)} {
        assert(a_ && i_);
    }
    const expression& i() const { return *i_; }
    const expression& a() const { return *a_; }
private:
    expression_ptr a_;
    expression_ptr i_;
    void do_print(std::ostream& os) const override;
};

class function_call_expression : public expression {
public:
    explicit function_call_expression(expression_ptr&& f, std::vector<expression_ptr>&& args) : f_{std::move(f)}, args_{std::move(args)} {
        assert(f_ && std::none_of(args.begin(), args.end(), [](auto& a) {return !a; }));
    }
    const expression& f() const { return *f_; }
    const std::vector<expression_ptr>& args() const { return args_; }
private:
    expression_ptr f_;
    std::vector<expression_ptr> args_;

    void do_print(std::ostream& os) const override;
};

class access_expression : public expression {
public:
    explicit access_expression(token_type op, expression_ptr&& e, const std::string& id) : op_{op}, e_{std::move(e)}, id_{id} {
        assert(op_ == token_type::dot || op_ == token_type::arrow);
        assert(e_ && !id_.empty());
    }

    token_type op() const { return op_; }
    const expression& e() const { return *e_; }
    const std::string& id() const { return id_; }

private:
    token_type op_;
    expression_ptr e_;
    std::string id_;

    void do_print(std::ostream& os) const override;
};

class sizeof_expression : public expression {
public:
    explicit sizeof_expression(const std::shared_ptr<const type>& t) : val_{t} {
        assert(std::get<0>(val_));
    }
 
    explicit sizeof_expression(expression_ptr&& e) : val_{std::move(e)} {
        assert(std::get<1>(val_));
    }

    bool arg_is_type() const { return val_.index() == 0; }
    const std::shared_ptr<const type>& t() const { assert(val_.index()==0); return std::get<0>(val_); }
    const expression& e() const { assert(val_.index()==1); return *std::get<1>(val_); }

private:
    std::variant<std::shared_ptr<const type>, expression_ptr> val_;

    void do_print(std::ostream& os) const override;
};

class unary_expression : public expression {
public:
    token_type op() const { return op_; }
    const expression& e() const { return *e_; }

protected:
    explicit unary_expression(bool is_prefix, token_type op, expression_ptr&& e) : is_prefix_{is_prefix}, op_{op}, e_{std::move(e)} {
        assert(e_ && op != token_type::sizeof_);
    }

private:
    bool is_prefix_;
    token_type op_;
    expression_ptr e_;

    void do_print(std::ostream& os) const override;
};

class prefix_expression : public unary_expression {
public:
    explicit prefix_expression(token_type op, expression_ptr&& e) : unary_expression{true, op, std::move(e)} {
    }
};

class postfix_expression : public unary_expression {
public:
    explicit postfix_expression(token_type op, expression_ptr&& e) : unary_expression{false, op, std::move(e)} {
    }
};

class cast_expression : public expression {
public:
    explicit cast_expression(const std::shared_ptr<const type>& t, expression_ptr&& e) : t_{std::move(t)}, e_{std::move(e)} {
        assert(t_ && e_);
    }
    const std::shared_ptr<const type>& t() const { return t_; }
    const expression& e() const { return *e_; }
private:
    std::shared_ptr<const type> t_;
    expression_ptr e_;
    void do_print(std::ostream& os) const override;
};

class binary_expression : public expression {
public:
    explicit binary_expression(token_type op, expression_ptr&& l, expression_ptr&& r) : op_{op}, l_{std::move(l)}, r_{std::move(r)} {
        assert(l_ && r_);
    }

    token_type op() const { return op_; }
    const expression& l() const { return *l_; }
    const expression& r() const { return *r_; }

private:
    token_type op_;
    expression_ptr l_;
    expression_ptr r_;

    void do_print(std::ostream& os) const override;
};

class conditional_expression : public expression {
public:
    explicit conditional_expression(expression_ptr&& cond, expression_ptr&& l, expression_ptr&& r) : cond_{std::move(cond)}, l_{std::move(l)}, r_{std::move(r)} {
        assert(l_ && r_);
    }

    const expression& cond() const { return *cond_; }
    const expression& l() const { return *l_; }
    const expression& r() const { return *r_; }

private:
    expression_ptr cond_;
    expression_ptr l_;
    expression_ptr r_;

    void do_print(std::ostream& os) const override;
};

#define EXPRESSION_TYPES(X) \
    X(identifier) \
    X(const_int) \
    X(const_float) \
    X(string_lit) \
    X(initializer) \
    X(array_access) \
    X(function_call) \
    X(access) \
    X(sizeof) \
    X(prefix) \
    X(postfix) \
    X(cast) \
    X(binary) \
    X(conditional)

//
// Statement
//

class init_decl;

class statement {
public:
    virtual ~statement() {}

    friend std::ostream& operator<<(std::ostream& os, const statement& e) {
        e.do_print(os);
        return os;
    }
private:
    virtual void do_print(std::ostream& os) const = 0;
};

class empty_statement : public statement {
public:
    explicit empty_statement() {}
private:
    void do_print(std::ostream& os) const override;
};

class declaration_statement : public statement {
public:
    explicit declaration_statement(std::vector<std::unique_ptr<init_decl>>&& ds) : ds_{std::move(ds)} {
    }

    const std::vector<std::unique_ptr<init_decl>>& ds() const { return ds_; }
private:
    std::vector<std::unique_ptr<init_decl>> ds_;

   void do_print(std::ostream& os) const override;
};

class labeled_statement : public statement {
public:
    explicit labeled_statement(statement_ptr&& s) : val_{}, s_{std::move(s)} {
        assert(s_);
    }
    explicit labeled_statement(const std::string& label, statement_ptr&& s) : val_{label}, s_{std::move(s)} {
        assert(s_);
    }
    explicit labeled_statement(expression_ptr&& e, statement_ptr&& s) : val_{std::move(e)}, s_{std::move(s)} {
        assert(s_ && std::get<2>(val_));
    }

    const statement& s() { return *s_; }

    bool is_default_label() const { return val_.index() == 0; }
    bool is_case_label()    const { return val_.index() == 2; }
    bool is_normal_label()  const { return val_.index() == 1; }

    const std::string& label() const { assert(is_normal_label()); return std::get<1>(val_); }

private:
    std::variant<std::monostate, std::string, expression_ptr> val_;
    statement_ptr s_;

    void do_print(std::ostream& os) const override;
};

class compound_statement : public statement {
public:
    explicit compound_statement(std::vector<statement_ptr>&& ss) : ss_{std::move(ss)} {
    }

    const std::vector<statement_ptr>& ss() const { return ss_; }

private:
    std::vector<statement_ptr> ss_;

    void do_print(std::ostream& os) const override;
};

class expression_statement : public statement {
public:
    explicit expression_statement(expression_ptr&& e) : e_{std::move(e)} {
        assert(e_);
    }

    const expression& e() const { return *e_; }

private:
    expression_ptr e_;

    void do_print(std::ostream& os) const override;
};

class if_statement : public statement {
public:
    explicit if_statement(expression_ptr&& cond, statement_ptr&& if_s, statement_ptr&& else_s) : cond_{std::move(cond)}, if_{std::move(if_s)}, else_{std::move(else_s)} {
        assert(cond_ && if_);
    }

    const expression& cond() const { return *cond_; }
    const statement& if_s() const { return *if_; }
    const statement_ptr& else_s() const { return else_; }

private:
    expression_ptr cond_;
    statement_ptr if_;
    statement_ptr else_;

    void do_print(std::ostream& os) const override;
};

class switch_statement : public statement {
public:
    explicit switch_statement(expression_ptr&& e, statement_ptr&& s) : e_{std::move(e)}, s_{std::move(s)} {
        assert(e_ && s_);
    }

    const expression& e() const { return *e_; }
    const statement& s() const { return *s_; }

private:
    expression_ptr e_;
    statement_ptr s_;

    void do_print(std::ostream& os) const override;
};

class while_statement : public statement {
public:
    explicit while_statement(expression_ptr&& cond, statement_ptr&& s) : cond_{std::move(cond)}, s_{std::move(s)} {
        assert(cond_ && s_);
    }

    const expression& cond() const { return *cond_; }
    const statement& s() const { return *s_; }

private:
    expression_ptr cond_;
    statement_ptr s_;

    void do_print(std::ostream& os) const override;
};

class do_statement : public statement {
public:
    explicit do_statement(expression_ptr&& cond, statement_ptr&& s) : cond_{std::move(cond)}, s_{std::move(s)} {
        assert(cond_ && s_);
    }

    const expression& cond() const { return *cond_; }
    const statement& s() const { return *s_; }

private:
    expression_ptr cond_;
    statement_ptr s_;

    void do_print(std::ostream& os) const override;
};

class for_statement : public statement {
public:
    explicit for_statement(statement_ptr&& init, expression_ptr&& cond, expression_ptr&& iter, statement_ptr&& body) : init_{std::move(init)}, cond_{std::move(cond)}, iter_{std::move(iter)}, body_{std::move(body)} {
        assert(init_ && body_);
    }

    const statement& init() const { return *init_; }
    const expression_ptr& cond() const { return cond_; }
    const expression_ptr& iter() const { return iter_; }
    const statement& body() const { return *body_; }

private:
    statement_ptr init_;
    expression_ptr cond_;
    expression_ptr iter_;
    statement_ptr body_;

    void do_print(std::ostream& os) const override;
};

class goto_statement : public statement {
public:
    explicit goto_statement(const std::string& target) : target_{target} {
    }

    const std::string& target() const { return target_; }

private:
    std::string target_;

    void do_print(std::ostream& os) const override;
};

class continue_statement : public statement {
public:
    explicit continue_statement() {}

private:
    void do_print(std::ostream& os) const override;
};

class break_statement : public statement {
public:
    explicit break_statement() {}

private:
    void do_print(std::ostream& os) const override;
};

class return_statement : public statement {
public:
    explicit return_statement(expression_ptr&& e) : e_{std::move(e)} {
    }

    const expression_ptr& e() const { return e_; }

private:
    expression_ptr e_;

    void do_print(std::ostream& os) const override;
};

#define STATEMENT_TYPES(X) \
    X(empty) \
    X(declaration) \
    X(labeled) \
    X(compound) \
    X(expression) \
    X(if) \
    X(switch) \
    X(while) \
    X(do) \
    X(for) \
    X(goto) \
    X(continue) \
    X(break) \
    X(return)


//
// Declaration
//
class init_decl {
public:
    explicit init_decl(decl&& d) : d_{std::move(d)}, val_{} {
    }

    explicit init_decl(decl&& d, expression_ptr&& init) : d_{std::move(d)}, val_{std::move(init)} {
        assert(d_.t()->base() != ctype::function_t && std::get<1>(val_));
    }

    explicit init_decl(decl&& d, std::unique_ptr<compound_statement>&& body) : d_{std::move(d)}, val_{std::move(body)} {
        assert(d_.t()->base() == ctype::function_t && std::get<2>(val_));
    }

    const decl& d() const { return d_; }

    bool has_init_val() const {
        return val_.index() != 0;
    }

    const expression& init_expr() const {
        assert(val_.index() == 1);
        return *std::get<1>(val_);
    }

    const compound_statement& body() const {
        assert(d_.t()->base() == ctype::function_t && std::get<2>(val_));
        return *std::get<2>(val_);
    }

    friend std::ostream& operator<<(std::ostream& os, const init_decl& d) {
        os << d.d_;
        if (d.val_.index() == 1) {
            os << " = " << d.init_expr();
        }
        return os;
    }

private:
    decl d_;
    std::variant<std::monostate, expression_ptr, std::unique_ptr<compound_statement>> val_;
};

//
// Parser
//

std::vector<std::unique_ptr<init_decl>> parse(source_manager& sm, const source_file& source);

template<typename V>
auto visit(V&& v, const expression& e) {
#define DISPATCH(t) if (auto p = dynamic_cast<const t##_expression*>(&e)) return v(*p);
    EXPRESSION_TYPES(DISPATCH);
#undef DISPATCH
    throw std::runtime_error("Unknown expression");
}

template<typename V>
auto visit(V&& v, const statement& s) {
#define DISPATCH(t) if (auto p = dynamic_cast<const t##_statement*>(&s)) return v(*p);
    STATEMENT_TYPES(DISPATCH);
#undef DISPATCH
    throw std::runtime_error("Unknown statement");
}


} // namespace mcc

#endif
