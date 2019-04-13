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

//
// Expression
//

class expression {
public:
    virtual ~expression() {}

    friend std::ostream& operator<<(std::ostream& os, const expression& e) {
        e.do_print(os);
        return os;
    }
private:
    virtual void do_print(std::ostream& os) const = 0;
};

class identifier_expression : public expression {
public:
    explicit identifier_expression(const std::string& id) : id_{id} {
        assert(!id_.empty());
    }

    const std::string& id() const { return id_; }

private:
    std::string id_;
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

class string_lit_expression : public expression {
public:
    explicit string_lit_expression(const std::string& text) : text_(text) {
    }
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
    void do_print(std::ostream& os) const override {
        os << "{";
        for (size_t i = 0; i < es_.size(); ++i) {
            os << (i ? ", ": " ");
            os << *es_[i];
        }
        os << " }";
    }
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
    void do_print(std::ostream& os) const override {
        os << *a_ << "[" << *i_ << "]";
    }
};

class function_call_expression : public expression {
public:
    explicit function_call_expression(expression_ptr&& f, std::vector<expression_ptr>&& args) : f_{std::move(f)}, args_{std::move(args)} {
        assert(f_ && std::none_of(args.begin(), args.end(), [](auto& a) {return !a; }));
    }
    const expression& f() const { return *f_; }
    const std::vector<expression_ptr>& args() { return args_; }
private:
    expression_ptr f_;
    std::vector<expression_ptr> args_;

    void do_print(std::ostream& os) const override {
        os << *f_ << "(";
        for (size_t i = 0; i < args_.size(); ++i) {
            if (i) os << ", ";
            os << *args_[i];
        }
        os << ")";
    }
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

    void do_print(std::ostream& os) const override {
        os << *e_ << op_ << id_;
    }
};

class sizeof_expression : public expression {
public:
    explicit sizeof_expression(const std::shared_ptr<const type>& t) : val_{t} {
        assert(std::get<0>(val_));
    }
    explicit sizeof_expression(expression_ptr&& e) : val_{std::move(e)} {
        assert(std::get<1>(val_));
    }
private:
    std::variant<std::shared_ptr<const type>, expression_ptr> val_;

    void do_print(std::ostream& os) const override {
        os << "sizeof ";
        if (val_.index() == 0) {
            os << "(" << *std::get<0>(val_) << ")";
        } else {
            os << *std::get<1>(val_);
        }
    }
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

    void do_print(std::ostream& os) const override {
        if (is_prefix_) {
            os << op_ << *e_;
        } else {
            os << *e_ << op_;
        }
    }
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
    void do_print(std::ostream& os) const override {
        os << "(" << *t_ << ")" << *e_;
    }
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

    void do_print(std::ostream& os) const override {
        os << *l_ << op_ << *r_;
    }
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

    void do_print(std::ostream& os) const override {
        os << *cond_ << "?" << *l_ << ":" << *r_;
    }
};

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
    void do_print(std::ostream& os) const {
        os << ";";
    }
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

private:
    std::variant<std::monostate, std::string, expression_ptr> val_;
    statement_ptr s_;

    void do_print(std::ostream& os) const override {
        switch (val_.index()) {
        case 0: os << "default"; break;
        case 1: os << std::get<1>(val_); break;
        case 2: os << "case " << *std::get<2>(val_); break;
        default:
            assert(false);
        }
        os << ": " << *s_;
    }
};

class compound_statement : public statement {
public:
    explicit compound_statement(std::vector<statement_ptr>&& ss) : ss_{std::move(ss)} {
    }

    const std::vector<statement_ptr>& ss() const { return ss_; }

private:
    std::vector<statement_ptr> ss_;

    void do_print(std::ostream& os) const override {
        os << "{";
        for (const auto& s: ss_) {
            os << " " << *s;
        }
        os << " }";
    }
};

class expression_statement : public statement {
public:
    explicit expression_statement(expression_ptr&& e) : e_{std::move(e)} {
        assert(e_);
    }

    const expression& e() const { return *e_; }

private:
    expression_ptr e_;

    void do_print(std::ostream& os) const override {
        os << *e_ << ";";
    }
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

    void do_print(std::ostream& os) const override {
        os << "if (" << *cond_ << ") " << *if_;
        if (else_) {
            os << " else " << *else_;
        }
    }
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

    void do_print(std::ostream& os) const override {
        os << "switch (" << *e_ << ") " << *s_;
    }
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

    void do_print(std::ostream& os) const override {
        os << "while (" << *cond_ << ") " << *s_;
    }
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

    void do_print(std::ostream& os) const override {
        os << "do " << *s_ << " while (" << *cond_ << ");";
    }
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

    void do_print(std::ostream& os) const override {
        os << "for (" << *init_ << " ";
        if (cond_) os << *cond_;
        os << "; ";
        if (iter_) os << *iter_;
        os << ") " << *body_;
    }

};

class goto_statement : public statement {
public:
    explicit goto_statement(const std::string& target) : target_{target} {
    }

    const std::string& target() const { return target_; }

private:
    std::string target_;

    void do_print(std::ostream& os) const override {
        os << "goto " << target_ << ";";
    }
};

class continue_statement : public statement {
public:
    explicit continue_statement() {}

private:
    void do_print(std::ostream& os) const override {
        os << "continue;";
    }
};

class break_statement : public statement {
public:
    explicit break_statement() {}

private:
    void do_print(std::ostream& os) const override {
        os << "break;";
    }
};

class return_statement : public statement {
public:
    explicit return_statement(expression_ptr&& e) : e_{std::move(e)} {
    }

    const expression_ptr& e() const { return e_; }

private:
    expression_ptr e_;

    void do_print(std::ostream& os) const override {
        os << "return";
        if (e_) {
            os << " " << *e_;
        }
        os << ";";
    }
};

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

class parser {
public:
    explicit parser(source_manager& sm, const source_file& source);
    ~parser();

    std::vector<source_position> position() const;

    void parse();

private:
    class impl;
    std::unique_ptr<impl> impl_;
};


} // namespace mcc

#endif
