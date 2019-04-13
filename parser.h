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

//class labeled_statement : public statement {};
class compound_statement : public statement {
public:
    explicit compound_statement(std::vector<statement_ptr>&& ss) : ss_{std::move(ss)} {
    }

    const std::vector<statement_ptr>& ss() const { return ss_; }

private:
    std::vector<statement_ptr> ss_;

    void do_print(std::ostream& os) const override {
        os << "{ ";
        for (const auto& s: ss_) {
            os << s;
        }
        os << "}";
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

//class if_statement : public statement {};
//class switch_statement : public statement {};
//class while_statement : public statement {};
//class do_statement : public statement {};
//class for_statement : public statement {};
//class goto_statement : public statement {};
//class continue_statement : public statement {};
//class break_statement : public statement {};

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
        assert(is_arithmetic(d_.t()->base()) && std::get<1>(val_));
    }

    explicit init_decl(decl&& d, std::unique_ptr<compound_statement>&& body) : d_{std::move(d)}, val_{std::move(body)} {
        assert(d_.t()->base() == ctype::function_t && std::get<2>(val_));
    }

    const decl& d() const { return d_; }

    bool has_init_val() const {
        return val_.index() != 0;
    }

    const expression& init_expr() const {
        assert(is_arithmetic(d_.t()->base()) && val_.index() == 1);
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
//
//

const_int_val const_int_eval(const expression& e);

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
