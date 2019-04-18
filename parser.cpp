#include "parser.h"
#include "util.h"
#include "lexer.h"
#include <map>
#include <set>
#include <iostream>

namespace mcc {

class push_precedence {
public:
    explicit push_precedence(std::ostream& os, int precedence) : os_{os}, sf_{os}, need_parenthesis_{sf_.precedence(precedence)} {
        if (need_parenthesis_) os_ << '(';
    }
    explicit push_precedence(std::ostream& os, token_type op) : push_precedence{os, operator_precedence(op)} {
    }
    ~push_precedence() {
        if (need_parenthesis_) os_ << ')';
    }
private:
    std::ostream& os_;
    source_formatter sf_;
    bool need_parenthesis_;
};

void string_lit_expression::do_print(std::ostream& os) const {
    os << quoted(text_);
}

void initializer_expression::do_print(std::ostream& os) const {
    os << "{";
    for (size_t i = 0; i < es_.size(); ++i) {
        os << (i ? ", ": " ");
        os << *es_[i];
    }
    os << " }";
}

void array_access_expression::do_print(std::ostream& os) const {
    push_precedence pp{os, 2};
    os << *a_ << "[" << *i_ << "]";
}


void function_call_expression::do_print(std::ostream& os) const {
    push_precedence pp{os, 2};
    os << *f_ << '(';
    source_formatter sf{os};
    sf.precedence(operator_precedence(token_type::comma));
    for (size_t i = 0; i < args_.size(); ++i) {
        if (i) os << ", ";
        os << *args_[i];
    }
    os << ')';
}


void access_expression::do_print(std::ostream& os) const {
    push_precedence pp{os, 2};
    assert(op_ == token_type::dot || op_ == token_type::arrow);
    os << *e_ << op_ << id_;
}

void sizeof_expression::do_print(std::ostream& os) const {
    push_precedence pp{os, 3};
    os << "sizeof";
    if (val_.index() == 0) {
        os << "(" << *std::get<0>(val_) << ")";
    } else {
        os << " " << *std::get<1>(val_);
    }
}

void unary_expression::do_print(std::ostream& os) const {
    if (is_prefix_) {
        push_precedence pp{os, 3};
        os << op_ << *e_;
    } else {
        push_precedence pp{os, 2};
        os << *e_ << op_;
    }
}

void cast_expression::do_print(std::ostream& os) const {
    push_precedence pp{os, 3};
    os << "(" << *et() << ")" << *e_;
}

void binary_expression::do_print(std::ostream& os) const {
    push_precedence pp{os, operator_precedence(op_)};
    os << *l_ << ' ' << op_ << ' ' << *r_;
}

void conditional_expression::do_print(std::ostream& os) const {
    push_precedence pp{os, token_type::question};
    os << *cond_ << " ? " << *l_ << " : " << *r_;
}

void empty_statement::do_print(std::ostream& os) const {
    os << indent{} << ";";
}

void declaration_statement::do_print(std::ostream& os) const {
    bool first = true;
    for (const auto& d: ds_) {
        if (first) first = false;
        else os << "\n";
        os << indent{} << *d << ";";
    }
}

void labeled_statement::do_print(std::ostream& os) const {
    {
        source_formatter sf{os, -default_indent};
        os << indent{};
    }
    switch (val_.index()) {
    case 0: os << "default"; break;
    case 1: os << std::get<1>(val_); break;
    case 2: os << "case " << *std::get<2>(val_); break;
    default:
        assert(false);
    }
    os << ":\n" << *s_;
}

void compound_statement::do_print(std::ostream& os) const {
    source_formatter sf0{os, -default_indent};
    os << indent{} << "{\n";
    {
        source_formatter sf{os, default_indent};
        for (const auto& s: ss_) {
            os << *s << "\n";
        }
    }
    os << indent{} << "}";
}

void expression_statement::do_print(std::ostream& os) const {
    os << indent{} << *e_ << ";";
}

void if_statement::do_print(std::ostream& os) const {
    os << indent{} << "if (" << *cond_ << ")\n";
    {
        source_formatter sf{os, default_indent};
        os << *if_;
    }
    if (else_) {
        const bool is_else_if = dynamic_cast<const if_statement*>(else_.get());
        os << "\n" << indent{} << "else" << (is_else_if ? ' ' : '\n');
        source_formatter sf{os, is_else_if ? 0 : default_indent};
        if (is_else_if) {
            sf.suppress_next();
        }
        os << *else_;
    }
}

void switch_statement::do_print(std::ostream& os) const {
    os << indent{} << "switch (" << *e_ << ")\n";
    source_formatter sf{os, default_indent};
    os << *s_;
}

void while_statement::do_print(std::ostream& os) const {
    os << indent{} << "while (" << *cond_ << ")\n";
    source_formatter sf{os, default_indent};
    os << *s_;
}

void do_statement::do_print(std::ostream& os) const {
    os << indent{} << "do\n";
    {
        source_formatter sf{os, default_indent};
        os << *s_;
    }
    os << "\n" << indent{} << "while (" << *cond_ << ");";
}

void for_statement::do_print(std::ostream& os) const {
    os << indent{} << "for (";
    {
        source_formatter sf{os};
        sf.suppress_next();
        os  << *init_ << " ";
    }
    if (cond_)  os << *cond_;
    os << "; ";
    if (iter_) os << *iter_;
    os << ")\n";
    source_formatter sf{os, default_indent};
    os << *body_;
}

void goto_statement::do_print(std::ostream& os) const {
    os << indent{} << "goto " << target_ << ";";
}

void continue_statement::do_print(std::ostream& os) const {
    os << indent{} << "continue;";
}

void break_statement::do_print(std::ostream& os) const {
    os << indent{} << "break;";
}

void return_statement::do_print(std::ostream& os) const {
    os << indent{} << "return";
    if (e_) {
        os << " " << *e_;
    }
    os << ";";
}


#define EXPECT(tok) do { if (current().type() != token_type::tok) NOT_IMPLEMENTED("Expected " << token_type::tok << " got " << current()); next(); } while (0)
#define TRACE(msg) std::cout << __FILE__ << ":" << __LINE__ << ": " << __func__ <<  " Current: " << current() << " " << msg << "\n"

using tag_info_ptr = std::shared_ptr<tag_info_type>;

const decl* search_decl(const decl& d, const std::string_view id);
const decl* search_decl(const type_ptr& t, const std::string_view id) {
    for (const auto& m: struct_union_members(*t)) {
        if (auto d = search_decl(m, id)) {
            return d;
        }
    }
    return nullptr;
}

const decl* search_decl(const decl& d, const std::string_view id) {
    if (d.id().empty()) {
        return search_decl(d.t(), id);
    } else {
        return d.id() == id ? &d : nullptr;
    }
}

const decl& struct_union_member(const type_ptr& t, const std::string_view id) {
    if (auto d = search_decl(t, id)) {
        return *d;
    }
    NOT_IMPLEMENTED(id << " not found in " << *t);
}

// Label names have function scope
// Tag names can be scope but are otherwise global (no shadowing allowed)
// Orignary names can shadow (but are still disallowed in the same scope)
class symbol {
public:
    explicit symbol(const std::string_view id)
        : id_{id}
        , declaration_{nullptr}
        , tag_info_{nullptr}
        , definition_{nullptr}
        , civ_{0, ctype::none}
        , label_state_{0} {
    }

    const std::string& id() const { return id_; }
    type_ptr decl_type() const { return declaration_; }
    const tag_info_ptr& tag_info() const { return tag_info_; }

    bool has_const_int_def() const { return civ_.type != ctype::none; }
    const const_int_val& const_int_def() { assert(has_const_int_def()); return civ_; }

    void declare(const type_ptr& t) {
        assert(t);
        if (declaration_) {
            if (!redecl_type_compare(*declaration_, *t)) {
                NOT_IMPLEMENTED(id_ << " already declared as " << *declaration_ << " invalid redeclaration as " << *t);
            }
            return;
        }
        declaration_ = t;
    }

    void define(init_decl& id) {
        assert(id.d().id() == id_);
        declare(id.d().t());
        if (!id.has_init_val()) {
            return;
        }
        if (definition_) {
            NOT_IMPLEMENTED(id_ << " already defined as " << *definition_ << " invalid redefinition as " << id);
        }
        if (civ_.type != ctype::none) {
            NOT_IMPLEMENTED(id_ << " already defined as " << civ_ << " invalid redefinition as " << id);
        }
        definition_ = &id;
    }

    void define(const const_int_val& civ) {
        if (!declaration_ || civ.type != declaration_->ct()) {
            NOT_IMPLEMENTED("Invalid definition of " << id() << " as " << civ);
        }
        if (definition_) {
            NOT_IMPLEMENTED(id_ << " already defined as " << *definition_ << " invalid redefinition as " << civ);
        }
        if (civ_.type != ctype::none) {
            NOT_IMPLEMENTED(id_ << " already defined as " << civ_ << " invalid redefinition as " << civ);
        }
        if (!declaration_) {
            declaration_ = std::make_shared<type>(civ.type);
        }
        civ_ = civ;
    }

    void define_tag_type(const tag_info_ptr& ti) {
        if (tag_info_) {
            NOT_IMPLEMENTED(id_ << " already definition as " << tag_info_->base_type() << " invalid redefinition as " << ti->base_type());
        }
        tag_info_ = ti;
    }

    void define_label() {
        if (label_state_ & 2) {
            NOT_IMPLEMENTED(id_ << " already defined as label");
        }
        label_state_ |= 2;
    }

    void use_label() {
        label_state_ |= 1;
    }

    void check_label() const {
        if (label_state_ == 1) {
            NOT_IMPLEMENTED(id_ << " used as label but not defined");
        } else if (label_state_ == 2) {
            NOT_IMPLEMENTED(id_ << " defined as label but not used"); // Warning...
        }
    }

private:
    const std::string   id_;
    type_ptr            declaration_;
    tag_info_ptr        tag_info_;
    init_decl*          definition_;
    const_int_val       civ_;
    int                 label_state_;
};

class scope {
public:
    explicit scope(function_info* func_info) : func_info_{func_info} {
    }

    bool is_function_scope() const { return !!func_info_; }
    function_info& func_info() { assert(func_info_); return *func_info_; }

    symbol* find(const std::string_view id) {
        assert(!id.empty());
        auto it = std::find_if(symbols_.begin(), symbols_.end(), [id](const auto& s) { return s->id() == id; });
        return it != symbols_.end() ? it->get() : nullptr;
    }

    symbol* find_or_get(const std::string_view id) {
        assert(!id.empty());
        if (auto sym = find(id)) {
            return sym;
        }
        symbols_.push_back(std::make_unique<symbol>(id));
        return symbols_.back().get();
    }

    void declare(const decl& d) {
        find_or_get(d.id())->declare(d.t());
    }

    void declare(const std::string_view id, const tag_info_ptr& tag_info) {
        find_or_get(id)->define_tag_type(tag_info);
    }

    void define(init_decl& decl) {
        find_or_get(decl.d().id())->define(decl);
    }

    void define(const decl& d, const const_int_val& v) {
        auto sym = find_or_get(d.id());
        sym->declare(d.t());
        sym->define(v);
    }

    void check_labels() {
        assert(is_function_scope());
        for (const auto& sp: symbols_) {
            sp->check_label();
        }
    }

private:
    function_info* const func_info_;
    std::vector<std::unique_ptr<symbol>> symbols_;
};

class parser {
public:
    explicit parser(source_manager& sm, const source_file& source) : lex_{sm, source}, current_source_pos_{lex_.position()} {        
    }
    ~parser() {
        assert(active_scopes_.empty());
    }

    std::vector<std::unique_ptr<init_decl>> parse() {
        try {
            push_scope global_scope{*this};
            std::vector<std::unique_ptr<init_decl>> res;
            while (current().type() != token_type::eof) {
                if (current().type() == token_type::semicolon) {
                    std::cout << "Ignoring stray semicolon at " << current_source_pos_ << "\n";
                    next();
                    continue;
                }
                auto d = parse_declaration(false);
                res.insert(res.end(), std::make_move_iterator(d.begin()), std::make_move_iterator(d.end()));
            }
            return res;
        } catch (const std::exception& e) {
            std::ostringstream oss;
            oss << e.what() << "\n\n";
            for (const auto& p : lex_.position_trace()) {
                oss << p << "\n";
            }
            oss << "Current token: " << current();
            throw std::runtime_error(oss.str());
        }
    }

private:
    lexer lex_;
    int unnamed_cnt_ = 0;
    std::vector<std::unique_ptr<scope>> active_scopes_;
    source_position current_source_pos_;

    const type_ptr void_pointer_type = make_ptr_t(std::make_shared<type>(ctype::void_t));
    const type_ptr bool_type = std::make_shared<type>(ctype::bool_t);
    const type_ptr int_type = std::make_shared<type>(ctype::int_t);
    const type_ptr const_char_type = std::make_shared<type>(ctype::plain_char_t | ctype::const_f);
    const type_ptr ptrdiff_t_type = std::make_shared<type>(ctype::long_long_t);
    const type_ptr size_t_type = std::make_shared<type>(ctype::long_long_t | ctype::unsigned_f);

    class push_scope {
    public:
        explicit push_scope(parser& p, function_info* func_info = nullptr) : p_{p} {
            p_.active_scopes_.push_back(std::make_unique<scope>(func_info));
        }
        ~push_scope() {
            p_.active_scopes_.pop_back();
        }
        scope& this_scope() {
            return *p_.active_scopes_.back();
        }
    private:
        parser& p_;
    };

    scope& current_scope() {
        assert(!active_scopes_.empty());
        return *active_scopes_.back();
    }

    symbol* id_lookup(const std::string_view id) const {
        assert(!active_scopes_.empty());
        for (auto it = active_scopes_.crbegin(), end = active_scopes_.crend(); it != end; ++it) {
            if (auto sym = (*it)->find(id); sym && sym->decl_type()) {
                return sym;
            }
        }
        return nullptr;
    }

    symbol* tag_lookup(const std::string_view id) const {
        assert(!active_scopes_.empty());
        for (auto it = active_scopes_.crbegin(), end = active_scopes_.crend(); it != end; ++it) {
            if (auto sym = (*it)->find(id); sym && sym->tag_info()) {
                return sym;
            }
        }
        return nullptr;
    }

    symbol* get_label(const std::string_view id) const {
        assert(!active_scopes_.empty());
        for (auto it = active_scopes_.crbegin(), end = active_scopes_.crend(); it != end; ++it) {
            if ((*it)->is_function_scope()) {
                return (*it)->find_or_get(id);
            }
        }
        NOT_IMPLEMENTED(id);
    }

    function_info& current_function_info() {
        assert(!active_scopes_.empty());
        for (auto it = active_scopes_.rbegin(), end = active_scopes_.rend(); it != end; ++it) {
            if ((*it)->is_function_scope()) {
                return (*it)->func_info();
            }
        }
        NOT_IMPLEMENTED("Not inside function");
    }

    // Handle null pointer constant
    void handle_const_null(type_ptr& t, const expression& e) {
        if (t->base() != ctype::int_t) return;
        auto ci = dynamic_cast<const const_int_expression*>(&e);
        if (!ci) return;
        if (ci->val().val != 0) return;
        t = void_pointer_type;
    }


    void next() {
        //std::cout << "Consuming " << current() << "\n";
        assert(current().type() != token_type::eof);
        current_source_pos_ = lex_.position();
        lex_.next();
    }

    const token& current() const {
        return lex_.current();
    }

    bool is_current_type_name() const {
        const auto t = current().type();
        if (is_literal(t)) {
            return false;
        } else if (t == token_type::struct_ || t == token_type::union_ || t == token_type::enum_) {
            return true;
        } else if (is_storage_class_specifier(t) || is_type_qualifier(t) || is_type_qualifier(t) || is_simple_type_specifier(t)) {
            return true;
        } else if (t == token_type::id) {
            auto sym = id_lookup(current().text());
            if (sym) {
                return !!(sym->decl_type()->ct() & ctype::typedef_f);
            }
        }
        return false;
    }

    std::vector<std::unique_ptr<init_decl>> parse_declaration(bool parsing_struct_or_union) {

        // declaration
        //    declaration_specifiers init-declarator-list? ';'
        // init-declarator-list
        //    init-declarator
        //    init-declarator-list , init-declarator        
        // init-declarator
        //     declarator
        //     declarator = initializer

        const auto decl_start = current_source_pos_;

        const auto ds = parse_declaration_specifiers();

        std::vector<std::unique_ptr<init_decl>> decls;
        if (current().type() == token_type::semicolon) {
            next();
            decls.push_back(std::make_unique<init_decl>(decl_start, decl{ds, ""}));
            return decls;
        }

        for (;;) {
            auto d = parse_declarator(ds);

            if (current().type() == token_type::colon) {
                // Bitfield
                next();
                if (!is_integral(d.t()->base())) {
                    NOT_IMPLEMENTED("Bitfield for " << d);
                }
                const auto size = const_int_eval(*parse_constant_expression()).val;
                if (size > 63) {
                    NOT_IMPLEMENTED(d << ":" << size);
                }
                auto ct = modified_bitfield(d.t()->ct() | ctype::bitfield_f, static_cast<uint8_t>(size));
                d = decl{std::make_shared<type>(ct), d.id()};
            }

            if (!parsing_struct_or_union) {
                // To support "struct S* s = malloc(*s)" we need to define "s" before parsing the initializer
                // Same for int x, *y=&x;
                current_scope().declare(d);
            }

            if (current().type() == token_type::lbrace) {
                if (d.t()->base() == ctype::function_t) {
                    if (!decls.empty()) {
                        NOT_IMPLEMENTED(decls.size() << " " << d);
                    }
                    if (parsing_struct_or_union) {
                        NOT_IMPLEMENTED("Function definition in struct/union");
                    }
                    {
                        push_scope ps{*this, const_cast<function_info*>(&d.t()->function_val())}; // Function scope
                        for (const auto& a: d.t()->function_val().params()) {
                            current_scope().declare(a);
                        }
                        decls.push_back(std::make_unique<init_decl>(decl_start, std::move(d), parse_compound_statement()));
                        ps.this_scope().check_labels();
                    }
                    current_scope().define(*decls.back());
                    return decls;
                } else {
                    NOT_IMPLEMENTED(d << " " << current() << " " << decls.size());
                }
            } else if (current().type() == token_type::eq) {
                if (parsing_struct_or_union) {
                    NOT_IMPLEMENTED("Initializer inside struct/union definition");
                }
                next();
                auto t = d.t();
                decls.push_back(std::make_unique<init_decl>(decl_start, std::move(d), parse_initializer(t)));
                current_scope().define(*decls.back());
            } else {
                decls.push_back(std::make_unique<init_decl>(decl_start, std::move(d)));
            }

            if (current().type() != token_type::comma) {
                break;
            }
            next();
        }

        try {
            EXPECT(semicolon);
        } catch (...) {
            std::cout << "Parsed declarations:\n";
            for (const auto& d: decls) {
                std::cout << "\t" << *d << "\n";
            }
            throw;
        }


        return decls;
    }

    expression_ptr parse_initializer(const type_ptr& t) {
        if (current().type() != token_type::lbrace) {
            auto e = parse_assignment_expression();
            auto et = e->et();
            if (t->base() == ctype::array_t) {
                if (et->base() == ctype::reference_t && et->reference_val()->base() == ctype::array_t) {
                    const auto at = t->array_val().t();
                    const auto it = et->reference_val()->array_val().t();
                    if (at->base() == it->base()) {
                        const auto b = at->base();
                        if (!is_arithmetic(b)) {
                            NOT_IMPLEMENTED(b << " in " << *e);
                        }
                        return e;
                    }
                }
            } else {
                et = decay(et);
                if (t->base() == ctype::pointer_t) {
                    handle_const_null(et, *e);
                }
                if (is_convertible(t, et)) {
                    return e;
                }
            }
            NOT_IMPLEMENTED("Invalid initializer for type " << *t << ": " << *et << " in " << *e);
        }

        type_ptr element_t;
        const std::vector<decl>* ds = nullptr;
        

        if (t->base() == ctype::array_t) {
            element_t = t->array_val().t();
        } else if (t->base() == ctype::struct_t || t->base() == ctype::union_t) {
            ds = &struct_union_members(*t);
        } else {
            NOT_IMPLEMENTED(*t);
        }

        const auto expression_start = current_source_pos_;
        next();
        std::vector<expression_ptr> es;
        while (current().type() != token_type::rbrace) {
            if (current().type() == token_type::lbracket || current().type() == token_type::dot) {
                NOT_IMPLEMENTED("designator " << current());
            }
            if (ds) {
                if (es.size() == ds->size()) {
                    NOT_IMPLEMENTED("Too many initializers");
                }
                element_t = (*ds)[es.size()].t();
            }
            es.push_back(parse_initializer(element_t));
            if (current().type() != token_type::comma) {
                break;
            }
            next();
        }
        EXPECT(rbrace);
        auto ret_t = t->base() == ctype::array_t ? make_ref_t(make_array_t(element_t, es.size())) : t;
        return std::make_unique<initializer_expression>(expression_start, ret_t, std::move(es));
    }

    std::shared_ptr<type> parse_declaration_specifiers() {
        auto res_type = std::make_shared<type>();
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
                res_type->add_flags(ctype_from_storage_class_token(t));
                next();
                continue;
            }

            if (is_type_qualifier(t)) {
                res_type->add_flags(ctype_from_type_qualifier_token(t));
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
                    if (res_type->base() != ctype::none) {
                        NOT_IMPLEMENTED(t << " in addition to " << *res_type);
                    }
                    switch (t) {
                    case token_type::void_:   res_type->set_base_type(ctype::void_t); break;
                    case token_type::char_:   res_type->set_base_type(ctype::plain_char_t); break;
                    case token_type::short_:  res_type->set_base_type(ctype::short_t); break;
                    case token_type::float_:  res_type->set_base_type(ctype::float_t); break;
                    case token_type::double_: res_type->set_base_type(ctype::double_t); break;
                    default:
                        NOT_IMPLEMENTED(t);
                    }
                }
                next();
                continue;
            }

            if (t == token_type::inline_) {
                res_type->add_flags(ctype::inline_f);
                next();
                continue;
            }

            if (t == token_type::__attribute___) {
                next();
                EXPECT(lparen);
                EXPECT(lparen);
                if (current().type() != token_type::id) {
                    NOT_IMPLEMENTED("__attribute__ " << current());
                }
                const auto id = current().text();
                next();
                EXPECT(rparen);
                EXPECT(rparen);
                std::cout << "Ignoring __attribute__((" << id << "))\n";
                continue;
            }

            if (t == token_type::struct_
                || t == token_type::union_
                || t == token_type::enum_) {
                next();

                if (res_type->base() != ctype::none) {
                    NOT_IMPLEMENTED("Invalid decl " << *res_type << " and " << t);
                }

                std::shared_ptr<tag_info_type> tag_type;
                std::string id;

                auto make_tag_info = [&]() {
                    assert(!id.empty());
                    assert(!tag_lookup(id));
                    if (t == token_type::struct_)  {
                        tag_type = std::make_shared<struct_info>(id);
                    } else if (t == token_type::union_) {
                        tag_type = std::make_shared<union_info>(id);
                    } else if (t == token_type::enum_) {
                        tag_type = std::make_shared<enum_info>(id);
                    } else {
                        NOT_IMPLEMENTED(t);
                    }
                    current_scope().declare(id, tag_type);
                };

                if (current().type() == token_type::id) {
                    id = current().text();
                    next();

                    if (auto sym = tag_lookup(id)) {
                        tag_type = sym->tag_info();
                    } else {
                        make_tag_info();
                    }
                }
                if (current().type() == token_type::lbrace) {
                    next();
                    if (!tag_type) {
                        assert(id.empty());
                        id = "__unnamed" + std::to_string(unnamed_cnt_++);
                        make_tag_info();
                    }
                    if (t == token_type::enum_) {
                        auto& eivs = dynamic_cast<enum_info&>(*tag_type).values_;
                        if (!eivs.empty()) {
                            NOT_IMPLEMENTED("Redefinition of " << t << " " << id);
                        }
                        eivs = parse_enum_list();
                    } else {
                        auto& sds = t == token_type::struct_ ? dynamic_cast<struct_info&>(*tag_type).members_ : dynamic_cast<union_info&>(*tag_type).members_;
                        if (!sds.empty()) {
                            NOT_IMPLEMENTED("Redefinition of " << t << " " << id);
                        }
                        sds = parse_struct_declaration_list();
                    }
                    EXPECT(rbrace);
                } else if (id.empty()) {
                    NOT_IMPLEMENTED(current());
                }

                assert(tag_type);
                res_type = make_tag_type(tag_type, res_type->ct());
                continue;
            }

            if (t == token_type::id) {
                auto sym = id_lookup(current().text());
                if (sym && !!(sym->decl_type()->ct() & ctype::typedef_f)) {
                    next();
                    auto saved_flags = res_type->ct() & ~ctype::base_f;
                    res_type = std::make_shared<type>(*sym->decl_type());
                    res_type->remove_flags(ctype::typedef_f);
                    res_type->add_flags(saved_flags);
                    continue;
                }
            }

            break;
        }

        if (!long_ && !int_ && sign == -1 && res_type->base() != ctype::none) {
            return res_type;
        }

        // Long double
        if (long_ == 1 && !int_ && sign == -1 && res_type->base() == ctype::double_t) {
            res_type->set_base_type(ctype::long_double_t);
            return res_type;
        }

        if (sign == 0) {
            res_type->add_flags(ctype::unsigned_f);
        }

        // short int/int/long int/long long int
        if (int_ == 0 || int_ == 1) {
            if (res_type->base() == ctype::none) {
                if (long_ == 0 && (int_ || sign != -1)) {
                    res_type->set_base_type(ctype::int_t);
                    return res_type;
                } else if (long_ == 1) {
                    res_type->set_base_type(ctype::long_t);
                    return res_type;
                } else if (long_ == 2) {
                    res_type->set_base_type(ctype::long_long_t);
                    return res_type;
                }
            } else if (res_type->base() == ctype::short_t) {
                return res_type;
            }
        }

        // signed char/ unsigned char
        if (!long_ && !int_ && sign != -1 && res_type->base() == ctype::plain_char_t) {
            res_type->set_base_type(ctype::char_t);
            return res_type;
        }

        NOT_IMPLEMENTED(*res_type << " long: " << long_ << " int: " << int_ << " sign: " << sign << " current: " << current());
    }

    decl parse_declarator(std::shared_ptr<const type> t) {
        // '*' type-qualifier-list? pointer?
        const auto storage_flags = t->ct() & ctype::storage_f;
        while (current().type() == token_type::star) {
            next();
            auto flags = storage_flags;
            while (is_type_qualifier(current().type())) {
                flags |=  ctype_from_type_qualifier_token(current().type());
                next();
            }
            auto pointee = std::make_shared<type>(*t);
            pointee->remove_flags(ctype::storage_f);
            t = make_ptr_t(pointee, flags);
        }
        return parse_direct_declarator(t);
    }

    decl parse_direct_declarator(std::shared_ptr<const type> t) {
        std::shared_ptr<type> inner_type{};
        std::string id;
        const auto storage_flags = t->ct() & ctype::storage_f;
        if (current().type() == token_type::lparen) {
            next();
            auto decl = parse_declarator(std::make_shared<type>());
            EXPECT(rparen);
            inner_type = std::make_shared<type>(*decl.t());
            id = decl.id();
        } else if (current().type() == token_type::id) {
            id = current().text();
            next();
        }
        while (current().type() == token_type::lbracket) {
            auto bound = array_info::unbounded;
            next();
            if (current().type() != token_type::rbracket) {
                auto civ = const_int_eval(*parse_assignment_expression());
                bound = cast(civ, ctype::long_long_t|ctype::unsigned_f).val;
                if (civ.val > bound) {
                    NOT_IMPLEMENTED(civ << " ~~ " << bound  << " array bound was negative?");
                }
            }
            EXPECT(rbracket);
            auto array_type = std::make_shared<type>(*t);
            array_type->remove_flags(ctype::storage_f);
            t = make_array_t(array_type, bound, storage_flags);
        }
        if (current().type() == token_type::lparen) {
            next();
            auto return_type = std::make_shared<type>(*t);
            return_type->remove_flags(ctype::storage_f);
            auto fi = parse_parameter_type_list(std::move(return_type));
            EXPECT(rparen);
            t = std::make_shared<type>(ctype::function_t | storage_flags, std::move(fi));
        }
        if (inner_type) {
            inner_type->remove_flags(ctype::storage_f);
            inner_type->modify_inner(t);
            inner_type->add_flags(storage_flags);
            t = inner_type;
        }
        return decl{t, id};
    }

    std::unique_ptr<function_info> parse_parameter_type_list(std::shared_ptr<type>&& return_type) {
        std::vector<decl> arg_types;
        bool variadic = false;
        while (current().type() != token_type::rparen) {
            if (current().type() == token_type::ellipsis) {
                next();
                variadic = true;
                break;
            }

            const auto ds = parse_declaration_specifiers();
            const auto d = parse_declarator(ds);
            if (d.t()->base() == ctype::none) {
                NOT_IMPLEMENTED(d);
            }
            arg_types.push_back(d);
            if (current().type() != token_type::comma) {
                break;
            }
            next();
        }
        if (arg_types.empty()) {
            variadic = true;
        }
        if (!variadic && arg_types.size() == 1 && arg_types[0].id().empty() && arg_types[0].t()->base() == ctype::void_t) {
            arg_types.clear();
        }
        return std::make_unique<function_info>(return_type, arg_types, variadic);
    }

    static void add_names(std::set<std::string>& s, const decl& d) {
        if (d.id().empty()) {
            add_names(s, d.t());
        } else if (!s.insert(d.id()).second) {
            NOT_IMPLEMENTED("Redefinitoin of struct/union member " << d);
        }
    }

    static void add_names(std::set<std::string>& s, const type_ptr& t) {
        if (t->base() != ctype::struct_t && t->base() != ctype::union_t) {
            NOT_IMPLEMENTED(*t << " not valid for unnamed struct/union member");
        }
        for (const auto& m: struct_union_members(*t)) {
            add_names(s, m);
        }
    }

    std::vector<decl> parse_struct_declaration_list() {
        std::vector<decl> decls;
        std::set<std::string> names;
        while (current().type() != token_type::rbrace) {
            auto ds = parse_declaration(true);
            for (auto& d: ds) {
                if (d->has_init_val()) {
                    NOT_IMPLEMENTED(*d);
                }
                add_names(names, d->d());
                decls.push_back(d->d());
            }
        }
        return decls;
    }

    std::vector<enum_value> parse_enum_list() {
        std::vector<enum_value> vals;
        const auto enum_val_ct = ctype::int_t;
        const auto enum_val_t = int_type;
        for (int64_t val = 0; current().type() != token_type::rbrace; ++val) {
            if (current().type() != token_type::id) {
                NOT_IMPLEMENTED("Expected identifier got " << current());
            }
            const auto id = current().text();
            next();
            if (current().type() == token_type::eq) {
                next();
                auto e = parse_constant_expression();
                val = cast(const_int_eval(*e), enum_val_ct).val;
            }
            vals.push_back(enum_value{id, val});
            current_scope().define(decl{enum_val_t, id}, const_int_val{static_cast<uint64_t>(val), enum_val_ct});
            if (current().type() != token_type::comma) {
                break;
            }
            next();
        }
        return vals;
    }

    std::shared_ptr<const type> parse_type_name() {
        // type-name
        //   specifier-qualifier-list abstract-declarator?
        auto ds = parse_declaration_specifiers();
        if (current().type() == token_type::rparen) {
            return ds;
        }
        auto d = parse_declarator(ds);
        if (!d.id().empty()) {
            NOT_IMPLEMENTED(d);
        }
        return d.t();
    }

    //
    // Expression
    //

    expression_ptr parse_identifier_expression(const source_position& pos, const std::string& id) {
        auto sym = this->id_lookup(id);
        if (!sym) {
            NOT_IMPLEMENTED("Unexpected identifier " << id);
        }
        if (sym->has_const_int_def()) {
            const auto ci = sym->const_int_def();
            return std::make_unique<const_int_expression>(pos, sym->decl_type(), ci);
        }
        return std::make_unique<identifier_expression>(pos, make_ref_t(sym->decl_type()), id);
    }

    expression_ptr parse_primary_expression() {
        const auto expression_start = current_source_pos_;
        // identifier
        // constant
        // string-literal
        // ( expression )
        const auto t = current().type();
        if (t == token_type::id) {
            auto id = current().text();
            next();
            return parse_identifier_expression(expression_start, id);
        } else if (t == token_type::const_int) {
            const auto v = current().int_val();
            next();
            return std::make_unique<const_int_expression>(expression_start, std::make_shared<type>(v.type), v);
        } else if (t == token_type::const_float) {
            const auto v = current().float_val();
            const auto ct = ctype::double_t;
            next();
            return std::make_unique<const_float_expression>(expression_start, std::make_shared<type>(ct), v, ct);
        } else if (t == token_type::char_lit) {
            const auto v = current().char_val();
            next();
            return std::make_unique<const_int_expression>(expression_start, int_type, const_int_val{v, ctype::int_t});
        } else if (t == token_type::string_lit) {
            auto e = std::make_unique<string_lit_expression>(expression_start, make_ref_t(make_array_t(const_char_type, 1+current().text().length())), current().text());
            next();
            return e;
        } else if (t == token_type::lparen) {
            next();
            auto e = parse_expression();
            EXPECT(rparen);
            return e;
        }
        NOT_IMPLEMENTED("Expected primary expression got " << current());
    }

    expression_ptr parse_postfix_expression1(expression_ptr&& e) {
        for (;;) {
            const auto expression_start = current_source_pos_;
            const auto t = current().type();
            if (t == token_type::lbracket) {
                auto at = decay(e->et());
                if (at->base() != ctype::pointer_t) {
                    NOT_IMPLEMENTED("Invalid array expression: " << *e);
                }
                next();
                auto index = parse_expression();
                auto it = decay(index->et())->ct();
                if (!is_integral(it)) {
                    NOT_IMPLEMENTED("Invalid index expression: " << *index << " type " << *index->et());
                }
                EXPECT(rbracket);
                e = std::make_unique<array_access_expression>(expression_start, make_ref_t(at->pointer_val()), std::move(e), std::move(index));
            } else if (t == token_type::lparen) {
                auto ft = decay(e->et());
                if (ft->base() != ctype::pointer_t || ft->pointer_val()->base() != ctype::function_t) {
                    NOT_IMPLEMENTED("Expected function in " << *e << " got " << *ft);
                }
                next();
                std::vector<expression_ptr> args;
                while (current().type() != token_type::rparen) {
                    args.push_back(parse_assignment_expression());
                    if (current().type() != token_type::comma) {
                        break;
                    }
                    next();
                }
                EXPECT(rparen);
                e = std::make_unique<function_call_expression>(expression_start, ft->pointer_val()->function_val().ret_type(), std::move(e), std::move(args));
            } else if (t == token_type::dot
                || t == token_type::arrow) {

                auto et = decay(e->et());
                if (t == token_type::arrow) {
                    if (et->base() != ctype::pointer_t) {
                        NOT_IMPLEMENTED("Expected pointer in " << *e << " got " << *et);
                    }
                    et = et->pointer_val();
                }
                if (et->base() != ctype::struct_t && et->base() != ctype::union_t) {
                    NOT_IMPLEMENTED(*et << " in " << *e);
                }

                next();
                const auto id = current().text();
                EXPECT(id);
                const auto& m = struct_union_member(et, id);
                auto mt = m.t();
                if (!!(et->ct() & ctype::cvr_f)) {
                    std::shared_ptr<type> temp = std::make_shared<type>(*m.t());
                    temp->add_flags(et->ct() & ctype::cvr_f);
                    mt = temp;
                }
                e = std::make_unique<access_expression>(expression_start, make_ref_t(m.t()), t, std::move(e), id);
            } else if (t == token_type::plusplus
                || t == token_type::minusminus) {
                next();

                auto et = e->et();
                if (et->base() != ctype::reference_t) {
                    NOT_IMPLEMENTED("Expected lvalue got " << *et << " in " << *e);
                }

                const auto rt = et->reference_val();

                if (rt->base() != ctype::pointer_t && !is_arithmetic(rt->base())) {
                    NOT_IMPLEMENTED("Expected pointer or arithmetic type got " << *et << " in " << *e);
                }

                e = std::make_unique<postfix_expression>(expression_start, et, t, std::move(e));
            } else {
                break;
            }
        }
        return std::move(e);
    }

    expression_ptr parse_postfix_expression() {
        return parse_postfix_expression1(parse_primary_expression());
    }

    expression_ptr parse_sizeof_expression(const source_position& expression_start) {
        if (current().type() != token_type::lparen) {
            return std::make_unique<sizeof_expression>(expression_start, size_t_type, parse_unary_expression());
        }
        next();
        if (is_current_type_name()) {
            auto st = parse_type_name();
            EXPECT(rparen);                
            return std::make_unique<sizeof_expression>(expression_start, size_t_type, st);
        } else {
            auto e = parse_expression();
            EXPECT(rparen);
            return std::make_unique<sizeof_expression>(expression_start, size_t_type, std::move(e));
        }
    }

    expression_ptr parse_unary_expression() {
        const auto expression_start = current_source_pos_;
        const auto t = current().type();

        if (t == token_type::plusplus
            || t == token_type::minusminus) {
            next();
            auto e = parse_unary_expression();
            auto et = e->et();
            if (et->base() != ctype::reference_t) {
                NOT_IMPLEMENTED("Expected lvalue in " << *e << " got " << *et);
            }
            return std::make_unique<prefix_expression>(expression_start, decay(et), t, std::move(e));
        }
        if (t == token_type::and_
            || t == token_type::star
            || t == token_type::plus
            || t == token_type::minus
            || t == token_type::bnot
            || t == token_type::not_) {
            next();
            auto e = parse_cast_expression();
            type_ptr et;
            if (t == token_type::and_) {
                et = e->et();
                if (et->base() != ctype::reference_t) {
                    NOT_IMPLEMENTED("Expected lvalue in " << *e << " got " << *et);
                }
                et = make_ptr_t(et->reference_val());
            } else if (t == token_type::star) {
                et = decay(e->et());
                if (et->base() != ctype::pointer_t) {
                    NOT_IMPLEMENTED("Expected pointer in " << *e << " got " << *et);
                }
                if (!!(et->ct() & ctype::cvr_f)) {
                    NOT_IMPLEMENTED(*et);
                }
                et = make_ref_t(et->pointer_val());                
            } else if (t == token_type::not_) {
                et = decay(e->et());
                if (!is_convertible(bool_type, et)) {
                    NOT_IMPLEMENTED("Invalid argument in " << *e << ": " << *et);
                }
                et = bool_type;
            } else if (t == token_type::bnot) {
                et = decay(e->et());
                if (!is_integral(et->ct())) {
                    NOT_IMPLEMENTED("Expected integral expression: " << t << " " << *e->et() << " " << *e);
                }
            } else {
                assert(t == token_type::plus || t == token_type::minus);
                et = decay(e->et());
                if (!is_arithmetic(et->ct())) {
                    NOT_IMPLEMENTED("Expected arithmetic expression: " << t << " " << *e->et() << " " << *e);
                }
            }

            return std::make_unique<prefix_expression>(expression_start, et, t, std::move(e));
        }
        if (t == token_type::sizeof_) {
            next();
            auto se = parse_sizeof_expression(expression_start);
            return std::make_unique<const_int_expression>(se->pos(), size_t_type, const_int_eval(*se));
        }
        return parse_postfix_expression();
    }

    expression_ptr parse_cast_expression() {
        const auto expression_start = current_source_pos_;
        if (current().type() == token_type::lparen) {
            next();
            if (is_current_type_name()) {
                auto cast_type = parse_type_name();
                EXPECT(rparen);
                expression_ptr e;
                if (current().type() == token_type::lbrace) {
                    // Bit of a hack to support compound literals
                    e = parse_initializer(cast_type);
                    assert(dynamic_cast<initializer_expression*>(e.get()));
                } else {
                    e = parse_cast_expression();
                }
                return std::make_unique<cast_expression>(expression_start, cast_type, std::move(e));
            } else {
                auto e = parse_expression();
                EXPECT(rparen);
                return parse_postfix_expression1(std::move(e));
            }
        }
        return parse_unary_expression();
    }

    expression_ptr parse_expression1(expression_ptr&& lhs, int outer_precedence) {
        for (;;) {
            const auto expression_start = lhs->pos();
            const auto op = current().type();
            const auto precedence = operator_precedence(op);
            if (precedence > outer_precedence) {
                break;
            }
            next();
            if (op == token_type::question) {
                auto l = parse_assignment_expression();
                EXPECT(colon);
                auto r = parse_assignment_expression();
                auto t = common_type(decay(l->et()), decay(r->et()));
                lhs = std::make_unique<conditional_expression>(expression_start, t, std::move(lhs), std::move(l), std::move(r));
                continue;
            }

            auto rhs = parse_cast_expression();
            for (;;) {
                const auto look_ahead = current().type();
                const auto look_ahead_precedence = operator_precedence(look_ahead);
                if (look_ahead_precedence > precedence /*|| (look_ahead_precedence == precedence && !is_right_to_left(look_ahead))*/) {
                    break;
                }
                rhs = parse_expression1(std::move(rhs), look_ahead_precedence);
            }

            auto lt = lhs->et();
            auto dlt = decay(lt);
            auto rt = decay(rhs->et());
            type_ptr t;

            const bool lp = dlt->base() == ctype::pointer_t;
            const bool rp = rt->base() == ctype::pointer_t;

            auto check_integral = [&](const type_ptr& ct) {
                if (!is_integral(ct->base())) {
                    NOT_IMPLEMENTED(*ct << " is not integral in " << *lhs << op << *rhs);
                }
            };
            auto check_convertible = [&](const type_ptr& l, const type_ptr& r, bool ignore_cvr = false) {
                if (l->base() == ctype::pointer_t && r->base() == ctype::pointer_t) {
                    if (is_compatible_pointer_type(l->pointer_val(), r->pointer_val(), ignore_cvr)) {
                        return;
                    }
                } else if (is_convertible(l, r)) {
                    return;
                }
                NOT_IMPLEMENTED(*r << " is not convertible to " << *l << " in " << *lhs << op << *rhs);
            };

            if (op == token_type::comma) {
                t = rt;
            } else if (op == token_type::andand || op == token_type::oror) {
                check_convertible(bool_type, dlt);
                check_convertible(bool_type, rt);
                t = bool_type;
            } else if (is_assignment_op(op)) {
                if (lt->base() != ctype::reference_t) {
                    NOT_IMPLEMENTED("Expected lvalue in " << *lhs << " got " << *lt);
                }
                if (op == token_type::eq && dlt->base() >= ctype::pointer_t) {
                    if (dlt->base() == ctype::pointer_t) {
                        handle_const_null(rt, *rhs);
                        check_convertible(dlt, rt);
                    } else if (dlt->base() == ctype::struct_t) {
                        if (rt->base() != ctype::struct_t || &dlt->struct_val() != &rt->struct_val()) {
                            NOT_IMPLEMENTED("Assignment of " << *rt << " to " << *lt);
                        }
                    } else if (dlt->base() == ctype::union_t) {
                        if (rt->base() != ctype::union_t || &dlt->union_val() != &rt->union_val()) {
                            NOT_IMPLEMENTED("Assignment of " << *rt << " to " << *lt);
                        }
                    } else {
                        NOT_IMPLEMENTED(*lhs << op << *rhs << " " << *lt << " " << *rt);
                    }
                } else if ((op == token_type::pluseq || op == token_type::minuseq) && lp) {
                    check_integral(rt);
                } else {
                    check_convertible(dlt, rt);
                }
                t = lt;
            } else if (is_comparison_op(op)) {
                if (lp) handle_const_null(rt, *rhs);
                if (rp) handle_const_null(dlt, *lhs);
                check_convertible(dlt, rt, true);
                t = bool_type;
            } else if ((op == token_type::plus || op == token_type::minus) && (lp || rp)) {
                if (lp && rp) {
                    if (op == token_type::plus || !is_compatible_pointer_type(dlt->pointer_val(), rt->pointer_val(), true)) {
                        NOT_IMPLEMENTED(*lhs << op << *rhs << " " << *dlt << " " << *rt);
                    }
                    t = ptrdiff_t_type;
                } else if (lp) {
                    check_integral(rt);
                    t = dlt;
                } else {
                    check_integral(dlt);
                    t = rt;
                }
            } else {
                if (dlt->base() >= ctype::pointer_t || rt->base() >= ctype::pointer_t) {
                    NOT_IMPLEMENTED(*lhs << op << *rhs << " " << *dlt << " " << *rt);
                }
                t = std::make_shared<type>(common_type(dlt->ct(), rt->ct()));
            }
            lhs = std::make_unique<binary_expression>(expression_start, t, op, std::move(lhs), std::move(rhs));
        }
        return std::move(lhs);
    }

    expression_ptr parse_expression0(int precedence) {
        return parse_expression1(parse_cast_expression(), precedence);
    }

    expression_ptr parse_expression() {
        return parse_expression0(operator_precedence(token_type::comma));
    }

    expression_ptr parse_assignment_expression() {
        return parse_expression0(operator_precedence(token_type::eq));
    }

    expression_ptr parse_constant_expression() {
        return parse_expression0(operator_precedence(token_type::oror));
    }

    //
    // Statement
    //

    statement_ptr parse_statement() {
        const auto statement_pos = current_source_pos_;
        if (is_current_type_name()) {
            auto d = parse_declaration(false);
            return std::make_unique<declaration_statement>(statement_pos, std::move(d));
        }
        const auto t = current().type();
        switch (t) {
        case token_type::semicolon:
            next();
            return std::make_unique<empty_statement>(statement_pos);
        case token_type::lbrace:
            return parse_compound_statement();
            // selection-statement
        case token_type::if_:
            {
                next();
                EXPECT(lparen);
                auto cond = parse_expression();
                EXPECT(rparen);
                statement_ptr if_s;
                {
                    push_scope ps{*this};
                    if_s = parse_statement();
                }
                statement_ptr else_s{};
                if (current().type() == token_type::else_) {
                    next();
                    push_scope ps{*this};
                    else_s = parse_statement();
                }
                return std::make_unique<if_statement>(statement_pos, std::move(cond), std::move(if_s), std::move(else_s));
            }
        case token_type::switch_:
            {
                next();
                EXPECT(lparen);
                auto e = parse_expression();
                EXPECT(rparen);
                push_scope ps{*this};
                return std::make_unique<switch_statement>(statement_pos, std::move(e), parse_statement());
            }
            // iteration-statement
        case token_type::while_:
            {
                next();
                EXPECT(lparen);
                auto cond = parse_expression();
                EXPECT(rparen);
                push_scope ps{*this};
                return std::make_unique<while_statement>(statement_pos, std::move(cond), parse_statement());
            }
        case token_type::do_:
            {
                next();
                push_scope ps{*this};
                auto s = parse_statement();
                EXPECT(while_);
                EXPECT(lparen);
                auto cond = parse_expression();
                EXPECT(rparen);
                EXPECT(semicolon);
                return std::make_unique<do_statement>(statement_pos, std::move(cond), std::move(s));
            }
        case token_type::for_:
            {
                next();
                expression_ptr cond{}, iter{};
                EXPECT(lparen);
                push_scope ps{*this};
                auto init = parse_statement();
                if (current().type() != token_type::semicolon) {
                    cond = parse_expression();
                }
                EXPECT(semicolon);
                if (current().type() != token_type::rparen) {
                    iter = parse_expression();
                }
                EXPECT(rparen);
                return std::make_unique<for_statement>(statement_pos, std::move(init), std::move(cond), std::move(iter), parse_statement());
            }
            // jump-statement
        case token_type::goto_:
            {
                next();
                const auto id = current().text();
                EXPECT(id);
                EXPECT(semicolon);
                auto sym = get_label(id);
                sym->use_label();
                return std::make_unique<goto_statement>(statement_pos, id);
            }
        case token_type::continue_:
            next();
            EXPECT(semicolon);
            return std::make_unique<continue_statement>(statement_pos);
        case token_type::break_:
            next();
            EXPECT(semicolon);
            return std::make_unique<break_statement>(statement_pos);
        case token_type::return_:
            {
                next();
                auto rtype = current_function_info().ret_type();
                if (current().type() == token_type::semicolon) {
                    next();
                    if (rtype->base() != ctype::void_t) {
                        NOT_IMPLEMENTED("Expected return type " << *rtype);
                    }
                    return std::make_unique<return_statement>(statement_pos, nullptr);
                } else {
                    auto e = parse_expression();
                    EXPECT(semicolon);
                    if (!is_convertible(rtype, decay(e->et()))) {
                        NOT_IMPLEMENTED("Invalid return type " << *e->et() << " expecting " << *rtype);
                    }
                    return std::make_unique<return_statement>(statement_pos, std::move(e));
                }
            }
        case token_type::case_:
            {
                next();
                auto e = parse_constant_expression();
                EXPECT(colon);
                return std::make_unique<labeled_statement>(statement_pos, std::move(e), parse_statement());
            }
        case token_type::default_:
            next();
            EXPECT(colon);
            return std::make_unique<labeled_statement>(statement_pos, parse_statement());
        default:
            break;
        }

        expression_ptr e{};
        if (t == token_type::id) {
            auto id = current().text();
            next();
            if (current().type() == token_type::colon) {
                next();
                auto sym = get_label(id);
                sym->define_label();
                return std::make_unique<labeled_statement>(statement_pos, id, parse_statement());
            }
            e = parse_expression1(parse_postfix_expression1(parse_identifier_expression(statement_pos, id)), operator_precedence(token_type::comma));
        } else {
            e = parse_expression();
        }
        try {
            EXPECT(semicolon);
        } catch (...) {
            std::cout << "Parsed expression: " << *e << "\n";
            throw;
        }
        return std::make_unique<expression_statement>(statement_pos, std::move(e));
    }

    std::unique_ptr<compound_statement> parse_compound_statement() {
        const auto statement_pos = current_source_pos_;
        push_scope ps{*this};
        std::vector<statement_ptr> ss;
        EXPECT(lbrace);
        while (current().type() != token_type::rbrace) {
            ss.push_back(parse_statement());
        }
        assert(current().type() == token_type::rbrace);
        next();
        return std::make_unique<compound_statement>(statement_pos, std::move(ss));
    }

    const_int_val const_int_lookup(const std::string& id);
    const_int_val const_int_eval(const expression& e);
    size_t alignof_type(const type& t);
    size_t sizeof_type(const type& t);
};

const_int_val parser::const_int_lookup(const std::string& id) {    
    /*for (const auto& tt: tag_types_) {
        if (tt->base_type() != ctype::enum_t) {
            continue;
        }
        const auto ei = static_cast<const enum_info&>(*tt);
        for (const auto& v: ei.values()) {
            if (v.id() == id) {
                return const_int_val{static_cast<uint64_t>(v.val()), ctype::long_long_t};
            }
        }
    }*/
    NOT_IMPLEMENTED(id);
}

const_int_val parser::const_int_eval(const expression& e) {
    if (auto cie = dynamic_cast<const const_int_expression*>(&e)) {
        return cie->val();
    } else if (auto ie = dynamic_cast<const identifier_expression*>(&e)) {
        return const_int_lookup(ie->id());
    } else if (auto ue = dynamic_cast<const unary_expression*>(&e)) {
        auto val = const_int_eval(ue->e());
        switch (ue->op()) {
        case token_type::plus:
            break;
        case token_type::minus:
            val.val ^= UINT64_MAX;
            ++val.val;
            break;
        case token_type::bnot:
            val.val ^= UINT64_MAX;
            break;
        case token_type::not_:
            val.val = val.val ? 0 : 1;
            break;
        default:
            NOT_IMPLEMENTED(ue->op() << " " << val);
        }
        return val;
    } else if (auto be = dynamic_cast<const binary_expression*>(&e)) {
        auto l = const_int_eval(be->l());
        auto r = const_int_eval(be->r());
        const auto t = common_type(l.type, r.type);
        l = cast(l, t);
        r = cast(r, t);
        switch (be->op()) {
#define DO(op) return const_int_val{!(t & ctype::unsigned_f) ? static_cast<uint64_t>(static_cast<int64_t>(l.val) op static_cast<int64_t>(r.val)): l.val op r.val, t}
#define DO_DIV(op) if (!r.val) NOT_IMPLEMENTED("Division by zero"); DO(op)
        case token_type::plus:   DO(+);
        case token_type::minus:  DO(-);
        case token_type::star:   DO(*);
        case token_type::div:    DO_DIV(/);
        case token_type::mod:    DO_DIV(%);
        case token_type::lt:     DO(< );
        case token_type::lteq:   DO(<=);
        case token_type::gteq:   DO(>=);
        case token_type::gt:     DO(> );
        case token_type::eqeq:   DO(==);
        case token_type::noteq:  DO(!=);
        case token_type::lshift: DO(<<);
        case token_type::rshift: DO(>>);
#undef DO
        default:
            NOT_IMPLEMENTED(l << " " << be->op() << " " << r);
        }
    } else if (auto se = dynamic_cast<const sizeof_expression*>(&e)) {
        uint64_t size;
        if (se->arg_is_type()) {
            size = sizeof_type(*se->t());
        } else {
            size = sizeof_type(*to_rvalue(se->e().et()));
        }
        return const_int_val{size, ctype::long_long_t | ctype::unsigned_f};
    } else if (auto ce = dynamic_cast<const conditional_expression*>(&e)) {
        if (const_int_eval(ce->cond()).val) {
            return const_int_eval(ce->l());
        } else {
            return const_int_eval(ce->r());
        }
    }

    NOT_IMPLEMENTED(e);
}

size_t parser::alignof_type(const type& t) {
    const auto base = t.base();
    if (is_arithmetic(base) || base == ctype::pointer_t) {
        return sizeof_type(t);
    } else if (base == ctype::struct_t || base == ctype::union_t) {
        size_t align = 1;
        for (const auto& m: struct_union_members(t)) {
            align = std::max(align, alignof_type(*m.t()));
        }
        return align;
    } else if (base == ctype::array_t) {
        return alignof_type(*t.array_val().t());
    } else if (base == ctype::enum_t) {
        return sizeof_type(*int_type);
    }

    NOT_IMPLEMENTED(t);
}

constexpr size_t round_up(size_t val, size_t align) {
    return (val + align - 1) & ~(align - 1);
}

size_t parser::sizeof_type(const type& t) {
    constexpr size_t pointer_size = 8;
    switch (t.base()) {
    case ctype::void_t:         NOT_IMPLEMENTED(t);
    case ctype::plain_char_t:   return 1;
    case ctype::char_t:         return 1;
    case ctype::short_t:        return 2;
    case ctype::int_t:          return 4;
    case ctype::long_t:         return 4;
    case ctype::long_long_t:    return 8;
    case ctype::float_t:        return 4;
    case ctype::double_t:       return 8;
    case ctype::long_double_t:  return 8;
    case ctype::pointer_t:      return pointer_size;
    case ctype::array_t:
        if (const auto& ai = t.array_val(); ai.bound() == array_info::unbounded) {
            return pointer_size;
        } else {
            if (ai.bound() == 0) {
                NOT_IMPLEMENTED(t);
            }
            return ai.bound() * sizeof_type(*ai.t());
        }
    case ctype::struct_t:
        {
            size_t size = 0;
            size_t max_align = 1;
            for (const auto& m: t.struct_val().members()) {
                const auto a = alignof_type(*m.t());
                size = round_up(size, a) + sizeof_type(*m.t());
                max_align = std::max(max_align, a);
            }
            return round_up(size, max_align);
        }
    case ctype::union_t:
        {
            size_t size = 0;
            for (const auto& m: t.union_val().members()) {
                size = std::max(size, sizeof_type(*m.t()));                
            }
            return size;
        }
    case ctype::enum_t:         return sizeof_type(*int_type);
    case ctype::function_t:     NOT_IMPLEMENTED(t);
    default:
        NOT_IMPLEMENTED(t);
    }
}

std::vector<std::unique_ptr<init_decl>> parse(source_manager& sm, const source_file& source) {
    parser p{sm, source};
    return p.parse();
}

} // namespace mcc
