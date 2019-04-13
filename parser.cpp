#include "parser.h"
#include "util.h"
#include "lexer.h"
#include <map>
#include <iostream>

namespace mcc {

void string_lit_expression::do_print(std::ostream& os) const {
    os << quoted(text_);
}

void declaration_statement::do_print(std::ostream& os) const {
    for (const auto& d: ds_) {
        os << *d << ";";
    }
}

#define EXPECT(tok) do { if (current().type() != token_type::tok) NOT_IMPLEMENTED("Expected " << token_type::tok << " got " << current()); next(); } while (0)
#define TRACE(msg) std::cout << __FILE__ << ":" << __LINE__ << ": " << __func__ <<  " Current: " << current() << " " << msg << "\n"

class scope {
public:
    explicit scope() {
    }

    void add(const std::string& id, const std::shared_ptr<const type>& t) {
        if (auto it = vals_.find(id); it != vals_.end()) {
            const auto type_diff = t->ct() ^ it->second->ct();
            if (type_diff != ctype::none && type_diff != ctype::static_f && type_diff != ctype::extern_f) {
                NOT_IMPLEMENTED(*t << " " << id << " already defined as " << *it->second << " (diff: " << type_diff << ")");
            }
            return;
        }
        vals_.insert({id, t});
    }

    std::shared_ptr<const type> lookup(const std::string& id) {
        auto it = vals_.find(id);
        return it != vals_.end() ? it->second : nullptr;
    }

private:
    std::map<std::string, std::shared_ptr<const type>> vals_;
};

class parser {
public:
    explicit parser(source_manager& sm, const source_file& source) : lex_{sm, source} {
    }
    ~parser() {
        assert(active_scopes_.empty());
    }

    std::vector<std::unique_ptr<init_decl>> parse() {
        try {
            push_scope global_scope{*this};
            std::vector<std::unique_ptr<init_decl>> res;
            while (current().type() != token_type::eof) {
                auto d = parse_declaration();
                add_scope_decls(d);
                res.insert(res.end(), std::make_move_iterator(d.begin()), std::make_move_iterator(d.end()));
            }
            return res;
        } catch (const std::exception& e) {
            std::ostringstream oss;
            oss << e.what() << "\n\n";
            for (const auto& p : lex_.position()) {
                oss << p << "\n";
            }
            oss << "Current token: " << current();
            throw std::runtime_error(oss.str());
        }
    }

private:
    lexer lex_;
    int unnamed_cnt_ = 0;
    std::vector<std::shared_ptr<tag_info_type>> tag_types_;
    std::map<std::string, std::shared_ptr<type>> typedefs_;
    std::vector<std::unique_ptr<scope>> active_scopes_;

    class push_scope {
    public:
        explicit push_scope(parser& p) : p_{p} {
            p_.active_scopes_.push_back(std::make_unique<scope>());
        }
        ~push_scope() {
            p_.active_scopes_.pop_back();
        }
    private:
        parser& p_;
    };

    void add_scope_decl(const std::string& id, const std::shared_ptr<const type>& t) {
        assert(!active_scopes_.empty());
        assert(!id.empty());
        active_scopes_.back()->add(id, t);
    }

    void add_scope_decls(const std::vector<std::unique_ptr<init_decl>>& ds) {
        for (const auto& d: ds) {
            add_scope_decl(d->d().id(), d->d().t());
        }
    }

    std::shared_ptr<const type> scope_lookup(const std::string& id) {
        assert(!active_scopes_.empty());
        for (auto it = active_scopes_.crbegin(), end = active_scopes_.crend(); it != end; ++it) {
            if (auto t = (*it)->lookup(id)) {
                return t;
            }
        }
        NOT_IMPLEMENTED(id << " not found in current scope");
    }

    std::shared_ptr<tag_info_type> find_tag_type(const std::string_view id) {
        for (auto& s: tag_types_) {
            if (s->id() == id) {
                return s;
            }
        }
        return nullptr;
    }

    std::shared_ptr<type> find_typedef(const std::string& id) const {
        if (auto it = typedefs_.find(id); it != typedefs_.end()) {
            assert(!(it->second->ct() & ctype::typedef_f));
            return it->second;
        }
        return nullptr;
    }

    void next() {
        //std::cout << "Consuming " << current() << "\n";
        assert(current().type() != token_type::eof);
        lex_.next();
    }

    const token& current() const {
        return lex_.current();
    }

    bool is_current_type_name() const {
        const auto t = current().type();
        if (t == token_type::struct_ || t == token_type::union_ || t == token_type::enum_) {
            return true;
        }
        if (t == token_type::id) {
            auto id = current().text();
            if (find_typedef(id)) {
                return true;
            }
            return false;
        }
        if (is_literal(t)) {
            return false;
        }
        if (is_storage_class_specifier(t) || is_type_qualifier(t) || is_type_qualifier(t) || is_simple_type_specifier(t)) {
            return true;
        }
        return false;
    }

    std::vector<std::unique_ptr<init_decl>> parse_declaration() {
        // declaration
        //    declaration_specifiers init-declarator-list? ';'
        // init-declarator-list
        //    init-declarator
        //    init-declarator-list , init-declarator        
        // init-declarator
        //     declarator
        //     declarator = initializer

        const auto ds = parse_declaration_specifiers();

        if (current().type() == token_type::semicolon) {
            // Type decl.
            if (!!(ds->ct() & ctype::typedef_f)) {
                NOT_IMPLEMENTED(*ds);
            }
            next();
            return {};
        }

        std::vector<std::unique_ptr<init_decl>> decls;
        for (;;) {
            auto d = parse_declarator(ds);

            if (!!(d.t()->ct() & ctype::typedef_f)) {
                if (d.id().empty()) {
                    NOT_IMPLEMENTED(d);
                }
                if (auto it = typedefs_.find(d.id()); it != typedefs_.end()) {
                    NOT_IMPLEMENTED(d << " Already defined as " << it->first << " " << it->second);
                }
                auto t = std::make_shared<type>(*d.t());
                t->remove_flags(ctype::typedef_f);
                typedefs_[d.id()] = t;
            } else {
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

                if (current().type() == token_type::lbrace) {
                    if (d.t()->base() == ctype::function_t) {
                        if (!decls.empty()) {
                            NOT_IMPLEMENTED(decls.size() << " " << d);
                        }
                        decls.push_back(std::make_unique<init_decl>(std::move(d), parse_compound_statement()));
                        return decls;
                    } else {
                        NOT_IMPLEMENTED(d << " " << current() << " " << decls.size());
                    }
                } else if (current().type() == token_type::eq) {
                    next();
                    decls.push_back(std::make_unique<init_decl>(std::move(d), parse_initializer()));
                } else {
                    decls.push_back(std::make_unique<init_decl>(std::move(d)));
                }
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

    expression_ptr parse_initializer() {
        if (current().type() != token_type::lbrace) {
            return parse_assignment_expression();
        }
        next();
        std::vector<expression_ptr> es;
        while (current().type() != token_type::rbrace) {
            if (current().type() == token_type::lbracket || current().type() == token_type::dot) {
                NOT_IMPLEMENTED("designator " << current());
            }
            es.push_back(parse_initializer());
            if (current().type() != token_type::comma) {
                break;
            }
            next();
        }
        EXPECT(rbrace);
        return std::make_unique<initializer_expression>(std::move(es));
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
                    assert(!find_tag_type(id));
                    if (t == token_type::struct_)  {
                        tag_type = std::make_shared<struct_info>(id);
                    } else if (t == token_type::union_) {
                        tag_type = std::make_shared<union_info>(id);
                    } else if (t == token_type::enum_) {
                        tag_type = std::make_shared<enum_info>(id);
                    } else {
                        NOT_IMPLEMENTED(t);
                    }
                    tag_types_.push_back(tag_type);
                };

                if (current().type() == token_type::id) {
                    id = current().text();
                    next();

                    tag_type = find_tag_type(id);
                    if (!tag_type) {
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
                if (auto td = find_typedef(current().text())) {
                    if (res_type->base() != ctype::none) {
                        NOT_IMPLEMENTED("typedef " << *td << " combine with " << *res_type);
                    }
                    auto saved_flags = res_type->ct() & ~ctype::base_f;
                    res_type = std::make_shared<type>(*td);
                    res_type->add_flags(saved_flags);
                    next();
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
            ctype pt = ctype::pointer_t | storage_flags;
            while (is_type_qualifier(current().type())) {
                pt |=  ctype_from_type_qualifier_token(current().type());
                next();
            }
            auto pointee = std::make_shared<type>(*t);
            pointee->remove_flags(ctype::storage_f);
            t = std::make_shared<type>(pt, t);
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
            t = std::make_shared<type>(ctype::array_t | storage_flags, std::make_unique<array_info>(array_type, bound));
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
            inner_type->modify_inner(t);
            t = inner_type;
        }
        return decl{t, id};
    }

    std::unique_ptr<function_info> parse_parameter_type_list(std::shared_ptr<type>&& return_type) {
        std::vector<decl> arg_types;
        bool variadic = false;
        for (;;) {
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
        return std::make_unique<function_info>(return_type, arg_types, variadic);
    }

    std::vector<decl> parse_struct_declaration_list() {
        std::vector<decl> decls;
        while (current().type() != token_type::rbrace) {
            auto ds = parse_declaration();
            for (auto& d: ds) {
                if (d->has_init_val()) {
                    NOT_IMPLEMENTED(*d);
                }
                decls.push_back(d->d());
            }
        }
        return decls;
    }

    std::vector<enum_value> parse_enum_list() {
        std::vector<enum_value> vals;        
        for (int64_t val = 0; current().type() != token_type::rbrace; ++val) {
            if (current().type() != token_type::id) {
                NOT_IMPLEMENTED("Expected identifier got " << current());
            }
            const auto id = current().text();
            next();
            if (current().type() == token_type::eq) {
                next();
                auto e = parse_constant_expression();
                val = cast(const_int_eval(*e), ctype::long_long_t).val;
            }
            vals.push_back(enum_value{id, val});
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

    expression_ptr parse_primary_expression() {
        // identifier
        // constant
        // string-literal
        // ( expression )
        const auto t = current().type();
        if (t == token_type::id) {
            auto id = current().text();
            next();
            return std::make_unique<identifier_expression>(id);
        } else if (t == token_type::const_int) {
            const auto v = current().int_val();
            next();
            return std::make_unique<const_int_expression>(v);
        } else if (t == token_type::const_float) {
            const auto v = current().float_val();
            next();
            return std::make_unique<const_float_expression>(v, ctype::double_t);
        } else if (t == token_type::char_lit) {
            const auto v = current().char_val();
            next();
            return std::make_unique<const_int_expression>(const_int_val{v, ctype::int_t});
        } else if (t == token_type::string_lit) {
            auto e = std::make_unique<string_lit_expression>(current().text());
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
            const auto t = current().type();
            if (t == token_type::lbracket) {
                next();
                auto index = parse_expression();
                EXPECT(rbracket);
                e = std::make_unique<array_access_expression>(std::move(e), std::move(index));
            } else if (t == token_type::lparen) {
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
                e = std::make_unique<function_call_expression>(std::move(e), std::move(args));
            } else if (t == token_type::dot
                || t == token_type::arrow) {
                next();
                const auto id = current().text();
                EXPECT(id);
                e = std::make_unique<access_expression>(t, std::move(e), id);
            } else if (t == token_type::plusplus
                || t == token_type::minusminus) {
                next();
                e = std::make_unique<postfix_expression>(t, std::move(e));
            } else {
                break;
            }
        }
        return std::move(e);
    }

    expression_ptr parse_postfix_expression() {
        return parse_postfix_expression1(parse_primary_expression());
    }

    expression_ptr parse_unary_expression() {
        const auto t = current().type();
        if (t == token_type::plusplus
            || t == token_type::minusminus) {
            next();
            return std::make_unique<prefix_expression>(t, parse_unary_expression());
        }
        if (t == token_type::and_
            || t == token_type::star
            || t == token_type::plus
            || t == token_type::minus
            || t == token_type::bnot
            || t == token_type::not_) {
            next();
            return std::make_unique<prefix_expression>(t, parse_cast_expression());
        }
        if (t == token_type::sizeof_) {
            next();
            if (current().type() != token_type::lparen) {
                return std::make_unique<sizeof_expression>(parse_unary_expression());
            }
            next();
            if (is_current_type_name()) {
                auto st = parse_type_name();
                EXPECT(rparen);
                return std::make_unique<sizeof_expression>(st);
            } else {
                auto e = parse_expression();
                EXPECT(rparen);
                return std::make_unique<sizeof_expression>(std::move(e));
            }
        }
        return parse_postfix_expression();
    }

    expression_ptr parse_cast_expression() {
        if (current().type() == token_type::lparen) {
            next();
            if (is_current_type_name()) {
                auto cast_type = parse_type_name();
                EXPECT(rparen);
                return std::make_unique<cast_expression>(cast_type, parse_cast_expression());
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
            const auto op = current().type();
            const auto precedence = operator_precedence(op);
            if (precedence > outer_precedence) {
                break;
            }
            next();
            if (op == token_type::question) {
                auto l = parse_assignment_expression();
                EXPECT(colon);
                lhs = std::make_unique<conditional_expression>(std::move(lhs), std::move(l), parse_assignment_expression());
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

            lhs = std::make_unique<binary_expression>(op, std::move(lhs), std::move(rhs));
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
        if (is_current_type_name()) {
            auto d = parse_declaration();
            add_scope_decls(d);
            return std::make_unique<declaration_statement>(std::move(d));
        }
        const auto t = current().type();
        switch (t) {
        case token_type::semicolon:
            next();
            return std::make_unique<empty_statement>();
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
                return std::make_unique<if_statement>(std::move(cond), std::move(if_s), std::move(else_s));
            }
        case token_type::switch_:
            {
                next();
                EXPECT(lparen);
                auto e = parse_expression();
                EXPECT(rparen);
                push_scope ps{*this};
                return std::make_unique<switch_statement>(std::move(e), parse_statement());
            }
            // iteration-statement
        case token_type::while_:
            {
                next();
                EXPECT(lparen);
                auto cond = parse_expression();
                EXPECT(rparen);
                push_scope ps{*this};
                return std::make_unique<while_statement>(std::move(cond), parse_statement());
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
                return std::make_unique<do_statement>(std::move(cond), std::move(s));
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
                return std::make_unique<for_statement>(std::move(init), std::move(cond), std::move(iter), parse_statement());
            }
            // jump-statement
        case token_type::goto_:
            {
                next();
                const auto id = current().text();
                EXPECT(id);
                EXPECT(semicolon);
                return std::make_unique<goto_statement>(id);
            }
        case token_type::continue_:
            next();
            EXPECT(semicolon);
            return std::make_unique<continue_statement>();
        case token_type::break_:
            next();
            EXPECT(semicolon);
            return std::make_unique<break_statement>();
        case token_type::return_:
            next();
            if (current().type() == token_type::semicolon) {
                next();
                return std::make_unique<return_statement>(nullptr);
            } else {
                auto e = parse_expression();
                EXPECT(semicolon);
                return std::make_unique<return_statement>(std::move(e));
            }
        case token_type::case_:
            {
                next();
                auto e = parse_constant_expression();
                EXPECT(colon);
                return std::make_unique<labeled_statement>(std::move(e), parse_statement());
            }
        case token_type::default_:
            next();
            EXPECT(colon);
            return std::make_unique<labeled_statement>(parse_statement());
        default:
            break;
        }

        expression_ptr e{};
        if (t == token_type::id) {
            auto id = current().text();
            next();
            if (current().type() == token_type::colon) {
                next();
                return std::make_unique<labeled_statement>(id, parse_statement());
            }
            e = parse_expression1(parse_postfix_expression1(std::make_unique<identifier_expression>(id)), operator_precedence(token_type::comma));
        } else {
            e = parse_expression();
        }
        try {
            EXPECT(semicolon);
        } catch (...) {
            std::cout << "Parsed expression: " << *e << "\n";
            throw;
        }
        return std::make_unique<expression_statement>(std::move(e));
    }

    std::unique_ptr<compound_statement> parse_compound_statement() {
        push_scope ps{*this};
        std::vector<statement_ptr> ss;
        EXPECT(lbrace);
        while (current().type() != token_type::rbrace) {
            ss.push_back(parse_statement());
        }
        assert(current().type() == token_type::rbrace);
        next();
        return std::make_unique<compound_statement>(std::move(ss));
    }

    const_int_val const_int_lookup(const std::string& id);
    const_int_val const_int_eval(const expression& e);
    std::shared_ptr<const type> expression_type(const expression& e);
    size_t sizeof_type(const type& t);
};

const_int_val parser::const_int_lookup(const std::string& id) {    
    for (const auto& tt: tag_types_) {
        if (tt->base_type() != ctype::enum_t) {
            continue;
        }
        const auto ei = static_cast<const enum_info&>(*tt);
        for (const auto& v: ei.values()) {
            if (v.id() == id) {
                return const_int_val{static_cast<uint64_t>(v.val()), ctype::long_long_t};
            }
        }
    }
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
        case token_type::plus:  return const_int_val{l.val+r.val, t};
        case token_type::minus: return const_int_val{l.val-r.val, t};
        case token_type::star:  return const_int_val{l.val*r.val, t};
        case token_type::div:   if (!r.val) NOT_IMPLEMENTED("Division by zero"); return const_int_val{l.val/r.val, t};
        case token_type::mod:   if (!r.val) NOT_IMPLEMENTED("Division by zero"); return const_int_val{l.val%r.val, t};
        default:
            NOT_IMPLEMENTED(l << " " << be->op() << " " << r);
        }
    } else if (auto se = dynamic_cast<const sizeof_expression*>(&e)) {
        if (se->arg_is_type()) {
            NOT_IMPLEMENTED("sizeof " << *se->t());
        } else {
            return const_int_val{sizeof_type(*expression_type(se->e())), ctype::long_long_t | ctype::unsigned_f};
        }
    }

    NOT_IMPLEMENTED(e);
}

std::shared_ptr<const type> parser::expression_type(const expression& e) {
    if (auto ae = dynamic_cast<const access_expression*>(&e)) {
        auto t = expression_type(ae->e());
        if (ae->op() == token_type::arrow) {
            if (t->base() != ctype::pointer_t) {
                NOT_IMPLEMENTED(e << " t: " << *t << " id: " << ae->id());
            }
            t = t->pointer_val();
        }
        if (t->base() == ctype::struct_t || t->base() == ctype::union_t) {
            const auto& members = t->base() == ctype::struct_t ? t->struct_val().members() : t->union_val().members();
            for (const auto& m: members) {
                if (m.id() == ae->id()) {
                    return m.t();
                }
            }
        }
        NOT_IMPLEMENTED(e << " t: " << *t << " id: " << ae->id());
    } else if (auto ie = dynamic_cast<const identifier_expression*>(&e)) {
        return scope_lookup(ie->id());
    }
    NOT_IMPLEMENTED(e);
}

size_t parser::sizeof_type(const type& t) {
    const size_t pointer_size = 8;
    switch (t.base()) {
    case ctype::void_t:         NOT_IMPLEMENTED(t);
    case ctype::plain_char_t:   return 1;
    case ctype::char_t:         return 1;
    case ctype::short_t:        return 2;
    case ctype::int_t:          return 4;
    case ctype::long_t:         return 8;
    case ctype::long_long_t:    return 8;
    case ctype::float_t:        return 4;
    case ctype::double_t:       return 8;
    case ctype::long_double_t:  return 8;
    case ctype::pointer_t:      return pointer_size;
    case ctype::array_t:
        if (const auto& ai = t.array_val(); ai.bound() == array_info::unbounded) {
            return pointer_size;
        } else {
            return ai.bound() * sizeof_type(*ai.t());
        }
    case ctype::struct_t:       NOT_IMPLEMENTED(t);
    case ctype::union_t:        NOT_IMPLEMENTED(t);
    case ctype::enum_t:         NOT_IMPLEMENTED(t);
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
