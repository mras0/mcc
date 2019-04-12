#include "parser.h"
#include "util.h"
#include "lexer.h"
#include <map>
#include <iostream>

namespace mcc {

const_int_val const_int_eval(const expression& e) {
    if (auto cie = dynamic_cast<const const_int_expression*>(&e)) {
        return cie->val();
    } else if (auto be = dynamic_cast<const binary_expression*>(&e)) {
        auto l = const_int_eval(be->l());
        auto r = const_int_eval(be->r());
        const auto t = common_type(l.type, r.type);
        l = cast(l, t);
        r = cast(r, t);
        switch (be->op()) {
        case token_type::plus:  return const_int_val{l.val+r.val, t};
        case token_type::minus: return const_int_val{l.val-r.val, t};
        default:
            NOT_IMPLEMENTED(l << " " << be->op() << " " << r);
        }
    }

    NOT_IMPLEMENTED(e);
}

#define EXPECT(tok) do { if (current().type() != token_type::tok) NOT_IMPLEMENTED("Expected " << token_type::tok << " got " << current()); next(); } while (0)
#define TRACE(msg) std::cout << __FILE__ << ":" << __LINE__ << ": " << __func__ <<  " Current: " << current() << " " << msg << "\n"

class parser::impl {
public:
    explicit impl(source_manager& sm, const source_file& source) : lex_{sm, source} {
    }

    auto position() const {
        return lex_.position();
    }

    void parse() {
        for (;;) {
            parse_declaration();
        }
    }

private:
    lexer lex_;
    int unnamed_cnt_ = 0;
    std::vector<std::shared_ptr<tag_info_type>> tag_types_;
    std::map<std::string, std::shared_ptr<type>> typedefs_;

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
        assert(current().type() != token_type::eof);
        lex_.next();
    }

    const token& current() const {
        return lex_.current();
    }

    void parse_declaration() {
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
                NOT_IMPLEMENTED(ds);
            }
            std::cout << *ds << "\n";
            next();
            return;
        }

        std::vector<decl> decls;
        for (;;) {
            auto d = parse_declarator(ds);

            if (!!(d.t()->ct() & ctype::typedef_f)) {
                std::cout << d << "\n";
                if (d.id().empty()) {
                    NOT_IMPLEMENTED(d);
                }
                if (auto it = typedefs_.find(d.id()); it != typedefs_.end()) {
                    NOT_IMPLEMENTED(d << " Already defined as " << it->first << " " << it->second);
                }
                auto t = std::make_shared<type>(*d.t());
                t->remove_flags(ctype::typedef_f);
                typedefs_[d.id()] = t;
                break;
            }

            if (current().type() == token_type::colon) {
                // TODO: Bitfield
                next();
                auto e = parse_constant_expression();
                std::cout << "Ignoring bitfield: " << *e << "\n";
            }

            decls.push_back(d);

            if (current().type() != token_type::comma) {
                break;
            }
            next();
        }

        if (decls.size() != 1 || decls[0].t()->base() != ctype::function_t || current().type() != token_type::lbrace) {
            EXPECT(semicolon);
            return;
        }
        parse_compound_statement();
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
                    if (t == token_type::enum_) {
                        parse_enum_list();
                    } else {
                        parse_struct_declaration_list();
                    }
                    EXPECT(rbrace);
                    TRACE("Not using " << t << " definition");
                    if (!tag_type) {
                        assert(id.empty());
                        id = "__unnamed" + std::to_string(unnamed_cnt_++);
                        make_tag_info();
                    } else {
                        // TODO: Check that the struct hasn't been defined before
                    }
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

        if (!long_ && !int_ && sign == -1) {
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
        while (current().type() == token_type::star) {
            next();
            ctype pt = ctype::pointer_t;
            while (is_type_qualifier(current().type())) {
                pt |=  ctype_from_type_qualifier_token(current().type());
                next();
            }
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
        if (current().type() == token_type::lbracket) {
            next();
            parse_assignment_expression(); // FIXME: Use result
            EXPECT(rbracket);
            auto array_type = std::make_shared<type>(*t);
            array_type->remove_flags(ctype::storage_f);
            t = std::make_shared<type>(ctype::array_t | storage_flags, std::make_unique<array_info>(array_type, array_info::unbounded));
        } else if (current().type() == token_type::lparen) {
            next();
            auto arg_types = parse_parameter_type_list();
            EXPECT(rparen);
            auto return_type = std::make_shared<type>(*t);
            return_type->remove_flags(ctype::storage_f);
            t = std::make_shared<type>(ctype::function_t | storage_flags, std::make_unique<function_info>(return_type, arg_types));
        }
        if (inner_type) {
            inner_type->modify_inner(t);
            t = inner_type;
        }
        return decl{t, id};
    }

    std::vector<decl> parse_parameter_type_list() {
        std::vector<decl> arg_types;
        for (size_t cnt = 0; current().type() != token_type::rparen; ++cnt) {
            if (cnt) {
                EXPECT(comma);
            }
            if (current().type() == token_type::ellipsis) {
                std::cout << "TODO: Handle ellipsis\n";
                arg_types.push_back(decl{std::make_shared<type>(), "..."});
                next();
            } else {
                const auto ds = parse_declaration_specifiers();
                const auto d = parse_declarator(ds);
                if (d.t()->base() == ctype::none) {
                    NOT_IMPLEMENTED(d);
                }
                arg_types.push_back(d);
            }
        }
        return arg_types;
    }

    void parse_struct_declaration_list() {
        while (current().type() != token_type::rbrace) {
            parse_declaration();
        }
    }

    void parse_enum_list() {
        while (current().type() != token_type::rbrace) {
            if (current().type() != token_type::id) {
                NOT_IMPLEMENTED("Expected identifier got " << current());
            }
            const auto id = current().text();
            next();
            if (current().type() == token_type::eq) {
                next();
                auto e = parse_constant_expression();
                std::cout << "Ignoring " << id << " = " << const_int_eval(*e) << " in enum definition\n";
            }
            if (current().type() != token_type::comma) {
                break;
            }
            next();
        }
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
            NOT_IMPLEMENTED(t);
        } else if (t == token_type::char_lit) {
            const auto v = current().char_val();
            next();
            return std::make_unique<const_int_expression>(const_int_val{v, ctype::int_t});
        } else if (t == token_type::string_lit) {
            NOT_IMPLEMENTED(t);
        } else if (t == token_type::lparen) {
            next();
            auto e = parse_expression();
            EXPECT(rparen);
            return e;
        }
        NOT_IMPLEMENTED("Expected primary expression got " << current());
    }

    expression_ptr parse_postfix_expression() {
        auto e = parse_primary_expression();
        const auto t = current().type();
        if (t == token_type::lbracket
            || t == token_type::lparen
            || t == token_type::dot
            || t == token_type::arrow
            || t == token_type::plusplus
            || t == token_type::minusminus) {
            NOT_IMPLEMENTED(t);
        }
        return e;
    }

    expression_ptr parse_unary_expression() {
        const auto t = current().type();
        if (t == token_type::plusplus
            || t == token_type::minusminus) {
            NOT_IMPLEMENTED(t);
        }
        if (t == token_type::and_
            || t == token_type::star
            || t == token_type::plus
            || t == token_type::minus
            || t == token_type::bnot
            || t == token_type::not_) {
            NOT_IMPLEMENTED(t);
            // parse_cast_expression
        }
        if (t == token_type::sizeof_) {
            NOT_IMPLEMENTED(t);
            // parse_unary_expression or if parenthesis: parse typename
        }
        return parse_postfix_expression();
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

            auto rhs = parse_unary_expression();
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
        return parse_expression1(parse_unary_expression(), precedence);
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
        const auto t = current().type();
        switch (t) {
            // selection-statement
        case token_type::if_: NOT_IMPLEMENTED(t);
        case token_type::switch_: NOT_IMPLEMENTED(t);
            // iteration-statement
        case token_type::while_: NOT_IMPLEMENTED(t);
        case token_type::do_: NOT_IMPLEMENTED(t);
        case token_type::for_: NOT_IMPLEMENTED(t);
            // jump-statement
        case token_type::goto_: NOT_IMPLEMENTED(t);
        case token_type::continue_: NOT_IMPLEMENTED(t);
        case token_type::break_: NOT_IMPLEMENTED(t);
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
        default:
            NOT_IMPLEMENTED(t);
        }
    }

    std::unique_ptr<compound_statement> parse_compound_statement() {
        std::vector<statement_ptr> ss;
        EXPECT(lbrace);
        while (current().type() != token_type::rbrace) {
            ss.push_back(parse_statement());
            std::cout << *ss.back() << "\n";
        }
        assert(current().type() == token_type::rbrace);
        next();
        return std::make_unique<compound_statement>(std::move(ss));
    }
};

parser::parser(source_manager& sm, const source_file& source) : impl_{new impl{sm, source}} {
}

parser::~parser() = default;

std::vector<source_position> parser::position() const {
    return impl_->position();
}

void parser::parse() {
    impl_->parse();
}

} // namespace mcc
