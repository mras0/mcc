#include "lexer.h"
#include "util.h"
#include <cmath>

namespace mcc {

void lexer::next() {
    const auto tok = pp_.current();
    if (!tok) {
        current_ = token{token_type::eof};
        return;
    }
    pp_.next();

    if (tok.type() == pp_token_type::identifier) {
        const auto kt = keyword_token_from_text(tok.text());
        if (kt == token_type::eof) {
            current_ = token{token_type::id, tok.text()};
        } else {
            current_ = token{kt};
        }
    } else if(tok.type() == pp_token_type::punctuation) {
        const auto pt = op_token_from(tok.text());
        if (pt == token_type::eof) {
            NOT_IMPLEMENTED(tok);
        }
        current_ = token{pt};
    } else if (tok.type() == pp_token_type::number) {
        char* end = nullptr;
        errno = 0;
        const auto n = strtoull(tok.text().c_str(), &end, 0);
        // FIXME: Use correct type
        ctype t = ctype::int_t;
        for (; end && *end; ++end) {
            const auto ch = static_cast<uint8_t>(*end | 0x20);
            if (ch == 'u') {
                t |= ctype::unsigned_f;
            } else if (ch == 'l') {
                if (base_type(t) == ctype::long_t) {
                    t = modified_base_type(t, ctype::long_long_t);
                }
            }
            else break;
        }
        if (!end || *end || (n == ULLONG_MAX && errno == ERANGE)) {
            NOT_IMPLEMENTED("Invalid number " << tok.text());
        }
        current_ = token{const_int_val{n, t}};
    } else if (tok.type() == pp_token_type::float_number) {
        char* end = nullptr;
        errno = 0;
        double v = std::strtod(tok.text().c_str(), &end);
        if (!end || *end) {
            NOT_IMPLEMENTED("Invalid float " << tok.text());
        }
        if (errno == ERANGE) {
            if (v == HUGE_VAL) {
                v = INFINITY;
            }
            errno = 0;
        }
        current_ = token{v};
    } else if (tok.type() == pp_token_type::character_constant) {
        assert(tok.text().size() >= 3 && tok.text().front() == '\'' && tok.text().back() == '\'');
        const auto [ch, len] = unescape_char(std::string_view{tok.text().c_str()+1,tok.text().size()-2});
        if (len + 2 != tok.text().size() || ch > 255) {
            NOT_IMPLEMENTED(tok << " ch = " << ch);
        }
        current_ = token{ch};
    } else if (tok.type() == pp_token_type::string_literal) {
        auto t = tok.text();
        std::string lit;
        for (;;) {
            assert(t.size() >= 2 && t.front() == '\"' && t.back() == '\"');
            lit += unescape(std::string_view{t.c_str() + 1, t.size()-2});
            if (pp_.current().type() != pp_token_type::string_literal) {
                break;
            }
            t = pp_.current().text();
            pp_.next();
        }
        current_ = token{token_type::string_lit, lit};
    } else {
        NOT_IMPLEMENTED(tok);
    }
}


} // namespace mcc
