#include "preprocessor.h"
#include "util.h"

#include <optional>
#include <variant>
#include <functional>
#include <map>
#include <cstring>

//#define PP_DEBUG

#ifdef PP_DEBUG
#include <iostream>
#endif

namespace mcc {

const pp_token pp_token::eof{};

std::ostream& operator<<(std::ostream& os, pp_token_type type) {
    switch (type) {
#define CASE_PP_TOK(t) case pp_token_type::t: return os << #t
    CASE_PP_TOK(whitespace);
    CASE_PP_TOK(newline);
    CASE_PP_TOK(header_name);
    CASE_PP_TOK(identifier);
    CASE_PP_TOK(number);
    CASE_PP_TOK(float_number);
    CASE_PP_TOK(character_constant);
    CASE_PP_TOK(string_literal);
    CASE_PP_TOK(punctuation);
    CASE_PP_TOK(eof);
#undef CASE_PP_TOK
    }
    return os << "pp_token_type{" << static_cast<int>(type) << "}";
}

std::ostream& operator<<(std::ostream& os, const pp_token& tok) {
    return os << "pp_token{" << tok.type() << ", " << quoted(tok.text()) << "}";
}

bool is_alpha(int ch) {
    return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
}

bool is_digit(int ch) {
    return ch >= '0' && ch <= '9';
}

bool is_xdigit(int ch) {
    return is_digit(ch) || (ch >= 'A' && ch <= 'F') || (ch >= 'a' && ch <= 'f');
}

bool is_whitespace_non_nl(int ch) {
    return ch == ' ' || ch == '\t' || ch == '\v' || ch == '\f';
}

bool is_whitespace(int ch) {
    return is_whitespace_non_nl(ch) || ch =='\n';
}

bool is_punct_char(int ch) {
    return std::strchr("{}[]#()<>%:;.?*+-/^&|~!=,", ch) != nullptr;
}

size_t punct_length(std::string_view s) {
    assert(!s.empty() && is_punct_char(s[0]));
    if (s.length() == 1) return 1;
    switch (s[0]) {
    case '.': return s.length() > 2 && s[1] == '.' && s[2] == '.' ? 3 : 1;
    case '#': return s[0] == s[1] ? 2 : 1;
    case '*': case '/': case '%': case '^': case '!':
        return s[1] == '=' ? 2 : 1;
    case '=': case '&': case '|': case '+':
        return s[0] == s[1] || s[1] == '=' ? 2 : 1;
    case '-':
        return s[1] == '-' || s[1] == '=' || s[1] == '>' ? 2 : 1;
    case '<': case '>':
        if (s[0] == s[1]) {
            return s.length() > 1 && s[2] == '=' ? 3 : 2;
        } else {
            return s[1] == '=' ? 2 : 1;
        }
    default: return 1;
    }
}

std::string combine_tokens(const std::vector<pp_token>& tokens) {
    std::string s;
    for (const auto& t: tokens) {
        if (t.type() == pp_token_type::whitespace) {
            continue;
        }
        if (!s.empty()) s += " ";
        s += t.text();
    }
    return s;
}

std::string make_string_literal(const std::vector<pp_token>& tokens) {
    return quoted(combine_tokens(tokens));
}

class pp_number {
public:
    explicit pp_number() : val_{0}, signed_{true} {}
    explicit pp_number(intmax_t val): val_{static_cast<uintmax_t>(val)}, signed_{true} {}
    explicit pp_number(uintmax_t val): val_{val}, signed_{false} {}

    bool is_signed() const { return signed_; }
    intmax_t val() const { return static_cast<intmax_t>(val_); }
    uintmax_t uval() const { return val_; }

private:
    uintmax_t val_;
    bool signed_;
};

std::ostream& operator<<(std::ostream& os, pp_number n) {
    if (n.is_signed()) {
        return os << n.val();
    } else {
        return os << n.uval();
    }
}

int operator_precedence(const std::string& op) {
    if (op == "*" || op == "/" || op == "%") {
        return 5;
    } else if (op == "+" || op == "-") {
        return 6;
    } else if (op == "<<" || op == ">>") {
        return 7;
    } else if (op == "<" || op == "<=" || op == ">=" || op == ">") {
        return 9;
    } else if (op == "==" || op == "!=") {
        return 10;
    } else if (op == "&") {
        return 11;
    } else if (op == "^") {
        return 12;
    } else if (op == "|") {
        return 13;
    } else if (op == "&&") {
        return 14;
    } else if (op == "||") {
        return 15;
    }
    return INT_MAX;
}

pp_number pp_compute(const std::string& op, pp_number l, pp_number r) {
    const bool is_signed = l.is_signed() && r.is_signed();
#define DO_OP(o) if (op == #o) { uintmax_t tmp = l.uval() o r.uval(); return is_signed?pp_number{static_cast<intmax_t>(tmp)}:pp_number{tmp}; }
    DO_OP(*);DO_OP(/);DO_OP(%);
    DO_OP(+);DO_OP(-);
    DO_OP(<<);DO_OP(>>);
    DO_OP(<);DO_OP(<=);DO_OP(>=);DO_OP(>);
    DO_OP(==);DO_OP(!=);
    DO_OP(&);
    DO_OP(^);
    DO_OP(|);
    DO_OP(&&);
    DO_OP(||);
#undef DO_OP

    NOT_IMPLEMENTED(l << op << r);
}

enum class conditional_state {
    active,  // Currently in active #if/#elif/#else block
    waiting, // Currently inactive, waiting for first active block
    done,    // Has been active, waiting for #endif
    endif,   // Passed #else, only #endif legal
};

std::ostream& operator<<(std::ostream& os, conditional_state s) {
    if (s == conditional_state::active)  return os << "active";
    if (s == conditional_state::waiting) return os << "waiting";
    if (s == conditional_state::done)    return os << "done";
    if (s == conditional_state::endif)   return os << "endif";
    NOT_IMPLEMENTED((int)s);
}

#define LEX_ERROR(args) NOT_IMPLEMENTED(position() << ": " << args)

class pp_lexer {
public:
    explicit pp_lexer(const source_file& source) : source_{source} {
#ifdef PP_DEBUG
        std::cout << "\n";
#endif
        next();
    }
    pp_lexer(const pp_lexer&) = delete;
    pp_lexer& operator=(const pp_lexer&) = delete;

    const source_file& source() const { return source_; }

    source_position position() const { return source_position{source_, last_index_, current_.text().size()}; }

    const pp_token& current() const { return current_; }

    void next() {
        const auto& t = source_.text();
        last_index_ = index_;

        for (;;) {
            const auto start_index = index_;
            if (start_index >= t.size()) {
                if (current_ && current_.type() != pp_token_type::newline) {
                    // If the file didn't end with a newline, insert one
                    current_ = pp_token{pp_token_type::newline, "\n"};
                    return;
                }
                // EOF
                current_ = pp_token{};
                return;
            }

            const auto ch = static_cast<uint8_t>(t[index_++]);
            const int next = index_ < t.size() ? static_cast<uint8_t>(t[index_]) : -1;
            pp_token_type type = pp_token_type::eof;
            if (ch == '/' && index_ < t.size() && (t[index_] == '/' || t[index_] == '*')) {
                type = pp_token_type::whitespace;
                const bool line_comment = t[index_] == '/';
                ++index_;
                for (bool has_star = false;; ++index_) {
                    if (index_ >= t.size()) {
                        LEX_ERROR("EOF in comment");
                    }
                    if (line_comment) {
                        if (t[index_] == '\n') break;
                    } else {
                        if (t[index_] == '*') {
                            has_star = true;
                        } else if (has_star && t[index_] == '/') {
                            ++index_;
                            break;
                        } else {
                            has_star = false;
                        }
                    }
                }
            } else if (ch == '\\') {
                // Handle line-continuation
                if (index_ >= t.size()) LEX_ERROR("backslash at en of file");
                if (t[index_] == '\n') {
                    ++index_;
                    continue;
                }
                if (t[index_] == '\r' && index_+1 < t.size() && t[index_+1] == '\n') {
                    index_ += 2;
                    continue;
                }
                LEX_ERROR(quoted(t.substr(start_index, index_ - start_index)));
            } else if (ch == '\n') {
                type = pp_token_type::newline;
            } else if (is_whitespace_non_nl(ch)) {
                type = pp_token_type::whitespace;
                consume_while(is_whitespace_non_nl);
            } else if (is_alpha(ch) || ch == '_') {
                type = pp_token_type::identifier;
                consume_while([](int ch) { return is_alpha(ch) || is_digit(ch) || ch == '_'; });
            } else if (is_digit(ch) || (ch == '.' && is_digit(next))) {
                bool integral = true;
                const bool hex = (next|0x20) == 'x';
                if (ch == '0' && (hex || is_digit(next))) {
                    if (hex) ++index_;
                    for (; index_ < t.size(); ++index_) {
                        const auto here = static_cast<uint8_t>(t[index_]);
                        if (!is_digit(here) && (!hex || !is_xdigit(here))) {
                            break;
                        }
                    }
                } else {
                    bool has_dot = ch == '.';
                    bool has_exp = false;
                    for (auto last = ch; index_ < t.size(); ++index_) {
                        const auto here = static_cast<uint8_t>(t[index_]);
                        if (is_digit(here)) {
                        } else if (here == '.') {
                            if (has_dot) {
                                LEX_ERROR("Invalid number");
                            }
                            has_dot = true;
                        } else if ((here|0x20) == 'e') {
                            if (has_exp) {
                                LEX_ERROR("Invalid number");
                            }
                            has_exp = true;
                        } else if (here == '+' || here == '-') {
                            if ((last|0x20) != 'e') {
                                break;
                            }
                        } else {
                            break;
                        }
                        last = here;
                    }
                    if (has_dot || has_exp) {
                        integral = false;
                    }
                }

                if (integral) {
                    for (; index_ < t.size(); ++index_) {
                        const auto here = static_cast<uint8_t>(t[index_] | 0x20);
                        if (here != 'u' && here != 'l') {
                            break;
                        }
                    }
                }
                type = integral ? pp_token_type::number : pp_token_type::float_number;
            } else if (ch == '\'' || ch == '\"') {
                type = ch == '\'' ? pp_token_type::character_constant : pp_token_type::string_literal;
                for (bool quote = false;; ++index_) {
                    if (index_ >= t.size()) {
                        LEX_ERROR("EOF in char/string literal");
                    } else if (quote) {
                        quote = false;
                    } else if (t[index_] == '\\') {
                        quote = true;
                    } else if (t[index_] == ch) {
                        ++index_;
                        break;
                    }
                }
            } else if (is_punct_char(ch)) {
                type = pp_token_type::punctuation;
                index_ += punct_length(std::string_view{&t[index_-1], t.size()-index_+1}) - 1;
            } else {
                LEX_ERROR(quoted(t.substr(start_index, index_ - start_index)));
            }
            current_ = pp_token{type, t.substr(start_index, index_ - start_index)};
            break;
        }
    }

    void fetch_filename() {
        const auto& t = source_.text();
        std::string res;
        int state_char = -1;
        for (;; ++index_) {
            if (index_ >= t.size()) {
                LEX_ERROR("EOF while scanning filename for #include directive");
            }

            const auto ch = static_cast<uint8_t>(t[index_]);
            if (ch == '\n') {
                LEX_ERROR("newline while scanning filename for #include directive");
            } else if (state_char < 0) {
                if (is_whitespace(ch)) {
                    continue;
                } else if (ch == '"') {
                    state_char = '"';
                } else if (ch == '<') {
                    state_char = '>';
                } else {
                    LEX_ERROR("Unexpected character while scanning for filename in #include directive: " << ch);
                }
                res += ch;
            } else {
                res += ch;
                if (ch == state_char) {
                    ++index_;
                    break;
                }
            }
        }
        current_ = pp_token{pp_token_type::header_name, res};
    }

private:
    const source_file& source_;
    pp_token current_;
    size_t index_ = 0;
    size_t last_index_ = 0;

    template<typename Pred>
    void consume_while(Pred pred) {
        const auto& t = source_.text();
        while (index_ < t.size() && pred(t[index_])) {
            ++index_;
        }
    }
};

#define EXPECT_PUNCT(punct) do { if (current_.type() != pp_token_type::punctuation || current_.text() != punct) { LEX_ERROR("Expected '" << punct << "' got " << current_); } internal_next(); } while (0)

class preprocessor::impl {
public:
    explicit impl(source_manager& sm, const source_file& source) : sm_{sm} {
        auto make_macro_func = [&](const char* name, auto&& f) {
            defines_[name] = macro_definition{macro_param_type{std::move(f)}, {}};
        };
        auto make_string_macro_func = [&](const char* name, auto&& f) {
            make_macro_func(name, [f]() -> std::vector<pp_token> {
                return { pp_token{ pp_token_type::string_literal, quoted(f()) } };
            });
        };
        auto make_number_macro_func = [&](const char* name, auto&& f) {
            make_macro_func(name, [f]() -> std::vector<pp_token> {
                return { pp_token{ pp_token_type::number, std::to_string(f()) } };
            });
        };
        make_string_macro_func("__FILE__", [&]() { return position().source().name(); });
        make_number_macro_func("__LINE__", [&]() { return position().extend().line; });
        make_string_macro_func("__DATE__", []() -> std::string { NOT_IMPLEMENTED("__DATE__"); });
        make_string_macro_func("__TIME__", []() -> std::string { NOT_IMPLEMENTED("__TIME__"); });
        //make_macro_func("__FILE__", [&]() -> std::vector<pp_token> { return pp_token{pp_token_type::string_literal, quoted(position().source().name())}; });
        //make_macro_func("__LINE__", [&]() -> std::vector<pp_token> { NOT_IMPLEMENTED("__LINE__"); });
        //make_macro_func("__DATE__", [&]() -> std::vector<pp_token> { NOT_IMPLEMENTED("__DATE__"); });
        //make_macro_func("__TIME__", [&]() -> std::vector<pp_token> { NOT_IMPLEMENTED("__TIME__"); });
        files_.push_back(std::make_unique<pp_lexer>(source));
        files_.push_back(std::make_unique<pp_lexer>(sm.builtin()));
        next();
    }

    ~impl() {
        assert(expanding_.empty());
        assert(std::uncaught_exceptions() || files_.size() == 1);
    }

    source_position position() const {
        assert(!files_.empty());
        return files_.back()->position();
    }

    std::vector<source_position> position_trace() const {
        std::vector<source_position> p;
        for (auto it = files_.crbegin(); it != files_.crend(); ++it) {
            p.push_back((*it)->position());
        }
        assert(!p.empty());
        return p;
    }

    const pp_token& current() const {
        return current_;
    }

    void next() {
        for (;;) {
            if (internal_next()) {
                if (!current_) {
                    if (!conds_.empty()) {
                        LEX_ERROR("Open #if/#ifdef block!");
                    }
                    if (files_.size() != 1) {
                        LEX_ERROR("Multiple files open?");
                    }
                    return;
                }
            } else {            
                if (current_.type() == pp_token_type::punctuation && current_.text() == "#") {
                    internal_next();
                    skip_whitespace();
                    handle_directive();
                    continue;
                }

                if (!is_cond_active()) {
                    continue;
                }
            
                if (current_.type() == pp_token_type::identifier) {
                    auto it = defines_.find(current_.text());
                    if (it != defines_.end()) {
                        internal_next();
                        replace_to_pending(*it);
                        continue;
                    }
                }
            }

            if (current_.type() != pp_token_type::whitespace && current_.type() != pp_token_type::newline) {
                assert(current_);
                return;
            }
        }
    }

private:
    using macro_param_type = std::variant<std::monostate, std::vector<std::string>, std::function<std::vector<pp_token>()>>;
    struct macro_definition {
        macro_param_type params;
        std::vector<pp_token> replacement;
    };
    struct conditional {
        conditional_state current;
        conditional_state next;
    };
    static constexpr const char* const variadic_arg_name = "__VA_ARGS__";

    source_manager& sm_;
    std::map<std::string, macro_definition> defines_;
    std::vector<pp_token> pending_tokens_;
    std::vector<std::string> expanding_;
    std::vector<conditional> conds_;
    std::vector<std::unique_ptr<pp_lexer>> files_;
    pp_token current_;

    using define_type = decltype(*defines_.begin());

    bool is_cond_active() const {
        return conds_.empty() || conds_.back().current == conditional_state::active;
    }

    bool internal_next() {
        assert(!files_.empty());
        if (!pending_tokens_.empty()) {
            current_ = pending_tokens_.front();
            pending_tokens_.erase(pending_tokens_.begin());
            assert(current_);
            return true;
        }
        for (;;) {
            current_ = files_.back()->current();
            if (!current_) {
                if (files_.size() == 1) {
                    return true;
                }
                //std::cout << "Exiting " << files_.back()->source().name() << "\n";
                files_.pop_back();
                continue;
            }
            //std::cout << files_.back()->position() << ": " << current_ << "\n";
            files_.back()->next();
            return false;
        }
    }

    void replace_to_pending(define_type def) {
        assert(pending_tokens_.empty());
        pp_token_stream ts{*this};
        pending_tokens_ = handle_replace(ts, def.first, def.second);
        if (current_) {
            pending_tokens_.push_back(current_);
        }
    }

    void skip_whitespace() {
        while (current_.type() == pp_token_type::whitespace) {
            internal_next();
        }
    }

    std::string get_identifer() {
        if (current_.type() != pp_token_type::identifier) {
            LEX_ERROR("Expected identifier got " << current_);
        }
        const auto text = current_.text();
        internal_next();
        return text;
    }

    std::vector<pp_token> read_to_newline() {
        std::vector<pp_token> res;
        for (; current_ && current_.type() != pp_token_type::newline; internal_next()) {
            if (current_.type() != pp_token_type::whitespace) {
                res.push_back(current_);
            }
        }
        return res;
    }

    void handle_directive() {
        if (current_.type() != pp_token_type::identifier) {
            // TODO: Support null directive
            LEX_ERROR("Expected identifier after # got " << current_);
        }
        const std::string dir = current_.text();
        std::string include_next;
        if (dir == "include") {
            if (!is_cond_active()) {
                (void)read_to_newline();
            } else {
                include_next = handle_include();
            }
        } else {
            internal_next();
            skip_whitespace();
            if (dir == "define") {
                if (!is_cond_active()) {
                    (void)read_to_newline();
                    return;
                }
                handle_define();
            } else if (dir == "undef") {
                if (!is_cond_active()) {
                    (void)read_to_newline();
                    return;
                }
                handle_undef();
            } else if (dir == "pragma") {
                (void)read_to_newline();
            } else if (dir == "error") {
                auto text = read_to_newline();
                if (is_cond_active()) {
                    LEX_ERROR("#error " << combine_tokens(text));
                }
            } else if (dir == "if") {
                handle_if();
            } else if (dir == "ifdef") {
                handle_ifdef();
            } else if (dir == "ifndef") {
                handle_ifndef();
            } else if (dir == "elif") {
                handle_elif();
            } else if (dir == "else") {
                handle_else();
            } else if (dir == "endif") {
                handle_endif();
            } else {
                if (is_cond_active()) {
                    LEX_ERROR("#" << dir);
                }
                (void)read_to_newline();
            }
        }
        skip_whitespace();
        if (current_.type() != pp_token_type::newline) {
            LEX_ERROR("Expected newline after #" << dir << " got " << current_);
        }
        if (!include_next.empty()) {
            assert(pending_tokens_.empty());
            assert(is_cond_active());
            files_.push_back(std::make_unique<pp_lexer>(sm_.include(files_.back()->source().name(), include_next)));
            //std::cout << "Entering " << files_.back()->source().name() << "\n";
        }
    }

    std::string handle_include() {
        assert(!files_.empty());
        files_.back()->fetch_filename();
        internal_next();
        assert(current_.type() == pp_token_type::header_name);
        assert(current_.text().size() >= 2);
        const auto filename = current_;
        internal_next();
        return filename.text();
    }

    void handle_define() {
        const auto id = get_identifer();
        std::optional<std::vector<std::string>> params;
        if (current_.type() == pp_token_type::punctuation) {
            if (current_.text() != "(") {
                LEX_ERROR("Unexpected " << current_ << " in macro definition");
            }
            internal_next();
            std::vector<std::string> ps;
            bool is_variadic = false;
            while (current_.type() != pp_token_type::punctuation || current_.text() != ")") {
                if (is_variadic) {
                    LEX_ERROR("Ellipsis may only appear as last argument name");
                }
                skip_whitespace();
                if (!ps.empty()) {
                    EXPECT_PUNCT(",");
                    skip_whitespace();
                }
                if (current_.type() == pp_token_type::punctuation && current_.text() == "...") {
                    ps.push_back(variadic_arg_name);
                    is_variadic = true;
                    internal_next();
                    skip_whitespace();
                } else {
                    ps.push_back(get_identifer());
                    if (ps.back() == variadic_arg_name) {
                        LEX_ERROR(variadic_arg_name << " must not be used as argument name");
                    }
                    // TODO: Check for duplicate argument names...
                }
            }
            internal_next(); // Skip ')'
            params = std::move(ps);
        }

        std::vector<pp_token> replacement = read_to_newline();

        auto it = defines_.find(id);
        if (it != defines_.end()) {
            if (it->second.replacement == replacement) {
                if (params) {
                    if (it->second.params.index() == 1 && *params == std::get<1>(it->second.params)) {
                        return;
                    }
                } else if (it->second.params.index() == 0) {
                    return;
                }
            }
            LEX_ERROR("Invalid redefinition of macro " << id);
        }

        if (id == "__attribute__") {
            // HACK: Don't allow _mingw.h to define __attribute__ ...
            auto fn = position().source().name();
            if (auto idx = fn.find_last_of('/'); idx != std::string::npos) fn = fn.substr(idx+1);
            if (fn == "_mingw.h") {
                return;
            }
            LEX_ERROR(fn);
        }

        defines_[id] = macro_definition{params ? macro_param_type{*params} : macro_param_type{}, replacement};
    }

    void handle_undef() {
        const auto id = get_identifer();
        auto it = defines_.find(id);
        if (it != defines_.end()) {
            defines_.erase(it);
        }      
    }
    
    void new_cond() {
        if (is_cond_active()) {
            conds_.push_back(conditional{conditional_state::waiting,conditional_state::waiting});
        } else {
            conds_.push_back(conditional{conditional_state::done,conditional_state::done});
        }
    }

    void activate_conditional(conditional_state next = conditional_state::done) {
        assert(!conds_.empty() && conds_.back().current == conditional_state::waiting);
        conds_.back().current = conditional_state::active;
        conds_.back().next = next;
        //std::cout << std::string(2*conds_.size(), ' ') << " Cond is active\n";
    }

    void handle_if_defined(bool non_inverted) {
        skip_whitespace();
        const bool activate = (defines_.find(get_identifer()) != defines_.end()) == non_inverted && is_cond_active();
        new_cond();
        if (activate) {
            activate_conditional();
        }
    }

    void handle_if() {
        //std::cout << std::string(2*conds_.size(), ' ') << "#if\n";
        const bool activate = eval_cond() && is_cond_active();
        new_cond();
        if (activate) {
            activate_conditional();
        }
    }

    void handle_ifdef() {
        //std::cout << std::string(2*conds_.size(), ' ') << "#ifdef " << current_.text() << "\n";
        handle_if_defined(true);
    }

    void handle_ifndef() {
        //std::cout << std::string(2*conds_.size(), ' ') << "#ifndef " << current_.text() << "\n";
        handle_if_defined(false);
    }

    void handle_elif() {
        //std::cout << std::string(2*conds_.size()-2, ' ') << "#elif\n";
        if (conds_.empty()) {
            LEX_ERROR("#else outside #if/#ifdef");
        }
        const auto res = eval_cond();
        conds_.back().current = conds_.back().next;
        switch (conds_.back().current) {
        case conditional_state::active:
            // Not legal
            break;
        case conditional_state::waiting:
            if (res) { 
                activate_conditional();
            }
            return;
        case conditional_state::done:
            // Skip this block as well
            return;
        case conditional_state::endif:
            // Not legal, we've already passed an #else
            break;
        }
        LEX_ERROR("#elif with " << conds_.back().current);
    }

    void handle_else() {
        //std::cout << std::string(2*conds_.size()-2, ' ') << "#else\n";
        if (conds_.empty()) {
            LEX_ERROR("#else outside #if/#ifdef");
        }
        conds_.back().current = conds_.back().next;
        switch (conds_.back().current) {
        case conditional_state::active:
            // Not legal
            break;
        case conditional_state::waiting:
            // Activate block
            activate_conditional(conditional_state::endif);
            conds_.back().current = conditional_state::active;
            return;
        case conditional_state::done:
            // Skip this block as well
            conds_.back().next = conditional_state::endif;
            return;
        case conditional_state::endif:
            // Not legal, we've already passed an #else
            break;
        }
        LEX_ERROR("#else with " << conds_.back().current);
    }

    void handle_endif() {
        if (conds_.empty()) {
            LEX_ERROR("#endif outside #if/#ifdef");
        }
        conds_.pop_back();
        //std::cout << std::string(2*conds_.size(), ' ') << "#endif\n";
    }

    struct token_stream {
        virtual void next() = 0;
        virtual const pp_token& current() const = 0;

        void skip_whitespace() {
            while (current() && (current().type() == pp_token_type::whitespace || current().type() == pp_token_type::newline)) {
                next();
            }
        }
    };

    struct pp_token_stream : token_stream {        
        explicit pp_token_stream(impl& pp) : pp_{pp} {
        }
        void next() override { pp_.internal_next(); }
        const pp_token& current() const override { return pp_.current_; }
    private:
        impl& pp_;
    };

    struct vec_token_stream : token_stream {
        explicit vec_token_stream(const std::vector<pp_token>& v) : v_{v} {
        }
        ~vec_token_stream() {
        }
        void next() override {
            assert(index_ < v_.size());
            ++index_;
        }
        const pp_token& current() const override { return index_ < v_.size() ? v_[index_] : pp_token::eof; }
    private:
        const std::vector<pp_token>& v_;
        size_t index_ = 0;
    };

    struct null_token_stream : token_stream {
        explicit null_token_stream() {};
        void next() override { NOT_IMPLEMENTED("null_token_stream::next()"); }
        const pp_token& current() const override { return pp_token::eof; }
    };

    struct combined_token_stream : token_stream {        
        explicit combined_token_stream(token_stream& first, token_stream& second) : first_{first}, second_{second}, first_exhausted_{!first_.current()} {
        }
        void next() override {
            if (first_exhausted_) {
                second_.next();
            } else {
                first_.next();
                if (!first_.current()) {
                    first_exhausted_ = true;
#ifdef PP_DEBUG
                    std::cout << "Switching to other stream\n";
#endif
                }
            }
        }
        const pp_token& current() const override { return first_exhausted_ ? second_.current() : first_.current(); }

    private:
        token_stream& first_;
        token_stream& second_;
        bool first_exhausted_;
    };

    std::vector<pp_token> do_replace(const std::vector<pp_token>& replacement, const std::string& macro_id, bool pop_when_empty, token_stream& cont_stream) {
#ifdef PP_DEBUG
        std::cout << "Replacing " << macro_id << " in {";
        for (const auto& t: replacement) std::cout << " " << t;
        std::cout << "}\n";
#endif
        bool popped = true;
        if (!macro_id.empty()) {
            expanding_.push_back(macro_id);
            popped = false;
        }
        auto pop_macro = [&]() {
            if (macro_id.empty()) {
                return;
            }
            if (popped) {
                return;
            }
            if (expanding_.empty() || expanding_.back() != macro_id) {
                LEX_ERROR("Error while expanding " << macro_id);
            }
            expanding_.pop_back();
            popped = true;
#ifdef PP_DEBUG
            std::cout << "No longer forbinding expansion of " << macro_id << "\n";
#endif
        };

        vec_token_stream tsr{replacement};
        std::vector<pp_token> res;
        int handling_defined = 0; // With at least GCC a macro can expand to defined(...)
        while (tsr.current()) {
            if (handling_defined) {
                if (tsr.current().type() == pp_token_type::identifier) {
                    handling_defined = 0;
#ifdef PP_DEBUG
                    std::cout << "No longer handling 'defined'\n";
#endif
                    goto push;
                }
            }
            if (tsr.current().type() == pp_token_type::identifier && std::find(expanding_.begin(), expanding_.end(), tsr.current().text()) == expanding_.end()) {
                if (tsr.current().text() == "defined") {
#ifdef PP_DEBUG
                    std::cout << "Expanded 'defined'\n";
#endif
                    handling_defined = 1;
                    goto push;
                }
                auto it = defines_.find(tsr.current().text());
                if (it != defines_.end()) {
                    tsr.next();
                    // Allow the current macro to be expanded again if we've finished all expansions
                    if (!tsr.current() && pop_when_empty) {
                        pop_macro();
                    }
                    combined_token_stream cs{tsr, cont_stream};
                    auto r = handle_replace(cs, it->first, it->second);
                    res.insert(res.end(), r.begin(), r.end());
                    continue;
                }
            }
        push:
            res.push_back(tsr.current());
            tsr.next();
        }
#ifdef PP_DEBUG
        std::cout << "Replaced  " << macro_id << " with {";
        for (const auto& t: res) std::cout << " " << t;
        std::cout << "}\n";
#endif
        pop_macro();
        return res;
    }

    std::vector<pp_token> handle_replace(token_stream& ts, const std::string& macro_id, const macro_definition& def) {
#ifdef PP_DEBUG
        std::cout << "Expanding " << macro_id << "\n";
#endif
        if (def.params.index() == 2) {
            return std::get<2>(def.params)();
        }

        std::vector<pp_token> replacement;

        if (def.params.index() == 1) {
            // Function-like macro
            const auto& params = std::get<1>(def.params);

            ts.skip_whitespace();
            if (ts.current().type() != pp_token_type::punctuation || ts.current().text() != "(") {
                // Not an invocation
                return { pp_token{ pp_token_type::identifier, macro_id } };
            }
            ts.next();

            const bool is_variadic = !params.empty() && params.back() == variadic_arg_name;
            std::vector<std::vector<pp_token>> args;
            std::vector<pp_token> current_arg;
            for (int nest = 1; ts.current(); ts.next()) {
                ts.skip_whitespace();
                const auto& t = ts.current();
                if (t.type() == pp_token_type::punctuation) {
                    if (t.text() == "(") {
                        ++nest;
                    } else if (t.text() == ")") {
                        if (--nest == 0) {
                            if (!params.empty()) {
                                args.push_back(std::move(current_arg));
                            }
                            break;
                        }
                    } else if (nest == 1 && t.text() == ",") {
                        args.push_back(std::move(current_arg));
                        current_arg.clear();
                        continue;
                    }
                }
                current_arg.push_back(t);
            }
            if (ts.current().type() != pp_token_type::punctuation || ts.current().text() != ")") {
                LEX_ERROR("Expected ')' after function-like macro");
            }
            ts.next();

            if (is_variadic) {
                const auto psize = params.size();
                if (args.size() == psize - 1) {
                    args.push_back({});
                } else if (args.size() > psize) {
                    for (size_t i = psize; i < args.size(); ++i) {
                        args[psize-1].push_back(pp_token{pp_token_type::punctuation, ","});
                        args[psize-1].insert(args[psize-1].end(), args[i].begin(), args[i].end());
                    }
                    args.erase(args.begin() + psize, args.end());
                }
            }

            if (args.size() != params.size()) {
                LEX_ERROR("Invalid number of arguments to macro got " << args.size() << " expecting " << params.size() << " for macro " << macro_id);
            }

            enum { combine_none, combine_str, combine_paste } combine_state = combine_none;
            for (size_t ri = 0, rs = def.replacement.size(); ri < rs; ++ri) {
                const auto& t = def.replacement[ri];

                if (t.type() == pp_token_type::punctuation) {
                    if (t.text() == "#") {
                        combine_state = combine_str;
                        continue;
                    } else if (t.text() == "##") {
                        if (combine_state != combine_none) {
                            LEX_ERROR("# and ## in same expression");
                        }
                        if (ri == 0) {
                            LEX_ERROR("## may not appear at the start of the replacement list");
                        }
                        combine_state = combine_paste;
                        continue;
                    }
                } else if (t.type() == pp_token_type::identifier) {
                    auto it = std::find(params.begin(), params.end(), t.text());
                    if (it != params.end()) {
                        const auto ai = std::distance(params.begin(), it);
                        auto nts = args[ai];

                        if (combine_state == combine_str) {
                            nts = { pp_token{pp_token_type::string_literal, make_string_literal(nts)} };
                        } else if (combine_state == combine_paste) {
                            if (!replacement.empty() && !nts.empty()) {
                                auto last = replacement.back();
                                std::string text = last.text();
                                for (const auto& nt: nts) {
                                    text += nt.text();
                                }
                                nts.clear();
                                replacement.back() = pp_token{ last.type(), text };
                            }
                        } else {
                            // Expand macros in argument now that we know it's not being combined/stringified
                            // (note: before restricting expansion of "macro_id")
                            null_token_stream null_ts{};
                            nts = do_replace(nts, "", true, null_ts);
                        }
                        combine_state = combine_none;
                        replacement.insert(replacement.end(), nts.begin(), nts.end());
                        continue;
                    }
                }
                if (combine_state == combine_paste) {
                    combine_state = combine_none;
                    if (!replacement.empty()) {
                        auto last = replacement.back();
                        replacement.back() = pp_token{ last.type(), last.text() + t.text() };
                        continue;
                    }
                }
                if (combine_state != combine_none) {
                    LEX_ERROR("Unused #/## in macro replacement list for " << macro_id << " before " << t);
                }
                replacement.push_back(t);
            }
            if (combine_state != combine_none) {
                LEX_ERROR("Unused #/## in macro replacement list for " << macro_id);
            }
        } else {
            // Object like-macro
            // Only paste needs to be handled
            for (size_t i = 0, s = def.replacement.size(); i < s; ++i) {
                if (i + 1 < s && def.replacement[i+1].type() == pp_token_type::punctuation && def.replacement[i+1].text() == "##") {
                    if (i + 2 >= s) {
                        LEX_ERROR("## at end of macro replacement list");
                    }
                    replacement.push_back(pp_token{def.replacement[i].type(), def.replacement[i].text() + def.replacement[i+2].text()});
                    i += 2;
                } else {
                    replacement.push_back(def.replacement[i]);
                }
            }
        }
        return do_replace(replacement, macro_id, def.params.index() == 1, ts);
    }

    pp_number eval_primary() {
        skip_whitespace();
        const auto t = current_;
        if (!t) LEX_ERROR("Unexpected EOF");
        internal_next();
        if (t.type() == pp_token_type::punctuation) {
            if (t.text() == "(") {
                auto res = eval_expr();
                EXPECT_PUNCT(")");                
                return res;
            }
        } else if (t.type() == pp_token_type::number) {
            const auto nt = t.text().c_str();
            char* end = nullptr; 
            errno = 0;
            const auto n = strtoull(nt, &end, 0);
            while (end && (*end == 'l' || *end == 'L' || *end == 'u' || *end == 'U')) {
                ++end;
            }
            if (!end || *end || (n == ULLONG_MAX && errno == ERANGE)) {
                LEX_ERROR("Invalid PP number '" << nt << "'");
            }
            return n > LLONG_MAX ? pp_number{n} : pp_number{static_cast<intmax_t>(n)};
        } else if (t.type() == pp_token_type::identifier) {
            if (t.text() == "defined") {
                skip_whitespace();
                std::string id;
                if (current_.type() == pp_token_type::identifier) {
                    id = current_.text();
                    internal_next();
                } else {
                    EXPECT_PUNCT("(");
                    skip_whitespace();
                    id = get_identifer();
                    skip_whitespace();
                    EXPECT_PUNCT(")");
                }
                return pp_number{defines_.find(id) != defines_.end() ? 1ULL : 0ULL};
            }
            auto it = defines_.find(t.text());
            if (it != defines_.end()) {
                replace_to_pending(*it);
                internal_next();
                return eval_primary();
            }
            return pp_number{0LL};
        }
        LEX_ERROR(t);
    }

    pp_number eval_unary() {
        skip_whitespace();
        if (current_.type() == pp_token_type::punctuation) {
            const auto op = current_.text();
            if (op == "!") {
                internal_next();
                return pp_number{eval_primary().uval() ? 0LL : 1LL};
            } else if (op == "~" || op == "+" || op == "-") {
                LEX_ERROR(op);
            }
        }
        return eval_primary();
    }

    pp_number eval_expr1(pp_number lhs, int outer_precedence) {
        for (;;) {
            skip_whitespace();
            if (current_.type() != pp_token_type::punctuation) {
                break;
            }
            const auto op = current_.text();
            if (op == "?") {
                internal_next();
                skip_whitespace();
                const auto l = eval_expr();
                skip_whitespace();
                EXPECT_PUNCT(":");
                skip_whitespace();
                const auto r = eval_expr();
                return lhs.uval() ? l : r; // TODO: Only actually evaluate (or give error) if branch is taken
            }
            const int precedence = operator_precedence(op);
            if (precedence > outer_precedence) {
                break;
            }
            internal_next();
            auto rhs = eval_unary();
            for (;;) {
                skip_whitespace();
                if (current_.type() != pp_token_type::punctuation) {
                    break;
                }
                const auto look_ahead_precedence = operator_precedence(current_.text());
                if (look_ahead_precedence > precedence || (look_ahead_precedence == precedence && current_.text() != "?")) {
                    break;
                }
                rhs = eval_expr1(rhs, look_ahead_precedence);
            }
            lhs = pp_compute(op, lhs, rhs);
        }
        return lhs;
    }

    pp_number eval_expr() {
        return eval_expr1(eval_unary(), 100);
    }

    bool eval_cond() {
        return eval_expr().uval() != 0;
    }
};

#undef EXPECT_PUNCT

preprocessor::preprocessor(source_manager& sm, const source_file& source) : impl_{new impl{sm, source}} {
}

preprocessor::~preprocessor() = default;

source_position preprocessor::position() const {
    return impl_->position();
}

std::vector<source_position> preprocessor::position_trace() const {
    return impl_->position_trace();
}

const pp_token& preprocessor::current() const {
    return impl_->current();
}

void preprocessor::next() {
    impl_->next();
}

const char* standard_builtin_text() {
    return R"(
#define __STDC__         1
#define __STDC_VERSION__ 199901L
#define __STDC_HOSTED__  1

#define __amd64__        1
#define __x86_64__       1
#define __x86_64         1

#define _WIN32           1
#define _WIN64           1

#define _VA_LIST_DEFINED 1
typedef char* __builtin_va_list;
#define va_list __builtin_va_list
#define __builtin_va_start(ap, param) ((ap) = (char*)&(param) + 8)
#define __builtin_va_arg(ap, t) *(t*)(((ap) += 8) - 8)
#define __builtin_va_copy(dst, src) ((dst) = (src))
#define __builtin_va_end(ap)

#define va_start(v,l) __builtin_va_start(v,l)
#define va_arg(v,l) __builtin_va_arg(v,l)
#define va_copy(d,s) __builtin_va_copy(d,s)
#define va_end(v) __builtin_va_end(v)

#define __builtin_offsetof(type,member) ((size_t)&((type*)0)->member)

typedef unsigned short wchar_t;
typedef int __int32;
typedef long long __int64;

#define __cdecl
#define __declspec(x)
#define __inline inline
#define __inline__ inline
#define __forceinline inline
#define __unaligned

#define __CRT__NO_INLINE // don't want inline asm
#define _CONST_RETURN
#define _CRT_ALIGN(x) __attribute__ ((__aligned__ (x)))

#define __has_builtin(x) 0

extern unsigned __int64 __readgsqword(unsigned long);

)";
}

}
