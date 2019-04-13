#include "preprocessor.h"
#include "util.h"

#include <optional>
#include <map>

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
    return strchr("{}[]#()<>%:;.?*+-/^&|~!=,", ch) != nullptr;
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

class pp_lexer {
public:
    explicit pp_lexer(const source_file& source) : source_{source} {
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
                        NOT_IMPLEMENTED("EOF in comment");
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
                if (index_ >= t.size()) NOT_IMPLEMENTED("backslash at en of file");
                if (t[index_] == '\n') {
                    ++index_;
                    continue;
                }
                if (t[index_] == '\r' && index_+1 < t.size() && t[index_+1] == '\n') {
                    index_ += 2;
                    continue;
                }
                NOT_IMPLEMENTED(quoted(t.substr(start_index, index_ - start_index)));
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
                                NOT_IMPLEMENTED("Invalid number");
                            }
                            has_dot = true;
                        } else if ((here|0x20) == 'e') {
                            if (has_exp) {
                                NOT_IMPLEMENTED("Invalid number");
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
                        NOT_IMPLEMENTED("EOF in char/string literal");
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
                NOT_IMPLEMENTED(quoted(t.substr(start_index, index_ - start_index)));
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
                NOT_IMPLEMENTED("EOF while scanning filename for #include directive");
            }

            const auto ch = static_cast<uint8_t>(t[index_]);
            if (ch == '\n') {
                NOT_IMPLEMENTED("newline while scanning filename for #include directive");
            } else if (state_char < 0) {
                if (is_whitespace(ch)) {
                    continue;
                } else if (ch == '"') {
                    state_char = '"';
                } else if (ch == '<') {
                    state_char = '>';
                } else {
                    NOT_IMPLEMENTED("Unexpected character while scanning for filename in #include directive: " << ch);
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

#define EXPECT_PUNCT(punct) do { if (current_.type() != pp_token_type::punctuation || current_.text() != punct) { NOT_IMPLEMENTED("Expected '" << punct << "' got " << current_); } internal_next(); } while (0)

class preprocessor::impl {
public:
    explicit impl(source_manager& sm, const source_file& source) : sm_{sm} {
        files_.push_back(std::make_unique<pp_lexer>(source));
        next();
    }

    std::vector<source_position> position() const {
        std::vector<source_position> p;
        for (auto it = files_.crbegin(); it != files_.crend(); ++it) {
            p.push_back((*it)->position());
        }
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
                        NOT_IMPLEMENTED("Open #if/#ifdef block!");
                    }
                    if (files_.size() != 1) {
                        NOT_IMPLEMENTED("Multiple files open?");
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
    struct macro_definition {
        std::optional<std::vector<std::string>> params;
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
            NOT_IMPLEMENTED("Expected identifier got " << current_);
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
            NOT_IMPLEMENTED("Expected identifier after # got " << current_);
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
                    NOT_IMPLEMENTED("#error " << combine_tokens(text));
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
                    NOT_IMPLEMENTED("#" << dir);
                }
                (void)read_to_newline();
            }
        }
        skip_whitespace();
        if (current_.type() != pp_token_type::newline) {
            NOT_IMPLEMENTED("Expected newline after #" << dir << " got " << current_);
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
                NOT_IMPLEMENTED("Unexpected " << current_ << " in macro definition");
            }
            internal_next();
            std::vector<std::string> ps;
            bool is_variadic = false;
            while (current_.type() != pp_token_type::punctuation || current_.text() != ")") {
                if (is_variadic) {
                    NOT_IMPLEMENTED("Ellipsis may only appear as last argument name");
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
                        NOT_IMPLEMENTED(variadic_arg_name << " must not be used as argument name");
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
            if (it->second.params == params && it->second.replacement == replacement) {
                return;
            }
            NOT_IMPLEMENTED("Invalid redefinition of macro " << id);
        }

        defines_[id] = macro_definition{params, replacement};
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
            NOT_IMPLEMENTED("#else outside #if/#ifdef");
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
        NOT_IMPLEMENTED("#elif with " << conds_.back().current);
    }

    void handle_else() {
        //std::cout << std::string(2*conds_.size()-2, ' ') << "#else\n";
        if (conds_.empty()) {
            NOT_IMPLEMENTED("#else outside #if/#ifdef");
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
        NOT_IMPLEMENTED("#else with " << conds_.back().current);
    }

    void handle_endif() {
        if (conds_.empty()) {
            NOT_IMPLEMENTED("#endif outside #if/#ifdef");
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
        void next() override { assert(index_ < v_.size()); ++index_; }
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
                }
            }
        }
        const pp_token& current() const override { return first_exhausted_ ? second_.current() : first_.current(); }
    private:
        token_stream& first_;
        token_stream& second_;
        bool first_exhausted_;
    };

    std::vector<pp_token> do_replace(const std::vector<pp_token>& replacement, token_stream& cont_stream) {
        vec_token_stream tsr{replacement};
        std::vector<pp_token> res;
        while (tsr.current()) {
            if (tsr.current().type() == pp_token_type::identifier && std::find(expanding_.begin(), expanding_.end(), tsr.current().text()) == expanding_.end()) {
                auto it = defines_.find(tsr.current().text());
                if (it != defines_.end()) {
                    tsr.next();
                    combined_token_stream cs{tsr, cont_stream};
                    auto r = handle_replace(cs, it->first, it->second);
                    res.insert(res.end(), r.begin(), r.end());
                    continue;
                }
            }
            res.push_back(tsr.current());
            tsr.next();
        }
        return res;
    }

    std::vector<pp_token> handle_replace(token_stream& ts, const std::string& macro_id, const macro_definition& def) {
        std::vector<pp_token> replacement;
        if (def.params) {
            // Function-like macro
            ts.skip_whitespace();
            if (ts.current().type() != pp_token_type::punctuation || ts.current().text() != "(") {
                // Not an invocation
                return { pp_token{ pp_token_type::identifier, macro_id } };
            }
            ts.next();

            const bool is_variadic = !def.params->empty() && def.params->back() == variadic_arg_name;
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
                            if (!def.params->empty()) {
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
                NOT_IMPLEMENTED("Expected ')' after function-like macro");
            }
            ts.next();

            if (is_variadic) {
                const auto psize = def.params->size();
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

            if (args.size() != def.params->size()) {
                NOT_IMPLEMENTED("Invalid number of arguments to macro got " << args.size() << " expecting " << def.params->size() << " for macro " << macro_id);
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
                            NOT_IMPLEMENTED("# and ## in same expression");
                        }
                        if (ri == 0) {
                            NOT_IMPLEMENTED("## may not appear at the start of the replacement list");
                        }
                        combine_state = combine_paste;
                        continue;
                    }
                } else if (t.type() == pp_token_type::identifier) {
                    auto it = std::find(def.params->begin(), def.params->end(), t.text());
                    if (it != def.params->end()) {
                        const auto ai = std::distance(def.params->begin(), it);
                        auto nts = args[ai];

                        if (combine_state == combine_str) {
                            nts = { pp_token{pp_token_type::string_literal, make_string_literal(nts)} };
                        } else if (combine_state == combine_paste) {
                            if (!replacement.empty() && !nts.empty()) {
                                auto last = replacement.back();
                                replacement.back() = pp_token{ last.type(), last.text() + nts.front().text() };
                                nts.erase(nts.begin());
                            }
                        } else {
                            // Expand macros in argument now that we know it's not being combined/stringified
                            // (note: before restricting expansion of "macro_id")
                            null_token_stream null_ts{};
                            nts = do_replace(nts, null_ts);
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
                    NOT_IMPLEMENTED("Unused #/## in macro replacement list for " << macro_id << " before " << t);
                }
                replacement.push_back(t);
            }
            if (combine_state != combine_none) {
                NOT_IMPLEMENTED("Unused #/## in macro replacement list for " << macro_id);
            }
        } else {
            // Object like-macro
            // Only paste needs to be handled
            for (size_t i = 0, s = def.replacement.size(); i < s; ++i) {
                if (i + 1 < s && def.replacement[i+1].type() == pp_token_type::punctuation && def.replacement[i+1].text() == "##") {
                    if (i + 2 >= s) {
                        NOT_IMPLEMENTED("## at end of macro replacement list");
                    }
                    replacement.push_back(pp_token{def.replacement[i].type(), def.replacement[i].text() + def.replacement[i+2].text()});
                    i += 2;
                } else {
                    replacement.push_back(def.replacement[i]);
                }
            }
        }
        struct push_expanding {
            explicit push_expanding(impl& pp, const std::string& val) : pp{pp} {
                pp.expanding_.push_back(val);
            }
            ~push_expanding() {
                pp.expanding_.pop_back();
            }
            impl& pp;
        } pe{*this, macro_id};
        return do_replace(replacement, ts);
    }

    pp_number eval_primary() {
        skip_whitespace();
        const auto t = current_;
        if (!t) NOT_IMPLEMENTED("Unexpected EOF");
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
            if (!end || *end || (n == ULLONG_MAX && errno == ERANGE)) {
                NOT_IMPLEMENTED("Invalid PP number '" << nt << "'");
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
        NOT_IMPLEMENTED(t);
    }

    pp_number eval_unary() {
        skip_whitespace();
        if (current_.type() == pp_token_type::punctuation) {
            const auto op = current_.text();
            if (op == "!") {
                internal_next();
                return pp_number{eval_primary().uval() ? 0LL : 1LL};
            } else if (op == "~" || op == "+" || op == "-") {
                NOT_IMPLEMENTED(op);
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
                if (look_ahead_precedence > precedence) {
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

std::vector<source_position> preprocessor::position() const {
    return impl_->position();
}

const pp_token& preprocessor::current() const {
    return impl_->current();
}

void preprocessor::next() {
    impl_->next();
}


void define_standard_headers(source_manager& sm) {
    sm.define_standard_headers("assert.h", "");
    sm.define_standard_headers("complex.h", "");
    sm.define_standard_headers("ctype.h", "");
    sm.define_standard_headers("errno.h", "");
    sm.define_standard_headers("fenv.h", "");
    sm.define_standard_headers("float.h", "");
    sm.define_standard_headers("inttypes.h", R"(
#include <stdint.h>
)");
    sm.define_standard_headers("iso646.h", "");
    sm.define_standard_headers("limits.h", "");
    sm.define_standard_headers("locale.h", "");
    sm.define_standard_headers("math.h", "");
    sm.define_standard_headers("setjmp.h", R"(
#ifndef _SETJMP_H
#define _SETJMP_H
struct _Jmp_buf;
typedef struct _Jmp_buf jmp_buf[1];
#endif
)");
    sm.define_standard_headers("signal.h", "");
    sm.define_standard_headers("stdalign.h", "");
    sm.define_standard_headers("stdarg.h", "");
    sm.define_standard_headers("stdatomic.h", "");
    sm.define_standard_headers("stdbool.h", "");
    sm.define_standard_headers("stddef.h", R"(
#ifndef _STDDEF_H
#define _STDDEF_H
typedef signed long long ptrdiff_t;
typedef unsigned long long size_t;
typedef unsigned short wchar_t;
#define NULL ((void*)0)
#define offsetof(type,member) ((size_t)&((type*)0)->member))
#endif
)");
    sm.define_standard_headers("stdint.h", R"(
#ifndef _STDINT_H
#define _STDINT_H
typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef long long int int64_t;
typedef unsigned char           uint8_t;
typedef unsigned short int      uint16_t;
typedef unsigned int            uint32_t;
typedef unsigned long long int  uint64_t;
#endif
)");
    sm.define_standard_headers("stdio.h", R"(
#ifndef _STDIO_H
#define _STDIO_H
#include <stddef.h>
struct _FILE;
typedef struct _FILE FILE;
#endif
)");
    sm.define_standard_headers("stdlib.h", "");
    sm.define_standard_headers("stdnoreturn.h", "");
    sm.define_standard_headers("string.h", "");
    sm.define_standard_headers("tgmath.h", "");
    sm.define_standard_headers("threads.h", "");
    sm.define_standard_headers("time.h", "");
    sm.define_standard_headers("uchar.h", "");
    sm.define_standard_headers("wchar.h", "");
    sm.define_standard_headers("wctype.h", "");
}

void define_posix_headers(source_manager& sm) {
    sm.define_standard_headers("dlfcn.h", "");
    sm.define_standard_headers("fcntl.h", "");
    sm.define_standard_headers("unistd.h", "");
    sm.define_standard_headers("sys/stat.h", "");
    sm.define_standard_headers("sys/time.h", "");
}

}

