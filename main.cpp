#include <cassert>
#include <iostream>
#include <string>
#include <string_view>
#include <sstream>
#include <stdexcept>
#include <vector>
#include <optional>
#include <map>
#include <algorithm>

#define NOT_IMPLEMENTED(stuff) do { std::ostringstream oss_; oss_ << __FILE__ << ":" << __LINE__ << ": " << __func__ << " Not implemented: " << stuff; throw std::runtime_error(oss_.str()); } while (0)

// HACK
namespace std {
template<typename T>
ostream& operator<<(ostream& os, const vector<T>& v) {
    os << "{";
    for (size_t i = 0, s = v.size(); i < s; ++i) {
        os << (i ? ", ": " ") << v[i];
    }
    return os << " }";
}
}

namespace mcc {

std::string quoted(std::string_view s) {
    std::string res;
    res += '\"';
    for (const auto ch: s) {
        const auto uch = static_cast<uint8_t>(ch);
        if (uch == '\"' || uch < 32 || uch > 127) {
            switch (ch) {
            case '\"': res += "\\\""; break;
            case '\a': res += "\\a"; break;
            case '\b': res += "\\b"; break;
            case '\f': res += "\\f"; break;
            case '\n': res += "\\n"; break;
            case '\r': res += "\\r"; break;
            case '\t': res += "\\t"; break;
            case '\v': res += "\\v"; break;
            default:
                const char* const hexchars = "0123456789abcdef";
                res += "\\x";
                res += hexchars[uch>>4];
                res += hexchars[uch&15];
            }
        } else {
            res += ch;
        }
    }
    res += '\"';
    return res;
}

class source_file {
public:
    explicit source_file(const std::string& name, const std::string& text) : name_{name}, text_{text} {
    }

    const std::string& name() const { return name_; }
    const std::string& text() const { return text_; }

private:
    std::string name_;
    std::string text_;
};

class source_position {
public:
    explicit source_position(const source_file& source, size_t index, size_t len) : source_{&source}, index_{index}, len_{len} {
        assert(len < source_->text().size() && index + len < source_->text().size());
    }

    const source_file& source() const { return *source_; }
    size_t index() const { return index_; }
    size_t length() const { return len_; }

private:
    const source_file* source_;
    size_t index_;
    size_t len_;
};

std::ostream& operator<<(std::ostream& os, const source_position& pos) {
    return os << pos.source().name() << ":" << pos.index() << "-" << pos.index()+pos.length() << "\n";
}

#define OPERATORS(X) \
X(=)\


enum class pp_token_type {
    whitespace,
    newline,
    header_name,
    identifier,
    number,
    character_constant,
    string_literal,
    punctuation,
    eof
};

std::ostream& operator<<(std::ostream& os, pp_token_type type) {
    switch (type) {
#define CASE_PP_TOK(t) case pp_token_type::t: return os << #t
    CASE_PP_TOK(whitespace);
    CASE_PP_TOK(newline);
    CASE_PP_TOK(header_name);
    CASE_PP_TOK(identifier);
    CASE_PP_TOK(number);
    CASE_PP_TOK(character_constant);
    CASE_PP_TOK(string_literal);
    CASE_PP_TOK(punctuation);
    CASE_PP_TOK(eof);
#undef CASE_PP_TOK
    }
    return os << "pp_token_type{" << static_cast<int>(type) << "}";
}

class pp_token {
public:
    explicit pp_token() : type_{pp_token_type::eof}, text_{} {
    }
    explicit pp_token(pp_token_type type, const std::string& text) : type_{type}, text_{text} {
    }

    explicit operator bool() const { return type_ != pp_token_type::eof; }

    pp_token_type type() const { return type_; }
    const std::string text() const { return text_; }
private:
    pp_token_type type_;
    std::string   text_;
};
const pp_token pp_eof_token{};

bool operator==(const pp_token& l, const pp_token& r) {
    return l.type() == r.type() && l.text() == r.text();
}

bool operator!=(const pp_token& l, const pp_token& r) {
    return !(l == r);
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
            return s[1] == '=' ? 1 : 1;
        }
    default: return 1;
    }
}

std::string make_string_literal(const std::vector<pp_token>& tokens) {
    std::string s;
    for (const auto& t: tokens) {
        if (t.type() == pp_token_type::whitespace) {
            continue;
        }
        if (!s.empty()) s += " ";
        s += t.text();
    }
    return quoted(s);
}

class pp_lexer {
public:
    explicit pp_lexer(const source_file& source) : source_{source} {
        next();
    }
    pp_lexer(const pp_lexer&) = delete;
    pp_lexer& operator=(const pp_lexer&) = delete;

    const pp_token& current() const { return current_; }

    // TODO: Handle line continuations
    void next() {
        const auto& t = source_.text();

        for (;;) {
            const auto start_index = index_;
            if (start_index >= t.size()) {
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
                        if (!has_star && t[index_] == '*') {
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
                type = pp_token_type::number;
               // TOOD: Match more correctly
                for (uint8_t last = ch; index_ < t.size(); ++index_) {
                    const uint8_t here = static_cast<uint8_t>(t[index_]);
                    if (is_digit(here) || here == '.' || here == 'e') {
                    } else if (last == 'e' && (here == '+' || here == '-')) {
                    } else if (here == 'u' || here == 'U' || here == 'l' || here == 'L') {
                    } else {
                        break;
                    }
                    last = here;
                }
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

private:
    const source_file& source_;
    pp_token current_;
    size_t index_ = 0;

    template<typename Pred>
    void consume_while(Pred pred) {
        const auto& t = source_.text();
        while (index_ < t.size() && pred(t[index_])) {
            ++index_;
        }
    }
};

class pre_processor {
public:
    explicit pre_processor(const source_file& source) : lex_{source} {
        next();
    }

    const pp_token& current() const {
        return current_;
    }

    void next() {
        for (;;) {
            if (!pending_tokens_.empty()) {
                current_ = pending_tokens_.front();
                pending_tokens_.erase(pending_tokens_.begin());
                return;
            }
            current_ = lex_.current();
            if (!current_) {
                break;
            }
            lex_.next();
            
            if (current_.type() == pp_token_type::punctuation && current_.text() == "#") {
                skip_whitespace();
                handle_directive();
                continue;
            } else if (current_.type() == pp_token_type::identifier) {
                auto it = defines_.find(current_.text());
                if (it != defines_.end()) {
                    pp_lex_token_stream ts{lex_};
                    pending_tokens_ = handle_replace(ts, it->first, it->second);
                    pending_tokens_.erase(std::remove_if(pending_tokens_.begin(), pending_tokens_.end(), [](const auto& t) {
                        return t.type() == pp_token_type::whitespace || t.type() == pp_token_type::newline;
                        }), pending_tokens_.end());
                    continue;
                }
            }
            if (current_.type() != pp_token_type::whitespace && current_.type() != pp_token_type::newline) {
                return;
            }
        }
    }

private:
    struct macro_definition {
        std::optional<std::vector<std::string>> params;
        std::vector<pp_token> replacement;
    };

    pp_lexer lex_;
    std::map<std::string, macro_definition> defines_;
    std::vector<pp_token> pending_tokens_;
    std::vector<std::string> expanding_;
    pp_token current_;

    void skip_whitespace() {
        while (lex_.current() && lex_.current().type() == pp_token_type::whitespace) {
            lex_.next();
        }
    }

    std::string get_identifer() {
        const auto& t = lex_.current();
        if (t.type() != pp_token_type::identifier) {
            NOT_IMPLEMENTED("Expected identifier got " << t);
        }
        const auto text = lex_.current().text();
        lex_.next();
        return text;
    }

    void handle_directive() {
        // TODO: Support null directive
        const auto dir = get_identifer();
        skip_whitespace();
        if (dir == "define") {
            handle_define();
        } else {
            NOT_IMPLEMENTED("#" + dir);
        }
    }

    void handle_define() {
        const auto id = get_identifer();
        std::optional<std::vector<std::string>> params;
        if (lex_.current().type() == pp_token_type::punctuation) {
            if (lex_.current().text() != "(") {
                NOT_IMPLEMENTED("Unexpected " << lex_.current() << " in macro definition");
            }
            lex_.next();
            std::vector<std::string> ps;
            while (lex_.current().type() != pp_token_type::punctuation || lex_.current().text() != ")") {
                skip_whitespace();
                if (!ps.empty()) {
                    if (lex_.current().type() != pp_token_type::punctuation || lex_.current().text() != ",") {
                        NOT_IMPLEMENTED("Expected ',' in macro argument list");
                    }
                    lex_.next();
                    skip_whitespace();
                }
                ps.push_back(get_identifer());
            }
            lex_.next(); // Skip ')'
            params = std::move(ps);
        }
        skip_whitespace();

        std::vector<pp_token> replacement_list;
        for (; lex_.current() && lex_.current().type() != pp_token_type::newline; lex_.next()) {
            if (lex_.current().type() != pp_token_type::whitespace) {
                replacement_list.push_back(lex_.current());
            }
        }

        if (defines_.find(id) != defines_.end()) {
            NOT_IMPLEMENTED("Redefinition of macro " << id);
        }

        defines_[id] = macro_definition{params, replacement_list};
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

    struct pp_lex_token_stream : token_stream {        
        explicit pp_lex_token_stream(pp_lexer& lex) : lex_{lex} {
        }
        void next() override { lex_.next(); }
        const pp_token& current() const override { return lex_.current(); }
    private:
        pp_lexer& lex_;
    };

    struct vec_token_stream : token_stream {
        explicit vec_token_stream(const std::vector<pp_token>& v) : v_{v} {
        }
        void next() override { assert(index_ < v_.size()); ++index_; }
        const pp_token& current() const override { return index_ < v_.size() ? v_[index_] : pp_eof_token; }
    private:
        const std::vector<pp_token>& v_;
        size_t index_ = 0;
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
        std::cout << "Replacing in " << replacement << "\n";
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
        std::cout << " --> " << res << "\n";
        return res;
    }

    std::vector<pp_token> handle_replace(token_stream& ts, const std::string& macro_id, const macro_definition& def) {
        std::cout << "Expanding " << macro_id << "\n";
        std::vector<pp_token> replacement;
        if (def.params) {
            // Function-like macro
            ts.skip_whitespace();
            if (ts.current().type() != pp_token_type::punctuation || ts.current().text() != "(") {
                // Not an invocation
                return { pp_token{ pp_token_type::identifier, macro_id } };
            }
            ts.next();

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
                            assert(!replacement.empty() && !nts.empty());
                            auto last = replacement.back();
                            replacement.back() = pp_token{ last.type(), last.text() + nts.front().text() };
                            std::cout << "paste '" << last.text() << "' and '" << nts.front().text() << "'\n";
                            nts.erase(nts.begin());
                        } else {
                            // Expand macros in argument now that we know it's not being combined/stringified
                            // (note: before restricting expansion of "macro_id")
                            vec_token_stream extra_tokens{{}};
                            nts = do_replace(nts, extra_tokens);
                        }
                        combine_state = combine_none;
                        replacement.insert(replacement.end(), nts.begin(), nts.end());
                        continue;
                    }
                }
                if (combine_state == combine_paste) {
                    assert(!replacement.empty());
                    auto last = replacement.back();
                    replacement.back() = pp_token{ last.type(), last.text() + t.text() };
                    combine_state = combine_none;
                    continue;
                }

                if (combine_state != combine_none) {
                    NOT_IMPLEMENTED("Unused #/## in macro replacement list for " << macro_id << " before " << t);
                }
                combine_state = combine_none;
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
            explicit push_expanding(pre_processor& pp, const std::string& val) : pp{pp} {
                pp.expanding_.push_back(val);
            }
            ~push_expanding() {
                pp.expanding_.pop_back();
            }
            pre_processor& pp;
        } pe{*this, macro_id};
        return do_replace(replacement, ts);
    }
};

} // namespace mcc

using namespace mcc;

auto make_source() {
    return source_file{"test.c",
R"(
#define A 2
#define B ((A)+1)
#define X A + B
X // 2 + ((2)+1)

#define D 42
#define E D /*

DSDS

*/
#define F(X) X+X
F(E) // 42 +42

#define G(A) #A
G(2 + 3) // "2 + 3"

#define H(A,B) A##B, 3 ## 4
H(1,2) // 12, 34
H (x,y) // xy, 34
(H)(1,2) // (H)(1,2)
)"
};
}

#define PP_NUM(n)   pp_token{pp_token_type::number, #n}
#define PP_PUNCT(p) pp_token{pp_token_type::punctuation, p}
#define PP_STR(s)   pp_token{pp_token_type::string_literal, #s}
#define PP_ID(id)   pp_token{pp_token_type::identifier, #id}

void test_pre_processor() {
    auto do_pp = [](const std::string& text) {
            const source_file source{"test", text};
            pre_processor pp{source};
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
        { "42.0", {PP_NUM(42.0)}}, 
        { "1.234", {PP_NUM(1.234)}}, 
        { "5e-10", {PP_NUM(5e-10)}}, 
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
    { "#define X 1\n#if defined(X)\n42\n#endif", {PP_NUM(32)}},
    { "#if X\n42\n#endif", {}},
    { "#if 0x2A==42\n1\n#endif", {PP_NUM(1)}},
    { "#if 2==(1?2:3)\n1\n#endif", {PP_NUM(1)}},

    };

    for (const auto& t: test_cases) {
        const char* delim = "-----------------------------------\n";
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

int main() {
    try {
        test_pre_processor();
    } catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
}