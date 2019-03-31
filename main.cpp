#include <cassert>
#include <iostream>
#include <string>
#include <string_view>
#include <sstream>
#include <stdexcept>
#include <vector>
#include <optional>
#include <map>

#define NOT_IMPLEMENTED(stuff) do { std::ostringstream oss_; oss_ << __FILE__ << ":" << __LINE__ << ": " << __func__ << " Not implemented: " << stuff; throw std::runtime_error(oss_.str()); } while (0)

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

std::ostream& operator<<(std::ostream& os, const pp_token& tok) {
    return os << "pp_token{" << tok.type() << ", " << quoted(tok.text()) << "}";
}

bool is_punct_char(int ch) {
    return strchr("_{}[]#()<>%:;.?*+-/^&|~!=,\\\"'", ch) != nullptr;
}

bool is_alpha(int ch) {
    return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
}

bool is_digit(int ch) {
    return ch >= '0' && ch <= '9';
}

bool is_whitespace_non_nl(int ch) {
    return ch == ' ' || ch == 't' || ch == '\v' || ch == '\f';
}

bool is_whitespace(int ch) {
    return is_whitespace_non_nl(ch) || ch =='\n';
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
        const auto start_index = index_;
        if (start_index >= t.size()) {
            // EOF
            current_ = pp_token{};
            return;
        }

        const auto ch = static_cast<uint8_t>(t[index_++]);
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
        } else if (ch == '\n') {
            type = pp_token_type::newline;
        } else if (is_whitespace_non_nl(ch)) {
            type = pp_token_type::whitespace;
            consume_while(is_whitespace_non_nl);
        } else if (is_alpha(ch) || ch == '_') {
            type = pp_token_type::identifier;
            consume_while([](int ch) { return is_alpha(ch) || is_digit(ch) || ch == '_'; });
        } else if (is_digit(ch)) {
            type = pp_token_type::number;
            consume_while(is_digit);
        } else if (is_punct_char(ch)) {
            type = pp_token_type::punctuation;
            // TODO: Handle multi-character punctuation
        }

        if (type == pp_token_type::eof) {
            NOT_IMPLEMENTED(quoted(t.substr(start_index, index_ - start_index)));
        }
        current_ = pp_token{type, t.substr(start_index, index_ - start_index)};
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
    }

    void run() {
        for (;;) {
            const auto& t = lex_.current();
            if (!t) break;
            if (t.type() == pp_token_type::punctuation && t.text() == "#") {
                lex_.next();
                skip_whitespace();
                handle_directive();
                continue;
            } else if (t.type() == pp_token_type::identifier) {
                auto it = defines_.find(t.text());
                if (it != defines_.end()) {
                    lex_.next();
                    pp_lex_token_stream ts{lex_};
                    auto toks = handle_replace(ts, it->first, it->second);
                    for (const auto& t2: toks) {
                        if (t2.type() != pp_token_type::whitespace && t2.type() != pp_token_type::newline) {
                            std::cout << "OUTPUT: " << t2 << "\n";
                        }
                    }
                    continue;
                }
            }
            if (t.type() != pp_token_type::whitespace && t.type() != pp_token_type::newline) {
                std::cout << "OUTPUT: " << t << "\n";
            }
            lex_.next();
        }
    }


private:
    struct macro_definition {
        std::optional<std::vector<std::string>> params;
        std::vector<pp_token> replacement;
    };


    pp_lexer lex_;
    std::map<std::string, macro_definition> defines_;

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
            while (lex_.current().type() != pp_token_type::punctuation && lex_.current().text() != ")") {
                if (lex_.current().type() == pp_token_type::whitespace) {
                    lex_.next();
                    continue;
                }
                ps.push_back(get_identifer());
            }
            lex_.next();
            params = std::move(ps);
        }
        skip_whitespace();

        std::vector<pp_token> replacement_list;
        for (; lex_.current() && lex_.current().type() != pp_token_type::newline; lex_.next()) {
            replacement_list.push_back(lex_.current());
        }

        if (defines_.find(id) != defines_.end()) {
            NOT_IMPLEMENTED("Redefinition of macro " << id);
        }

        defines_[id] = macro_definition{params, replacement_list};
    }

    struct token_stream {
        virtual void next() = 0;
        virtual const pp_token& current() const = 0;
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

    std::vector<pp_token> do_replace(const std::vector<pp_token>& replacement, const std::string& macro_id) {
        vec_token_stream tsr{replacement};
        std::vector<pp_token> res;
        while (tsr.current()) {
            if (tsr.current().type() == pp_token_type::identifier && tsr.current().text() != macro_id) {
                auto it = defines_.find(tsr.current().text());
                if (it != defines_.end()) {
                    tsr.next();
                    auto r = handle_replace(tsr, it->first, it->second);
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
        if (def.params) {
            // Function-like macro
            skip_whitespace();
            if (ts.current().type() != pp_token_type::punctuation || ts.current().text() != "(") {
                NOT_IMPLEMENTED("Expected '(' after function-like macro name");
            }
            ts.next();
            // TODO: Skip matching parenthesis
            std::vector<std::vector<pp_token>> args;
            std::vector<pp_token> current_arg;

            for (int nest = 1; ts.current(); ts.next()) {
                const auto& t = ts.current();
                if (t.type() == pp_token_type::punctuation) {
                    if (t.text() == "(") {
                        ++nest;
                        continue;
                    } else if (t.text() == ")") {
                        if (--nest == 0) {
                            break;
                        }
                        continue;
                    } else if (t.text() == ",") {
                        args.push_back(std::move(current_arg));
                        current_arg.clear();
                        continue;
                    }
                }
                current_arg.push_back(t);
            }
            if (!current_arg.empty()) {
                args.push_back(std::move(current_arg));
            }
            if (ts.current().type() != pp_token_type::punctuation || ts.current().text() != ")") {
                NOT_IMPLEMENTED("Expected ')' after function-like macro");
            }
            ts.next();

            if (args.size() != def.params->size()) {
                NOT_IMPLEMENTED("Invalid number of arguments to macro got " << args.size() << " expecting " << def.params->size());
            }

            std::vector<pp_token> replacement;
            for (size_t ri = 0, rs = def.replacement.size(); ri < rs; ++ri) {
                const auto& t = def.replacement[ri];
                if (t.type() == pp_token_type::identifier) {
                    auto it = std::find(def.params->begin(), def.params->end(), t.text());
                    if (it != def.params->end()) {
                        const auto ai = std::distance(def.params->begin(), it);
                        replacement.insert(replacement.end(), args[ai].begin(), args[ai].end());
                        continue;
                    }
                }
                replacement.push_back(t);
            }

            return do_replace(replacement, macro_id);
        } else {
            return do_replace(def.replacement, macro_id);
        }
    }
};

} // namespace mcc

auto make_source() {
    return mcc::source_file{"test.c",
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
)"
};
}

int main() {
    using namespace mcc;
    try {
        const auto source = make_source();        
        pre_processor pp{source};
        pp.run();
    } catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
}