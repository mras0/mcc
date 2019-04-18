#ifndef MCC_PREPROCESSOR_H
#define MCC_PREPROCESSOR_H

#include <string>
#include <memory>
#include <iosfwd>

#include "source.h"

namespace mcc {


enum class pp_token_type {
    whitespace,
    newline,
    header_name,
    identifier,
    number,
    float_number,
    character_constant,
    string_literal,
    punctuation,
    eof
};

std::ostream& operator<<(std::ostream& os, pp_token_type type);

class pp_token {
public:
    explicit pp_token() : type_{pp_token_type::eof}, text_{} {
    }
    explicit pp_token(pp_token_type type, const std::string& text) : type_{type}, text_{text} {
    }

    explicit operator bool() const { return type_ != pp_token_type::eof; }

    pp_token_type type() const { return type_; }
    const std::string& text() const { return text_; }

    static const pp_token eof;
private:
    pp_token_type type_;
    std::string   text_;
};

std::ostream& operator<<(std::ostream& os, const pp_token& tok);

inline bool operator==(const pp_token& l, const pp_token& r) {
    return l.type() == r.type() && l.text() == r.text();
}

inline bool operator!=(const pp_token& l, const pp_token& r) {
    return !(l == r);
}

class preprocessor {
public:
    explicit preprocessor(source_manager& sm, const source_file& source);
    ~preprocessor();

    source_position position() const;
    std::vector<source_position> position_trace() const;
    const pp_token& current() const;
    void next();

private:
    class impl;
    std::unique_ptr<impl> impl_;
};

void define_standard_headers(source_manager& sm);
void define_posix_headers(source_manager& sm);
const char* standard_builtin_text();

}

#endif

