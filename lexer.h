#ifndef MCC_LEXER_H
#define MCC_LEXER_H

#include "source.h"
#include "token.h"
#include "preprocessor.h"

namespace mcc {

class lexer {
public:
    explicit lexer(source_manager& sm, const source_file& source) : pp_{sm, source} {
        next();
    }

    auto position() const {
        return pp_.position();
    }

    const token& current() const {
        return current_;
    }

    void next();

private:
    preprocessor pp_;
    token current_;
};

} // namespace mcc

#endif
