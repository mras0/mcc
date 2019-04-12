#ifndef MCC_PARSER_H
#define MCC_PARSER_H

#include "source.h"

namespace mcc {

class parser {
public:
    explicit parser(source_manager& sm, const source_file& source);
    ~parser();

    std::vector<source_position> position() const;

    void parse();

private:
    class impl;
    std::unique_ptr<impl> impl_;
};


} // namespace mcc

#endif
