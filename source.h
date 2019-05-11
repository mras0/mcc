#ifndef MCC_SOURCE_H
#define MCC_SOURCE_H

#include <string>
#include <cassert>
#include <iosfwd>
#include <vector>
#include <memory>

namespace mcc {

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

struct source_extend {
    size_t line;
    size_t col_start;
    size_t col_end;
};

class source_position {
public:
    explicit source_position(const source_file& source, size_t index, size_t len) : source_{&source}, index_{index}, len_{len} {
        assert(len < source_->text().size() && index + len <= source_->text().size());
    }

    const source_file& source() const { return *source_; }
    size_t index() const { return index_; }
    size_t length() const { return len_; }

    source_extend extend() const;

private:
    const source_file* source_;
    size_t index_;
    size_t len_;
};

std::ostream& operator<<(std::ostream& os, const source_position& pos);
 
class source_manager {
public:
    explicit source_manager(const std::string& builtin_text) : builtin_{std::make_unique<source_file>("<builtin>", builtin_text)} {}

    void define_standard_headers(const std::string& name, const std::string& contents);
    void add_include_directory(const std::string& dir);

    const source_file& include(const std::string& included_from, const std::string_view filename);
    const source_file& load(const std::string_view filename);
    const source_file& builtin() { return *builtin_; }

private:
    std::vector<std::unique_ptr<source_file>> files_;
    std::vector<std::unique_ptr<source_file>> standard_headers_;
    std::vector<std::string> include_directories_;
    std::string base_dir_;
    std::unique_ptr<source_file> builtin_;
};

std::vector<std::string> process_wild_cards(const std::string_view name);

}

#endif
