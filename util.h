#ifndef MCC_UTIL_H
#define MCC_UTIL_H

#include <string>
#include <string_view>
#include <sstream>
#include <stdexcept>
#include <cassert>
#include <algorithm>

#define NOT_IMPLEMENTED(stuff) do { std::ostringstream oss_; oss_ << __FILE__ << ":" << __LINE__ << ": " << __func__ << " Not implemented: " << stuff; throw std::runtime_error(oss_.str()); } while (0)

namespace mcc {

std::string quoted(std::string_view s);
std::pair<unsigned, size_t> unescape_char(const std::string_view s);
std::string unescape(const std::string_view s);

class source_formatter {
public:
    explicit source_formatter(std::ostream& os, int indent_incr = 0)
        : os_{os}
        , prev_{get(os)}
        , indent_{std::max(0,indent_incr+(prev_?prev_->indent_:0))}
        , precedence_{prev_?prev_->precedence_:1000}
        , suppress_{false} {
        set(os_, this);
    }
    ~source_formatter() {
        assert(get(os_) == this);
        set(os_, prev_);
    }
    source_formatter(const source_formatter&) = delete;
    source_formatter& operator=(const source_formatter&) = delete;

    void suppress_next() {
        assert(get(os_) == this);
        suppress_ = true;
    }

    bool check_suppressed() {
        if (suppress_) {
            suppress_ = false;
            return true;
        }
        return false;
    }

    void indent(int val) {
        assert(get(os_) == this);
        indent_ = val;
    }

    bool precedence(int val) {
        assert(get(os_) == this);
        precedence_ = val;
        return prev_ && precedence_ > prev_->precedence_;
    }

    static int indent(std::ostream& os) {
        auto sf = get(os);
        return sf && !sf->check_suppressed() ? sf->indent_ : 0;
    }

private:
    static const int xindex_; 
    std::ostream& os_;
    source_formatter* const prev_;
    int indent_;
    int precedence_;
    bool suppress_;

    static source_formatter* get(std::ostream& os) {
        return static_cast<source_formatter*>(os.pword(xindex_));
    }

    static void set(std::ostream& os, source_formatter* sf) {
        os.pword(xindex_) = sf;
    }
};
constexpr int default_indent = 4;
struct indent {
    friend std::ostream& operator<<(std::ostream& os, indent) {
        return os << std::string(source_formatter::indent(os), ' ');
    }
};

}

#endif