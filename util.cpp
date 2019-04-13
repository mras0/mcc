#include "util.h"

namespace mcc {

const int source_formatter::xindex_ = std::ios_base::xalloc();

bool is_octal_digit(int ch) {
    return ch >= '0' && ch <= '7';
}

std::string quoted(std::string_view s) {
    std::string res;
    res += '\"';
    for (const auto ch: s) {
        const auto uch = static_cast<uint8_t>(ch);
        if (uch == '\"' || uch == '\\' || uch < 32 || uch > 127) {
            switch (ch) {
            case '\\': res += "\\\\"; break;
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

std::pair<unsigned, size_t> unescape_char(const std::string_view s) {
    const auto l = s.length();
    if (l > 0) {
        if (s[0] != '\\') {
            return { static_cast<uint8_t>(s[0]), 1 };
        } else if (l > 1) {
            switch (s[1]) {
            case '"':  return { '\"', 2 };
            case '\'': return { '\'', 2 };
            case '\\': return { '\\', 2 };
            case 'a':  return { '\a', 2 };
            case 'b':  return { '\b', 2 };
            case 'f':  return { '\f', 2 };
            case 'n':  return { '\n', 2 };
            case 'r':  return { '\r', 2 };
            case 't':  return { '\t', 2 };
            case 'v':  return { '\v', 2 };
            }
            if (is_octal_digit(s[1])) {
                unsigned val = s[1]-'0';
                size_t n = 1;
                if (l > n+1 && is_octal_digit(s[n+1])) {
                    val = val*8 + s[n+1]-'0';
                    ++n;
                    if (l > n+1 && is_octal_digit(s[n+1])) {
                        val = val*8 + s[n+1]-'0';
                        ++n;
                    }
                }
                return {val, n+1};
            }
        }
    }
    NOT_IMPLEMENTED(s);
}

std::string unescape(const std::string_view s) {
    const auto l = s.size();
    std::string lit;
    for (size_t j = 0; j < l;) {
        const auto [ch, len] = unescape_char(s.substr(j, l-j));
        assert(ch < 256);
        lit.push_back(static_cast<char>(ch));
        j += len;
        assert(j <= l);
    }
    return lit;
}

}
