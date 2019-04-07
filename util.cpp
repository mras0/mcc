#include "util.h"

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

}
