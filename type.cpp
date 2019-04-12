#include "type.h"
#include "util.h"
#include <ostream>

namespace mcc {

void output_flags(std::ostream& os, ctype t) {
#define CHECK_FLAG(f) if (!!(t & ctype::f##_f)) os << #f " "
    CHECK_FLAG(extern);
    CHECK_FLAG(static);
    CHECK_FLAG(typedef);
    CHECK_FLAG(register);
    CHECK_FLAG(const);
    CHECK_FLAG(restrict);
    CHECK_FLAG(volatile);
    CHECK_FLAG(unsigned);
#undef CHECK_FLAG
    if ((t & ctype::base_f) == ctype::char_t && !(t & ctype::unsigned_f)) {
        os << "signed ";
    }
}

std::ostream& operator<<(std::ostream& os, ctype t) {
    output_flags(os, t);
    switch (t & ctype::base_f) {
    case ctype::none:          return os << "none";
    case ctype::void_t:        return os << "void";
    case ctype::plain_char_t:  return os << "char";
    case ctype::char_t:        return os << "char";
    case ctype::short_t:       return os << "short";
    case ctype::int_t:         return os << "int";
    case ctype::long_t:        return os << "long";
    case ctype::long_long_t:   return os << "long long";
    case ctype::float_t:       return os << "float";
    case ctype::double_t:      return os << "double";
    case ctype::long_double_t: return os << "long double";
    case ctype::pointer_t:     return os << "pointer";
    case ctype::array_t:       return os << "array";
    case ctype::struct_t:      return os << "struct";
    case ctype::union_t:       return os << "union";
    case ctype::enum_t:        return os << "enum";
    case ctype::function_t:    return os << "function";
    }
    NOT_IMPLEMENTED(static_cast<uint32_t>(t & ctype::base_f));
}

void type::modify_inner(const std::shared_ptr<type>& t) {
    if (base() == ctype::pointer_t) {
        auto& pointed_type = std::get<1>(val_);
        if (pointed_type->ct() != ctype::none) {
            NOT_IMPLEMENTED(*this << " " << *pointed_type);
        }
        val_ = t;
    } else {
        NOT_IMPLEMENTED(*this << " " << *t);
    }        
}

std::ostream& operator<<(std::ostream& os, type t) {
    switch (t.base()) {
    case ctype::pointer_t: 
        os << t.pointer_val();
        os << " * ";
        output_flags(os, t.ct());
        return os;
    case ctype::array_t:
        output_flags(os, t.ct());
        return os << t.array_val();
    case ctype::struct_t:
        output_flags(os, t.ct());
        return os << "struct " << t.struct_val().id();
    case ctype::union_t:
        output_flags(os, t.ct());
        return os << "union " << t.union_val().id();
    case ctype::enum_t:
        output_flags(os, t.ct());
        return os << "enum " << t.enum_val().id();
    case ctype::function_t:
        output_flags(os, t.ct());
        return os << t.function_val();
    default:
        return os << t.ct();
    }
}

std::ostream& operator<<(std::ostream& os, const decl& d) {
    return os << *d.t() << " " << d.id();
}

std::ostream& operator<<(std::ostream& os, const array_info& ai) {
    os << *ai.t();
    if (ai.bound() != array_info::unbounded) {
        return os << "[" << ai.bound() << "]";
    } else {
        return os << "[]";
    }
}

std::ostream& operator<<(std::ostream& os, const tag_info_type& tit) {
    return os << tit.base_type()  << " " << tit.id();
}

std::ostream& operator<<(std::ostream& os, const function_info& fi) {
    os << *fi.ret_type() << "(";
    for (size_t i = 0; i < fi.params().size(); ++i) {
        if (i) os << ", ";
        os << fi.params()[i];
    }
    return os << ")";
}

std::shared_ptr<type> make_tag_type(const std::shared_ptr<tag_info_type>& tag_type, ctype flags) {
    assert(!(flags & ctype::base_f));
    const auto bt = tag_type->base_type();
    if (bt == ctype::struct_t) {
        return std::make_shared<type>(bt | flags, std::static_pointer_cast<struct_info>(tag_type));
    } else if (bt == ctype::union_t) {
        return std::make_shared<type>(bt | flags, std::static_pointer_cast<union_info>(tag_type));
    } else if (bt == ctype::enum_t) {
        return std::make_shared<type>(bt | flags, std::static_pointer_cast<enum_info>(tag_type));
    } else {
        NOT_IMPLEMENTED(flags << " " << *tag_type);
    }
}

} // namespace mcc
