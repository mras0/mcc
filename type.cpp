#include "type.h"
#include "util.h"
#include <ostream>

namespace mcc {

void output_flags(std::ostream& os, ctype t) {
#define CHECK_FLAG(f) if (!!(t & ctype::f##_f)) os << #f " "
    CHECK_FLAG(inline);
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
    case ctype::none:          os << "none"; break;
    case ctype::void_t:        os << "void"; break;
    case ctype::plain_char_t:  os << "char"; break;
    case ctype::char_t:        os << "char"; break;
    case ctype::short_t:       os << "short"; break;
    case ctype::int_t:         os << "int"; break;
    case ctype::long_t:        os << "long"; break;
    case ctype::long_long_t:   os << "long long"; break;
    case ctype::float_t:       os << "float"; break;
    case ctype::double_t:      os << "double"; break;
    case ctype::long_double_t: os << "long double"; break;
    case ctype::pointer_t:     os << "pointer"; break;
    case ctype::array_t:       os << "array"; break;
    case ctype::struct_t:      os << "struct"; break;
    case ctype::union_t:       os << "union"; break;
    case ctype::enum_t:        os << "enum"; break;
    case ctype::function_t:    os << "function"; break;
    default:
        NOT_IMPLEMENTED(static_cast<uint32_t>(t & ctype::base_f));
    }
    if (!!(t & ctype::bitfield_f)) {
        os << ":" << static_cast<int>(bitfield_value(t));
    }
    return os;
}

ctype common_type(ctype l, ctype r) {
    if (l == r) {
        return l;
    }
    NOT_IMPLEMENTED(l << " " << r);
}

void type::modify_inner(const std::shared_ptr<const type>& t) {
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
        os << *t.pointer_val();
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

void output_decl(std::ostream& os, const std::string& id, const type& t) {
    switch (t.base()) {
    case ctype::pointer_t:
    {
        const auto& pointee = *t.pointer_val();
        const bool need_paren = pointee.base() == ctype::array_t || pointee.base() == ctype::function_t;
        std::ostringstream oss;
        if (need_paren) {
            oss << "(";
        }
        oss << "* ";
        output_flags(os, t.ct());
        oss << id;
        if (need_paren) {
            oss << ")";
        }
        output_decl(os, oss.str(), pointee);
        break;
    }
    case ctype::array_t:
    {
        const auto& ai = t.array_val();
        output_flags(os, t.ct());
        os << *ai.t() << " " << id << "[";
        if (ai.bound() != array_info::unbounded) {
            os << ai.bound();
        }
        os << "]";
        break;
    }
    case ctype::function_t:
    {
        const auto& fi = t.function_val();
        output_flags(os, t.ct());
        os << *fi.ret_type() << " " << id << "(";
        for (size_t i = 0; i < fi.params().size(); ++i) {
            if (i) os << ", ";
            os << fi.params()[i];
        }
        if (fi.variadic()) {
            if (!fi.params().empty()) os << ", ";
            os << "...";
        }
        os << ")";
        break;
    }
    default:
        os << t << " " << id;
        break;
    }
}

std::ostream& operator<<(std::ostream& os, const decl& d) {
    output_decl(os, d.id(), *d.t());
    return os;
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
    if (fi.variadic()) {
        if (!fi.params().empty()) os << ", ";
        os << "...";
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
