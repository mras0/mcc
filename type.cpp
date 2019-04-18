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
    case ctype::bool_t:        os << "bool"; break;
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
    case ctype::reference_t:   os << "reference"; break;
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

void type::modify_inner(const std::shared_ptr<const type>& t) {
    if (base() == ctype::pointer_t) {
        auto& pointed_type = std::get<1>(val_);
        if (pointed_type->ct() != ctype::none) {
            NOT_IMPLEMENTED(*this << " " << *pointed_type);
        }
        val_ = t;
    } else if (base() == ctype::array_t) {
        auto& ai = std::get<2>(val_);
        const_cast<type*>(ai->t().get())->modify_inner(t);
    } else {
        NOT_IMPLEMENTED(*this << " " << *t);
    }        
}

std::ostream& operator<<(std::ostream& os, type t) {
    switch (t.base()) {
    case ctype::pointer_t:
    case ctype::reference_t:
        if (t.base() == ctype::pointer_t) {
            os << *t.pointer_val() << "*";
        } else {
            os << *t.reference_val() << "&";
        }
        if (!!(t.ct() & ctype::cvr_f)) {
            os << " ";
            output_flags(os, t.ct());
        }
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
    case ctype::reference_t:
    {
        const auto& pointee = *t.pointer_val();
        const bool need_paren = pointee.base() == ctype::array_t || pointee.base() == ctype::function_t;
        std::ostringstream oss;
        if (need_paren) {
            oss << "(";
        }
        oss << (t.base() == ctype::pointer_t ? "*" : "&");
        if (!!(t.ct() & ctype::cvr_f)) {
            oss << " ";
            output_flags(os, t.ct());
        }
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
        os << t;
        if (!id.empty()) {
            os << " " << id;
        }
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

ctype integral_promotion(ctype t) {
    assert(is_integral(t));
    if (base_type(t) < ctype::int_t) {
        // If int can represent the entire range of values of the original type (or the range of values of the original bit field),
        // the value is converted to type int. Otherwise the value is converted to unsigned int. 
        return ctype::int_t;
    }
    return t;
}

ctype common_type(ctype l, ctype r) {
    l &= ~(ctype::storage_f | ctype::cvr_f);
    r &= ~(ctype::storage_f | ctype::cvr_f);
    if (l == r) {
        return l;
    }
    assert(is_arithmetic(l));
    assert(is_arithmetic(r));

    if (l == ctype::long_double_t || r == ctype::long_double_t) {
        return ctype::long_double_t;
    } else if (l == ctype::double_t || r == ctype::double_t) {
        return ctype::double_t;
    } else if (l == ctype::float_t || r == ctype::float_t) {
        return ctype::float_t;
    }

    // First perform integral promotion
    l = integral_promotion(l);
    r = integral_promotion(r);
    // If the types are the same, we're done
    if (l == r) {
        return l;
    }
    const auto bl = base_type(l);
    const auto br = base_type(r);
    // If both have the same signedless
    if ((l & ctype::unsigned_f) == (r & ctype::unsigned_f)) {
        return bl < br ? r : l;
    }
    // Signs are different
    const auto [urank, srank] = !!(l & ctype::unsigned_f) ? std::pair{ bl, br } : std::pair{ br, bl };
    if (urank >= srank) {
        // Unsigned operand's rank is greater or equal
        return urank | ctype::unsigned_f;
    } else if (srank > urank) {
        // Signed operand's rank is greater
        // TODO: Check if the signed type can represent all values of the unsigned type
        return srank;
    }
    // both operands undergo implicit conversion to the unsigned type counterpart of the signed operand's type.
    NOT_IMPLEMENTED(l << " <> " << r << " urank: " << urank << " srank: " << srank);
    // return srank | ctype::unsigned_f
}

type_ptr common_type(const type_ptr& l, const type_ptr& r) {
    const auto lb = l->base(), rb = r->base();
    if (lb == ctype::void_t || rb > ctype::void_t) {
        return l;
    } else if (lb < ctype::pointer_t && rb < ctype::pointer_t) {
        return std::make_shared<type>(common_type(l->ct(), r->ct()));
    } else if ((is_integral(lb) && rb <= ctype::pointer_t) || (lb <= ctype::pointer_t && is_integral(rb))) {
        return std::make_shared<type>(ctype::bool_t);
    } else if (lb == ctype::pointer_t && rb == ctype::pointer_t) {
        const auto lpt = l->pointer_val();
        const auto rpt = r->pointer_val();
        const auto diff = (lpt->ct() ^ rpt->ct()) & ctype::cvr_f;
        if (!diff) {
            if (lpt->base() == rpt->base()) {
                if (lpt->base() < ctype::pointer_t) {
                    return l;
                }
                if (lpt->base() == ctype::struct_t && &lpt->struct_val() == &rpt->struct_val()) {
                    return l;
                }
                NOT_IMPLEMENTED(*l << " " << *r);
            } else {
                auto t = make_ptr_t(std::make_shared<type>(ctype::void_t | (lpt->ct() & ctype::cvr_f)));
                if (!!(lpt->ct() & ctype::cvr_f)) {
                    NOT_IMPLEMENTED(*l << " " << *r << " -> " << *t);
                }
                return t;
            }
        }
        NOT_IMPLEMENTED(*l << " " << *r << " diff: " << diff);
    }
    NOT_IMPLEMENTED("common_type(" << *l << ", " << *r << ")") ;
}

type_ptr make_ref_t(const type_ptr& t) {
    return std::make_shared<type>(ctype::reference_t, t);
}

type_ptr make_ptr_t(const type_ptr& t, ctype flags) {
    assert(!(flags & ctype::base_f));
    return std::make_shared<type>(ctype::pointer_t | flags, t);
}

type_ptr make_array_t(const type_ptr& element, uint64_t bound, ctype flags) {
    assert(!(flags & ctype::base_f));
    return std::make_shared<type>(ctype::array_t | flags, std::make_shared<array_info>(element, bound));
}           

type_ptr remove_flags(const type_ptr& t, ctype flags) {
    if (!(t->ct() & flags)) {
        return t;
    }
    auto cpy = std::make_shared<type>(*t);
    cpy->remove_flags(flags);
    return cpy;
}

type_ptr remove_cvr(const type_ptr& t) {
    return remove_flags(t, ctype::cvr_f);
}

type_ptr to_rvalue(const type_ptr& t) {
    return remove_cvr(t->base() == ctype::reference_t ? t->reference_val() : t);
}

type_ptr decay1(const type_ptr& t) {
    const auto base = t->base();
    if (base <= ctype::pointer_t || base == ctype::struct_t || base == ctype::union_t) {
        return t;
    } else if (base == ctype::function_t) {
        return make_ptr_t(t);
    } else if (base == ctype::array_t) {
        return make_ptr_t(t->array_val().t());
        return t;
    } else if (base == ctype::enum_t) {
        return std::make_shared<type>(ctype::int_t);
    }
    NOT_IMPLEMENTED(*t);
}

type_ptr decay(const type_ptr& t) {
    return decay1(to_rvalue(t));
}

// Check if pointer types are compatible
bool is_compatible_pointer_type(const type_ptr& l, const type_ptr& r, bool ignore_cvr) {
    // Area qualifiers missing from the lhs compared to the rhs?
    if (!ignore_cvr && !!((r->ct() & ctype::cvr_f) & ~l->ct())) {
        return false;
    }
    const auto lb = l->base(), rb = r->base();
    if (lb == ctype::void_t || rb == ctype::void_t) {
        return true;
    }
    if (lb < ctype::pointer_t && rb < ctype::pointer_t) {
        return lb == rb;
    } else if (lb == ctype::pointer_t && rb == ctype::pointer_t) {
        return is_compatible_pointer_type(l->pointer_val(), r->pointer_val());
    } else if (lb == rb && lb == ctype::struct_t) {
        return &l->struct_val() == &r->struct_val();
    } else if (lb == ctype::function_t) {
        return redecl_type_compare(*l, *r);
    }
    NOT_IMPLEMENTED(*l << " " << *r);
}

bool is_convertible(const type_ptr& l, const type_ptr& r) {
    // 6.5.16.1 constaints for simple assignment
    const auto lb = l->base(), rb = r->base();
    if (lb == ctype::bool_t) {
        return is_arithmetic(rb) || rb == ctype::pointer_t;
    } else if (rb == ctype::bool_t) {
        return is_convertible(r, l);
    } else if (is_arithmetic(lb) && is_arithmetic(rb)) {
        (void)common_type(lb, rb);
        return true;
    } else if (lb == ctype::pointer_t && rb == ctype::pointer_t) {
        return is_compatible_pointer_type(l->pointer_val(), r->pointer_val());
    } else if (lb == ctype::struct_t && rb == ctype::struct_t) {
        return &l->struct_val() == &r->struct_val();
    } else if (lb == ctype::union_t && rb == ctype::union_t) {
        return &l->union_val() == &r->union_val();
    }
    NOT_IMPLEMENTED(*l << " " << *r);
    return true;
}

bool redecl_type_compare(const type& l, const type& r) {
    if (l.base() != r.base()) {
        return false;
    }
    if (l.ct() != r.ct()) {
        // It's OK if later declarations drop extern/static
        const auto old_storage_flags = l.ct() & (ctype::extern_f|ctype::storage_f);
        if (l.ct() != (r.ct() | old_storage_flags)) {
            return false;
        }
    }
    const auto b = l.base();
    if (b < ctype::pointer_t) {
        return true;
    } else if (b == ctype::pointer_t) {
        const auto& lp = *l.pointer_val();
        const auto& rp = *r.pointer_val();
        return redecl_type_compare(lp, rp);
    } else if (b == ctype::struct_t) {
        return &l.struct_val() == &r.struct_val();
    } else if (b == ctype::union_t) {
        return &l.union_val() == &r.union_val();
    } else if (b == ctype::function_t) {
        const auto& lf = l.function_val();
        const auto& rf = r.function_val();
        if (lf.variadic() ^ rf.variadic()) {
            return false;
        }
        if (!redecl_type_compare(*lf.ret_type() , *rf.ret_type())) {
            return false;
        }
        if (lf.params().size() != rf.params().size()) {
            return false;
        }
        for (size_t i = 0, sz = lf.params().size(); i < sz; ++i) {
            if (!redecl_type_compare(*lf.params()[i].t(), *rf.params()[i].t())) {
                return false;
            }
        }
        return true;
    } else if (b == ctype::array_t) {
        const auto& la = l.array_val();
        const auto& ra = r.array_val();
        return la.bound() == ra.bound() && redecl_type_compare(*la.t(), *ra.t());
    }
    NOT_IMPLEMENTED(l << " <> " << r);
}


} // namespace mcc
