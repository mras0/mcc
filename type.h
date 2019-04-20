#ifndef MCC_TYPE_H
#define MCC_TYPE_H

#include "enum_ops.h"
#include <iosfwd>
#include <memory>
#include <variant>
#include <cassert>
#include <vector>
#include <string>

namespace mcc {

constexpr int bitfield_shift = 24;

enum class ctype {
    none,

    // Order of the type is significant
    void_t,
    bool_t,
    plain_char_t,
    char_t,
    short_t,
    int_t,
    long_t,
    long_long_t,
    float_t,
    double_t,
    long_double_t,
    pointer_t, // Must be first non-basic type in enum
    reference_t,
    array_t,
    struct_t,
    union_t,
    enum_t,
    function_t,

    base_f     = 0xff,

    unsigned_f = 1<<8,
    bitfield_f = 1<<9,
    extern_f   = 1<<10,
    static_f   = 1<<11,
    typedef_f  = 1<<12,
    register_f = 1<<13,
    const_f    = 1<<14,
    restrict_f = 1<<15,
    volatile_f = 1<<16,
    inline_f   = 1<<17,

    storage_f = extern_f | static_f | typedef_f | register_f | inline_f,
    cvr_f = const_f|volatile_f|restrict_f,

    bitfield_mask = 0x3f << bitfield_shift,
};

ENUM_BIT_OPS(ctype)

std::ostream& operator<<(std::ostream& os, ctype t);

constexpr ctype base_type(ctype t) {
    return t & ctype::base_f;
}

constexpr ctype modified_base_type(ctype t, ctype new_base) {
    return new_base | (t & ~ctype::base_f);
}

constexpr bool is_integral(ctype t) {
    return base_type(t) >= ctype::bool_t && base_type(t) <= ctype::long_long_t;
}

constexpr bool is_arithmetic(ctype t) {
    return base_type(t) >= ctype::bool_t && base_type(t) <= ctype::long_double_t;
}

constexpr ctype modified_bitfield(ctype t, uint8_t val) {
    assert(!!(t & ctype::bitfield_f) && val < 64);
    return t | static_cast<ctype>(static_cast<std::underlying_type_t<ctype>>(val) << bitfield_shift);
}

constexpr uint8_t bitfield_value(ctype t) {
    assert(!!(t & ctype::bitfield_f));
    return static_cast<uint8_t>(static_cast<std::underlying_type_t<ctype>>(t & ctype::bitfield_mask) >> bitfield_shift);
}

void output_flags(std::ostream& os, ctype t);

class array_info;
class struct_info;
class union_info;
class enum_info;
class function_info;
class type;
using type_ptr = std::shared_ptr<const type>;

class type {
public:
    explicit type() : t_{ctype::none}, val_{} {
    }

    explicit type(ctype t) : t_{t}, val_{} {
        assert(base_type(t_) < ctype::pointer_t);
    }

    explicit type(ctype t, const type_ptr& pointee) : t_{t}, val_{pointee} {
        assert(base_type(t_) == ctype::pointer_t || base_type(t_) == ctype::reference_t);
    }

    explicit type(ctype t, const std::shared_ptr<const array_info>& array_inf) : t_{t}, val_{array_inf} {
        assert(base_type(t_) == ctype::array_t);
    }

    explicit type(ctype t, const std::shared_ptr<const struct_info>& struct_inf) : t_{t}, val_{struct_inf} {
        assert(base_type(t_) == ctype::struct_t);
    }

    explicit type(ctype t, const std::shared_ptr<const union_info>& union_inf) : t_{t}, val_{union_inf} {
        assert(base_type(t_) == ctype::union_t);
    }

    explicit type(ctype t, const std::shared_ptr<const enum_info>& enum_inf) : t_{t}, val_{enum_inf} {
        assert(base_type(t_) == ctype::enum_t);
    }

    explicit type(ctype t, const std::shared_ptr<const function_info>& function_inf) : t_{t}, val_{function_inf} {
        assert(base_type(t_) == ctype::function_t);
    }

    ctype ct() const { return t_; }

    ctype base() const { return t_ & ctype::base_f; }

    void set_base_type(ctype new_base) {
        assert(!(new_base & ~ctype::base_f) && new_base <= ctype::long_double_t);
        t_ = modified_base_type(t_, new_base);
    }

    void add_flags(ctype flags) {
        assert(!(flags & ctype::base_f));
        t_ |= flags;
    }

    void remove_flags(ctype flags) {
        assert(!(flags & ctype::base_f));
        t_ &= ~flags;
    }

    void modify_inner(const type_ptr& t);

    const type_ptr& pointer_val() const {
        assert(base_type(t_) == ctype::pointer_t);
        return std::get<1>(val_);
    }

    const type_ptr& reference_val() const {
        assert(base_type(t_) == ctype::reference_t);
        return std::get<1>(val_);
    }

    const array_info& array_val() const {
        assert(base_type(t_) == ctype::array_t);
        return *std::get<2>(val_);
    }

    const struct_info& struct_val() const {
        assert(base_type(t_) == ctype::struct_t);
        return *std::get<3>(val_);
    }

    const union_info& union_val() const {
        assert(base_type(t_) == ctype::union_t);
        return *std::get<4>(val_);
    }

    const enum_info& enum_val() const {
        assert(base_type(t_) == ctype::enum_t);
        return *std::get<5>(val_);
    }

    const function_info& function_val() const {
        assert(base_type(t_) == ctype::function_t);
        return *std::get<6>(val_);
    }

private:
    ctype t_;
    std::variant<std::monostate,
                 type_ptr,
                 std::shared_ptr<const array_info>,
                 std::shared_ptr<const struct_info>,
                 std::shared_ptr<const union_info>,
                 std::shared_ptr<const enum_info>,
                 std::shared_ptr<const function_info>
        > val_;
};

std::ostream& operator<<(std::ostream& os, type t);

void output_decl(std::ostream& os, const std::string& id, const type& t);

class decl {
public:
    explicit decl(const type_ptr& t, const std::string& id) : type_{t}, id_{id} {}

    const type_ptr& t() const { return type_; }
    const std::string& id() const { return id_; }

private:
    type_ptr type_;
    std::string id_;
};

std::ostream& operator<<(std::ostream& os, const decl& d);

class array_info {
public:
    static constexpr uint64_t unbounded = UINT64_MAX;
    explicit array_info(const type_ptr& t, uint64_t bound) : t_{t}, bound_{bound} {}

    const type_ptr& t() const { return t_;}
    uint64_t bound() const { return bound_; }

private:
    type_ptr t_;
    uint64_t bound_;
};

std::ostream& operator<<(std::ostream& os, const array_info& ai);

class parser;

class tag_info_type {
public:
    explicit tag_info_type(const std::string& id) : id_{id} {}
    virtual ~tag_info_type() {}

    const std::string& id() const { return id_; }
    virtual ctype base_type() const = 0;
private:
    std::string id_;
};

std::ostream& operator<<(std::ostream& os, const tag_info_type& tit);

class struct_union_member : public decl {
public:
    explicit struct_union_member(const decl& d, size_t pos) : decl{d}, pos_{pos} {
    }
    size_t pos() const { return pos_; }
private:
    size_t pos_;
};

class struct_info : public tag_info_type {
public:
    explicit struct_info(const std::string& id) : tag_info_type{id} {}
    ctype base_type() const override { return ctype::struct_t; }
    const std::vector<struct_union_member>& members() const { return members_; }
    size_t size() const { return size_; }
    size_t align() const { return align_; }
private:
    std::vector<struct_union_member> members_;
    size_t size_ = 0;
    size_t align_ = 0;
    friend parser;
};

class union_info : public tag_info_type {
public:
    explicit union_info(const std::string& id) : tag_info_type{id} {}
    ctype base_type() const override { return ctype::union_t; }
    const std::vector<struct_union_member>& members() const { return members_; }
    size_t size() const { return size_; }
    size_t align() const { return align_; }
private:
    std::vector<struct_union_member> members_;
    size_t size_ = 0;
    size_t align_ = 0;
    friend parser;
};

class enum_value {
public:
    explicit enum_value(const std::string& id, int64_t val) : id_{id}, val_{val} {
    }

    const std::string& id() const { return id_; }
    int64_t val() const { return val_; }

private:
    std::string id_;
    int64_t val_;
};

class enum_info : public tag_info_type {
public:
    explicit enum_info(const std::string& id) : tag_info_type{id} {}
    ctype base_type() const override { return ctype::enum_t; }
    const std::vector<enum_value>& values() const { return values_; }
private:
    std::vector<enum_value> values_;
    friend parser;
};

class function_info {
public:
    explicit function_info(const type_ptr& ret_type, const std::vector<decl>& params, bool variadic) : ret_type_{ret_type}, params_{params}, variadic_{variadic} {
    }

    const type_ptr& ret_type() const { return ret_type_; }
    const std::vector<decl>& params() const { return params_; }
    bool variadic() const { return variadic_; }

private:
    type_ptr ret_type_;
    std::vector<decl> params_;
    bool variadic_;
};
std::ostream& operator<<(std::ostream& os, const function_info& fi);

std::shared_ptr<type> make_tag_type(const std::shared_ptr<tag_info_type>& tag_type, ctype flags);

inline const auto& struct_union_members(const type& t) {
    assert(t.base() == ctype::struct_t || t.base() == ctype::union_t);
    return t.base() == ctype::struct_t ? t.struct_val().members() : t.union_val().members();
}

type_ptr make_ptr_t(const type_ptr& t, ctype flags = ctype::none);
type_ptr make_ref_t(const type_ptr& t);
type_ptr make_array_t(const type_ptr& element, uint64_t bound, ctype flags = ctype::none);
ctype common_type(ctype l, ctype r);
type_ptr common_type(const type_ptr& lhs, const type_ptr& rhs);
type_ptr remove_cvr(const type_ptr& t);
type_ptr to_rvalue(const type_ptr& t);
type_ptr decay(const type_ptr& t);
bool is_convertible(const type_ptr& l, const type_ptr& r);
bool is_compatible_pointer_type(const type_ptr& l, const type_ptr& r, bool ignore_cvr = false);
bool redecl_type_compare(const type& l, const type& r);
bool types_equal(const type& l, const type& r);
type_ptr var_arg_type(const type_ptr& t);

size_t sizeof_type(ctype ct);
size_t sizeof_type(const type& t);
size_t alignof_type(const type& t);

constexpr size_t round_up(size_t val, size_t align) {
    return (val + align - 1) & ~(align - 1);
}

} // namespace mcc

#endif
