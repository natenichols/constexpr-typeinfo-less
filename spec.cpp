#include <string_view>
#include <memory>
#include <utility>
#include <variant>
#include <optional>
#include <compare>

namespace key {
  // names
  struct anonymous_t {
    constexpr auto operator<=>(anonymous_t const&) const -> std::strong_ordering = default;
  };
  anonymous_t anonymous_v;
  constexpr struct global_t {} global_v {};
  struct identifier_t {
    std::string_view value;
    constexpr auto operator<=>(identifier_t const&) const -> std::strong_ordering = default;
  };
  using name_t = std::variant<global_t, anonymous_t, identifier_t>;

  enum class refkind { none, lv, rv };
  struct qual {
    bool is_const;
    bool is_volatile;
    refkind ref;
    bool is_restrict;
    constexpr auto operator<=>(qual const&) const -> std::strong_ordering = default;
  };

  constexpr struct void_t {
    constexpr auto operator<=>(void_t const&)  const -> std::strong_ordering = default;
  } void_v {};
  constexpr struct nullptr_t {
    constexpr auto operator<=>(nullptr_t const&)  const -> std::strong_ordering = default;
  } nullptr_v {};

  constexpr struct bool_t {
    constexpr auto operator<=>(bool_t const&)  const -> std::strong_ordering = default;
  } bool_v {};

  // raw memory types
  constexpr struct char_t {
    constexpr auto operator<=>(char_t const&)  const -> std::strong_ordering = default;
  } char_v {};
  constexpr struct unsigned_char_t {
    constexpr auto operator<=>(unsigned_char_t const&)  const -> std::strong_ordering = default;
  } unsigned_char_v {};
  constexpr struct signed_char_t {
    constexpr auto operator<=>(signed_char_t const&)  const -> std::strong_ordering = default;
  } signed_char_v {};

  struct integral {
    unsigned size_bits;
    bool is_signed;
    std::string_view sort_name;
    constexpr auto operator<=>(integral const&)  const -> std::strong_ordering = default;
  };

  constexpr auto short_v = integral{16, true, "short"};
  constexpr auto unsigned_short_v = integral{16, false, "unsigned short"};
  constexpr auto int_v = integral{32, true, "int"};
  constexpr auto unsigned_int_v = integral{32, false, "unsigned int"};
  constexpr auto long_v = integral{64, true, "long"};
  constexpr auto unsigned_long_v = integral{64, false, "unsigned long"};
  constexpr auto long_long_v = integral{64, true, "long long"};
  constexpr auto unsigned_long_long_v = integral{64, false, "unsigned long long"};

  struct ch {
    unsigned size_bits;
    bool is_unicode;
    std::string_view sort_name;
    constexpr auto operator<=>(ch const&)  const -> std::strong_ordering = default;
  };
  constexpr auto wchar_v = ch{16, false, "wchar"};

  struct fp {
    unsigned size_bytes;
    std::string_view sort_name;
    constexpr auto operator<=>(fp const&)  const -> std::strong_ordering = default;
  };

  constexpr auto float_v = fp{32, "float"};
  constexpr auto double_v = fp{64, "double"};

  constexpr struct ptr_t {
    constexpr auto operator<=>(ptr_t const&)  const -> std::strong_ordering = default;
  } ptr_v {};
  constexpr struct array_ext_t {
    constexpr auto operator<=>(array_ext_t const&)  const -> std::strong_ordering = default;
  } array_ext_v {};
  struct array_ext_n_t {
    std::size_t n;
    constexpr auto operator<=>(array_ext_n_t const&)  const -> std::strong_ordering = default;
  };
  template <std::size_t n>
  constexpr auto array_ext_n_v = array_ext_n_t{n};
  constexpr struct ellipsis_t {
    constexpr auto operator<=>(ellipsis_t const&)  const -> std::strong_ordering = default;
  } ellipsis_v {};
  constexpr struct pack_t {
    constexpr auto operator<=>(pack_t const&)  const -> std::strong_ordering = default;
  } pack_v {};

  struct namespace_t {};
  struct class_t {};
  struct template_t {};
  struct template_parameter_t {};
}

namespace gmr {
  
}

int main() {

}
