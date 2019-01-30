# shared_string

basic_shared_string
C++ implementation of shared string class with small string optimization. 

basic_cow_string
C++ implementation of Copy-On-Write (COW) string class based on shared_string 

```c++
template <typename TChar = char, typename Traits = std::char_traits<TChar>>
class basic_shared_string
{
public:
	using traits_type = Traits;
	using value_type = TChar;
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;


	using const_pointer = const value_type *;
	using const_reference = const value_type &;
	using pointer = const_pointer;
	using reference = const_reference;

	using string_view = std::basic_string_view<value_type, traits_type>;
	
	using const_iterator = typename string_view::const_iterator;
	using iterator = const_iterator;

	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

	static constexpr size_type npos = string_view::npos;
	static const size_type sso_size = (sizeof(TLargeStr) - sizeof(TSmallSize)) / sizeof(value_type);

	constexpr basic_shared_string() noexcept;
	constexpr basic_shared_string(nullptr_t) noexcept;
	explicit constexpr basic_shared_string(const value_type *psz, size_type sz);
	explicit constexpr basic_shared_string(const string_view &src);
	template <typename T>	explicit constexpr basic_shared_string(const std::basic_string<value_type, traits_type, T> &src);
	explicit constexpr basic_shared_string(value_type ch) noexcept;
	explicit constexpr basic_shared_string(size_t n, value_type ch) noexcept;
	
	//Reserve sz characters and call func(args..., buf, sz) -> size_t 
	template <typename TFunc, typename... TT, typename = std::enable_if_t<std::is_invocable_v<TFunc, TT..., value_type  *, size_type>>>
	constexpr basic_shared_string(const size_t sz, TFunc &&func, TT&&... args);
	
	//Make string as concatination of vals...
	template <typename TMaker, typename... TT>
	constexpr basic_shared_string(const std::in_place_type_t<TMaker> &maker, TT&&... vals);
	
	constexpr basic_shared_string(const basic_shared_string &src) noexcept;
	constexpr basic_shared_string(basic_shared_string &&src) noexcept;

	constexpr string_view sv() const noexcept;
	constexpr const_pointer c_str() const noexcept;
	constexpr const_pointer data() const noexcept;
	constexpr size_t size() const noexcept;
	constexpr size_type length() const noexcept;
	constexpr const_reference front() const noexcept;
	const const_reference operator[](size_t pos) const noexcept;

	constexpr void swap(basic_shared_string &src) noexcept;
	constexpr void reset() noexcept;
	constexpr auto use_count() const noexcept;

	[[nodiscard]] constexpr bool is_small() const noexcept;
	[[nodiscard]] constexpr bool empty() const noexcept;

	constexpr basic_shared_string &operator =(const basic_shared_string &src) noexcept;
	constexpr basic_shared_string &operator =(basic_shared_string &&src) noexcept;
	constexpr basic_shared_string &operator =(nullptr_t) noexcept;
	constexpr operator string_view() const noexcept;

	//return {sv().substr(pos1, end1), sv().substr(pos2, end2)};
	constexpr std::pair<string_view, string_view> substr2(size_type pos1, size_type end1, size_type pos2, size_type end2) const;
  
	//return substr2(0, pos, pos, npos);
	constexpr std::pair<string_view, string_view> substr2(size_type pos) const;
  
#define SHARED_STRING_FUNCS(name) template <typename... TT> \
	constexpr auto name(TT&&... args) const noexcept(noexcept(sv().name(std::forward<TT>(args)...))) \
	{return sv().name(std::forward<TT>(args)...);}

	SHARED_STRING_FUNCS(at)
	SHARED_STRING_FUNCS(back)
	SHARED_STRING_FUNCS(begin)
	SHARED_STRING_FUNCS(cbegin)
	SHARED_STRING_FUNCS(end)
	SHARED_STRING_FUNCS(cend)
	SHARED_STRING_FUNCS(rbegin)
	SHARED_STRING_FUNCS(crbegin)
	SHARED_STRING_FUNCS(rend)
	SHARED_STRING_FUNCS(crend)
	SHARED_STRING_FUNCS(copy)
	SHARED_STRING_FUNCS(substr)
	SHARED_STRING_FUNCS(compare)
	SHARED_STRING_FUNCS(starts_with)
	SHARED_STRING_FUNCS(ends_with)
	SHARED_STRING_FUNCS(find)
	SHARED_STRING_FUNCS(rfind)
	SHARED_STRING_FUNCS(find_first_of)
	SHARED_STRING_FUNCS(find_last_of)
	SHARED_STRING_FUNCS(find_first_not_of)
	SHARED_STRING_FUNCS(find_last_not_of)

#undef SHARED_STRING_FUNCS

};

using shared_string = basic_shared_string<char>;
using shared_wstring = basic_shared_string<wchar_t>;

template <typename TChar> struct repeat_char : std::pair<std::size_t, TChar> {};

//make shared string as concatination of vals...
template <typename... TT> shared_string make_shared_string(TT&&... vals);
template <typename... TT> shared_wstring make_shared_wstring(TT&&... vals);


```
