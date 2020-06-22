#pragma once
/******
 * make_string
 *
 * Copyright (c) 2019 Oleg Melnikov https://github.com/oleg-m1973/shared_string
 *
 * License: MIT License
 *
 *!!! Compiler option /std:c++latest for Visual C++ or -std=c++17 for GCC should be used
 *****/

#include <string>
#include <tuple>

template <typename TChar>
struct clone_char
: std::pair<std::size_t, TChar>
{
	using std::pair<std::size_t, TChar>::pair;

	constexpr auto size() const noexcept
	{
		return this->first;
	}
};

template <typename TChar> clone_char(size_t, TChar)->clone_char<TChar>;

template <template <typename...> typename Pred>
struct type_traits_t
{
	template <typename... TT>
	static constexpr inline auto value = Pred<TT...>::value;

	template <typename... TT>
	constexpr auto operator()(TT&&... args) noexcept
	{
		return Pred<std::decay_t<TT>...>()(std::forward<TT>(args)...);
	}
};

template <typename Tuple, typename T, typename Pred = type_traits_t<std::is_same>, size_t I = 0>
constexpr size_t _tuple_index() noexcept
{
	if constexpr(I >= std::tuple_size_v<Tuple>)
		return I;
	else if constexpr(Pred::template value<T, std::tuple_element_t<I, Tuple>>)
		return I;
	else
		return _tuple_index<Tuple, T, Pred, I + 1>();
}

template <typename Tuple, typename T, typename Pred = type_traits_t<std::is_same>, typename NotFound = void>
using tuple_find_t = std::tuple_element_t<_tuple_index<Tuple, T, Pred>(), decltype(std::tuple_cat(std::declval<Tuple>(), std::declval<std::tuple<NotFound>>()))>;

template <typename Tuple, typename T, typename Pred = type_traits_t<std::is_same>>
static constexpr bool tuple_has = _tuple_index<Tuple, T, Pred>() < std::tuple_size_v<Tuple>;

template <typename Tuple, size_t I = 0>
constexpr bool tuple_unique_types() noexcept
{
	constexpr auto sz = std::tuple_size_v<Tuple>;
	if constexpr(I < sz)
		return true;
	else
		return (_tuple_index<Tuple, std::tuple_element_t<I, Tuple>, type_traits_t<std::is_same>, I + 1>() >= sz) && tuple_unique_types<Tuple, I + 1>();
}

template <typename TChar>
struct to_string_t
: public std::basic_string<TChar>
{
	to_string_t() = delete;

	template <typename T, typename = decltype(std::to_string(std::declval<T>()))>
	to_string_t(T &&val)
	: std::basic_string<TChar>(to_string(std::forward<T>(val)))
	{
	}

	template <typename T>
	static constexpr auto to_string(T &&val)
	{
		if constexpr(std::is_same_v<TChar, char>)
			return std::to_string(std::forward<T>(val));
		else if constexpr(std::is_same_v<TChar, wchar_t>)
			return std::to_wstring(std::forward<T>(val));
		else
			return std::forward<T>(val);
	}
};

template <typename TChar, typename Traits = std::char_traits<TChar>>
using TStringMakerTypes = std::tuple
<
	std::basic_string_view<TChar, Traits> &,
	std::basic_string<TChar, Traits> &,
	TChar &,
	clone_char<TChar> &,
	std::basic_string_view<TChar, Traits>,
	to_string_t<TChar>
>;

template <typename TChar, typename Traits = std::char_traits<TChar>, typename Types = TStringMakerTypes<TChar, Traits>>
struct CStringMaker
{
	using traits_type = Traits;
	using value_type = TChar;
	using size_type = std::size_t;

	using string_view = std::basic_string_view<value_type, traits_type>;

	using TypeMap = Types;
	static_assert(tuple_unique_types<TypeMap>());

	template <typename T, typename T2>
	using map_type_t = std::conditional_t<std::is_reference_v<T2>, std::is_same<std::remove_reference_t<T2>, std::decay_t<T>>, std::is_convertible<T, T2>>;

	template <bool Assert = true, typename T, typename T2 = tuple_find_t<TypeMap, T, type_traits_t<map_type_t>>>
	static constexpr decltype(auto) ToStr(T &&val) noexcept(std::is_reference_v<T2> || noexcept(std::decay_t<T2>(std::forward<T>(val))))
	{
		static_assert(!Assert || !std::is_void_v<T2>, "Can't make string from type");
		if constexpr(std::is_reference_v<T2>)
			return std::forward<T>(val);
		else
			return T2(std::forward<T>(val));
	}

	template <typename T>
	static constexpr auto size(const T& val) noexcept(noexcept(val.size())) -> decltype(std::declval<T>().size())
	{
		return val.size();
	}

	static constexpr size_type size(const value_type) noexcept
	{
		return 1;
	}

	template <typename TString, typename T, typename = std::enable_if_t<std::is_convertible_v<T, string_view>>>
	static constexpr void Append(TString &s, T &&val) noexcept(noexcept(s.append(std::forward<T>(val))))
	{
		s.append(std::forward<T>(val));
	}

	template <typename TString>
	static constexpr void Append(TString &s, const clone_char<value_type> &val) noexcept(noexcept(s.append(val.first, val.second)))
	{
		s.append(val.first, val.second);
	}

	template <typename TString, typename T, typename = std::enable_if_t<std::is_same_v<T, value_type>>>
	static constexpr void Append(TString &s, const T &val) noexcept(noexcept(s.append(1, val)))
	{
		s.append(1, val);
	}

	template <typename TString, typename T>
	static constexpr auto Append(TString &s, T &&val) noexcept(noexcept(val.append_to(s))) -> decltype(std::declval<T>().append_to(std::declval<TString &>()))
	{
		return val.append_to(s);
	}
};

template <typename TString, typename TMaker = CStringMaker<typename TString::value_type, typename TString::traits_type>, typename... TT>
auto _make_string(TT&&... vals)
{
	TString res;
	res.reserve((TMaker::size(vals) + ... + 0));
	(TMaker::Append(res, std::forward<TT>(vals)), ...);
	return res;
}

template <typename TMaker = CStringMaker<typename std::string::value_type, typename std::string::traits_type>, typename... TT>
std::string make_string(TT&&... vals)
{
	return _make_string<std::string, TMaker>(TMaker::ToStr(std::forward<TT>(vals))...);
}

template <typename TMaker = CStringMaker<typename std::wstring::value_type, typename std::wstring::traits_type>, typename... TT>
std::wstring make_wstring(TT&&... vals)
{
	return _make_string<std::string, TMaker>(TMaker::ToStr(std::forward<TT>(vals))...);
}

