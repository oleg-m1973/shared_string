/******
 * basic_cow_string
 *
 * Copyright (c) 2019 Oleg Melnikov https://github.com/oleg-m1973/shared_string
 *
 * Copy-On-Write (COW) string implementation
 *
 * License: MIT License
******/

#pragma once
#include "shared_string.h"

#ifdef SHARED_STRING_NAMESPACE
namespace SHARED_STRING_NAMESPACE {
#endif 

template <typename TChar = char, typename Traits = std::char_traits<TChar>, typename TMaker = CStringMaker<TChar, Traits>>
class basic_cow_string
: public basic_shared_string<TChar, Traits>
{
public:
	using string_maker = TMaker;

	using shared_string = basic_shared_string<TChar, Traits>;
	using shared_string::size_type;
	using shared_string::value_type;
	using shared_string::string_view;
	using shared_string::npos;

	using shared_string::basic_shared_string;
	using shared_string::is_small;
	using shared_string::substr2;
	
	basic_cow_string(const shared_string &src)
	: shared_string(src)
	{
	}

	basic_cow_string(shared_string &&src)
	: shared_string(std::move(src))
	{
	}

	basic_cow_string(const basic_cow_string &) = default;
	basic_cow_string &operator =(const basic_cow_string &) = default;

	basic_cow_string(basic_cow_string &&) = default;
	basic_cow_string &operator =(basic_cow_string &&) = default;

	constexpr basic_cow_string &append(const size_type n, const value_type ch)
	{
		shared_string(_maker, *this, clone_char(n, ch)).swap(*this);
		return *this;
	}

	constexpr basic_cow_string &append(const value_type *psz, const size_type sz)
	{
		shared_string(_maker, *this, string_vew(psz, sz)).swap(*this);
		return *this;
	}

	template <typename T, typename... TT>
	constexpr basic_cow_string &append(const T &arg, const TT&... args)
	{
		shared_string(_maker, *this, arg, args...).swap(*this);
		return *this;
	}

	template <typename T>
	constexpr basic_cow_string &insert(size_type pos, const T &val)
	{
		const auto s2 = substr2(pos);
		shared_string(_maker, s2.first, val, s2.second).swap(*this);
		return *this;
	}

	constexpr basic_cow_string &insert(size_type pos, size_type n, value_type ch)
	{
		const auto s2 = substr2(pos);
		shared_string(_maker, s2.first, clone_char(n, ch), s2.second).swap(*this);
		return *this;
	}

	constexpr basic_cow_string &erase(size_type pos = 0, size_type n = npos)
	{
		const auto s2 = substr2(0, pos, pos + n, npos);
		shared_string(_maker, s2.first, s2.second).swap(*this);
		return *this;
	}

	template<typename TFunc, typename... TT>
	constexpr basic_cow_string &modify(TFunc&& func, TT&&... args) noexcept(noexcept(std::invoke(std::forward<TFunc>(func), std::forward<TT>(args)..., this->data(), this->size())))
	{
		if (is_small())
			std::invoke(std::forward<TFunc>(func), std::forward<TT>(args)..., this->small_data(), this->small_size());
		else
			std::invoke(std::forward<TFunc>(func), std::forward<TT>(args)..., this->large_data(), this->large_size());
		return *this;
	}

	constexpr void clear() noexcept
	{
		this->reset();
	}

protected:
	static constexpr inline std::in_place_type_t<string_maker> _maker{};
};

using cow_string = basic_cow_string<char>;
using cow_wstring = basic_cow_string<wchar_t>;

#ifdef SHARED_STRING_NAMESPACE
} //namespace SHARED_STRING_NAMESPACE
#define NS(name) SHARED_STRING_NAMESPACE::name
#else 
#define NS(name) name
#endif 

namespace std
{
template <typename TChar, typename Traits>
struct hash<NS(basic_cow_string)<TChar, Traits>> : hash<NS(basic_shared_string)<TChar, Traits>> {};
}

#undef NS