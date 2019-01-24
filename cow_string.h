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
template <typename TChar = char, typename Traits = std::char_traits<TChar>>
class basic_cow_string
: protected basic_shared_string<TChar, Traits>
{
public:
	using shared_string = basic_shared_string<TChar, Traits>;
	using shared_string::basic_shared_string;

	constexpr basic_cow_string &append(const size_type n, const value_type ch)
	{
		make_shared_string<shared_string>(*this, repeat_char(n, ch))._swap(*this);
		return *this;
	}

	constexpr basic_cow_string &append(const value_type *psz, const size_type sz)
	{
		make_shared_string<shared_string>(*this, string_vew(psz, sz))._swap(*this);
		return *this;
	}

	template <typename T, typename... TT>
	constexpr basic_cow_string &append(const T &arg, const TT&... args)
	{
		make_shared_string<shared_string>(*this, arg, args...)._swap(*this);
		return *this;
	}

	constexpr void clear() noexcept
	{
		reset();
	}

};

using cow_string = basic_cow_string<char>;
using cow_wstring = basic_cow_string<wchar_t>;

#ifdef SHARED_STRING_NAMESPACE
} //namespace SHARED_STRING_NAMESPACE
#endif 

namespace std
{
#ifdef SHARED_STRING_NAMESPACE
#define NS SHARED_STRING_NAMESPACE::
#else 
#define NS
#endif

template <typename TChar, typename Traits>
struct hash<NS::basic_cow_string<TChar, Traits>> : hash<NS::basic_shared_string<TChar, Traits>> {};
}
