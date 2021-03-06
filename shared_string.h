/******
 * basic_shared_string
 *
 * Copyright (c) 2019 Oleg Melnikov https://github.com/oleg-m1973/shared_string
 *
 * License: MIT License
 *
 *!!! Compiler option /std:c++latest for Visual C++ or -std=c++17 for GCC should be used
 *****/

#pragma once
#include "make_string.h"

#include <atomic>
#include <memory>
#include <string_view>
#include <functional>

#ifdef SHARED_STRING_NAMESPACE
namespace SHARED_STRING_NAMESPACE {
#endif 

#ifdef SHARED_STRING_TEST
static std::atomic<size_t> _SharedStringLeaks = 0;
#endif

template <typename TLargeStr>
struct CSmallStringOpt
{
	using value_type = typename TLargeStr::value_type;
	using size_type = typename TLargeStr::size_type;
	
	using TSmallSize = std::make_unsigned_t<value_type>;
	using TSmallMask = 
		std::conditional_t<sizeof(value_type) == 1, uint16_t,
		std::conditional_t<sizeof(value_type) == 2, uint32_t,
		std::conditional_t<sizeof(value_type) == 4, uint64_t,
		void>>>;

	static constexpr size_type sso_size = ((sizeof(TLargeStr) - sizeof(TSmallSize)) / sizeof(value_type)) - 1;
	static constexpr TSmallSize ZeroSmallSize = 0x01;
	static constexpr TSmallMask SmallMask = 0x01;
		
	CSmallStringOpt() = default;
	
	constexpr CSmallStringOpt(nullptr_t) noexcept
	: m_mask(SmallMask)
	{
	}

	constexpr CSmallStringOpt(const TLargeStr &large) noexcept
	: m_large(large)
	{
	}
	
	constexpr value_type *Init(const size_type sz)
	{
		return Init(sz, sz);
	}

	constexpr value_type *Init(const size_type sz, const size_t cap)
	{
		if (cap == 0)
		{
			SetEmpty();
			return m_small.m_str;
		}
		else if (ShouldBeSmall(cap))
		{
			m_small.resize(sz);
			m_small.m_str[sz] = 0;
			return m_small.m_str;
		}
		else
		{
			SetEmpty();
			auto p = m_large.Init(sz, cap);
			if (is_small())
			{
				m_large.Destroy();
				SetEmpty();
				throw std::bad_alloc();
			}

			p[sz] = 0;
#ifdef SHARED_STRING_TEST
			++_SharedStringLeaks;
#endif 
			return p;	
		}
	}

	[[nodiscard]] constexpr bool is_small() const noexcept
	{
		return m_mask & SmallMask;
	}

	constexpr void SetEmpty() noexcept
	{
		m_mask = SmallMask;
	}

	static constexpr bool ShouldBeSmall(const size_type sz) noexcept
	{
		return sz <= sso_size;
	}

	[[nodiscard]] constexpr bool empty() const noexcept
	{
		return m_mask == SmallMask;
	}

	struct CSmallStr
	{
		CSmallStr() = default;
		
		constexpr size_type size() const noexcept
		{
			return m_sz >> 1;
		}

		constexpr void resize(size_type sz) noexcept
		{
			m_sz = (TSmallSize(sz) << 1) | ZeroSmallSize;
		}

		TSmallSize m_sz;
		value_type m_str[sso_size + 1];
	};
	
	union
	{
		CSmallStr m_small;
		TLargeStr m_large;
		TSmallMask m_mask;
	};

	static_assert(std::is_trivially_constructible_v<TLargeStr>);
	static_assert(std::is_trivially_destructible_v<TLargeStr>);
	static_assert(std::is_trivially_copyable_v<TLargeStr>);

	static_assert(sizeof(m_small) <= sizeof(m_large));
	static_assert(sizeof(m_mask) <= sizeof(m_large));
	static_assert(sizeof(m_mask) == sizeof(value_type) * 2);
};

template <typename TChar>
struct CSharedLargeStr
{
	using value_type = TChar;
	using size_type = std::size_t;

	struct CRefStr
	{
		bool Capture() noexcept
		{
			auto refs = m_refs.load(std::memory_order_acquire);
			while (refs != 0)
				if (m_refs.compare_exchange_weak(refs, refs + 1, std::memory_order_acquire))
					return true;

			return false;
		}

		[[nodiscard]] bool Release() const noexcept
		{
			return m_refs.fetch_sub(1, std::memory_order_release) == 1;
		}

		auto use_count() const noexcept
		{
			return m_refs.load(std::memory_order_relaxed);
		}

		mutable volatile std::atomic<size_t> m_refs{1};
		value_type m_data[1];
	};
	
	value_type *Init(const size_type sz, const size_t cap)
	{
		m_refs = new (new std::byte[sizeof(CRefStr) + (cap * sizeof(value_type))]) CRefStr; //-V119
		m_sz = sz;
		return m_str = m_refs->m_data;
	}
	
	bool Capture() const noexcept
	{
		return m_refs->Capture();
	}

	void Destroy() noexcept
	{
		std::destroy_at(m_refs);
		delete[] reinterpret_cast<std::byte *>(m_refs);
	}

	void Release() noexcept
	{
		if (m_refs->Release())
		{
			Destroy();
#ifdef SHARED_STRING_TEST
			--_SharedStringLeaks;
#endif 
		}
	}

	auto *data() noexcept
	{
		return m_str;
	}

	auto *data() const noexcept
	{
		return m_str;
	}

	constexpr size_type size() const noexcept
	{
		return m_sz;
	}

	auto use_count() const noexcept
	{
		return m_refs->use_count();
	}

	CRefStr *m_refs;
	value_type *m_str;
	size_type m_sz;
};

template <typename TChar, typename Traits> class shared_string_creator;
#define SHARED_STRING_CMP \
	TS_ITEM(==, ==) \
	TS_ITEM(!=, !=) \
	TS_ITEM(<=, >=) \
	TS_ITEM(>=, <=) \
	TS_ITEM(<, >) \
	TS_ITEM(>, <) \

template <typename TChar = char, typename Traits = std::char_traits<TChar>, typename TLargeStr = CSharedLargeStr<TChar>>
class basic_shared_string
: protected CSmallStringOpt<TLargeStr>
{
protected:
	using TSmallStringOpt = CSmallStringOpt<TLargeStr>;
		
	using TSmallStringOpt::m_small;
	using TSmallStringOpt::m_large;
	
	using TSmallStringOpt::SetEmpty;
	using TSmallStringOpt::ShouldBeSmall;

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

	using creator = shared_string_creator<value_type, traits_type>;
	
	using TSmallStringOpt::sso_size;
	using TSmallStringOpt::empty;
	using TSmallStringOpt::is_small;
	

	constexpr basic_shared_string() noexcept
	: TSmallStringOpt(nullptr)
	{
	}

	constexpr basic_shared_string(nullptr_t) noexcept
	: basic_shared_string()
	{
	}

	explicit constexpr basic_shared_string(const value_type *psz, size_type sz)
	{
		auto p = this->Init(sz);
		traits_type::copy(p, psz, sz);
	}

	explicit constexpr basic_shared_string(const string_view &src)
	: basic_shared_string(src.data(), src.size())
	{
	}

	template <typename Allocator>
	explicit constexpr basic_shared_string(const std::basic_string<value_type, traits_type, Allocator> &src)
	: basic_shared_string(src.data(), src.size())
	{
	}

	explicit constexpr basic_shared_string(value_type ch) noexcept
	{
		m_small.resize(1);
		traits_type::assign(m_small.m_str[0], ch);
		traits_type::assign(m_small.m_str[1], 0);
	}

	explicit constexpr basic_shared_string(size_t n, value_type ch) 
	{
		auto p = this->Init(n);
		Traits::assign(p, n, ch);
	}

	template <typename TFunc, typename... TT, typename = std::enable_if_t<std::is_invocable_v<TFunc, TT..., creator &>>>
	constexpr basic_shared_string(TFunc &&func, TT&&... args) 
	{
		creator s;
		std::invoke(std::forward<TFunc>(func), std::forward<TT>(args)..., s);
		m_large = s.m_large;
		s.SetEmpty();
	}

	template <typename TFunc, typename... TT, typename = std::enable_if_t<std::is_invocable_v<TFunc, TT..., value_type  *, size_type>>>
	constexpr basic_shared_string(const size_t sz, TFunc &&func, TT&&... args) 
	{
		value_type *p = this->Init(sz);
		const size_type sz2 = std::invoke(std::forward<TFunc>(func), std::forward<TT>(args)..., p, sz);
		if (sz2 > sz)
			throw std::out_of_range(__FUNCTION__);

		if (sz2 < sz)
		{
			if (is_small())
				m_small.resize(sz2);
			else
				m_large.m_sz = sz2;

			traits_type::assign(p[sz2], 0);
		}
	}
	
	template <typename TMaker, typename... TT>
	constexpr basic_shared_string(const std::in_place_type_t<TMaker> &maker, TT&&... vals)
	{
		CAppendHelper(*this, maker, TMaker::ToStr(std::forward<TT>(vals))...);
	}

	constexpr basic_shared_string(const basic_shared_string &src) noexcept
	{
		if (src.is_small())
			m_small = src.m_small;
		else if (src.m_large.Capture())
			m_large = src.m_large;
		else
			SetEmpty();
	}

	constexpr basic_shared_string(basic_shared_string &&src) noexcept
	: TSmallStringOpt(src.m_large)
	{
		src.SetEmpty();
	}

	~basic_shared_string()
	{
		_reset();
	}

	constexpr string_view sv() const noexcept
	{
		return is_small()? string_view(m_small.m_str, m_small.size()): string_view(m_large.data(), m_large.size());
	}

	constexpr const_pointer c_str() const noexcept
	{
		return data();
	}
	
	constexpr const_pointer data() const noexcept
	{
		return is_small()? m_small.m_str: m_large.data();
	}

	constexpr size_t size() const noexcept
	{
		return is_small()? m_small.size(): m_large.size();
	}
		
	constexpr size_type length() const noexcept
	{
		return size();
	}

	constexpr const_reference front() const noexcept 
	{
		return *data();
	}

	constexpr const_reference operator[](size_t pos) const noexcept
	{
		return data()[pos];
	}

	constexpr basic_shared_string &operator =(const basic_shared_string &src) noexcept
	{
		if (this != &src)
			basic_shared_string(src).swap(*this);

		return *this;
	}

	constexpr basic_shared_string &operator =(basic_shared_string &&src) noexcept
	{
		src._swap(*this);
		return *this;
	}

	constexpr basic_shared_string &operator =(nullptr_t) noexcept
	{
		_reset();
		return *this;
	}

	constexpr operator string_view() const noexcept
	{
		return sv();
	}

	constexpr void swap(basic_shared_string &src) & noexcept
	{
		if (this != &src)
			_swap(src);
	}

	constexpr void swap(basic_shared_string &src) && noexcept
	{
		_swap(src);
	}

	constexpr void reset() noexcept
	{
		_reset();
		SetEmpty();
	}

	constexpr auto use_count() const noexcept
	{
		return is_small()? 0: m_large.use_count();
	}

#define SHARED_STRING_FUNCS(name) template <typename... TT> \
	constexpr auto name(TT&&... args) const noexcept(noexcept(string_view().name(std::forward<TT>(args)...))) \
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

	constexpr std::pair<string_view, string_view> substr2(size_type pos1, size_type end1, size_type pos2, size_type end2) const
	{
		const auto s = this->sv();
		return {s.substr(pos1, end1), s.substr(pos2, end2)};
	}

	constexpr std::pair<string_view, string_view> substr2(size_type pos) const
	{
		return substr2(0, pos, pos, npos);
	}

#define TS_ITEM(op, nop) constexpr bool operator op(const basic_shared_string &s) const noexcept {return compare(s) op 0;}
	SHARED_STRING_CMP
#undef TS_ITEM

protected:
	struct CAppendHelper
	{
		template <typename TMaker, typename... TT>
		constexpr CAppendHelper(basic_shared_string &s, const std::in_place_type_t<TMaker> &maker, TT&&... vals)
		: m_p(s.Init((TMaker::size(vals) + ... + 0)))
		{
			(TMaker::Append(*this, std::forward<TT>(vals)), ...);
		}

		template <typename T, typename = std::void_t<decltype(std::declval<T>().data()), decltype(std::declval<T>().size())>>
		constexpr void append(const T &val) noexcept
		{
			const auto sz = val.size();
			traits_type::copy(m_p, val.data(), sz);
			m_p += sz;
		}

		constexpr void append(const value_type ch) noexcept
		{
			traits_type::assign(*(m_p++), ch);
		}

		constexpr void append(size_t n, const value_type ch) noexcept
		{
			traits_type::assign(m_p, n, ch);
			m_p += n;
		}

		value_type *m_p;
	};
	
	constexpr void _reset() noexcept
	{
		if (!is_small())
			m_large.Release();
	}

	constexpr void _swap(basic_shared_string &src) noexcept
	{
		std::swap(m_large, src.m_large);
	}

	constexpr value_type *data() noexcept
	{
		return is_small()? m_small.m_str: m_large.data();
	}

	constexpr value_type *small_data() noexcept
	{
		return m_small.m_str;
	}

	constexpr size_type small_size() const noexcept
	{
		return m_small.size();
	}

	constexpr value_type *large_data() noexcept
	{
		return m_large.data();
	}

	constexpr size_type large_size() const noexcept
	{
		return m_large.size();
	}
};

template <typename TChar, typename Traits>
class shared_string_creator
: public basic_shared_string<TChar, Traits>
{
friend class basic_shared_string<TChar, Traits>;
public:
	using shared_string = basic_shared_string<TChar, Traits>;
	using typename shared_string::value_type;
	using typename shared_string::traits_type;
	using typename shared_string::size_type;

	using typename shared_string::string_view;

	using shared_string::is_small;
	using shared_string::size;
	using shared_string::sso_size;

	using shared_string::sv;

	void reserve(const size_type cap)
	{
		if (cap > m_cap)
			shared_string_creator(cap, sv())._swap(*this);
	}

	constexpr auto &append(const value_type * const str, const size_type sz)
	{
		if (sz != 0)
			traits_type::copy(_append(sz), str, sz);

		return *this;
	}

	constexpr auto &append(const string_view &sv)
	{
		return append(sv.data(), sv.size());
	}

	constexpr auto &append(value_type ch)
	{
		auto p = _append(1);
		traits_type::assign(p[0], ch);
		return *this;
	}

	constexpr auto &append(size_type n, value_type ch)
	{
		traits_type::assign(_append(n), n, ch);
		return *this;
	}

	template <typename T>
	constexpr auto &operator +=(T &&val)
	{
		return append(std::forward<T>(val));
	}

protected:
	using shared_string::swap;

	using shared_string::small_data;
	using shared_string::small_size;

	using shared_string::large_data;
	using shared_string::large_size;

	using shared_string::m_small;
	using shared_string::m_large;
	
	shared_string_creator() = default;
	shared_string_creator(const shared_string_creator &) = delete;
	shared_string_creator &operator =(const shared_string_creator &) = delete;

	explicit constexpr shared_string_creator(size_t cap, const string_view &src)
	: m_cap(cap)
	{
		const auto sz = src.size();
		auto p = this->Init(sz, cap);
		traits_type::copy(p, src.data(), sz);
	}

	value_type *_append(const size_type sz)
	{
		const auto cap = size() + sz;
		if (cap > m_cap)
			shared_string_creator(cap, sv())._swap(*this);
		else if (is_small())
		{
			auto p = small_data() + small_size();
			traits_type::assign(p[sz], 0);
			m_small.resize(cap);
			return p;
		}

		auto p = large_data() + large_size();
		traits_type::assign(p[sz], 0);
		m_large.m_sz = cap;
		return p;
	}

	size_t m_cap = sso_size;
};


#define TS_ITEM(op, nop) \
	template <typename TChar, typename Traits, typename TLargeStr, typename T> constexpr bool operator op(const basic_shared_string<TChar, Traits, TLargeStr> &s1, const T &s2) noexcept {return s1.compare(s2) op 0;} \
	template <typename T, typename TChar, typename Traits, typename TLargeStr, typename = std::enable_if_t<!std::is_base_of_v<basic_shared_string<TChar, Traits, TLargeStr>, T> && !std::is_same_v<basic_shared_string<TChar, Traits, TLargeStr>, T>>> \
	constexpr bool operator op(const T &s1, const basic_shared_string<TChar, Traits, TLargeStr> &s2) noexcept {return s2.compare(s1) nop 0;} \


SHARED_STRING_CMP

#undef TS_ITEM

template <typename TChar, typename Traits> std::basic_ostream<TChar, Traits> &operator <<(std::basic_ostream<TChar, Traits>& stm, basic_shared_string <TChar, Traits> s)
{
	return stm << s.sv();
}

using shared_string = basic_shared_string<char>;
using shared_wstring = basic_shared_string<wchar_t>;

template <typename TMaker = CStringMaker<typename shared_string::value_type, typename shared_string::traits_type>, typename... TT>
shared_string make_shared_string(TT&&... vals)
{
	return shared_string(std::in_place_type<TMaker>, std::forward<TT>(vals)...);
}

template <typename TMaker = CStringMaker<typename shared_wstring::value_type, typename shared_wstring::traits_type>, typename... TT>
shared_wstring make_shared_wstring(TT&&... vals)
{
	return shared_wstring(std::in_place_type<TMaker>, std::forward<TT>(vals)...);
}


#ifdef SHARED_STRING_NAMESPACE
} //namespace SHARED_STRING_NAMESPACE
#define NS(name) SHARED_STRING_NAMESPACE::name
#else 
#define NS(name) name
#endif 

namespace std
{
template <typename TChar, typename Traits>
struct hash<NS(basic_shared_string)<TChar, Traits>> : hash<typename NS(basic_shared_string)<TChar, Traits>::string_view> {};
}
#undef NS

