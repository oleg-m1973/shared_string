// This is an independent project of an individual developer. Dear PVS-Studio, please check it.

// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com

/******
 * basic_shared_string
 *
 * Copyright (c) 2019 Oleg Melnikov https://github.com/oleg-m1973/shared_string
 *
 * License: MIT License
 *
 *****/

#pragma once

#include <atomic>
#include <tuple>
#include <memory>
#include <string>
#include <string_view>
#include <functional>

#ifdef SHARED_STRING_NAMESPACE
namespace SHARED_STRING_NAMESPACE {
#endif 

#ifdef SHARED_STRING_TEST
static size_t _SharedStringLeaks = 0;
#endif

template <typename TLargeStr>
struct CSmallStringOpt
{
	using value_type = typename TLargeStr::value_type;
	using size_type = typename TLargeStr::size_type;
	
	using TSmallSize = std::make_unsigned_t<value_type>;
	using TSmallMask = uint64_t;

	static const size_type sso_size = (sizeof(TLargeStr) - sizeof(TSmallSize)) / sizeof(value_type);
	static const TSmallSize ZeroSmallSize = 0x01;
	static const TSmallMask SmallMask = 0x01;

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
		if (sz == 0)
		{
			SetEmpty();
			return m_small.m_str;
		}
		else if (ShouldBeSmall(sz))
		{
			m_small.resize(sz);
			return m_small.m_str;
		}
		else
		{
			SetEmpty();
			auto p = m_large.Init(sz);
			if (is_small())
			{
				m_large.Destroy();
				SetEmpty();
				throw std::bad_alloc();
			}

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

	static constexpr bool ShouldBeSmall(size_type sz) noexcept
	{
		return sz < sso_size;
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
		value_type m_str[sso_size];
	};
	
	union
	{
		CSmallStr m_small;
		TLargeStr m_large;
		TSmallMask m_mask;
	};

	static_assert(sizeof(m_small) <= sizeof(m_large));
	static_assert(sizeof(m_mask) <= sizeof(m_large));
	static_assert(sizeof(m_mask) >= sizeof(TSmallSize) + sizeof(value_type));
};

class CRefCounter
{
public:
	bool Capture() noexcept
	{
		auto cnt = m_cnt.load(std::memory_order_acquire);
		while (cnt != 0)
			if (m_cnt.compare_exchange_weak(cnt, cnt + 1, std::memory_order_acquire))
				return true;

		return false;
	}

	[[nodiscard]] bool Release() noexcept
	{
		return m_cnt.fetch_sub(1, std::memory_order_release) == 1; 
	}

	auto use_count() const noexcept
	{
		return m_cnt.load(std::memory_order_relaxed);
	}

protected:
	volatile std::atomic<uintmax_t> m_cnt{1};
};

template <typename TChar>
struct CSharedLargeStr
{
	using value_type = TChar;
	using size_type = std::size_t;

	struct CRefStr
	: public CRefCounter
	{
		value_type m_data[1];
	};
	
	value_type *Init(size_type sz)
	{
		m_refs = new (new std::byte[sizeof(CRefStr) + (sz * sizeof(value_type))]) CRefStr();
		m_sz = sz;
		return m_str = m_refs->m_data;
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
	constexpr size_type size() const noexcept
	{
		return m_sz;
	}

	auto use_count() const noexcept
	{
		return m_refs->use_count();
	}

	CRefStr *m_refs;
	value_type *m_str;size_type m_sz;
};


template <typename TChar = char, typename Traits = std::char_traits<TChar>>
class basic_shared_string
: protected CSmallStringOpt<CSharedLargeStr<TChar>>
{
protected:
	using TSmallStringOpt = CSmallStringOpt<CSharedLargeStr<TChar>>;
		
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
		traits_type::assign(p[sz], 0);
	}

	explicit constexpr basic_shared_string(const string_view &src)
	: basic_shared_string(src.data(), src.size())
	{
	}

	template <typename T>
	explicit constexpr basic_shared_string(const std::basic_string<value_type, traits_type, T> &src)
	: basic_shared_string(src.data(), src.size())
	{
	}

	explicit constexpr basic_shared_string(value_type ch) noexcept
	{
		m_small.resize(1);
		traits_type::assign(m_small.m_str[0], ch);
		traits_type::assign(m_small.m_str[1], 0);
	}

	explicit constexpr basic_shared_string(size_t n, value_type ch) noexcept
	{
		auto p = this->Init(n);
		Traits::assign(p, n, ch);
		Traits::assign(p[n], 0);
	}

	template <typename TFunc, typename... TT, typename = std::enable_if_t<std::is_invocable_v<TFunc, TT..., value_type  *, size_type>>>
	constexpr basic_shared_string(const size_t sz, TFunc &&func, TT&&... args) 
	{
		value_type *p = this->Init(sz);
		const size_type sz2 = std::invoke(std::forward<TFunc>(func), std::forward<TT>(args)..., p, sz);
		if (sz2 > sz)
			throw std::out_of_range(__FUNCTION__);

		traits_type::assign(p[sz], 0);

		if (sz2 < sz)
		{
			if (is_small())
				m_small.resize(sz2);
			else
				m_large.m_sz = sz2;
		}
	}
	
	template <typename TMaker, typename... TT>
	constexpr basic_shared_string(const std::in_place_type_t<TMaker> &maker, TT&&... vals)
	{
		TMaker::VerifyType(vals...);
		CAppendHelper(*this, maker, typename TMaker::template T<TT>(std::forward<TT>(vals))...);
	}

	constexpr basic_shared_string(const basic_shared_string &src) noexcept
	{
		if (src.is_small())
			m_small = src.m_small;
		else if (src.m_large.m_refs->Capture())
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
		return is_small()? string_view(m_small.m_str, m_small.size()): string_view(m_large.m_str, m_large.size());
	}

	constexpr const_pointer c_str() const noexcept
	{
		return data();
	}
	
	constexpr const_pointer data() const noexcept
	{
		return is_small()? m_small.m_str: m_large.m_str;
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

	const const_reference operator[](size_t pos) const noexcept
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

	constexpr std::pair<string_view, string_view> substr2(size_type pos1, size_type end1, size_type pos2, size_type end2) const
	{
		const auto s = this->sv();
		return {s.substr(pos1, end1), s.substr(pos2, end2)};
	}

	constexpr std::pair<string_view, string_view> substr2(size_type pos) const
	{
		return substr2(0, pos, pos, npos);
	}
protected:
	struct CAppendHelper
	{
		template <typename TMaker, typename... TT>
		constexpr CAppendHelper(basic_shared_string &s, const std::in_place_type_t<TMaker> &maker, TT&&... vals)
		: m_p(s.Init((TMaker::size(vals) + ...)))
		{
			(TMaker::Append(*this, std::forward<TT>(vals)), ...);
			Traits::assign(*m_p, 0);
		}

		template <typename Allocator>
		constexpr void append(TChar *dst, const std::basic_string<TChar, Traits, Allocator> &val) noexcept
		{
			const auto sz = val.size();
			Traits::copy(m_p, val.data(), sz);
			m_p += sz;
		}

		constexpr void append(const string_view &val) noexcept
		{
			const auto sz = val.size();
			Traits::copy(m_p, val.data(), sz);
			m_p += sz;
		}

		constexpr void append(const TChar ch) noexcept
		{
			Traits::assign(*(m_p++), ch);
		}

		constexpr void append(size_t n, const TChar ch) noexcept
		{
			Traits::assign(m_p, n, ch);
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
		return is_small()? m_small.m_str: m_large.m_str;
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
		return m_large.m_str;
	}

	constexpr size_type large_size() const noexcept
	{
		return m_large.size();
	}

};

#define SHARED_STRING_CMP \
	TS_ITEM(==, ==) \
	TS_ITEM(!=, !=) \
	TS_ITEM(<=, >=) \
	TS_ITEM(>=, <=) \
	TS_ITEM(<, >) \
	TS_ITEM(>, <) \

#define TS_ITEM(op, nop) \
	template <typename TChar, typename Traits, typename T> constexpr bool operator op(const basic_shared_string<TChar, Traits> &s1, const T &s2) noexcept {return s1.compare(s2) op 0;} \
	template <typename TChar, typename Traits, typename T, typename = std::enable_if_t<!std::is_base_of_v<basic_shared_string<TChar, Traits>, T> && !std::is_same_v<basic_shared_string<TChar, Traits>, T>>> \
	constexpr bool operator op(const T &s1, const basic_shared_string<TChar, Traits> &s2) noexcept {return s2.compare(s1) nop 0;} \
	
SHARED_STRING_CMP

#undef TS_ITEM

template <typename TChar, typename Traits> std::basic_ostream<TChar, Traits> &operator<<(std::basic_ostream<TChar, Traits>& stm, basic_shared_string <TChar, Traits> s)
{
	return stm << s.sv();
}

using shared_string = basic_shared_string<char>;
using shared_wstring = basic_shared_string<wchar_t>;

template <typename... TT> 
struct CTypeMap
{
	template <size_t I> using T = void;

	template <typename , typename NotFound = void, template<typename, typename> typename = std::is_same> 
	using FindFirst = NotFound; 
};

template <> struct CTypeMap<void> : CTypeMap<> {};

template <typename _T, typename... TT> 
struct CTypeMap<_T, TT...>
: public CTypeMap<TT...>
{
	template <size_t I> using T = std::tuple_element_t<I, std::tuple<_T, TT...>>;

	template 
	<
		typename T2, 
		typename NotFound = void, 
		template<typename, typename> typename Pred = std::is_same
	> 
	using Find = std::conditional_t<Pred<T2, _T>::value, _T, typename CTypeMap<TT...>::template FindFirst<T2, NotFound, Pred>>;

	template 
	<
		typename T2, 
		template<typename, typename> typename Pred = std::is_same
	> 
	static constexpr bool Has() noexcept
	{
		const bool res = Pred<T2, _T>::value;
		if constexpr(sizeof...(TT) == 0)
			return res;
		else 
			return res || CTypeMap<TT...>::template Has<T2, Pred>();
	}
};

template <typename TChar> 
struct repeat_char 
: std::pair<std::size_t, TChar> 
{
	using std::pair<std::size_t, TChar>::pair;
};

template <typename TChar> repeat_char(size_t, TChar) -> repeat_char<TChar>;

template <typename TString, typename T, typename = std::void_t<>> struct CanAppend: std::false_type {};
template <typename TString, typename T> struct CanAppend<TString, T, std::void_t<decltype(std::declval<TString>().append(std::declval<T>()))>>: std::true_type{};

template <typename TChar, typename Traits = std::char_traits<TChar>, typename = void>
struct CStringMaker
{
	using traits_type = Traits;
	using value_type = TChar;
	using size_type = std::size_t;

	using string_view = std::basic_string_view<value_type, traits_type>;

	using TypeMap = CTypeMap
	<
		const string_view &,
		const value_type &,
		const repeat_char<value_type> &,
		string_view
	>;

	template <typename TSrc, typename TDst, typename TDst2 = std::decay_t<TDst>> 
	using _find_type = std::conditional_t<std::is_reference_v<TDst>, std::is_same<TSrc, TDst2>, std::is_convertible<TSrc, TDst2>>;

	template <typename _T> using T = typename TypeMap::template Find<_T, const _T &, _find_type>;

	static constexpr size_type size(const string_view& val) noexcept(noexcept(val.size()))
	{
		return val.size();
	}

	static constexpr size_type size(const value_type) noexcept
	{
		return 1;
	}

	static constexpr size_type size(const repeat_char<value_type> &val) noexcept
	{
		return val.first;
	}

	template <bool Assert = true, typename T>
	static constexpr bool VerifyType(const T &) noexcept
	{
		const bool res = TypeMap::template Has<T, _find_type>();
		static_assert(!Assert || res, "Can't make string from type");
		return res;
	}

	template <bool Assert = true, typename... TT>
	static constexpr bool VerifyType(const TT&... vals) noexcept
	{
		return (VerifyType<Assert>(vals) && ... && true);
	}

	template <typename TString, typename T> 
	static constexpr void Append(TString &s, const T &val) noexcept(noexcept(s.append(val)))
	{
		s.append(val);
	}

	template <typename TString>
	static constexpr void Append(TString &s, const repeat_char<value_type> &val) noexcept(noexcept(s.append(val.first, val.second)))
	{
		s.append(val.first, val.second);
	}

	template <typename TString, typename = std::enable_if_t<!CanAppend<TString, value_type>::value>>
	static constexpr void Append(TString &s, value_type val) noexcept(noexcept(s.append(1, val)))
	{
		s.append(1, val);
	}
	
	template <typename TString>
	static constexpr void Append(TString &s) noexcept
	{
	}

	template <typename TString, typename T, typename... TT>
	static constexpr void Append(TString &s, const T &val, const TT&... vals) noexcept(noexcept(Append(s, val)) && noexcept(Append(s, vals...)))
	{
		Append(s, val);
		Append(s, vals...);
	}
};

template <typename TString = shared_string, typename TMaker = CStringMaker<typename TString::value_type, typename TString::traits_type>, typename... TT>
TString _make_shared_string(TT&&... vals)
{
	return TString(std::in_place_type<TMaker>, std::forward<TT>(vals)...);
}

template <typename... TT>
shared_string make_shared_string(TT&&... vals)
{
	return _make_shared_string<shared_string>(TT&&... vals)
}

template <typename... TT>
shared_wstring make_shared_wstring(TT&&... vals)
{
	return _make_shared_string<shared_wstring>(TT&&... vals)
}

template <typename TString = std::string, typename TMaker = CStringMaker<typename TString::value_type, typename TString::traits_type>, typename... TT>
TString make_string(TT&&... vals)
{
	TMaker::VerifyType(vals...);
	TString res;
	res.reserve((TMaker::size(vals) + ...));
	TMaker::Append(res, std::forward<TT>(vals)...);
	return res;
}

template <typename TString = std::wstring, typename TMaker = CStringMaker<typename TString::value_type, typename TString::traits_type>, typename... TT>
shared_string make_wstring(TT&&... vals)
{
	return make_string<TString, TMaker>(std::forward<TT>(vals)...);
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