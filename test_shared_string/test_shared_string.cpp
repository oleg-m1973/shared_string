// This is an independent project of an individual developer. Dear PVS-Studio, please check it.

// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com

#include "stdafx.h"
#define SHARED_STRING_TEST
//#define SHARED_STRING_NAMESPACE test
#include "../shared_string.h"
#include "../cow_string.h"

#include <thread>
#include <mutex>
#include <set>
#include <unordered_set>


using namespace std::literals;
using namespace Microsoft::VisualStudio::CppUnitTestFramework;

//using namespace test;

#define SMALL_STR "1234560!!!"sv
#define SMALL_STR2 "12", "34"sv, "56"s, '0', repeat_char(3, '!')

#define LARGE_STR "123456789012345678901234567890!!!"sv
#define LARGE_STR2 "123", "456"s, "789"sv, '0', "123", "456"s, "789"sv, '0', "123", "456"s, "789"sv, '0', repeat_char(3, '!')

#define SMALL_WSTR L"1234560!!!"sv
#define SMALL_WSTR2 L"12", L"34"sv, L"56"s, L'0', repeat_char(3, L'!')

#define LARGE_WSTR L"123456789012345678901234567890!!!"sv
#define LARGE_WSTR2 L"123", L"456"s, L"789"sv, L'0', L"123", L"456"s, L"789"sv, L'0', L"123", L"456"s, L"789"sv, L'0', repeat_char(3, L'!')


#define NAMED(val) std::pair(L#val, val)

inline
decltype(auto) Format(std::wostream &out)
{
	return out;
}

template <typename T>
decltype(auto) Format(std::wostream &out, const T &val)
{
	return out << val;
}

template <typename T>
decltype(auto) Format(std::wostream &out, bool val)
{
	return out << (val? "true": "false");
}

template <typename T>
decltype(auto) Format(std::wostream &out, const std::pair<const wchar_t *, T> &val)
{
	return out << val.first << L'=' << val.second;
}

template <typename T, typename T2, typename... TT>
decltype(auto) Format(std::wostream &out, const T& val, const T2& val2, const TT&... vals)
{
	Format(out, val) << L", ";
	Format(out, val2);
	if constexpr(sizeof...(vals) != 0)
		Format(out << L", ", vals...);
	return out;
}

template <typename... TT>
std::wstring FormatStr(const TT&... vals)
{
	if constexpr(sizeof...(vals) == 0)
		return {};
	else
	{
		std::wstringstream ss;
		Format(ss, vals...);
		return ss.str();
	}
}

template <typename... TT>
std::wstring FormatStr(std::in_place_t, const TT&... vals)
{
	return FormatStr(vals...);
}

#define TEST_VERIFY(expr, ...) Assert::IsTrue(expr, FormatStr(std::in_place_t(), __VA_ARGS__, #expr).c_str())

namespace test_shared_string
{		

template <typename TDst, typename TSrc>
void LockedAssign(std::mutex &mx, TDst &dst, TSrc &&src)
{
	std::lock_guard lock(mx);
	dst = std::forward<TSrc>(src);
}

template <typename T>
T LockedCopy(std::mutex &mx, const T &s)
{
	std::lock_guard lock(mx);
	return s;
}
	
TEST_CLASS(CTestSharedString)
{
public:
	TEST_METHOD(CommonSmallTests)
	{
		CommonTests(SMALL_STR, is_equal, SMALL_STR2);
		CommonTests(SMALL_STR, is_greater, LARGE_STR2);
		CommonTests(SMALL_WSTR, is_equal, SMALL_WSTR2);
		CommonTests(SMALL_WSTR, is_greater, LARGE_WSTR2);
		
		TEST_VERIFY(_SharedStringLeaks == 0, L"Memory leaks");
	}

	TEST_METHOD(CommonLargeTests)
	{
		CommonTests(LARGE_STR, is_equal, LARGE_STR2);
		CommonTests(LARGE_STR, is_less, SMALL_STR2);
		CommonTests(LARGE_WSTR, is_equal, LARGE_WSTR2);
		CommonTests(LARGE_WSTR, is_less, SMALL_WSTR2);

		TEST_VERIFY(_SharedStringLeaks == 0, L"Memory leaks");
	}

	TEST_METHOD(MultiThreadingTests)
	{
		MultiThreadingTests(SMALL_STR);
		MultiThreadingTests(LARGE_STR);
		TEST_VERIFY(_SharedStringLeaks == 0, L"Memory leaks");
	}
protected:
	enum : int
	{
		is_equal = 0,
		is_less = -1,
		is_greater = 1,
	};

	template <typename TString, typename T>
	TString CreateString(const T &arg)
	{
		if constexpr(std::is_same_v<T, repeat_char<typename TString::value_type>>)
			return TString(arg.first, arg.second);
		else if constexpr(std::is_same_v<T, TString::value_type>)
			return TString(1, arg);
		else
			return TString(arg);
	}

	template <typename TString, typename T, typename... TT>
	void TestConstruct(const T &arg, const TT&... args)
	{
		using TMaker = CStringMaker<typename TString::value_type, typename TString::traits_type>;
		auto s = make_string<std::basic_string<typename TString::value_type>>(arg);
		
		TEST_VERIFY(s.size() == TMaker::size(arg), L"make_string", typeid(T).name());

		TEST_VERIFY(CreateString<TString>(arg) == s, L"Constuctor");
			
		TEST_VERIFY(make_shared_string<TString>(arg) == s, L"make_shared_string", typeid(T).name());

		if constexpr(sizeof...(args) != 0)
			TestConstruct<TString>(args...);
	}

	template <typename TString>
	void TestSSO()
	{
		using TSmallOpt = CSmallStringOpt<CSharedLargeStr<TString::value_type>>;

		static const size_t sso = sizeof(void *) * 3 / sizeof(TString::value_type) - 1;
		TEST_VERIFY(sso == TString::sso_size, L"Small size", NAMED(sso), NAMED(TString::sso_size));

		for (size_t i = 1; i < 100; ++i)
		{
			TString s(i, 'X');
			TEST_VERIFY((s.size() < sso) == s.is_small(), L"SSO", NAMED(sso), NAMED(s.size()));
		}
	}

	template <typename TString>
	void TestRefCounter(const TString &s)
	{
		const auto refs = s.use_count();
		std::list<TString> items;
		for (int i = 0; i < 100; ++i)
		{
			items.emplace_back(s);
			TEST_VERIFY(s.use_count() == (!s.is_small()? refs + i + 1: 0), L"Refs count", NAMED(s.use_count()), NAMED(refs));
		}

		
		items.clear();
		TEST_VERIFY(s.use_count() == refs, L"Refs count", NAMED(s.use_count()), NAMED(refs));
	}

	template <typename TSet, typename T>
	void TestSet(const T &mst)
	{
		using TString = TSet::key_type;
		TSet items;

		auto to_string = [&mst](int i)
		{
			if constexpr(std::is_same_v<char, TString::value_type>)
				return make_shared_string<TString>(mst, std::to_string(i));
			else 
				return make_shared_string<TString>(mst, std::to_wstring(i));
		};
		
		items.emplace(to_string(0));

		for (int i = 1; i < 100; ++i)
		{
			items.emplace(to_string(i));
			
			auto s = to_string(i - 1);
			TEST_VERIFY(items.find(s) != items.end(), L"Find item", i - 1, NAMED(s.is_small()));
			TEST_VERIFY(items.size() == i + 1, L"Size of set", items.size());
		}
	}

	template <typename TChar>
	void TestCowString(const basic_shared_string<TChar> &mst)
	{
		const auto npos = basic_cow_string<TChar>::npos;

		using cow_string = basic_cow_string<TChar>;
		using string = std::basic_string<TChar>;

		string s(mst);
		cow_string cow = mst;

		TEST_VERIFY(cow.is_small() != (cow.c_str() == mst.c_str()));
		TEST_VERIFY(s == cow);

#define TEST_COW_STRING(func) {cow_string s = mst; string s2(mst); \
	s.func; s2.func; TEST_VERIFY(s == s2, L#func);}

		TEST_COW_STRING(append(mst));
		TEST_COW_STRING(insert(0, mst));
		TEST_COW_STRING(insert(mst.size() / 2, mst));
		TEST_COW_STRING(insert(mst.size(), mst));
		Assert::ExpectException<std::out_of_range>([&mst]()
		{
			TEST_COW_STRING(insert(mst.size() + 1, mst));
		}, L"std::out_of_range exception expected");

		if (mst.empty())
			Assert::ExpectException<std::out_of_range>([&mst]()
			{
				TEST_COW_STRING(erase(mst.size() - 2, 1));
			}, L"std::out_of_range exception expected");
		else
		{
			TEST_COW_STRING(erase(0, 1));
			TEST_COW_STRING(erase(mst.size() - 2, 1));
			TEST_COW_STRING(erase(mst.size() / 2, 1));
		}

#undef TEST_COW_STRING
		{
			const size_t n = mst.size();
			cow_string s(n, '0');
			string s2(n, '1');

			s.modify([](auto *data, size_t sz) noexcept
			{
				cow_string::traits_type::assign(data, sz, '1');
			});

			TEST_VERIFY(s == s2, "modify");
		}

	}

	template <typename TChar, typename... TT>
	void CommonTests(const std::basic_string_view<TChar> &mst, int cmp, const TT&... args)
	{
		_CommonTests<basic_shared_string<TChar>>(mst, cmp, args...);
		_CommonTests<basic_cow_string<TChar>>(mst, cmp, args...);
	}

	template <typename TString, typename... TT>
	void _CommonTests(const std::basic_string_view<typename TString::value_type> &mst, int cmp, const TT&... args)
	{
		using TChar = typename TString::value_type; 

		using TSmallOpt = CSmallStringOpt<CSharedLargeStr<TChar>>;
		using TMaker = CStringMaker<typename TString::value_type, typename TString::traits_type>;
	
		TestConstruct<TString>(args...);

		TestSSO<TString>();

		const size_t sz = (TMaker::size(args) + ...);
			   
		auto s = make_shared_string<TString>(args...);

		TEST_VERIFY(std::basic_string<TChar>(s) == s, "std::string()");
		TEST_VERIFY((std::basic_string<TChar>() = s) == s, "std::string() assign");

		TEST_VERIFY(s.is_small() == TSmallOpt::ShouldBeSmall(sz), L"Test should be small small");
		TEST_VERIFY(s.size() == sz, L"Compare sizes");

		TEST_VERIFY(TString(s) == s, L"Copy constructor");
		TEST_VERIFY(s.is_small() == TString(s).is_small(), TSmallOpt::ShouldBeSmall(sz)? L"String is small": L"String is large");
		TEST_VERIFY(s.is_small() != (TString(s).c_str() == s.c_str()), L"Copy constructor, large string pointers not same");

		TEST_VERIFY(TString(TString(s)) == s, L"Move constructor");

		TEST_VERIFY((TString() = s) == s, L"Copy assignment");

		TEST_VERIFY((TString() = TString(s)) == s, L"Move assignment");

		{
			typename TString::size_type i = 0;
			bool res = true;
			for (auto &item: s)
				if (i >= s.size() || item != s[i++])
				{
					res = false;
					break;
				}

			TEST_VERIFY(res && i == s.size(), L"Iterators and subscript operator");
		}

		{
			const auto cmp2 = s.compare(mst);
			TEST_VERIFY(((cmp < 0) == (cmp2 < 0)) && ((cmp == 0) == (cmp2 == 0)), L"Compare");
		}

		{
			TString s2(mst.size(), [&mst](TChar *dst, size_t sz)
			{
				return mst.copy(dst, sz);
			});

			TEST_VERIFY(s2 == mst, L"Reserve constructor");
		}

		{
			std::basic_string<TChar> mst2;
			TString s2(size_t(255), [&mst2](TChar *dst, size_t sz)
			{
				TChar *p = dst;
				for (int i = 0; i < 10; ++i)
				{
					int n;
					if constexpr(std::is_same_v<TString::value_type, char>)
						n = sprintf_s(p, sz, "%d", i);
					else 
						n = swprintf_s(p, sz, L"%d", i);
				
					mst2.append(p, n);
					p += n;
					sz -= size_t(n);
				}
				return size_t(p - dst);
			});

			TEST_VERIFY((s2 == mst2) && (mst2 == s2), L"Reserve constructor, sprintf");
		}

#define TS_ITEM(op, ...) \
	TEST_VERIFY((s op mst) == (s2 op mst)); \
	TEST_VERIFY((mst op s) == (mst op s2));

		{
			auto s2 = make_string<std::basic_string<TChar>>(args...);
			TEST_VERIFY(s == s2);
			SHARED_STRING_CMP
		}

#undef TS_ITEM
		{
			std::basic_stringstream<TChar> ss;
			ss << s;
			TEST_VERIFY(s == ss.str());
		}

		TestRefCounter(s);
		TestSet<std::set<TString>>(mst);
		TestSet<std::unordered_set<TString>>(mst);

		TEST_VERIFY((sz == 0) == s.empty(), L"Test empty");

		TestCowString(s);
	}

	void MultiThreadingTests(const std::string_view &mst)
	{
		volatile bool stop = false;
		std::mutex mx;
		shared_string s(mst);
		std::list<std::thread> threads;
		auto s2 = s;

		std::wstring err;
		threads.emplace_back([&stop, &mx, &s, &s2, &mst, &err]()
		{
			while (!stop)
			{
				LockedAssign(mx, s, shared_string(mst));

				shared_string s3;
				s3 = s2;
				if (s3 != mst)
				{
					LockedAssign(mx, err, L"Not locked assign: Strings not equal");
					break;
				}

				shared_string s4(s2);
				if (s4 != mst)
				{
					LockedAssign(mx, err, L"Not locked copy: Strings not equal");
					break;
				}

				std::this_thread::sleep_for(10ms);
			}
			stop = true;
		});

		for (int i = 0; i < 10; ++i)
		{
			threads.emplace_back([&stop, &mx, &s, &mst, &err]()
			{
				while (!stop)
				{
					shared_string s2;
					LockedAssign(mx, s2, s);
					if (s2 != mst)
					{
						LockedAssign(mx, err, L"Assign: Strings not equal");
						break;
					}
					std::this_thread::sleep_for(100ms);
				}
				stop = true;
			});
		}

		for (int i = 0; i < 10; ++i)
		{
			threads.emplace_back([&stop, &mx, &s, &mst, &err]()
			{
				while (!stop)
				{
					auto s2 = LockedCopy(mx, s);
					if (s2 != mst)
					{
						LockedAssign(mx, err, L"Copy: Strings not equal");
						break;
					}

					std::this_thread::sleep_for(100ms);
				}

				stop = true;
			});
		}

		for (int i = 0; i < 10 && !stop; ++i)
		{
			{
				std::lock_guard lock(mx);
				TEST_VERIFY(s == mst, L"Strings not equal");
			}
			std::this_thread::sleep_for(1s);
		}

		stop = true;
		for (auto &item: threads)
			item.join();

		Assert::IsTrue(err.empty(), err.c_str());
	}

};
}