// This is an independent project of an individual developer. Dear PVS-Studio, please check it.

// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
#include "../shared_string.h"
#include "../cow_string.h"

#include <iostream>
#include <conio.h>

int main()
{
	std::string src1 = "hello";
	std::string_view src2 = "world";
	
	std::cout << "shared_string(std::string): " << shared_string(src1) << std::endl;
	std::cout << "shared_string(std::string_view): " << shared_string(src2) << std::endl;
	std::cout << "shared_string(size_t, char): " << shared_string(10, '!') << std::endl;
	std::cout << "shared_string(char): " << shared_string('!') << std::endl;
	
	std::cout << "shared_string(size_t, size_t(char *, size_t)): " << shared_string(10, [](char *dst, size_t sz)
	{
		for (size_t i = 0; i < sz; ++i, ++dst)
			*dst = '0' + char(i);
		
		return sz;
	}) << std::endl;

	std::cout << "shared_string(void(shared_string_creator &)): " << shared_string([&](auto &s)
	{
		s.reserve(src1.size() + 1 + src2.size() + 3);
		s += src1;
		s += ' ';
		s += src2;
		s.append(3, '!');
	}) << std::endl;

	shared_string s = make_shared_string(src1, ' ', src2, repeat_char(3, '!'));
	std::cout << "make_shared_string: " << s << std::endl;

	cow_string cow = s;
	cow.erase(src1.size(), src2.size() + 1);
	std::cout << "cow_string: " << cow << std::endl;

	while (!_kbhit());
	return 0;
}