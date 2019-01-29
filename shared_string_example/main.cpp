#include "../shared_string.h"
#include "../cow_string.h"

#include <iostream>
#include <conio.h>

int main()
{
	std::string s1 = "hello";
	std::string_view s2 = "world";
	
	shared_string s = make_shared_string(s1, ' ', s2, "!!!");
	std::cout << "shared string: " << s << std::endl;

	cow_string cow = s;
	cow.erase(s1.size(), s2.size() + 1);
	std::cout << "COW string: " << cow << std::endl;

	while (!_kbhit());
	return 0;
}