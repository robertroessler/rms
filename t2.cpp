#include <iostream>
#include <chrono>
#include "rms.h"

#ifdef _DEBUG
const int Iterations = 1;
const int Transactions = 1;
#else
const int Iterations = 100000;
const int Transactions = 10;
#endif // DEBUG

using namespace std;
using namespace chrono;
using namespace rms;

int main(int argc, char* argv[])
{
	// start up RMs with 4 MB
	cout << "rms::initialize => " << initialize(1024) << endl;
	// accept any tag that STARTS with a OR t, followed by ZERO or MORE anything
	subscription sub("[at]*");
	// sub should be EMPTY
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	// publish message sub is NOT subscribed to...
	string z = "z"; // (for use as a std::string value param)
	cout << "rms::put_with_tag => " << publisher::put_with_tag("y", z) << endl;
	// ... so sub should be STILL be empty
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	// publish 4 messages with 2 strings, 1 int, and 1 long long payload
	char a[] = { 'a' }; // (for use as a char* param)
	cout << "rms::put_with_tag => " << publisher::put_with_tag("b", a) << endl;
	// sub should NOT be empty...
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	cout << "rms::put_with_tag " << publisher::put_with_tag("d", "ab") << endl;
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	cout << "rms::put_with_tag " << publisher::put_with_tag(42, "ac") << endl;
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	cout << "rms::put_with_tag " << publisher::put_with_tag(84LL, "ad") << endl;
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	// ... until AFTER the next 4 "gets"...
	cout << "rms::get() => " << sub.get<string>() << endl;
	cout << "rms::get() => " << sub.get<string>() << endl;
	auto t32 = sub.get_with_tag<int>();
	cout << "rms::get_with_tag() => " << get<0>(t32) << ':' << get<1>(t32) << endl;
	auto t64 = sub.get_with_tag<long long>();
	cout << "rms::get_with_tag() => " << get<0>(t64) << ':' << get<1>(t64) << endl;
	// ... as in, NOW
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	// INSIDE a loop... 
	const auto t0 = high_resolution_clock::now();
	for (int i = 0; i < Iterations; i++) {
		// ... publish 10 messages...
		for (int j = 0; j < Transactions; j++)
			publisher::put_with_tag(42LL, "tag");
		// ... and then consume them
		for (int j = 0; j < Transactions; j++)
			auto td = sub.get_with_tag<long long>();
	}
	const auto t1 = high_resolution_clock::now();
	cout << "time for " << Iterations * Transactions << " Publish/Wait pairs = "
		<< duration_cast<milliseconds>(t1 - t0).count() << endl;
	return 0;
}