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

using std::cout, std::endl, std::boolalpha, std::string;
using namespace std::chrono;
using namespace rms;

int main(int argc, char* argv[])
{
	// start up RMs with 4 MB
	initialize(1024), cout << "rms::initialize completed" << endl;
	// accept any tag that STARTS with a OR t, followed by ZERO or MORE anything
	subscription sub("[at]*");
	// sub should be EMPTY
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	// publish message sub is NOT subscribed to...
	string z = "z"; // (for use as a std::string value param)
	publisher::put_with_tag("y", z), cout << "rms::put_with_tag completed" << endl;
	// ... so sub should be STILL be empty
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	// publish 4 messages with 2 strings, 1 int, and 1 long long payload
	char a[] = "a"; // (for use as a char* param, equiv to { 'a', 0 })
	publisher::put_with_tag("b", a), cout << "rms::put_with_tag completed" << endl;
	// sub should NOT be empty...
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	publisher::put_with_tag("d", "ab"), cout << "rms::put_with_tag completed" << endl;
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	publisher::put_with_tag(42, "ac"), cout << "rms::put_with_tag completed" << endl;
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	publisher::put_with_tag(84LL, "ad"), cout << "rms::put_with_tag completed" << endl;
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	// ... until AFTER the next 4 "gets"...
	cout << "rms::get() => " << sub.get<string>() << endl;
	cout << "rms::get() => " << sub.get<string>() << endl;
	{ auto [data, tag] = sub.get_with_tag<int>();
	cout << "rms::get_with_tag() => " << data << ':' << tag << endl; }
	{ auto [data, tag] = sub.get_with_tag<long long>();
	cout << "rms::get_with_tag() => " << data << ':' << tag << endl; }
	// ... as in, NOW
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	// INSIDE a loop... 
	const auto t0 = high_resolution_clock::now();
	for (auto i = 0; i < Iterations; i++) {
		// ... publish 10 messages...
		for (auto j = 0; j < Transactions; j++)
			publisher::put_with_tag(42LL, "tag");
		long long sum = 0;
		// ... and then consume them
		for (auto j = 0; j < Transactions; j++)
			// do something with the returned data
			sum += sub.get<long long>();
	}
	const auto t1 = high_resolution_clock::now();
	cout << "timing for " << Iterations * Transactions << " Publish/Wait pairs = "
		<< (duration_cast<nanoseconds>(t1 - t0).count() / (double(Iterations) * Transactions)) << " ns/round-trip" << endl;
	return 0;
}
