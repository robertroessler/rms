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

constexpr auto qa_multi_q_bare = false;	// (simple[r] case - flushes for extra puts)
constexpr auto qa_multi_q_full = false;	// (full case - do real gets for extra puts)

int main(int argc, char* argv[])
{
	// start up RMs with 4 MB
	initialize(1024), cout << "rms::initialize completed" << endl;
	// accept any tag that STARTS with a OR t, followed by ZERO or MORE anything
	subscription sub("[at]*");
	// (used to CONDITIONALLY test multi-queue perf)
	subscription sub2, sub3, sub4, sub5;
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
	if constexpr (qa_multi_q_bare || qa_multi_q_full)
		sub2.subscribe("t*"),
		sub3.subscribe("ta*"),
		sub4.subscribe("tag"),
		sub5.subscribe("t?g");
	const auto t0 = high_resolution_clock::now();
	for (auto i = 0; i < Iterations; i++) {
		// ... publish 10 messages...
		for (auto j = 0; j < Transactions; j++)
			publisher::put_with_tag(42LL, "tag");
		long long sum = 0;
		// ... and then consume them
		for (auto j = 0; j < Transactions; j++) {
			// do something with the returned data
			sum += sub.get<long long>();
			if constexpr (qa_multi_q_bare)
				sub2.flush(),
				sub3.flush(),
				sub4.flush(),
				sub5.flush();
			else if constexpr (qa_multi_q_full)
				sum += sub2.get<long long>(),
				sum += sub3.get<long long>(),
				sum += sub4.get<long long>(),
				sum += sub5.get<long long>();
		}
	}
	const auto t1 = high_resolution_clock::now();
	auto multiplier = 1;
	if constexpr (qa_multi_q_bare || qa_multi_q_full)
		multiplier *= 5;
	auto label = " Publish/Get pairs = ";
	if constexpr (qa_multi_q_bare)
		label = " Publish/Get (or Flush) pairs = ";
	cout << "timing for " << Iterations * Transactions * multiplier << label
		<< (duration_cast<nanoseconds>(t1 - t0).count() / (double(Iterations) * Transactions * multiplier)) << " ns/round-trip" << endl;
	return 0;
}
