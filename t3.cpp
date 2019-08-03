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

using std::cout, std::endl, std::boolalpha;
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
	std::string x, y, z = "z"; // (for use as std::string value params)
	publisher::put_with_tag("y", z), cout << "rms::put_with_tag completed" << endl;
	// ... so sub should be STILL be empty
	cout << "rms::empty => " << boolalpha << sub.empty() << endl;
	// publish 4 messages with 2 strings, 1 int, and 1 long long payload
	char a[] = { 'a', 0 }; // (for use as a char* param)
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
	sub >> x >> y;
	cout << "rms::operator>> => " << x << endl;
	cout << "rms::operator>> => " << y << endl;
	rms_pair<int> t32;		// (for use as "extraction operator" targets)
	rms_pair<long long> t64;
	sub >> t32 >> t64;
	cout << "rms::operator>> => " << t32.first << ':' << t32.second << endl;
	cout << "rms::operator>> => " << t64.first << ':' << t64.second << endl;
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
			if (long long d64; sub.try_get(d64))
				// do something with the returned data
				sum += d64;
	}
	const auto t1 = high_resolution_clock::now();
	cout << "timing for " << Iterations * Transactions << " Publish/Wait pairs = "
		<< (duration_cast<microseconds>(t1 - t0).count() / (double(Iterations) * Transactions / 1000)) << " ns/round-trip" << endl;
	// now for some exciting multi-threaded tests...
	// ... first, create a thread watching our queue
	std::thread t([&]() {
		// have RMs query the queue, running the supplied lambda on any messages
		sub.for_each<int>([&](auto data) { cout << "thread pulled " << data << " from the queue" << endl; });
	});
	// ... next, feed the queue with some messages...
	publisher::put_with_tag(42, "t2");
	publisher::put_with_tag(43, "t2");
	publisher::put_with_tag(44, "t2");
	// ... pause for a short time so the thread can get the messages...
	std::this_thread::sleep_for(100ms);
	// ... finally, "signal" the queue to close it
	sub.signal(RMsSigInterrupt);
	t.join();
	return 0;
}
