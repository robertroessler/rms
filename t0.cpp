#include <iostream>
#include <string>
#include "rms.h"

using namespace std::string_literals;
using namespace std::string_view_literals;
using namespace rms;

using std::string;

/*
	t0 is basically a grab-bag showing different RMs (rms) features...
	... of particular interest is the ability to detect disallowed types
	AT COMPILE TIME.
*/
int main(int argc, char* argv[]) {
	initialize(1024);
	subscription s("x*");
	publisher::put_tag("xyz");
	publisher::put("xy", "snarf");
//	publisher::put("xyz", nullptr); // INVALID @ compile-time
	publisher::put("xyz", string()); // (OK - same result, put_tag is preferred)
	publisher::put("xyz", 0);
	publisher::put("xyz", 0.0);
//	publisher::put("xyz", 0ull); // INVALID @ compile-time
	publisher::put("xyz", rms_int64(0ull)); // (OK)
	s.flush();
//	publisher::put_rec("x", 0, 0ll, 0.0, "abc", "def"s, nullptr); // INVALID...
	publisher::put_rec("x", 0, 0xdeadbeefll, 123456789.0e-42, "abc", "def"s);
	// note in the following output the formatting from std::iostream...
	const auto [a, b, c, d, e] = s.get_rec<int, long long, double, string, string>();
	std::cout << "a=" << a << ", b=" << b << ", c=" << c << ", d=" << d << ", e=" << e << std::endl;
	// ... versus that from rms::to_string - which is using std::to_chars
	// ... and also "cheats", deliberately choosing base 16 for long long
	publisher::put_rec("x", 0, 0xdeadbeefll, 123456789.0e-42, "abc", "def"s);
	auto xxx = s.get<rms_any>();
	std::cout << rms::to_string(xxx) << std::endl;
	// the below is interesting because it differs for Release and Debug builds
	std::cout << "sizeof rms_any=" << sizeof rms_any << std::endl;
	publisher::put_rec("x", 0, 0xdeadbeefll, 123456789.0e-42, "abc", "def"s);
	publisher::put_rec("x", 0, 0xdeadbeefll, 987654321.0e-42, "uvw"s, "xyz"sv);
	s.for_each<rms_any>([](rms_any yyy) {
		// this should execute TWICE and then HANG waiting for the subscription
		std::cout << rms::to_string(yyy) << std::endl;
	});
}