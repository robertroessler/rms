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
	publisher::put("xyz", 0ull);
	auto ii = 0;
	publisher::put("xyz", ii);
	publisher::put_rec("xyz", ii);
	s.flush();
	// note use of both "old" and "new" enums, plus bool...
	enum old_style { OS_a, OS_b };
	enum class new_style { a, b };
	publisher::put_rec("xyz", OS_a, new_style::b, true);
	const auto reb = s.get_rec();
	const auto [e1, e2, bv] = s.unpack_rec<old_style, new_style, bool>(reb);
	// ... to output enums, cast to "underlying_type" (often/typically int)
	std::cout
		<<   "e1=" << (std::underlying_type_t<old_style>)e1
		<< ", e2=" << (std::underlying_type_t<new_style>)e2
		<< ", bv=" << std::boolalpha << bv << std::endl;
	publisher::put_rec("xyz", OS_a, new_style::b, true);
	// ... or, just use the "universal" get<rms_any>() and to_string()
	auto eeb = s.get<rms_any>();
	std::cout << rms::to_string(eeb) << std::endl;
	//	publisher::put_rec("x", 0, 0ll, 0.0, "abc", "def"s, nullptr); // INVALID...
	publisher::put_rec("x", 0, 0xdeadbeefull, 123456789.0e-42, "abc", "def"s);
	// note in the following output the formatting from std::iostream...
	const auto rec = s.get_rec();
	const auto [a, b, c, d, e] = s.unpack_rec<int, long long, double, string, string>(rec);
	std::cout
		<< "a=" << a << ", b=" << b << ", c=" << c
		<< ", d=" << d << ", e=" << e << std::endl;
	// ... versus that from rms::to_string - which is using std::to_chars
	// ... and also "cheats", deliberately choosing base 16 for long long
	publisher::put_rec("x", 0, 0xdeadbeefll, 123456789.0e-42, "abc", "def"s);
	auto xxx = s.get<rms_any>();
	std::cout << rms::to_string(xxx) << std::endl;
	// the below is interesting because it differs for Release and Debug builds
	std::cout << "sizeof(rms_any)=" << sizeof(rms_any) << std::endl;
	publisher::put_rec("x", 0, 0xdeadbeefll, 123456789.0e-42, "abc", "def"s);
	publisher::put_rec("x", 0, 0xdeadbeefll, 987654321.0e-42, "uvw"s, "xyz"sv);
	s.for_each<rms_any>([](auto yyy) {
		// this should execute TWICE and then HANG waiting for the subscription
		std::cout << rms::to_string(yyy) << std::endl;
	});
}