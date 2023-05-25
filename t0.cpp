#include <iostream>
#include <string>
#include <format>
#include "rms.h"

using namespace std::string_literals;
using namespace std::string_view_literals;
using namespace rms;

using std::string;

static string format_rms_any(rms_any& v) {
	auto explode_rec = [](rms_rec& rec) {
		string o;
		o.reserve(256);
		for (auto& a : rec)
			if (o.empty())
				std::format_to(std::back_inserter(o), "[ {}", format_rms_any(a));
			else
				std::format_to(std::back_inserter(o), ", {}", format_rms_any(a));
		return o += " ]";
	};
	return
		std::holds_alternative<rms_int32>(v) ? std::format("{}", std::get<rms_int32>(v)) :
		std::holds_alternative<rms_int64>(v) ? std::format("{:#0x}", std::get<rms_int64>(v)) :
		std::holds_alternative<rms_ieee>(v) ? std::format("{}", std::get<rms_ieee>(v)) :
		std::holds_alternative<rms_rec>(v) ? explode_rec(std::get<rms_rec>(v)) :
		std::get<string>(v);
}

template<rms_num_type T>
//requires (rms_num_type<T> && !std::same_as<T, nullptr_t>)
static constexpr auto D(T v) { return v; }

static constexpr auto D(std::string_view v) { return v; }

int main(int argc, char* argv[]) {
	initialize(1024);
	subscription s("x*");
	publisher::put_tag("xyz");
	publisher::put("xy", "snarf");
//	publisher::put("xyz", nullptr);
	publisher::put("xyz", 0);
	publisher::put("xyz", 0.0);
	s.flush();
	publisher::put_rec("x", 0, 0ll, 0.0, "abc", "def"s);
	const auto [a, b, c, d, e] = s.get_rec<int, long long, double, string, string>();
	std::cout << "a=" << a << ", b=" << b << ", c=" << c << ", d=" << d << ", e=" << e << std::endl;
	publisher::put_rec("x", 0, 0ll, 0.0, "abc", "def"s);
	auto xxx = s.get<rms_any>();
	std::cout << format_rms_any(xxx) << std::endl;
	std::cout << "sizeof rms_any=" << sizeof rms_any << std::endl;
}