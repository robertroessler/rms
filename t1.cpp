#include <chrono>
#include <stdio.h>
#include "rms.h"

#ifdef _DEBUG
const int Iterations = 1;
const int Transactions = 1;
#else
const int Iterations = 100000;
const int Transactions = 10;
#endif // DEBUG

using namespace std::chrono;
using namespace rms;

int main(int argc, char* argv[])
{
	// start up RMs with 4 MB
	rms_initialize(1024), printf("rms_initialize completed\n");
	// accept any tag that STARTS with a OR t, followed by ZERO or MORE anything
	const int id = rms_subscribe("[at]*");
	printf("rms_subscribe => %d\n", id);
	// sub should be EMPTY
	printf("rms_peek => %d\n", rms_peek(id));
	// publish message sub is NOT subscribed to...
	rms_publish_string("z", "y"), printf("rms_publish_string completed\n");
	// ... so sub should be STILL be empty
	printf("rms_peek => %d\n", rms_peek(id));
	// publish 4 messages with 2 strings, 1 int, and 1 long long payload
	rms_publish_string("a", "b"), printf("rms_publish_string completed\n");
	// sub should NOT be empty...
	printf("rms_peek => %d\n", rms_peek(id));
	rms_publish_string("ab", "d"), printf("rms_publish_string completed\n");
	printf("rms_peek => %d\n", rms_peek(id));
	rms_publish_int32("ac", 42), printf("rms_publish_int32 completed\n");
	printf("rms_peek => %d\n", rms_peek(id));
	rms_publish_int64("ad", 84), printf("rms_publish_int64 completed\n");
	printf("rms_peek => %d\n", rms_peek(id));
	constexpr auto xs = "THIS is a string!";
	rms_publish_intptr("ae", (std::intptr_t)xs), printf("rms_publish_intptr completed\n");
	printf("rms_peek => %d\n", rms_peek(id));
	int v;
	long long w;
	std::intptr_t x;
	char vb[4], vd[4], vt[4], vu[4], vx[4];
	size_t nb = sizeof vb, nd = sizeof vd, nt = sizeof vt, nu = sizeof vu, nx = sizeof vx;
	// ... until AFTER the next 5 "rms_waits"...
	if (rms_wait_string(id, nullptr, nullptr, vb, &nb, 1) == 1)
		printf("rms_wait_string => %s\n", vb);
	if (rms_wait_string(id, nullptr, nullptr, vd, &nd, 1) == 1)
		printf("rms_wait_string => %s\n", vd);
	if (rms_wait_int32(id, vt, &nt, &v, 3) == 3)
		printf("rms_wait_int32  => '%s'(%zd): %d\n", vt, nt, v);
	if (rms_wait_int64(id, vu, &nu, &w, 3) == 3)
		printf("rms_wait_int64  => '%s'(%zd): %lld\n", vu, nu, w);
	if (rms_wait_intptr(id, vx, &nx, &x, 3) == 3)
		printf("rms_wait_intptr => '%s'(%zd): %p, pointer %s ORIGINAL string!\n",
			vx, nx, (void*)x, strcmp((char*)x, xs) ? "!=" : "==");
	// ... as in, NOW
	printf("rms_peek => %d\n", rms_peek(id));
	// INSIDE a loop... 
	const auto t0 = high_resolution_clock::now();
	for (auto i = 0; i < Iterations; i++) {
		// ... publish 10 messages...
		for (auto j = 0; j < Transactions; j++)
			rms_publish_int64("tag", 42LL);
		auto sum = 0ll;
		// ... and then consume them
		for (auto j = 0; j < Transactions; j++) {
			char t[8];
			long long d;
			auto nT = sizeof t;
			rms_wait_int64(id, nullptr, &nT, &d, 1);
			// do something with the returned data
			sum += d;
		}
	}
	const auto t1 = high_resolution_clock::now();
	rms_close(id);
	printf("timing for %d Publish/Wait pairs = %g ns/round-trip\n", Iterations * Transactions,
		(duration_cast<microseconds>(t1 - t0).count() / (double(Iterations) * Transactions / 1000)));
	return 0;
}
