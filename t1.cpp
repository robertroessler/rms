#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <windows.h>
#include <stdio.h>
#include "rms.h"

#ifdef _DEBUG
const int Iterations = 1;
const int Transactions = 1;
#else
const int Iterations = 100000;
const int Transactions = 10;
#endif // DEBUG

int main(int argc, char* argv[])
{
	// start up RMs with 4 MB
	printf("rms_initialize => %d\n", rms_initialize(1024));
	// accept any tag that STARTS with a OR t, followed by ZERO or MORE anything
	const int id = rms_subscribe("[at]*");
	printf("rms_subscribe => %d\n", id);
	// sub should be EMPTY
	printf("rms_peek => %d\n", rms_peek(id));
	// publish message sub is NOT subscribed to...
	printf("rms_publish_string => %d\n", rms_publish_string("z", "y"));
	// ... so sub should be STILL be empty
	printf("rms_peek => %d\n", rms_peek(id));
	// publish 4 messages with 2 strings, 1 int, and 1 long long payload
	printf("rms_publish_string => %d\n", rms_publish_string("a", "b"));
	// sub should NOT be empty...
	printf("rms_peek => %d\n", rms_peek(id));
	printf("rms_publish_string => %d\n", rms_publish_string("ab", "d"));
	printf("rms_peek => %d\n", rms_peek(id));
	printf("rms_publish_int32 => %d\n", rms_publish_int32("ac", 42));
	printf("rms_peek => %d\n", rms_peek(id));
	printf("rms_publish_int64 => %d\n", rms_publish_int64("ad", 84));
	printf("rms_peek => %d\n", rms_peek(id));
	int v;
	__int64 w;
	char vb[4], vd[4], vt[4], vu[4];
	int nb = sizeof vb, nd = sizeof vd, nt = sizeof vt, nu = sizeof vu;
	// ... until AFTER the next 4 "rms_waits"...
	if (rms_wait_string(id, NULL, NULL, vb, &nb, 1) == 1)
		printf("rms_wait_string => %s\n", vb);
	if (rms_wait_string(id, NULL, NULL, vd, &nd, 1) == 1)
		printf("rms_wait_string => %s\n", vd);
	if (rms_wait_int32(id, vt, &nt, &v, 3) == 3)
		printf("rms_wait_int32 => '%s'(%d): %d\n", vt, nt, v);
	if (rms_wait_int64(id, vu, &nu, &w, 3) == 3)
		printf("rms_wait_int64 => '%s'(%d): %lld\n", vu, nu, w);
	// ... as in, NOW
	printf("rms_peek => %d\n", rms_peek(id));
	// INSIDE a loop... 
	const DWORD t0 = ::GetTickCount();
	for (int i = 0; i < Iterations; i++) {
		// ... publish 10 messages...
		for (int j = 0; j < Transactions; j++)
			rms_publish_int64("tag", 42LL);
		long long sum = 0;
		// ... and then consume them
		for (int j = 0; j < Transactions; j++) {
			char t[8];
			long long d;
			int nT = sizeof t;
			rms_wait_int64(id, t, &nT, &d, 3);
			// do something with the returned data
			sum += d;
		}
	}
	const DWORD t1 = ::GetTickCount();
	rms_close(id);
	printf("time for %d Publish/Wait pairs = %d\n", Iterations * Transactions, t1 - t0);
	return 0;
}
