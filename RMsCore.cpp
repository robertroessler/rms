/*
	RMsCore.cpp - "core" functionality of the RMs messaging system

	Copyright(c) 2004-2016, Robert Roessler
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:

	1. Redistributions of source code must retain the above copyright notice,
	this list of conditions and the following disclaimer.

	2. Redistributions in binary form must reproduce the above copyright notice,
	this list of conditions and the following disclaimer in the documentation
	and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
	ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
	LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
	CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
	SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
	CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
	ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
	POSSIBILITY OF SUCH DAMAGE.
*/

#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <windows.h>
#include <cstdio>
#include "RMs.h"
#include "rglob.h"

using namespace std;
using namespace rms;

static HANDLE rmsM = NULL;				// shared memory mapping object(s)
char* rms::rmsB = NULL;					// shared memory view pointer(s)
RMsRoot* rms::rmsRoot = NULL;			// shared "root" object
static int NQPage = 0;					// # of pages of RMsQueue "extras"

//	Dump root object info
template <typename ...Params>
static void dumpRoot(Params&&... params) {
#ifdef DUMP_ROOT
	char bb[128];
	const int n = snprintf(bb, sizeof bb, "RMsRoot[%x]::", std::this_thread::get_id());
	if (n > 0) {
		snprintf(bb + n, sizeof bb - n, forward<Params>(params)...);
		::OutputDebugString(bb);
	}
#endif
}

//	Dump queue object info
template <typename ...Params>
static void dumpQueue(Params&&... params) {
#ifdef DUMP_QUEUE
	char bb[128];
	const int n = snprintf(bb, sizeof bb, "RMsQueue[%x]::", std::this_thread::get_id());
	if (n > 0) {
		snprintf(bb + n, sizeof bb - n, forward<Params>(params)...);
		::OutputDebugString(bb);
	}
#endif
}

//	Dump exported low-level interface info
template <typename ...Params>
static void dumpExported(Params&&... params) {
#ifdef DUMP_EXPORTED
	char bb[128];
	snprintf(bb, sizeof bb, forward<Params>(params)...);
	::OutputDebugString(bb);
#endif
}

//	Dump "check" info
template <typename ...Params>
static void dumpCheck(Params&&... params) {
#if	defined(CHECK_ALLOC_FULL) || defined(CHECK_QUEUE_FULL)
	char bb[128];
	snprintf(bb, sizeof bb, forward<Params>(params)...);
	::OutputDebugString(bb);
#endif
}

int rms::initialize(int np)
{
	return rms_initialize(np);
}

inline int rms::isValidQueue(int pg)
{
#if defined(CHECK_ALLOC) || defined(CHECK_ALLOC_FULL)
	if (!rmsRoot->CheckAlloc(pg))
		return 0;	// we're OUTTA here!
#elif !defined(CHECK_NOTHING)
	if (pg <= 0 || pg >= rmsRoot->high)
		return 0;	// we're OUTTA here!
#endif
#ifdef CHECK_QUEUE_FULL
	if (!rmsRoot->CheckQueue(pg))
		return 0;	// we're OUTTA here!
	if (!((RMsQueue*)pg2xp(pg))->Validate())
		return 0;	// we're OUTTA here!
#elif defined(CHECK_QUEUE)
	if (!((RMsQueue*)pg2xp(pg))->Validate())
		return 0;	// we're OUTTA here!
#elif !defined(CHECK_NOTHING)
	if (((RMsQueue*)pg2xp(pg))->magic != QueueMagic)
		return 0;	// we're OUTTA here!
#endif
	return 1;	// indicate success
}

/*
	With the exception of "rms_initizlize()", the "rms_xxx" entry points are
	meant to provide "C" access for a lower-level interface to RMs, and in most
	cases simply add some tracing and argument-checking prior to invoking the
	"real" object-oriented implementation interface.

	Both these and the public calls on the RMsRoot and RMsQueue objects are
	considered deprecated for general use, with the suggested interface to RMs
	being exemplified by the "rms::publisher" and "rms::subscription" classes.
*/

RMS_EXPORT
int rms_close(int id)
{
	dumpExported("rms_close(%d)...\n", id);
	if (!isValidQueue(id))
		return 0;	// we're OUTTA here!
	dumpExported("rms_close(%d)...Close()\n", id);
	return ((RMsQueue*)pg2xp(id))->Close();
}

RMS_EXPORT
int rms_flush(int id)
{
	dumpExported("rms_flush(%d)...\n", id);
	if (!isValidQueue(id))
		return 0;	// we're OUTTA here!
	dumpExported("rms_flush(%d)...Flush()\n", id);
	return ((RMsQueue*)pg2xp(id))->Flush();
}

/*
	Set up the RMs messaging system for use, using the number of 4 KiB pages
	specified (the implementation-allowed maximum is 65,536).

	An "anonymous" file mapping (on Windows) is used, based only on system
	"backing store"... the intent has been to store no actual pointers in any
	of these pages, so that sharing could be possible across processes having
	different virtual address mapping for this storage.

	The requested amount of virtual storage is "reserved", with the actual
	"commits" occurring in units of "rms::PageIncrement" pages (currently 16).
*/
RMS_EXPORT
int rms_initialize(int np)
{
	dumpExported("rms_initialize(%d)...\n", np);
	rmsM = ::CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE|SEC_RESERVE,
		0, np*4096, NULL);
	if (rmsM == NULL)
		return 0;	// we're OUTTA here!
	const DWORD mapE = ::GetLastError();// (stash for later)
	// [attempt to] map view
	rmsB = (char*)::MapViewOfFile(rmsM, FILE_MAP_ALL_ACCESS, 0, 0, 0);
	if (rmsB == NULL) {
		::CloseHandle(rmsM), rmsM = NULL;
		return 0;	// we're OUTTA here!
	}
	// [attempt to] commit FIRST page (bootstrapping)
	if (::VirtualAlloc(rmsB, 1*4096, MEM_COMMIT, PAGE_READWRITE) == NULL) {
		::UnmapViewOfFile(rmsB), rmsB = NULL;
		::CloseHandle(rmsM), rmsM = NULL;
		return 0;	// we're OUTTA here!
	}
	// init mapping AS REQUIRED
	if (mapE != ERROR_ALREADY_EXISTS) {
		// use "placement" new!
		new(rmsB) RMsRoot();
		rmsRoot = (RMsRoot*)rmsB;
		if (!rmsRoot->Initialize(np)) {
			::UnmapViewOfFile(rmsB), rmsB = NULL;
			::CloseHandle(rmsM), rmsM = NULL;
			return 0;	// we're OUTTA here!
		}
	}
	// verify [expected] invariants
	static_assert(sizeof RMsRoot <= 4096, "RMsRoot instance MUST be <= 4096 bytes long!");
	static_assert(sizeof RMsQueue <= 4096, "RMsQueue instance MUST be <= 4096 bytes long!");
	// compute any architecture-dependent values
	NQPage = (4096 - offsetof(RMsQueue, pageE)) / sizeof(unsigned short);
	dumpExported("rms_initialize(%d) NQPage=%d\n", np, NQPage);
	dumpExported("rms_initialize(%d)...SUCCESS\n", np);
	return 1;	// indicate success
}

#ifdef _DEBUG
RMS_EXPORT
int rms_is_valid_queue(int id)
{
	return isValidQueue(id);
}
#endif

RMS_EXPORT
int rms_peek(int id)
{
	dumpExported("rms_peek(%d)...\n", id);
	if (!isValidQueue(id))
		return 0;	// we're OUTTA here!
	dumpExported("rms_peek(%d)...%d\n", id, ((RMsQueue*)pg2xp(id))->Peek());
	return ((RMsQueue*)pg2xp(id))->Peek();
}

RMS_EXPORT
int rms_publish_bytes(const char* tag, const unsigned char* data, int n)
{
	dumpExported("rms_publish_bytes('%s',%02x...,%d)...\n", tag, data[0], n);
	if (tag == NULL)
		return 0;	// we're OUTTA here!
	// N.B. - limit n to 0 <= n <= 4096!
	if (n < 0 || n > 4096)
		return 0;	// we're OUTTA here!
	dumpExported("rms_publish_bytes('%s'...)...Distribute()\n", tag);
	return rmsRoot->Distribute(tag, data, n);
}

RMS_EXPORT
int rms_publish_ieee(const char* tag, rms_ieee data)
{
	dumpExported("rms_publish_ieee('%s',%ld)...\n", tag, data);
	if (tag == NULL)
		return 0;	// we're OUTTA here!
	dumpExported("rms_publish_ieee('%s'...)...Distribute()\n", tag);
	return rmsRoot->Distribute(tag, &data, sizeof rms_ieee, RMsTypeIeee);
}

RMS_EXPORT
int rms_publish_int32(const char* tag, rms_int32 data)
{
	dumpExported("rms_publish_int32('%s',%d)...\n", tag, data);
	if (tag == NULL)
		return 0;	// we're OUTTA here!
	dumpExported("rms_publish_int32('%s'...)...Distribute()\n", tag);
	return rmsRoot->Distribute(tag, &data, sizeof rms_int32, RMsTypeInt32);
}

RMS_EXPORT
int rms_publish_int64(const char* tag, rms_int64 data)
{
	dumpExported("rms_publish_int64('%s',%ld)...\n", tag, data);
	if (tag == NULL)
		return 0;	// we're OUTTA here!
	dumpExported("rms_publish_int64('%s'...)...Distribute()\n", tag);
	return rmsRoot->Distribute(tag, &data, sizeof rms_int64, RMsTypeInt64);
}

RMS_EXPORT
int rms_publish_string(const char* tag, const char* data)
{
	dumpExported("rms_publish_string('%s','%.8s...')...()\n", tag, data);
	if (tag == NULL)
		return 0;	// we're OUTTA here!
	// N.B. - limit any data to 0 <= strlen(data) <= 4095!
	const int n = data? ((int)strlen(data) + 1): 0;
	if (n > 4096)
		return 0;	// we're OUTTA here!
	dumpExported("rms_publish_string('%s'...)...Distribute()\n", tag);
	return rmsRoot->Distribute(tag, data, n);
}

RMS_EXPORT
int rms_signal(int id, int flags)
{
	dumpExported("rms_signal(%d,%08x)...\n", id, flags);
	if (!isValidQueue(id))
		return 0;	// we're OUTTA here!
	dumpExported("rms_signal(%d,%08x)...Signal()\n", id, flags);
	return ((RMsQueue*)pg2xp(id))->Signal(flags);
}

RMS_EXPORT
int rms_subscribe(const char* pattern)
{
	dumpExported("rms_subscribe('%s')...\n", pattern);
	if (pattern == NULL)
		return 0;	// we're OUTTA here!
	dumpExported("rms_subscribe('%s')...Create()\n", pattern);
	return RMsQueue::Create(pattern);
}

RMS_EXPORT
int rms_wait_bytes(int id, char tag[], int* tagN, unsigned char data[], int* dataN, int flags)
{
	dumpExported("rms_wait_bytes(%d...%d)...\n", id, flags);
	if (!isValidQueue(id))
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetTag) && tagN == NULL)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && dataN == NULL)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_bytes(%d...%d)...Wait()\n", id, flags);
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, dataN, flags);
}

RMS_EXPORT
int rms_wait_ieee(int id, char tag[], int* tagN, rms_ieee* data, int flags)
{
	dumpExported("rms_wait_ieee(%d...%d)...\n", id, flags);
	if (!isValidQueue(id))
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetTag) && tagN == NULL)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && data == NULL)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_ieee(%d...%d)...Wait()\n", id, flags);
	int dataN = sizeof rms_ieee;
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, &dataN, flags);
}

RMS_EXPORT
int rms_wait_int32(int id, char tag[], int* tagN, rms_int32* data, int flags)
{
	dumpExported("rms_wait_int32(%d...%d)...\n", id, flags);
	if (!isValidQueue(id))
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetTag) && tagN == NULL)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && data == NULL)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_int32(%d...%d)...Wait()\n", id, flags);
	int dataN = sizeof rms_int32;
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, &dataN, flags);
}

RMS_EXPORT
int rms_wait_int64(int id, char tag[], int* tagN, rms_int64* data, int flags)
{
	dumpExported("rms_wait_int64(%d...%d)...\n", id, flags);
	if (!isValidQueue(id))
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetTag) && tagN == NULL)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && data == NULL)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_int64(%d...%d)...Wait()\n", id, flags);
	int dataN = sizeof rms_int64;
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, &dataN, flags);
}

RMS_EXPORT
int rms_wait_string(int id, char tag[], int* tagN, char data[], int* dataN, int flags)
{
	dumpExported("rms_wait_string(%d...%d)...\n", id, flags);
	if (!isValidQueue(id))
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetTag) && tagN == NULL)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && dataN == NULL)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_string(%d...%d)...Wait()\n", id, flags);
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, dataN, flags);
}

/*
	Add the supplied page to the list of active [subscription] queues.
*/
int RMsRoot::AddQueue(int pg)
{
	dumpRoot("AddQueue(%d)...\n", pg);
	lock_guard<RSpinLockEx> acquire(spin);
	((RMsQueue*)pg2xp(pg))->prev = queueTail;
	((RMsQueue*)pg2xp(pg))->next = 0;
	if (queueTail)
		((RMsQueue*)pg2xp(queueTail))->next = pg;
	else
		queueHead = pg;
	queueTail = pg;
	dumpRoot("AddQueue(%d)...h=%d,t=%d\n", pg, queueHead, queueTail);
	return pg;
}

/*
	Allocate a new page - 1st try recycling, then try to extend the
	high-water mark, then attempt to COMMIT another PageIncrement pages.
*/
int RMsRoot::AllocPage()
{
	dumpRoot("AllocPage()...\n");
	int pg;
	lock_guard<RSpinLockEx> acquire(spin);
	if (pageFree)
		dumpRoot("AllocPage()...using freed page\n"),
		pg = pageFree, pageFree = *(int*)pg2xp(pg);
	else {
		dumpRoot("AllocPage()...extending high-water mark\n");
		if (high == committed) {
			if (committed >= pages) {
				dumpRoot("AllocPage()...Attempted to EXCEED LIMIT of %d pages!\n", pages);
				return 0;	// we're OUTTA here!
			}
			dumpRoot("AllocPage()...committing %d pages\n", PageIncrement);
			if (::VirtualAlloc(rmsB + high*4096, PageIncrement*4096,
				MEM_COMMIT, PAGE_READWRITE) == NULL) {
				dumpRoot("AllocPage()...COMMIT FAILED!\n");
				return 0;	// we're OUTTA here!
			}
			committed += PageIncrement;
		}
		pg = high++;
	}
	dumpRoot("AllocPage()...%d\n", pg);
	return pg;
}

/*
	Allocate storage for data of the requested type and return an RMs ptr -
	if type is RMsTypeAuto, infer actual type from length parameter.

	An RMs ptr contains a page #, a type, a page offset, and a byte length:

	 31    16 15  12 11      loOff hiLen       0
	+--------+------+-------------+-------------+
	| page # | type | page offset | item length |
	+--------+------+---------------------------+

	Notes
	=====
	1. Implementation limits:
	   Page length =     4 KiB
	   Pages = 64 Ki = 256 MiB
	   Min item length = 4 B   (1024 items)
	   Max item length = 4 KiB (1 item)
	2. All items in a page are SAME length
	3. Length coding of 0 means full-sized
	4. Length is fixed for RMs primitives:
	   RMsInt32 = 4 B ("int")
	   RMsInt64 = 8 B ("long long")
	   RMsIeee =  8 B ("double")
	5. Length is variable for RMs byte vec
	   RMs type  1 = 1-4 B
	   RMs type  2 = 1-8 B
	   RMs type  3 = 1-16 B
	   :
	   RMs type 11 = 1-4 KiB
*/
rms_ptr_t RMsRoot::AllocRP(int ty, int n)
{
	dumpRoot("AllocRP(%d, %d)...\n", ty, n);
	rms_ptr_t rp = 0;
	if (ty == RMsTypeAuto)
		ty = n2ty(n);
	lock_guard<RSpinLockEx> acquire(spin);
	if (!typeFree[ty]) {
		const int pg = AllocPage();
		if (pg)
			initTypedPageAsFree(pg, ty);
	}
	if (typeFree[ty])
		rp = rp2rp(typeFree[ty], n), typeFree[ty] = *(rms_ptr_t*)rp2xp(rp);
	dumpRoot("AllocRP(%d, %d)...%08x\n", ty, n, rp);
	return rp;
}

#if defined(CHECK_ALLOC) || defined(CHECK_ALLOC_FULL)
int RMsRoot::CheckAlloc(int pg)
{
	if (pg <= 0 || pg >= high) {
		dumpCheck("RMsRoot::CheckAlloc(%d)... is BOGUS (illegal value)\n", pg);
		return 0;	// we're OUTTA here!
	}
	int status = 1;
#ifdef CHECK_ALLOC_FULL
	lock_guard<RSpinLockEx> acquire(spin);
	for (int p = pageFree; p; p = *(int*)pg2xp(p))
		if (p == pg)
			dumpCheck("RMsRoot::CheckAlloc(%d)... is BOGUS (on FREE list)\n", pg),
				status = 0;	// indicate failure
#endif
	return status;
}
#endif

#ifdef CHECK_QUEUE_FULL
int RMsRoot::CheckQueue(int pg)
{
	int n = 0;
	lock_guard<RSpinLockEx> acquire(spin);
	for (int q = queueHead; q; q = ((RMsQueue*)pg2xp(q))->next)
		if (q == pg)
			n++;
	if (!n)
		dumpCheck("RMsRoot::CheckQueue(%d)... is BOGUS (NOT found)\n", pg);
	else if (n > 1)
		dumpCheck("RMsRoot::CheckQueue(%d)... is BOGUS (present %d times)\n", pg, n);
	return n == 1;
}
#endif

/*
	Add this tag/typed-data pair to any subscription queue that "matches" the tag.
*/
int RMsRoot::Distribute(const char* tag, const void* data, int n, int ty)
{
	lock_guard<RSpinLockEx> acquire(spin);
	for (int q = queueHead; q; q = ((RMsQueue*)pg2xp(q))->next)
		if (((RMsQueue*)pg2xp(q))->Match(tag) > 0)
			((RMsQueue*)pg2xp(q))->Append(tag, data, n, ty);
	return 1;	// indicate success (what if Append failed?)
}

/*
	Free a page and make it available for re-use by adding at the front of the
	free pages list.
*/
void RMsRoot::FreePage(int pg)
{
	dumpRoot("FreePage(%d)...\n", pg);
	lock_guard<RSpinLockEx> acquire(spin);
	*(int*)pg2xp(pg) = pageFree, pageFree = pg;
	dumpRoot("FreePage(%d)...DONE\n", pg);
}

/*
	Free both RMs ptrs from a tag/data pair (data may be empty) by adding each
	to front of their respective typed free lists.
*/
void RMsRoot::FreePair(td_pair_t p)
{
	dumpRoot("FreePair(%x:%x)...\n", p.tag, p.data);
	lock_guard<RSpinLockEx> acquire(spin);
	free_rp(p.tag);
	if (p.data)
		free_rp(p.data);
	dumpRoot("FreePair(%x:%x)...DONE\n", p.tag, p.data);
}

/*
	Free a single RMs ptr value by adding to front of typed free list.
*/
void RMsRoot::FreeRP(rms_ptr_t rp)
{
	dumpRoot("FreeRP(%08x)...\n", rp);
	lock_guard<RSpinLockEx> acquire(spin);
	free_rp(rp);
	dumpRoot("FreeRP(%08x)...DONE\n", rp);
}

/*
	Perform non-constructor setup of RMs "root" data structure.

	N.B. - no mutex locks are needed, since the world doesn't exist yet
*/
int RMsRoot::Initialize(int np)
{
	dumpRoot("Initialize(%d)...\n", np);
	// do "real" initial COMMIT (re-commits 1st page)
	if (!::VirtualAlloc(rmsB, PageIncrement*4096, MEM_COMMIT, PAGE_READWRITE))
		return 0;	// indicate failure
	pages = np, committed = PageIncrement;
	high = 1;	// we ARE the 1st page...
	dumpRoot("Initialize(%d)...DONE\n", np);
	return 1;	// indicate success
}

/*
	Initialize supplied page as a typed linked list of RMs ptrs for the given
	type, with the byte length of each entry in the list being determined by
	this type - see the entry for RMsRoot::AllocRP for details.

	N.B. - the type MUST be supplied, there is no length to use for inference
	N.B.2 - call with RMsRoot mutex LOCKED!
*/
void RMsRoot::initTypedPageAsFree(int pg, int ty)
{
	dumpRoot("initTypedPageAsFree(%d,%d)...\n", pg, ty);
	char* p = (char*)pg2xp(pg);
	// compute max length of typed item...
	const int d = 2 << ty2logM1(ty);
	// ... link page of these from back to front...
	for (int o = 4096 - d, n = 0; o >= 0; n = o, o -= d)
		*(int*)(p + o) = n? make_rp(pg, ty, n): 0;
	// finally, store as "head" of type's free list
	typeFree[ty] = make_rp(pg, ty, 0);
	dumpRoot("initTypedPageAsFree(%d,%d)...%08x\n", pg, ty, typeFree[ty]);
}

/*
	Remove the supplied page from the list of active [subscription] queues.
*/
void RMsRoot::RemoveQueue(int pg)
{
	dumpRoot("RemoveQueue(%d)...\n", pg);
	lock_guard<RSpinLockEx> acquire(spin);
	const int prev = ((RMsQueue*)pg2xp(pg))->prev;
	const int next = ((RMsQueue*)pg2xp(pg))->next;
	if (prev)
		((RMsQueue*)pg2xp(prev))->next = next;
	else
		queueHead = next;
	if (next)
		((RMsQueue*)pg2xp(next))->prev = prev;
	else
		queueTail = prev;
	dumpRoot("RemoveQueue(%d)...h=%d,t=%d\n", pg, queueHead, queueTail);
}

/*
	Publish tag/data to this subscription queue (I) - perform final parameter
	validation, and all the higher-level logical checks, allocations, and
	data copying to prepare the "td pair" for actual enqueuing.

	N.B. - no need to acquire the mutex lock at THIS level of processing
*/
int RMsQueue::Append(const char* tag, const void* data, int n, int ty)
{
	dumpQueue("<%d>::Append('%s'...%d)...\n", xp2pg(this), tag, ty);
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	const int tN = (int)strlen(tag) + 1;
	// N.B. - limit tN to 2 <= tN <= 4096!
	if (tN < 2 || tN > 4096)
		return 0;	// we're OUTTA here!
	rms_ptr_t tRP = rmsRoot->AllocRP(RMsTypeAuto, tN);
	if (!tRP)
		return 0;	// we're OUTTA here!
	memcpy(rp2xp(tRP), tag, tN);
	rms_ptr_t dRP = 0;
	if (n) {
		if (!(dRP = rmsRoot->AllocRP(ty, n))) {
			rmsRoot->FreeRP(tRP);
			return 0;	// we're OUTTA here!
		}
		memcpy(rp2xp(dRP), data, n);
	}
	dumpQueue("<%d>::Append('%s'...%d)...append()\n", xp2pg(this), tag, ty);
	return append(tRP, dRP);
}

/*
	Publish tag/data to this subscription queue (II) - perform low-level checks
	and enqueue the td pair (remember, data may not be present), then signal
	our semaphore to reflect this publication.
*/
int RMsQueue::append(rms_ptr_t tag, rms_ptr_t data)
{
	dumpQueue("<%d>::append(%x:%x)...\n", xp2pg(this), tag, data);
	int pg, pi;
	const td_pair_t td = {tag, data};
	lock_guard<RSpinLock> acquire(spin);
	if (state) {
		rmsRoot->FreePair(td);
		return RMsStatusSignaled;	// early out; indicate "signaled"
	}
	const int status = checkWrite(pg, pi);
	if (status) {
		if (write < NQuick)
			quickE[write] = td;
		else
			((pq_pag_t*)pg2xp(pg))->pqTD[pi] = td;
		write++;
		semaphore.signal();
	} else
		rmsRoot->FreePair(td);
	dumpQueue("<%d>::append(%x:%x)...%d\n", xp2pg(this), tag, data, status);
	return status;
}

/*
	Ensure the prospective queue addition is either "quick" (the queue control
	data has room for NQuick entries without allocating any "indirect" pages),
	there is room on the current indirect page, or we are able to allocate an
	additional indirect queue page.

	N.B. - call with RMsQueue mutex LOCKED!
*/
int RMsQueue::checkWrite(int& pg, int& pi)
{
	if (write >= NQuick) {
		const int p = qp2pq(write);
		if (p >= pages) {
			if (pages >= NQPage)
				return 0;	// we're OUTTA here!
			const int pg = rmsRoot->AllocPage();
			if (!pg)
				return 0;	// we're OUTTA here!
			pageE[pages++] = pg;
		}
		pg = pageE[p], pi = qp2pi(write);
	}
	return 1;	// indicate success
}

/*
	Fully shut down this queue, releasing all resources, including any undelivered
	tag/data pairs and their associated storage.
*/
int RMsQueue::Close()
{
	const int pg = xp2pg(this);
	dumpQueue("<%d>::Close()...state=%08x\n", pg, state);
	state |= RMsStatusClosing;
	if (magic.exchange(0)) {
		rmsRoot->RemoveQueue(pg);
		Flush();
		dumpQueue("<%d>::Close()...freeing %d pages\n", pg, pages);
		for (int i = 0; i < pages; i++)
			rmsRoot->FreePage(pageE[i]);
		rmsRoot->FreeRP(pattern);
		this->~RMsQueue();
		rmsRoot->FreePage(pg);
		dumpQueue("<%d>::Close()...SUCCESS\n", pg);
	} else
		dumpQueue("<%d>::Close()...ALREADY closed\n", pg);
	return 1;	// indicate success
}

/*
	Perform non-constructor setup of RMsQueue data structure (I) - this is a
	static method, so we act like an RMsQueue "factory".
*/
int RMsQueue::Create(const char* pattern)
{
	dumpQueue("<?>::Create('%s')...\n", pattern);
	const int pg = rmsRoot->AllocPage();
	if (pg) {
		// use "placement" new!
		RMsQueue* qp = (RMsQueue*)pg2xp(pg);
		new(qp) RMsQueue();
		if (qp->initialize(pattern))
			return rmsRoot->AddQueue(pg);
		else
			rmsRoot->FreePage(pg);
	}
	return 0;	// we're OUTTA here!
}

/*
	Free any "undelivered" tag/data pairs in queue, as well as any associated
	data, finally setting the queue to a LOGICAL "empty" state.
*/
int RMsQueue::Flush()
{
	lock_guard<RSpinLock> acquire(spin);
	while (read < write) {
		td_pair_t td;
		if (read < NQuick)
			td = quickE[read];
		else
			td = ((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)];
		rmsRoot->FreePair(td), read++;
	}
	read = 0, write = 0, state = 0;
	return 1;	// indicate success
}

/*
	Perform non-constructor setup of RMsQueue data structure (II) - the primary
	concern is creating and initializing the "glob" used for our "subscription".
*/
int RMsQueue::initialize(const char* pattern)
{
	if (!pattern[0])
		return 0;	// we're OUTTA here!
	rglob::compiler c;
	c.compile(pattern);
	const auto fsm = c.machine();
	const auto n = fsm.length() + 1;
	const rms_ptr_t rp = rmsRoot->AllocRP(RMsTypeAuto, (int)n);
	if (!rp)
		return 0;	// we're OUTTA here!
	memcpy(rp2xp(rp), fsm.c_str(), n);
	RMsQueue::pattern = rp;
	return 1;	// indicate success
}

/*
	Compare a string "tag" that is being published with our subscription's
	"glob", returning the match/fail status.
*/
int RMsQueue::Match(const char* tag) const
{
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	rglob::matcher m((const char*)rp2xp(pattern));
	if (m.match(tag))
		return 1;	// indicate match
	return 0;
}

/*
	Perform out-of-band "signaling" of our queue - typically used to set an
	"end of data" state when we want to shut down the queue, and let any readers
	know that no additional data will be forthcoming.
*/
int RMsQueue::Signal(int flags)
{
	dumpQueue("<%d>::Signal(%08x)...state=%08x\n", xp2pg(this), flags, state);
	state |= flags;
	semaphore.signal();
	dumpQueue("<%d>::Signal(%08x)...state=%08x\n", xp2pg(this), flags, state);
	return 1;	// indicate success
}

#if defined(CHECK_QUEUE) || defined(CHECK_QUEUE_FULL)
int RMsQueue::Validate()
{
	if (magic != QueueMagic) {
		dumpCheck("RMsQueue<%d>::Validate()... FAILED\n", xp2pg(this));
		return 0;	// we're OUTTA here!
	}
	// figure out more tests...
	return 1;	// indicate success
}
#endif

/*
	Synchronously wait on our semaphore for data to be published that matches
	our subscription's "glob", but also respond to out-of-band signals, typically
	indicating "end of data".

	This is deprecated now in favor of the lighter-weight Wait2, but will be
	retained for some time to be used possibly for diagnostics/testing/validation
	purposes.
*/
int RMsQueue::Wait(char* tag, int* tagN, void* data, int* dataN, int flags)
{
	dumpQueue("<%d>::Wait(...%d)...\n", xp2pg(this), flags);
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	semaphore.wait();
	if (state) {
		dumpQueue("<%d>::Wait(...%d)...WAIT => SIGNALED\n", xp2pg(this), flags);
		return RMsStatusSignaled;	// early out; indicate "signaled"
	}
	dumpQueue("<%d>::Wait(...%d)...back from WAIT\n", xp2pg(this), flags);
	td_pair_t td;
	if (read < NQuick)
		td = quickE[read];
	else
		td = ((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)];
	dumpQueue("<%d>::Wait(...%d)...%x:%x\n", xp2pg(this), flags, td.tag, td.data);
	int status = flags;
	if (flags & RMsGetTag) {
		const int n = rp2n(td.tag);
		if (tag) {
			if (n <= *tagN)
				memcpy(tag, rp2xp(td.tag), n);
			else
				status ^= RMsGetTag;	// indicate length issue with TAG
		} else
			status |= 4;	// indicate only length data returned
		*tagN = n;	// indicate actual length
	}
	if (flags & RMsGetData) {
		const int n = rp2n(td.data);
		if (data) {
			if (n <= *dataN)
				memcpy(data, rp2xp(td.data), n);
			else
				status ^= RMsGetData;	// indicate length issue with DATA
		} else
			status |= 4;	// indicate only length data returned
		*dataN = n;	// indicate actual length
	}
	// "consume" element IFF all requested tag info/data bits returned
	if (status == flags) {
		rmsRoot->FreePair(td), read++;
		if (read == write) {
			lock_guard<RSpinLock> acquire(spin);
			if (read == write)
				read = 0, write = 0;
		}
	}
	dumpQueue("<%d>::Wait('%s'...%d)...%d\n", xp2pg(this), tag, flags, status);
	return status;
}