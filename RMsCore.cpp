/*
	RMsCore.cpp - "core" functionality of the RMs messaging system

	Copyright(c) 2004-2023, Robert Roessler
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

#if defined(WIN32) || defined(_WIN32)
#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <windows.h>
#else
#include <stdlib.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/mman.h>
#endif

#include <cstdio>
#include <sstream>
#include <format>
#include "rms.h"
#include "rglob.h"

using namespace std;
using namespace rms;

char* rms::rmsB = nullptr;				// shared memory view pointer(s)
RMsRoot* rms::rmsRoot = nullptr;		// shared "root" object
static auto NQPage = 0;					// # of pages of RMsQueue "extras"

//	control run-time state display and testing
//	N.B. - typically ALL false in production!!
constexpr auto qa_dump_all = false;		// (make it easy to turn on ALL display)
constexpr auto qa_dump_root = qa_dump_all || false;
constexpr auto qa_dump_queue = qa_dump_all || false;
constexpr auto qa_dump_exported = qa_dump_all || false;

constexpr auto qa_check_all = false;	// (make it easy to turn on ALL testing)
constexpr auto qa_check_alloc = qa_check_all || false;
constexpr auto qa_check_alloc_full = qa_check_all || false;
constexpr auto qa_check_queue = qa_check_all || false;
constexpr auto qa_check_queue_full = qa_check_all || false;
constexpr auto qa_check_nothing = !(qa_check_alloc || qa_check_alloc_full || qa_check_queue || qa_check_queue_full);

//	OS-specific trace function to use
#if defined(WIN32) || defined(_WIN32)
#define rms_trace(buf) ::OutputDebugString(buf)
#else
#define rms_trace(buf) fputs(buf, stderr)
#endif

//	custom c++20 "formatter" for std::thread::id values
//	N.B. - MAY not be needed in c++23(?)
template<>
struct formatter<thread::id> : formatter<string_view> {
	auto format(const thread::id& id, format_context& ctx) {
		ostringstream s;
		s << hex << id;
		return formatter<string_view>::format(s.str(), ctx);
	}
};

//	custom c++20 "formatter" for RMsType values
template<>
struct formatter<RMsType> : formatter<string_view> {
	auto format(const RMsType& ty, format_context& ctx) {
		string o{ std::format("{}:", (int)ty) };
		switch (ty) {
		case RMsType::Auto: o += "Auto"; break;
		case RMsType::Int32: o += "Int32"; break;
		case RMsType::Int64: o += "Int64"; break;
		case RMsType::Ieee: o += "Ieee"; break;
		case RMsType::Reserved: o += "Reserved"; break;
		default:
			format_to(back_inserter(o), "Bytes[{}]", 2 << (int)ty);
		}
		return formatter<string_view>::format(o, ctx);
	}
};

//	Dump root object info
template <typename... ARGS>
static void dumpRoot(string_view fmt, const ARGS&... args) {
	if constexpr (qa_dump_root) {
		// construct trace message prefix including current thread's thread::id
		auto tracePre = []() {
			return format("RMsRoot[{}]::", this_thread::get_id());
		};
		rms_trace(vformat(tracePre().append(fmt), make_format_args(args...)).c_str());
	}
}

//	Dump queue object info (SAME implementation as dumpRoot, but independent)
template <typename... ARGS>
static void dumpQueue(string_view fmt, const ARGS&... args) {
	if constexpr (qa_dump_queue) {
		// construct trace message prefix including current thread's thread::id
		auto tracePre = []() {
			return format("RMsRoot[{}]::", this_thread::get_id());
		};
		rms_trace(vformat(tracePre().append(fmt), make_format_args(args...)).c_str());
	}
}

//	Dump exported low-level interface info
template <typename... ARGS>
static void dumpExported(string_view fmt, const ARGS&... args) noexcept(false) {
	if constexpr (qa_dump_exported)
		rms_trace(vformat(fmt, make_format_args(args...)).c_str());
}

//	Dump "check" info
template <typename... ARGS>
static void dumpCheck(string_view fmt, const ARGS&... args) {
	if constexpr (qa_check_alloc_full || qa_check_queue_full)
		rms_trace(vformat(fmt, make_format_args(args...)).c_str());
}

void rms::initialize(int np)
{
	rms_initialize(np);
}

inline static bool rms::isValidQueue(int pg)
{
	if constexpr (qa_check_alloc || qa_check_alloc_full)
		if (!rmsRoot->CheckAlloc(pg))
			return false;	// we're OUTTA here!
	if constexpr (!qa_check_nothing)
		if (pg <= 0 || pg >= rmsRoot->high)
			return false;	// we're OUTTA here!
	if constexpr (qa_check_queue_full) {
		if (!rmsRoot->CheckQueue(pg))
			return false;	// we're OUTTA here!
		if (!((RMsQueue*)pg2xp(pg))->Validate())
			return false;	// we're OUTTA here!
	} else if constexpr (qa_check_queue) {
		if (!((RMsQueue*)pg2xp(pg))->Validate())
			return false;	// we're OUTTA here!
	} else if constexpr (!qa_check_nothing)
		if (((RMsQueue*)pg2xp(pg))->magic != QueueMagic)
			return false;	// we're OUTTA here!
	return true;	// indicate success
}

/*
	With the exception of "rms_initialize()", the "rms_xxx" entry points are
	meant to provide "C" access for a lower-level interface to RMs, and in most
	cases simply add some tracing and argument-checking prior to invoking the
	"real" object-oriented implementation interface.

	Both these and the public calls on the RMsRoot and RMsQueue objects are
	considered deprecated for general use, with the suggested interface to RMs
	being exemplified by the "rms::publisher" and "rms::subscription" classes.
*/

RMS_EXPORT
void rms_close(int id) noexcept(false)
{
	dumpExported("rms_close({})...\n", id);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_close passed invalid queue #") + to_string(id));
	dumpExported("rms_close({})...Close()\n", id);
	return ((RMsQueue*)pg2xp(id))->Close();
}

RMS_EXPORT
void rms_flush(int id) noexcept(false)
{
	dumpExported("rms_flush({})...\n", id);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_flush passed invalid queue #") + to_string(id));
	dumpExported("rms_flush({})...Flush()\n", id);
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
void rms_initialize(int np) noexcept(false)
{
	dumpExported("rms_initialize({})...\n", np);
#if defined(WIN32) || defined(_WIN32)
	static HANDLE rmsM = nullptr;				// shared memory mapping object(s)
	rmsM = ::CreateFileMapping(INVALID_HANDLE_VALUE, nullptr, PAGE_READWRITE|SEC_RESERVE,
		0, np*4096, nullptr);
	if (rmsM == nullptr)
		throw runtime_error("rms_initialize failed in CreateFileMapping()");
	const DWORD mapE = ::GetLastError();// (stash for later)
	// [attempt to] map view
	rmsB = (char*)::MapViewOfFile(rmsM, FILE_MAP_ALL_ACCESS, 0, 0, 0);
	if (rmsB == nullptr) {
		::CloseHandle(rmsM), rmsM = nullptr;
		throw runtime_error("rms_initialize failed in MapViewOfFile()");
	}
	// [attempt to] commit FIRST page (bootstrapping)
	if (::VirtualAlloc(rmsB, 1*4096, MEM_COMMIT, PAGE_READWRITE) == nullptr) {
		::UnmapViewOfFile(rmsB), rmsB = nullptr;
		::CloseHandle(rmsM), rmsM = nullptr;
		throw runtime_error("rms_initialize failed in VirtualAlloc()");
	}
	// init mapping AS REQUIRED
	if (mapE != ERROR_ALREADY_EXISTS) {
		// use "placement" new!
		new(rmsB) RMsRoot();
		rmsRoot = (RMsRoot*)rmsB;
		if (!rmsRoot->Initialize(np)) {
			::UnmapViewOfFile(rmsB), rmsB = nullptr;
			::CloseHandle(rmsM), rmsM = nullptr;
			throw runtime_error("rms_initialize failed in RMsRoot:Initialize()");
		}
	}
#else
	auto fd = shm_open("/rhps_rms_shared", O_CREAT | O_TRUNC | O_RDWR, 0666);
	if (fd == -1)
		return; // we're OUTTA here!
	auto stat = ftruncate(fd, np * 4096);
	if (stat != 0)
		return; // we're OUTTA here!
	rmsB = (char*)mmap(0, np * 4096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	close(fd);
	if (rmsB == MAP_FAILED)
		return; // we're OUTTA here!
	// use "placement" new!
	new(rmsB) RMsRoot();
	rmsRoot = (RMsRoot*)rmsB;
	if (!rmsRoot->Initialize(np)) {
		munmap(rmsB, np * 4096), rmsB = nullptr;
		shm_unlink("rhps_rms_shared");
		return;	// we're OUTTA here!
	}
#endif
	// verify [expected] invariants
	static_assert(sizeof(RMsRoot) <= 4096, "RMsRoot instance MUST be <= 4096 bytes long!");
	static_assert(sizeof(RMsQueue) <= 4096, "RMsQueue instance MUST be <= 4096 bytes long!");
	// compute any architecture-dependent values
	NQPage = (4096 - offsetof(RMsQueue, pageE)) / sizeof(unsigned short);
	dumpExported("rms_initialize({}) NQPage={}\n", np, NQPage);
	dumpExported("rms_initialize({})...SUCCESS\n", np);
}

#ifdef _DEBUG
RMS_EXPORT
int rms_is_valid_queue(int id)
{
	return isValidQueue(id) ? 1 : 0;
}
#endif

RMS_EXPORT
int rms_peek(int id) noexcept(false)
{
	dumpExported("rms_peek({})...\n", id);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_peek passed invalid queue #") + to_string(id));
	dumpExported("rms_peek({})...{}\n", id, ((RMsQueue*)pg2xp(id))->Peek());
	return ((RMsQueue*)pg2xp(id))->Peek();
}

RMS_EXPORT
void rms_publish_bytes(std::string_view tag, const unsigned char* data, size_t n) noexcept(false)
{
	dumpExported("rms_publish_bytes('{}',{:02x}...,{})...\n", tag, (data != nullptr && n > 0) ? data[0] : 0, n);
	if (tag.empty())
		return;	// we're OUTTA here!
	// N.B. - limit n to 0 <= n <= 4096!
	if (n < 0 || n > 4096)
		throw invalid_argument(string("rms_publish_bytes passed invalid length ") + to_string(n));
	dumpExported("rms_publish_bytes('{}'...)...Distribute()\n", tag);
	rmsRoot->Distribute(tag, data, n);
}

RMS_EXPORT
void rms_publish_ieee(std::string_view tag, rms_ieee data)
{
	dumpExported("rms_publish_ieee('{}',{})...\n", tag, data);
	if (tag.empty())
		return;	// we're OUTTA here!
	dumpExported("rms_publish_ieee('{}'...)...Distribute()\n", tag);
	rmsRoot->Distribute(tag, &data, sizeof(rms_ieee), RMsType::Ieee);
}

RMS_EXPORT
void rms_publish_int32(std::string_view tag, rms_int32 data)
{
	dumpExported("rms_publish_int32('{}',{})...\n", tag, data);
	if (tag.empty())
		return;	// we're OUTTA here!
	dumpExported("rms_publish_int32('{}'...)...Distribute()\n", tag);
	rmsRoot->Distribute(tag, &data, sizeof(rms_int32), RMsType::Int32);
}

RMS_EXPORT
void rms_publish_int64(std::string_view tag, rms_int64 data)
{
	dumpExported("rms_publish_int64('{}',{})...\n", tag, data);
	if (tag.empty())
		return;	// we're OUTTA here!
	dumpExported("rms_publish_int64('{}'...)...Distribute()\n", tag);
	rmsRoot->Distribute(tag, &data, sizeof(rms_int64), RMsType::Int64);
}

RMS_EXPORT
/*
	The writing (and subsequent reading) of the std::intptr_t to first publish
	[and then read back] pointer values is dependent on the queue consumers
	mapping the addresses identically to the publishers... this is automatic in
	the case of different threads in the same process, and *could* be true with
	identically mapped shared memory segments in the multi-process case - but as
	this isn't necessarily a given, a word to the wise should be sufficient.
*/
void rms_publish_intptr(std::string_view tag, rms_intptr data)
{
	dumpExported("rms_publish_intptr('{}',{})...\n", tag, data);
	if (tag.empty())
		return;	// we're OUTTA here!
	dumpExported("rms_publish_intptr('{}'...)...Distribute()\n", tag);
	rmsRoot->Distribute(tag, &data, sizeof(rms_intptr), RMsType::IntPtr);
}

RMS_EXPORT
void rms_publish_string(std::string_view tag, std::string_view data) noexcept(false)
{
	dumpExported("rms_publish_string('{}','{:.8}...')...()\n", tag, data);
	if (tag.empty())
		return;	// we're OUTTA here!
	// N.B. - limit any data to 0 <= strlen(data) <= 4095!
	if (data.size() > 4096)
		throw invalid_argument(string("rms_publish_string passed invalid length ") + to_string(data.size()));
	dumpExported("rms_publish_string('{}'...)...Distribute()\n", tag);
	rmsRoot->Distribute(tag, data.data(), (int)data.size());
}

RMS_EXPORT
void rms_signal(int id, int flags) noexcept(false)
{
	dumpExported("rms_signal({},{:08x})...\n", id, flags);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_signal passed invalid queue #") + to_string(id));
	dumpExported("rms_signal({},{:08x})...Signal()\n", id, flags);
	return ((RMsQueue*)pg2xp(id))->Signal(flags);
}

RMS_EXPORT
int rms_subscribe(std::string_view pattern) noexcept(false)
{
	dumpExported("rms_subscribe({})...\n", pattern);
	if (pattern.empty())
		throw invalid_argument(string("rms_subscribe passed empty pattern"));
	dumpExported("rms_subscribe({})...Create()\n", pattern);
	return RMsQueue::Create(pattern);
}

RMS_EXPORT
int rms_wait_bytes(int id, char tag[], size_t* tagN, unsigned char data[], size_t* dataN, int flags)  noexcept(false)
{
	dumpExported("rms_wait_bytes({}...{})...\n", id, flags);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_wait_bytes passed invalid queue #") + to_string(id));
	if ((flags & RMsGetTag) && tagN == nullptr)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && dataN == nullptr)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_bytes({}...{})...Wait()\n", id, flags);
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, dataN, flags);
}

RMS_EXPORT
int rms_wait_ieee(int id, char tag[], size_t* tagN, rms_ieee* data, int flags) noexcept(false)
{
	dumpExported("rms_wait_ieee({}...{})...\n", id, flags);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_wait_ieee passed invalid queue #") + to_string(id));
	if ((flags & RMsGetTag) && tagN == nullptr)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && data == nullptr)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_ieee({}...{})...Wait()\n", id, flags);
	auto dataN = sizeof(rms_ieee);
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, &dataN, flags);
}

RMS_EXPORT
int rms_wait_int32(int id, char tag[], size_t* tagN, rms_int32* data, int flags) noexcept(false)
{
	dumpExported("rms_wait_int32({}...{})...\n", id, flags);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_wait_int32 passed invalid queue #") + to_string(id));
	if ((flags & RMsGetTag) && tagN == nullptr)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && data == nullptr)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_int32({}...{})...Wait()\n", id, flags);
	auto dataN = sizeof(rms_int32);
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, &dataN, flags);
}

RMS_EXPORT
int rms_wait_int64(int id, char tag[], size_t* tagN, rms_int64* data, int flags) noexcept(false)
{
	dumpExported("rms_wait_int64({}...{})...\n", id, flags);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_wait_int64 passed invalid queue #") + to_string(id));
	if ((flags & RMsGetTag) && tagN == nullptr)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && data == nullptr)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_int64({}...{})...Wait()\n", id, flags);
	auto dataN = sizeof(rms_int64);
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, &dataN, flags);
}

RMS_EXPORT
/*
	(see detailed comment on rms_publish_intptr(...) fn)
*/
int rms_wait_intptr(int id, char tag[], size_t* tagN, rms_intptr* data, int flags) noexcept(false)
{
	dumpExported("rms_wait_intptr({}...{})...\n", id, flags);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_wait_int64 passed invalid queue #") + to_string(id));
	if ((flags & RMsGetTag) && tagN == nullptr)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && data == nullptr)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_intptr({}...{})...Wait()\n", id, flags);
	auto dataN = sizeof(rms_intptr);
	return ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, &dataN, flags);
}

RMS_EXPORT
int rms_wait_string(int id, char tag[], size_t* tagN, char data[], size_t* dataN, int flags) noexcept(false)
{
	dumpExported("rms_wait_string({}...{})...\n", id, flags);
	if (!isValidQueue(id))
		throw invalid_argument(string("rms_wait_string passed invalid queue #") + to_string(id));
	if ((flags & RMsGetTag) && tagN == nullptr)
		return 0;	// we're OUTTA here!
	if ((flags & RMsGetData) && dataN == nullptr)
		return 0;	// we're OUTTA here!
	dumpExported("rms_wait_string({}...{})...Wait()\n", id, flags);
	// special-case semantics for [assumed] "C" strings...
	const auto orig_dataN = *dataN;
	const auto f = ((RMsQueue*)pg2xp(id))->Wait(tag, tagN, data, dataN, flags);
	// ... iff the string data was successfully retrieved...
	if ((f & (RMsGetData | RMsOnlyLength)) == RMsGetData)
		// ... and there is still room...
		if (*dataN < orig_dataN)
			data[*dataN] = '\0';	// ... NUL-terminate!
	return f;
}

/*
	Add the supplied page to the list of active [subscription] queues.
*/
int RMsRoot::AddQueue(int pg)
{
	dumpRoot("AddQueue({})...\n", pg);
	lock_guard acquire(spin);
	((RMsQueue*)pg2xp(pg))->prev = queueTail;
	((RMsQueue*)pg2xp(pg))->next = 0;
	if (queueTail)
		((RMsQueue*)pg2xp(queueTail))->next = pg;
	else
		queueHead = pg;
	queueTail = pg;
	dumpRoot("AddQueue({})...h={},t={}\n", pg, remove_volatile_t<int>(queueHead), remove_volatile_t<int>(queueTail));
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
	lock_guard acquire(spin);
	if (pageFree)
		dumpRoot("AllocPage()...using freed page\n"),
		pg = pageFree, pageFree = *(int*)pg2xp(pg);
	else {
		dumpRoot("AllocPage()...extending high-water mark\n");
		if (high == committed) {
			if (committed >= pages) {
				dumpRoot("AllocPage()...Attempted to EXCEED LIMIT of {} pages!\n", remove_volatile_t<int>(pages));
				return 0;	// we're OUTTA here!
			}
			dumpRoot("AllocPage()...committing {} pages\n", PageIncrement);
#if defined(WIN32) || defined(_WIN32)
			if (::VirtualAlloc(rmsB + high*size_t(4096), PageIncrement*4096,
				MEM_COMMIT, PAGE_READWRITE) == nullptr) {
				dumpRoot("AllocPage()...COMMIT FAILED!\n");
				return 0;	// we're OUTTA here!
			}
#endif
			committed += PageIncrement;
		}
		pg = high++;
	}
	dumpRoot("AllocPage()...{}\n", pg);
	return pg;
}

/*
	Allocate storage for data of the requested type and return an RMs ptr -
	if type is RMsType::Auto, infer actual [string] type from length parameter.

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
	   RMsIntptr = 4 or 8 B ("int" or "long long")
	   RMsIeee =  8 B ("double")
	5. Length is variable for RMs byte vec
	   RMs type  1 = 1-4 B
	   RMs type  2 = 1-8 B
	   RMs type  3 = 1-16 B
	   :
	   RMs type 11 = 1-4 KiB
*/
rms_ptr_t RMsRoot::AllocRP(RMsType ty, size_t n)
{
	dumpRoot("AllocRP({}, {})...\n", ty, n);
	rms_ptr_t rp = 0;
	if (ty == RMsType::Auto)
		ty = n2ty(int(n));
	lock_guard acquire(spin);
	if (!typeFree[(int)ty])
		if (const auto pg = AllocPage(); pg)
			initTypedPageAsFree(pg, ty);
	if (typeFree[(int)ty])
		rp = rp2rp(typeFree[(int)ty], int(n)), typeFree[(int)ty] = *(rms_ptr_t*)rp2xp(rp);
	dumpRoot("AllocRP({}, {})...{:08x}\n", ty, n, rp);
	return rp;
}

int RMsRoot::CheckAlloc(int pg) const
{
	if constexpr (qa_check_alloc)
		if (pg <= 0 || pg >= high) {
			dumpCheck("RMsRoot::CheckAlloc({})... is BOGUS (illegal value)\n", pg);
			return 0;	// we're OUTTA here!
		}
	auto status = 1;
	if constexpr (qa_check_alloc_full) {
		lock_guard<rms::RSpinLockEx> acquire(remove_const_t<rms::RSpinLockEx>(spin));
		for (auto p = pageFree; p; p = *(int*)pg2xp(p))
			if (p == pg)
				dumpCheck("RMsRoot::CheckAlloc({})... is BOGUS (on FREE list)\n", pg),
				status = 0;	// indicate failure
	}
	return status;
}

int RMsRoot::CheckQueue(int pg) const
{
	auto n = 0;
	if constexpr (qa_check_queue_full) {
		lock_guard<rms::RSpinLockEx> acquire(remove_const_t<rms::RSpinLockEx>(spin));
		for (auto q = queueHead; q; q = ((RMsQueue*)pg2xp(q))->next)
			if (q == pg)
				n++;
		if (!n)
			dumpCheck("RMsRoot::CheckQueue({})... is BOGUS (NOT found)\n", pg);
		else if (n > 1)
			dumpCheck("RMsRoot::CheckQueue({})... is BOGUS (present {} times)\n", pg, n);
	}
	return n == 1 ? 1 : 0;
}

/*
	Add this tag/typed-data pair to any subscription queue that "matches" the tag.
*/
void RMsRoot::Distribute(std::string_view tag, const void* data, size_t n, RMsType ty)
{
	lock_guard acquire(spin);
	for (auto q = queueHead; q; q = ((RMsQueue*)pg2xp(q))->next)
		if (((RMsQueue*)pg2xp(q))->Match(tag) > 0)
			((RMsQueue*)pg2xp(q))->Append(tag, data, n, ty);
}

/*
	Free a page and make it available for re-use by adding at the front of the
	free pages list.
*/
void RMsRoot::FreePage(int pg)
{
	dumpRoot("FreePage({})...\n", pg);
	lock_guard acquire(spin);
	*(int*)pg2xp(pg) = pageFree, pageFree = pg;
	dumpRoot("FreePage({})...DONE\n", pg);
}

/*
	Free both RMs ptrs from a tag/data pair (data may be empty) by adding each
	to front of their respective typed free lists.
*/
void RMsRoot::FreePair(td_pair_t p)
{
	dumpRoot("FreePair({:x}:{:x})...\n", p.tag, p.data);
	lock_guard acquire(spin);
	free_rp(p.tag);
	if (p.data)
		free_rp(p.data);
	dumpRoot("FreePair({:x}:{:x})...DONE\n", p.tag, p.data);
}

/*
	Free a single RMs ptr value by adding to front of typed free list.
*/
void RMsRoot::FreeRP(rms_ptr_t rp)
{
	dumpRoot("FreeRP({:08x})...\n", rp);
	lock_guard acquire(spin);
	free_rp(rp);
	dumpRoot("FreeRP({:08x})...DONE\n", rp);
}

/*
	Perform non-constructor setup of RMs "root" data structure.

	N.B. - no mutex locks are needed, since the world doesn't exist yet
*/
bool RMsRoot::Initialize(int np)
{
	dumpRoot("Initialize({})...\n", np);
	// do "real" initial COMMIT (re-commits 1st page)
#if defined(WIN32) || defined(_WIN32)
	if (!::VirtualAlloc(rmsB, PageIncrement*4096, MEM_COMMIT, PAGE_READWRITE))
		return false;	// indicate failure
#endif
	pages = np, committed = PageIncrement;
	high = 1;	// we ARE the 1st page...
	dumpRoot("Initialize({})...DONE\n", np);
	return true;	// indicate success
}

/*
	Initialize supplied page as a typed linked list of RMs ptrs for the given
	type, with the byte length of each entry in the list being determined by
	this type - see the entry for RMsRoot::AllocRP for details.

	N.B. - the type MUST be supplied, there is no length to use for inference
	N.B.2 - call with RMsRoot mutex LOCKED!
*/
void RMsRoot::initTypedPageAsFree(int pg, RMsType ty)
{
	dumpRoot("initTypedPageAsFree({},{})...\n", pg, ty);
	auto p = (char*)pg2xp(pg);
	// compute max length of typed item...
	const auto d = 2 << ty2logM1(ty);
	// ... link page of these from back to front...
	for (auto o = 4096 - d, n = 0; o >= 0; n = o, o -= d)
		*(rms_ptr_t*)(p + o) = n ? make_rp(pg, ty, n) : 0;
	// finally, store as "head" of type's free list
	typeFree[(int)ty] = make_rp(pg, ty, 0);
	dumpRoot("initTypedPageAsFree({},{})...{:08x}\n", pg, ty, typeFree[(int)ty]);
}

/*
	Remove the supplied page from the list of active [subscription] queues.
*/
void RMsRoot::RemoveQueue(int pg)
{
	dumpRoot("RemoveQueue({})...\n", pg);
	lock_guard acquire(spin);
	const auto prev = ((RMsQueue*)pg2xp(pg))->prev;
	const auto next = ((RMsQueue*)pg2xp(pg))->next;
	if (prev)
		((RMsQueue*)pg2xp(prev))->next = next;
	else
		queueHead = next;
	if (next)
		((RMsQueue*)pg2xp(next))->prev = prev;
	else
		queueTail = prev;
	dumpRoot("RemoveQueue({})...h={},t={}\n", pg, remove_volatile_t<int>(queueHead), remove_volatile_t<int>(queueTail));
}

/*
	Perform any needed shutdown / resource-freeing operations on RMsQueue object
	after first removing from the set of active queue objects.
*/
RMsQueue::~RMsQueue()
{
	const auto pg = xp2pg(this);
	dumpQueue("<{}>::~RMsQueue()...removing from list of active queues\n", pg);
	rmsRoot->RemoveQueue(pg);
	Flush();
	dumpQueue("<{}>::~RMsQueue()...freeing {} indirect pages\n", pg, remove_volatile_t<int>(pages));
	for (auto i = 0; i < pages; i++)
		rmsRoot->FreePage(pageE[i]);
	rmsRoot->FreeRP(pattern);
	rmsRoot->FreePage(pg);
}

/*
	Publish tag/data to this subscription queue (I) - perform final parameter
	validation, and all the higher-level logical checks, allocations, and
	data copying to prepare the "td pair" for actual enqueuing.

	N.B. - there is an open architectural question here: since Append (and its
	workhorse append) is only called as part of a publish broadcast (aka "fire
	and forget") operation, there isn't a clear path for returning status... so
	we settle for detecting when we shouldn't proceed further, and then don't.
	N.B.2 - no need to acquire the mutex lock at THIS level of processing
*/
void RMsQueue::Append(std::string_view tag, const void* data, size_t n, RMsType ty)
{
	dumpQueue("<{}>::Append('{}'...{})...\n", xp2pg(this), tag, ty);
	if (state)
		return;	// early out; "signaled"
	const auto tN = tag.size();
	// N.B. - limit tN to 1 <= tN <= 4096!
	if (tN < 1 || tN > 4096)
		return;	// we're OUTTA here!
	const auto tRP = rmsRoot->AllocRP(RMsType::Auto, tN);
	if (!tRP)
		return;	// we're OUTTA here!
	memcpy(rp2xp(tRP), tag.data(), tN);
	rms_ptr_t dRP = 0;
	if (n > 0) {
		if (!(dRP = rmsRoot->AllocRP(ty, n)))
			return rmsRoot->FreeRP(tRP);	// we're OUTTA here!
		memcpy(rp2xp(dRP), data, n);
	}
	dumpQueue("<{}>::Append('{}'...{})...append()\n", xp2pg(this), tag, ty);
	append(tRP, dRP);
}

/*
	Publish tag/data to this subscription queue (II) - perform low-level checks
	and enqueue the td pair (remember, data may not be present), then signal
	our semaphore to reflect this publication.
*/
void RMsQueue::append(rms_ptr_t tag, rms_ptr_t data)
{
	dumpQueue("<{}>::append({:x}:{:x})...{}\n", xp2pg(this), tag, data, remove_volatile_t<int>(write));
	const td_pair_t td{ tag, data };
	lock_guard acquire(spin);
	if (state)
		return rmsRoot->FreePair(td);	// early out; "signaled"
	if (const auto [status, pg, pi] = checkWrite(); status)
		(write < NQuick ? quickE[write] : ((pq_pag_t*)pg2xp(pg))->pqTD[pi]) = td,
			write++, semaphore.signal();
	else
		rmsRoot->FreePair(td);
	dumpQueue("<{}>::append({:x}:{:x})...{}\n", xp2pg(this), tag, data, remove_volatile_t<int>(write));
}

/*
	Ensure the prospective queue addition is either "quick" (the queue control
	data has room for NQuick entries without allocating any "indirect" pages),
	there is room on the current indirect page, or we are able to allocate an
	additional indirect queue page.

	N.B. - call with RMsQueue mutex LOCKED!
*/
std::tuple<bool, int, int> RMsQueue::checkWrite()
{
	if (write < NQuick)
		return { true, 0, 0 };	// write qualifies for "fast-path" handling
	const auto p = qp2pq(write);
	if (p >= pages) {
		if (pages >= NQPage)
			return { false, 0, 0 };	// we're OUTTA here!
		const auto iqp = rmsRoot->AllocPage();
		if (!iqp)
			return { false, 0, 0 };	// we're OUTTA here!
		pageE[pages++] = iqp;
	}
	return { true, pageE[p], qp2pi(write) };
}

/*
	Fully shut down this queue, releasing all resources, including any undelivered
	tag/data pairs and their associated storage.
*/
void RMsQueue::Close()
{
	const auto pg = xp2pg(this);
	dumpQueue("<{}>::Close()...state={:08x}\n", pg, int(state));
	state |= RMsStatusClosing;
	if (magic.exchange(0)) {
		this->~RMsQueue();
		dumpQueue("<{}>::Close()...SUCCESS\n", pg);
	} else
		dumpQueue("<{}>::Close()...ALREADY closed\n", pg);
}

/*
	Perform non-constructor setup of RMsQueue data structure (I) - this is a
	static method, so we act like an RMsQueue "factory".
*/
int RMsQueue::Create(std::string_view pattern)
{
	dumpQueue("<?>::Create('{}')...\n", pattern);
	if (const auto pg = rmsRoot->AllocPage(); pg) {
		// use "placement" new!
		auto qp = (RMsQueue*)pg2xp(pg);
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
void RMsQueue::Flush()
{
	lock_guard acquire(spin);
	while (read < write)
		rmsRoot->FreePair(read < NQuick ?
			quickE[read] :
			((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)]), read++;
	read = 0, write = 0, state = 0;
}

/*
	Perform non-constructor setup of RMsQueue data structure (II) - the primary
	concern is creating and initializing the "glob" used for our "subscription".
*/
bool RMsQueue::initialize(std::string_view pattern)
{
	if (pattern.empty())
		return false;	// we're OUTTA here!
	rglob::compiler c;
	c.compile(pattern);
	const auto fsm = c.machine();
	const auto n = fsm.size();
	const auto rp = rmsRoot->AllocRP(RMsType::Auto, (int)n);
	if (!rp)
		return false;	// we're OUTTA here!
	memcpy(rp2xp(rp), fsm.c_str(), n);
	RMsQueue::pattern = rp;
	return true;	// indicate success
}

/*
	Compare a string "tag" that is being published with our subscription's
	"glob", returning the match/fail status.

	N.B. - isn't "bool" because of POSSIBLE "signal" return
*/
int RMsQueue::Match(std::string_view tag) const
{
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	rglob::matcher m(std::string_view((const char*)rp2xp(pattern), rp2n(pattern)));
	if (m.match(tag))
		return 1;	// indicate match
	return 0;
}

/*
	Perform out-of-band "signaling" of our queue - typically used to set an
	"end of data" state when we want to shut down the queue, and let any readers
	know that no additional data will be forthcoming.
*/
void RMsQueue::Signal(int flags)
{
	dumpQueue("<{}>::Signal({:08x})...state={:08x}\n", xp2pg(this), flags, int(state));
	state |= flags;
	semaphore.signal();
	dumpQueue("<{}>::Signal({:08x})...state={:08x}\n", xp2pg(this), flags, int(state));
}

bool RMsQueue::Validate() const
{
	if constexpr (qa_check_queue || qa_check_queue_full)
		if (magic != QueueMagic) {
			dumpCheck("RMsQueue<{}>::Validate()... FAILED\n", xp2pg(remove_const_t<RMsQueue*>(this)));
			return false;	// we're OUTTA here!
		}
	// figure out more tests...
	return true;	// indicate success
}

/*
	Synchronously wait on our semaphore for data to be published that matches
	our subscription's "glob", but also respond to out-of-band signals, typically
	indicating "end of data".

	This is deprecated now in favor of the lighter-weight Wait2, but will be
	retained for some time to be used possibly for diagnostics/testing/validation
	purposes.
*/
int RMsQueue::Wait(char* tag, size_t* tagN, void* data, size_t* dataN, int flags)
{
	dumpQueue("<{}>::Wait(...{})...\n", xp2pg(this), flags);
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	semaphore.wait();
	if (state) {
		dumpQueue("<{}>::Wait(...{})...WAIT => SIGNALED\n", xp2pg(this), flags);
		return RMsStatusSignaled;	// early out; indicate "signaled"
	}
	dumpQueue("<{}>::Wait(...{})...back from WAIT\n", xp2pg(this), flags);
	const auto td = read < NQuick ? quickE[read] :
		((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)];
	dumpQueue("<{}>::Wait(...{})...{:x}:{:x}\n", xp2pg(this), flags, td.tag, td.data);
	auto status = flags;
	if (flags & RMsGetTag) {
		const auto n = rp2n(td.tag);
		if (tag != nullptr) {
			if (n <= *tagN - 1)
				memcpy(tag, rp2xp(td.tag), n), tag[n] = '\0';	// ("C" string!)
			else
				status ^= RMsGetTag;	// indicate length issue with TAG
		} else
			status |= RMsOnlyLength;	// indicate only length data returned
		*tagN = n;	// indicate actual length
	}
	if (flags & RMsGetData) {
		const auto n = rp2n(td.data);
		if (data != nullptr) {
			if (n <= *dataN)
				memcpy(data, rp2xp(td.data), n);
			else
				status ^= RMsGetData;	// indicate length issue with DATA
		} else
			status |= RMsOnlyLength;	// indicate only length data returned
		*dataN = n;	// indicate actual length
	}
	// "consume" element IFF all requested tag info/data bits returned
	if (status == flags) {
		rmsRoot->FreePair(td), read++;
		if (read == write) {
			lock_guard acquire(spin);
			if (read == write)
				read = 0, write = 0;
		}
	}
	dumpQueue("<{}>::Wait('{}'...{})...{}\n", xp2pg(this), tag != nullptr ? tag : "(NA)", flags, status);
	return status;
}
