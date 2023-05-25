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

#pragma once

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

using namespace std::string_literals;
using namespace std::string_view_literals;
using namespace rms;

using std::string;
using std::string_view;

char* rms::rmsB = nullptr;				// shared memory view pointer(s)
RMsRoot* rms::rmsRoot = nullptr;		// shared "root" object
static auto NQPage = 0;					// # of pages of RMsQueue "extras"

//	control run-time state display and testing
//	N.B. - typically ALL false in production!!
constexpr auto qa_dump_all = false;		// (make it easy to turn on ALL display)
constexpr auto qa_dump_root = qa_dump_all || false;
constexpr auto qa_dump_queue = qa_dump_all || false;

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
struct std::formatter<std::thread::id> : std::formatter<string_view> {
	auto format(const std::thread::id& id, std::format_context& ctx) {
		std::ostringstream s;
		s << hex << id;
		return std::formatter<string_view>::format(s.str(), ctx);
	}
};

//	custom c++20 "formatter" for RMsType values
template<>
struct std::formatter<RMsType> : std::formatter<string_view> {
	auto format(const RMsType& ty, std::format_context& ctx) {
		string o{ std::format("{}:", (int)ty) };
		switch (ty) {
		case RMsType::Auto: o += "Auto"; break;
		case RMsType::Int32: o += "Int32"; break;
		case RMsType::Int64: o += "Int64"; break;
		case RMsType::Ieee: o += "Ieee"; break;
		case RMsType::Record: o += "Record"; break;
		default:
			std::format_to(back_inserter(o), "Bytes[{}]", 2 << (int)ty);
		}
		return std::formatter<string_view>::format(o, ctx);
	}
};

//	Dump root object info
template <typename... ARGS>
static void dumpRoot(string_view fmt, const ARGS&... args) {
	if constexpr (qa_dump_root) {
		// construct trace message prefix including current thread's thread::id
		auto tracePre = []() {
			return format("RMsRoot[{}]::", std::this_thread::get_id());
		};
		rms_trace(vformat(tracePre().append(fmt), std::make_format_args(args...)).data());
	}
}

//	Dump queue object info (SAME implementation as dumpRoot, but independent)
template <typename... ARGS>
static void dumpQueue(string_view fmt, const ARGS&... args) {
	if constexpr (qa_dump_queue) {
		// construct trace message prefix including current thread's thread::id
		auto tracePre = []() {
			return format("RMsQueue[{}]::", std::this_thread::get_id());
		};
		rms_trace(vformat(tracePre().append(fmt), std::make_format_args(args...)).data());
	}
}

//	Dump "check" info
template <typename... ARGS>
static void dumpCheck(string_view fmt, const ARGS&... args) {
	if constexpr (qa_check_alloc_full || qa_check_queue_full)
		rms_trace(vformat(fmt, std::make_format_args(args...)).data());
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
void rms::initialize(int np) noexcept(false)
{
	dumpRoot("rms::initialize({})...\n", np);
#if defined(WIN32) || defined(_WIN32)
	static HANDLE rmsM = nullptr;				// shared memory mapping object(s)
	rmsM = ::CreateFileMapping(INVALID_HANDLE_VALUE, nullptr, PAGE_READWRITE | SEC_RESERVE,
		0, np * 4096, nullptr);
	if (rmsM == nullptr)
		throw std::runtime_error("rms::initialize failed in ::CreateFileMapping()");
	const DWORD mapE = ::GetLastError();// (stash for later)
	// [attempt to] map view
	rmsB = (char*)::MapViewOfFile(rmsM, FILE_MAP_ALL_ACCESS, 0, 0, 0);
	if (rmsB == nullptr) {
		::CloseHandle(rmsM), rmsM = nullptr;
		throw std::runtime_error("rms::initialize failed in ::MapViewOfFile()");
	}
	// [attempt to] commit FIRST page (bootstrapping)
	if (::VirtualAlloc(rmsB, 1 * 4096, MEM_COMMIT, PAGE_READWRITE) == nullptr) {
		::UnmapViewOfFile(rmsB), rmsB = nullptr;
		::CloseHandle(rmsM), rmsM = nullptr;
		throw std::runtime_error("rms::initialize failed in ::VirtualAlloc()");
	}
	// init mapping AS REQUIRED
	if (mapE != ERROR_ALREADY_EXISTS) {
		// use "placement" new!
		new(rmsB) RMsRoot();
		rmsRoot = (RMsRoot*)rmsB;
		if (!rmsRoot->Initialize(np)) {
			::UnmapViewOfFile(rmsB), rmsB = nullptr;
			::CloseHandle(rmsM), rmsM = nullptr;
			throw std::runtime_error("rms::initialize failed in RMsRoot:Initialize()");
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
	dumpRoot("rms::initialize({}) NQPage={}\n", np, NQPage);
	dumpRoot("rms::initialize({})...SUCCESS\n", np);
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

#ifdef _DEBUG
int rms::is_valid_queue(int id)
{
	return isValidQueue(id) ? 1 : 0;
}
#endif

/*
	Add the supplied page to the list of active [subscription] queues.
*/
int RMsRoot::AddQueue(int pg)
{
	dumpRoot("AddQueue({})...\n", pg);
	std::lock_guard acquire(spin);
	matches.clear(); // (cache will be rebuilt by subsequent message publishing)
	((RMsQueue*)pg2xp(pg))->prev = queueTail;
	((RMsQueue*)pg2xp(pg))->next = 0;
	if (queueTail)
		((RMsQueue*)pg2xp(queueTail))->next = pg;
	else
		queueHead = pg;
	queueTail = pg;
	dumpRoot("AddQueue({})...h={},t={}\n", pg, std::remove_volatile_t<int>(queueHead), std::remove_volatile_t<int>(queueTail));
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
	std::lock_guard acquire(spin);
	if (pageFree)
		dumpRoot("AllocPage()...using freed page\n"),
		pg = pageFree, pageFree = *(int*)pg2xp(pg);
	else {
		dumpRoot("AllocPage()...extending high-water mark\n");
		if (high == committed) {
			if (committed >= pages) {
				dumpRoot("AllocPage()...Attempted to EXCEED LIMIT of {} pages!\n", std::remove_volatile_t<int>(pages));
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
	6. RMsRecord is a "hybrid" type, containing up
	   to 16 "slots" of RMs ptrs in a 64 B object.
*/
rms_ptr_t RMsRoot::AllocRP(RMsType ty, size_t n, bool zero)
{
	dumpRoot("AllocRP({}, {})...\n", ty, n);
	rms_ptr_t rp{};
	if (ty == RMsType::Auto)
		ty = n2ty(int(n));
	std::lock_guard acquire(spin);
	if (!typeFree[(int)ty])
		if (const auto pg = AllocPage(); pg)
			initTypedPageAsFree(pg, ty);
	if (typeFree[(int)ty]) {
		rp = rp2rp(typeFree[(int)ty], int(n)), typeFree[(int)ty] = *(rms_ptr_t*)rp2xp(rp);
		if (zero)
			std::memset(rp2xp(rp), 0, 2u << ty2logM1(ty));
	}
	dumpRoot("AllocRP({}, {})...{:08x}\n", ty, n, rp);
	return rp;
}

int RMsRoot::CheckAlloc(int pg) //const
{
	if constexpr (qa_check_alloc)
		if (pg <= 0 || pg >= high) {
			dumpCheck("RMsRoot::CheckAlloc({})... is BOGUS (illegal value)\n", pg);
			return 0;	// we're OUTTA here!
		}
	auto status = 1;
	if constexpr (qa_check_alloc_full) {
		std::lock_guard acquire(spin);
		for (auto p = pageFree; p; p = *(int*)pg2xp(p))
			if (p == pg)
				dumpCheck("RMsRoot::CheckAlloc({})... is BOGUS (on FREE list)\n", pg),
				status = 0;	// indicate failure
	}
	return status;
}

int RMsRoot::CheckQueue(int pg) //const
{
	auto n = 0;
	if constexpr (qa_check_queue_full) {
		std::lock_guard acquire(spin);
		for (auto q = queueHead; q; q = ((RMsQueue*)pg2xp(q))->next)
			if (q == pg)
				++n;
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
template<rms_full_type T>
void RMsRoot::Distribute(string_view tag, T d)
{
	std::lock_guard acquire(spin);
	auto tqp = get_matches(tag);
	// deliver to ALL cached tag->queue pairs which [now] match
	for (auto i = tqp.first; i != tqp.second; ++i)
		((RMsQueue*)pg2xp(i->second))->Append(tag, d);
}

// [explicitly] instantiate Distribute (and Append and Marshal)
// for ALL supported data types!
template void RMsRoot::Distribute(string_view, rms_int32);
template void RMsRoot::Distribute(string_view, rms_int64);
template void RMsRoot::Distribute(string_view, rms_ieee);
template void RMsRoot::Distribute(string_view, string_view);
template void RMsRoot::Distribute(string_view, nullptr_t);

/*
	Free a page and make it available for re-use by adding at the front of the
	free pages list.
*/
void RMsRoot::FreePage(int pg)
{
	dumpRoot("FreePage({})...\n", pg);
	std::lock_guard acquire(spin);
	*(int*)pg2xp(pg) = pageFree, pageFree = pg;
	dumpRoot("FreePage({})...DONE\n", pg);
}

/*
	Free both RMs ptrs from a tag/data pair (data may be empty) by adding each
	to front of their respective typed free lists.

	N.B. - make sure to INDIVIDUALLY free RMs ptrs in active RECORD slots!
*/
void RMsRoot::FreePair(td_pair_t p)
{
	dumpRoot("FreePair({:x}:{:x})...\n", p.tag, p.data);
	std::lock_guard acquire(spin);
	free_rp(p.tag);
	if (p.data) {
		if (rp2ty(p.data) == RMsType::Record)
			free_rec(p.data);
		free_rp(p.data);
	}
	dumpRoot("FreePair({:x}:{:x})...DONE\n", p.tag, p.data);
}

/*
	Free a single RMs ptr value by adding to front of typed free list.

	N.B. - make sure to INDIVIDUALLY free RMs ptrs in active RECORD slots!
*/
void RMsRoot::FreeRP(rms_ptr_t rp)
{
	dumpRoot("FreeRP({:08x})...\n", rp);
	std::lock_guard acquire(spin);
	// free USED "record" slots FIRST
	if (rp2ty(rp) == RMsType::Record)
		free_rec(rp);
	free_rp(rp);
	dumpRoot("FreeRP({:08x})...DONE\n", rp);
}

auto RMsRoot::get_matches(std::string_view tag) -> decltype(matches.equal_range(tag))
{
	auto tqp = matches.equal_range(tag);
	if (tqp.first == tqp.second) {
		// ... NO match, cache ALL queues which match this tag...
		for (auto q = queueHead; q; q = ((RMsQueue*)pg2xp(q))->next)
			if (((RMsQueue*)pg2xp(q))->Match(tag))
				matches.insert(std::pair(tag, q));
		// ... and re-compute the initial cache query
		tqp = matches.equal_range(tag);
	}
	return tqp;
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

template<rms_full_type T>
static constexpr void rmscpy(rms_ptr_t rp, T d) {
	*(T*)rp2xp(rp) = d;
}
template<>
static void rmscpy(rms_ptr_t rp, std::string_view d) {
	memcpy(rp2xp(rp), d.data(), d.size());
}

/*
	Allocate a new RMs ptr of the specified type and copy the data.

	Note that we will ONLY see values matching the concepts "rms_num_type"
	or "rms_string_view"... NO nullptr_t values.
*/
template<rms_full_type T>
rms_ptr_t RMsRoot::Marshal(T d)
{
	const rms_ptr_t rp{ AllocRP(publisher::ty_of(d), publisher::n_of(d)) };
	rmscpy(rp, d);
	return rp;
}

/*
	Remove the supplied page from the list of active [subscription] queues.

	(Also removes any "matches" referencing this queue from the cache.)
*/
void RMsRoot::RemoveQueue(int pg)
{
	dumpRoot("RemoveQueue({})...\n", pg);
	std::lock_guard acquire(spin);
	std::erase_if(matches, [pg](const auto& tq) { return tq.second == pg; });
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
	dumpRoot("RemoveQueue({})...h={},t={}\n", pg, std::remove_volatile_t<int>(queueHead), std::remove_volatile_t<int>(queueTail));
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
	dumpQueue("<{}>::~RMsQueue()...freeing {} indirect pages\n", pg, std::remove_volatile_t<int>(pages));
	for (auto i = 0; i < pages; ++i)
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
template<rms_full_type T>
void RMsQueue::Append(std::string_view tag, T d)
{
	const auto tRP = rmsRoot->Marshal(tag);
	rms_ptr_t dRP{};
	if (publisher::n_of(d) > 0)
		dRP = rmsRoot->Marshal(d);
	append(tRP, dRP);
}
/*
	Append [specialization] supporting publisher::put_tag.
*/
template<>
void RMsQueue::Append(std::string_view tag, nullptr_t d)
{
	const auto tRP = rmsRoot->Marshal(tag);
	append(tRP, 0);
}

/*
	Append [overload] supporting publication of RECORDs.

	N.B. - the supplied "data" RMs ptr is already marshaled!
*/
void RMsQueue::Append(std::string_view tag, rms_ptr_t dRP)
{
	const auto tRP = rmsRoot->Marshal(tag);
	append(tRP, dRP);
}

/*
	Publish tag/data to this subscription queue (II) - perform low-level checks
	and enqueue the td pair (remember, data may not be present), then signal
	our semaphore to reflect this publication.
*/
void RMsQueue::append(rms_ptr_t tag, rms_ptr_t data)
{
	dumpQueue("<{}>::append({:x}:{:x})...{}\n", xp2pg(this), tag, data, std::remove_volatile_t<int>(write));
	const td_pair_t td{ tag, data };
	std::lock_guard acquire(spin);
	if (state)
		return rmsRoot->FreePair(td);	// early out; "signaled"
	if (const auto [status, pg, pi] = checkWrite(); status)
		(write < NQuick ? quickE[write] : ((pq_pag_t*)pg2xp(pg))->pqTD[pi]) = td,
			++write, semaphore.signal();
	else
		rmsRoot->FreePair(td);
	dumpQueue("<{}>::append({:x}:{:x})...{}\n", xp2pg(this), tag, data, std::remove_volatile_t<int>(write));
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
int RMsQueue::Create(string_view pattern)
{
	dumpQueue("<?>::Create('{}')...\n", pattern);
	if (const auto pg = rmsRoot->AllocPage(); pg) {
		auto guard = sg::make_scope_guard([&]() -> void { rmsRoot->FreePage(pg); });
		// use "placement" new!
		auto qp = (RMsQueue*)pg2xp(pg);
		new(qp) RMsQueue();
		if (qp->initialize(pattern))
			return guard.dismiss(), rmsRoot->AddQueue(pg);
	}
	return 0;	// we're OUTTA here!
}

/*
	Free any "undelivered" tag/data pairs in queue, as well as any associated
	data, finally setting the queue to a LOGICAL "empty" state.
*/
void RMsQueue::Flush()
{
	std::lock_guard acquire(spin);
	while (read < write)
		rmsRoot->FreePair(read < NQuick ?
			quickE[read] :
			((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)]), ++read;
	read = 0, write = 0, state = 0;
}

/*
	Perform non-constructor setup of RMsQueue data structure (II) - the primary
	concern is creating and initializing the "glob" used for our "subscription".
*/
bool RMsQueue::initialize(string_view pattern)
{
	if (pattern.empty())
		return false;	// we're OUTTA here!
	rglob::compiler c;
	c.compile(pattern);
	RMsQueue::pattern = rmsRoot->Marshal(c.machine());
	return true;	// indicate success
}

/*
	Compare a string "tag" that is being published with our subscription's
	"glob", returning the match/fail status.
*/
bool RMsQueue::Match(string_view tag) const
{
	const rglob::matcher m({ (const char*)rp2xp(pattern), rp2n(pattern) });
	return m.match(tag);
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
			dumpCheck("RMsQueue<{}>::Validate()... FAILED\n", xp2pg(std::remove_const_t<RMsQueue*>(this)));
			return false;	// we're OUTTA here!
		}
	// figure out more tests...
	return true;	// indicate success
}

/*
	Supports the older-style explicit "data and/or tag" -style get, which is
	still used by some of the subscriber class get variants for simple vals.
*/
template<typename T>
int RMsQueue::Wait2(string& tag, T& data, int flags) {
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	semaphore.wait();
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	const auto td = read < NQuick ?
		quickE[read] :
		((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)];
	if (flags & RMsGetTag)
		tag = std::move(getRValue<std::string>(td.tag));
	if (flags & RMsGetData)
		data = std::move(getRValue<T>(td.data));
	// "consume" element
	rmsRoot->FreePair(td);
	std::lock_guard<RSpinLock> acquire(spin);
	if (++read == write)
		read = 0, write = 0; // reset to "quick" entries when we can
	return flags;
}

// [explicitly] instantiate Wait2 for ALL supported data types!
template int RMsQueue::Wait2(string&, rms_int32&, int);
template int RMsQueue::Wait2(string&, rms_int64&, int);
template int RMsQueue::Wait2(string&, rms_ieee&, int);
template int RMsQueue::Wait2(string&, string&, int);

/*
	Supports the sometimes-convoluted accessing of queue elements as "rms_any"
	variant values, which definitions are enabled by the use of  rva::variant.
*/
int RMsQueue::Wait3(string& tag, rms_any& data) {
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	semaphore.wait();
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	const auto td = read < NQuick ?
		quickE[read] :
		((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)];
	switch (rp2ty(td.data)) {
	case RMsType::Int32: data = getRValue<rms_int32>(td.data); break;
	case RMsType::Int64: data = getRValue<rms_int64>(td.data); break;
	case RMsType::Ieee: data = getRValue<rms_ieee>(td.data); break;
	case RMsType::Record: {
		data = rms_rec();
		auto& vr = std::get<rms_rec>(data);
		const auto rec = (rms_ptr_t*)rp2xp(td.data);
		const auto n = rp2c(td.data);
		vr.reserve(n);
		for (auto i = 0; i < n; ++i)
			switch (const auto v = rec[i]; rp2ty(v)) {
			case RMsType::Int32: vr.emplace_back(getRValue<rms_int32>(v)); break;
			case RMsType::Int64: vr.emplace_back(getRValue<rms_int64>(v)); break;
			case RMsType::Ieee: vr.emplace_back(getRValue<rms_ieee>(v)); break;
			// 2nd-level recursion ("record of record") DISALLOWED [for now]!
			case RMsType::Record: vr.emplace_back(std::move(std::string())); break;
			default:
				vr.emplace_back(std::move(getRValue<std::string>(v))); break;
			}
		break;
	}
	default:
		data = std::move(getRValue<std::string>(td.data)); break;
	}
	tag = std::move(getRValue<std::string>(td.tag));
	// "consume" element
	rmsRoot->FreePair(td);
	std::lock_guard<RSpinLock> acquire(spin);
	if (++read == write)
		read = 0, write = 0; // reset to "quick" entries when we can
	return 0;
}

/*
	Supports the [new] RMs API for "records", which are simply collections of
	the standard RMs "primitive" data types, implemented as a contiguous set
	of packed "fields" in the RMs data store: the RMsType::Record.

	N.B. - this returns an rms_ptr_t to these [still-packed] fields, which the
	caller MUST pass to RMsRoot::FreeRP when it is done unpacking the values.
*/
rms_ptr_t RMsQueue::Wait4(string& tag) {
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	semaphore.wait();
	if (state)
		return RMsStatusSignaled;	// early out; indicate "signaled"
	const auto td = read < NQuick ?
		quickE[read] :
		((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)];
	tag = std::move(getRValue<std::string>(td.tag));
	// "consume" element (ONLY the tag just now, we are RETURNING the data)
	rmsRoot->FreeRP(td.tag);
	std::lock_guard<RSpinLock> acquire(spin);
	if (++read == write)
		read = 0, write = 0; // reset to "quick" entries when we can
	return td.data; // return the RECORD RMs ptr, MUST BE FREED BY CALLER!
}
