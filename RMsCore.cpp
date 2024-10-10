/*
	RMsCore.cpp - "core" functionality of the RMs messaging system

	Copyright(c) 2004-2024, Robert Roessler
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

#include <exception>
#include <charconv>
#include "rms.h"
#include "rglob.h"

using namespace std::string_literals;
using namespace std::string_view_literals;
using namespace rms;

using std::string;
using std::string_view;

// (the mysterious and magical "overload" template for std::visit())
template<class... Ts> struct overload : Ts... { using Ts::operator()...; };

char* rms::rmsB{ nullptr };				// shared memory view pointer(s)
RMsRoot* rms::rmsRoot{ nullptr };		// shared "root" object
int RMsQueue::NQPage{ 0 };				// # of pages of RMsQueue "extras"

//	control run-time state display and testing
//	N.B. - typically ALL false in production!!
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
void rms::initialize(int np)
{
#if defined(WIN32) || defined(_WIN32)
	static HANDLE rmsM = nullptr;				// shared memory mapping object(s)
	rmsM = ::CreateFileMapping(INVALID_HANDLE_VALUE, nullptr, PAGE_READWRITE | SEC_RESERVE,
		0, np * 4096, nullptr);
	if (rmsM == nullptr)
		throw std::bad_alloc();
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
		throw std::bad_alloc();
	}
	// init mapping AS REQUIRED
	if (mapE != ERROR_ALREADY_EXISTS) {
		// use "placement" new!
		new(rmsB) RMsRoot();
		rmsRoot = (RMsRoot*)rmsB;
		if (!rmsRoot->Initialize(np)) {
			::UnmapViewOfFile(rmsB), rmsB = nullptr;
			::CloseHandle(rmsM), rmsM = nullptr;
			throw std::runtime_error("rms::initialize failed in RMsRoot::Initialize()");
		}
	}
#else
	auto fd = shm_open("/rhps_rms_shared", O_CREAT | O_TRUNC | O_RDWR, 0666);
	if (fd == -1)
		throw std::bad_alloc();
	auto stat = ftruncate(fd, np * 4096);
	if (stat != 0)
		throw std::bad_alloc();
	rmsB = (char*)mmap(0, np * 4096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	close(fd);
	if (rmsB == MAP_FAILED)
		throw std::runtime_error("rms::initialize failed in ::mmap()");
	// use "placement" new!
	new(rmsB) RMsRoot();
	rmsRoot = (RMsRoot*)rmsB;
	if (!rmsRoot->Initialize(np)) {
		munmap(rmsB, np * 4096), rmsB = nullptr;
		shm_unlink("rhps_rms_shared");
		throw std::runtime_error("rms::initialize failed in RMsRoot::Initialize()");
	}
#endif
	// verify [expected] invariants
	static_assert(sizeof(RMsRoot) <= 4096, "RMsRoot instance MUST be <= 4096 bytes long!");
	static_assert(sizeof(RMsQueue) <= 4096, "RMsQueue instance MUST be <= 4096 bytes long!");
	// compute any architecture-dependent values
	RMsQueue::NQPage = (4096 - offsetof(RMsQueue, pageE)) / sizeof(unsigned short);
}

/*
	Create a string representation of the supplied rms_any [recursive variant].

	This will be of the form "[ val, val, ... ]", and is based on the output
	formatting provided by the C++17 std:to_chars() function, which provides
	a fairly clean and consistent "style" - with limited formatting choices.

	The upside is substantial, at least for use in older / embedded toolings:
	the C++20 std::format package is not required.
*/
string rms::to_string(const rms_any& v) {
	auto string_of = [](rms_num_type auto v) -> string {
		char b[24];
		if constexpr (std::same_as<decltype(v), rms_int64>) {
			// special-case rms_int64, providing "0x..." plus base 16 output
			b[0] = '0', b[1] = 'x';
			auto [ptr, ec] = std::to_chars(b + 2, b + std::size(b), v, 16);
			return { b, ptr };
		} else {
			auto [ptr, ec] = std::to_chars(b, b + std::size(b), v);
			return { b, ptr };
		}
	};
	auto flatten_vec = [](auto vec) {
		string o;
		o.reserve(256);
		for (const auto& a : vec)
			if (o.empty())
				o.append("[ "sv).append(to_string(a));
			else
				o.append(", "sv).append(to_string(a));
		return o.append(" ]"sv);
	};
	return std::visit(overload{
		[&](rms_int32) { return string_of(std::get<rms_int32>(v)); },
		[&](rms_int64) { return string_of(std::get<rms_int64>(v)); },
		[&](rms_ieee) { return string_of(std::get<rms_ieee>(v)); },
		[&](const string&) { return std::get<string>(v); },
		[&](const rms_vec&) { return flatten_vec(std::get<rms_vec>(v)); }
	}, v);
}

/*
	Wrapper function for performing [paranoid] checks on the queue identified
	by 'pg'... most useful during early development of RMs, not so much now.
*/
static bool rms::isValidQueue(int pg)
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
int RMsRoot::AddQueue(int pg) noexcept
{
	std::lock_guard acquire(spin);
	matches.clear(); // (cache will be rebuilt by subsequent message publishing)
	((RMsQueue*)pg2xp(pg))->prev = queueTail;
	((RMsQueue*)pg2xp(pg))->next = 0;
	if (queueTail)
		((RMsQueue*)pg2xp(queueTail))->next = pg;
	else
		queueHead = pg;
	queueTail = pg;
	return pg;
}

/*
	Allocate a new page - 1st try recycling, then try to extend the
	high-water mark, then attempt to COMMIT another PageIncrement pages.
*/
int RMsRoot::AllocPage()
{
	int pg{};
	std::lock_guard acquire(spin);
	if (pageFree)
		// using freed page
		pg = pageFree, pageFree = *(int*)pg2xp(pg);
	else {
		// ...extending high-water mark
		if (high == committed) {
			if (committed >= pages)
				throw std::bad_alloc();
#if defined(WIN32) || defined(_WIN32)
			// ...committing 'PageIncrement' pages
			else if (::VirtualAlloc(rmsB + high*size_t(4096), PageIncrement*4096,
				MEM_COMMIT, PAGE_READWRITE) == nullptr)
				throw std::bad_alloc();
#endif
			committed += PageIncrement;
		}
		pg = high++;
	}
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
rms_ptr_t RMsRoot::AllocRP(RMsType ty, size_t n)
{
	rms_ptr_t rp{};
	if (ty == RMsType::Auto)
		ty = n2ty(int(n));
	std::lock_guard acquire(spin);
	if (!typeFree[(int)ty])
		if (const auto pg = AllocPage(); pg)
			initTypedPageAsFree(pg, ty);
	if (typeFree[(int)ty])
		rp = rp2rp(typeFree[(int)ty], int(n)), typeFree[(int)ty] = *(rms_ptr_t*)rp2xp(rp);
	// did we FAIL to allocate a new RMs ptr?
	if (!rp)
		throw std::bad_alloc();
	return rp;
}

bool RMsRoot::CheckAlloc(int pg) noexcept
{
	if constexpr (qa_check_alloc)
		if (pg <= 0 || pg >= high)
			// is BOGUS (illegal value)
			return false;	// we're OUTTA here!
	if constexpr (qa_check_alloc_full) {
		std::lock_guard acquire(spin);
		for (auto p = pageFree; p; p = *(int*)pg2xp(p))
			if (p == pg)
				// is BOGUS (on FREE list)
				return false;	// indicate failure
	}
	return true;
}

bool RMsRoot::CheckQueue(int pg) noexcept
{
	auto n = 1;
	if constexpr (qa_check_queue_full) {
		n = 0;
		std::lock_guard acquire(spin);
		for (auto q = queueHead; q; q = ((RMsQueue*)pg2xp(q))->next)
			if (q == pg)
				++n;
	}
	return n == 1; // any other value is Bad(tm)
}

/*
	Add this tag/typed-data pair to any subscription queue that "matches" the tag.
*/
void RMsRoot::Distribute(string_view tag, rms_max_type auto d)
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
void RMsRoot::FreePage(int pg) noexcept
{
	std::lock_guard acquire(spin);
	*(int*)pg2xp(pg) = pageFree, pageFree = pg;
}

/*
	Free both RMs ptrs from a tag/data pair (data may be empty) by adding each
	to front of their respective typed free lists.

	N.B. - make sure to INDIVIDUALLY free RMs ptrs in active RECORD slots!
*/
void RMsRoot::FreePair(td_pair_t p) noexcept
{
	std::lock_guard acquire(spin);
	free_rp(p.tag);
	if (p.data) {
		if (rp2ty(p.data) == RMsType::Record)
			free_rec(p.data);
		free_rp(p.data);
	}
}

/*
	Free a single RMs ptr value by adding to front of typed free list.

	N.B. - make sure to INDIVIDUALLY free RMs ptrs in active RECORD slots!
*/
void RMsRoot::FreeRP(rms_ptr_t rp) noexcept
{
	std::lock_guard acquire(spin);
	if (rp2ty(rp) == RMsType::Record)
		free_rec(rp);
	free_rp(rp);
}

/*
	Find the CACHED "range" of matches for this tag, i.e., which queues have
	subscriptions matching it... if we fail to find any CACHED matches, then
	do the ugly full linear search through all queues, returning THAT result.

	N.B. - this means perf will be quite good - on tags with at least ONE or
	more matching subscriptions, not so good if tags are regularly published
	that have NO matching subscriptions.
	N.B.2 - call with RMsRoot mutex LOCKED!
*/
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

	N.B. - NO mutex locks are needed, since the world doesn't exist yet
	N.B.2 - NO exceptions are thrown on bad param OR failure to allocate mem
*/
bool RMsRoot::Initialize(size_t np) noexcept
{
	if (np > 65536)
		return false;	// indicate failure
	// do "real" initial COMMIT (re-commits 1st page)
#if defined(WIN32) || defined(_WIN32)
	if (!::VirtualAlloc(rmsB, PageIncrement*4096, MEM_COMMIT, PAGE_READWRITE))
		return false;	// indicate failure
#endif
	pages = (int)np, committed = PageIncrement;
	high = 1;	// we ARE the 1st page...
	return true;	// indicate success
}

/*
	Initialize supplied page as a typed linked list of RMs ptrs for the given
	type, with the byte length of each entry in the list being determined by
	this type - see the entry for RMsRoot::AllocRP for details.

	N.B. - the type MUST be supplied, there is no length to use for inference
	N.B.2 - call with RMsRoot mutex LOCKED!
*/
void RMsRoot::initTypedPageAsFree(int pg, RMsType ty) noexcept
{
	auto p = (char*)pg2xp(pg);
	// compute max length of typed item...
	const auto d = 2 << ty2logM1(ty);
	// ... link page of these from back to front...
	for (auto o = 4096 - d, n = 0; o >= 0; n = o, o -= d)
		*(rms_ptr_t*)(p + o) = n ? make_rp(pg, ty, n) : 0;
	// finally, store as "head" of type's free list
	typeFree[(int)ty] = make_rp(pg, ty, 0);
}

/*
	Allocate an RMs record with 'n' available [and zeroed] slots.

	N.B. - the # of slots MUST be multiplied by the size of an RMs ptr!
*/
rms_ptr_t RMsRoot::MakeRecord(size_t n) {
	constexpr auto nn = 2u << ty2logM1(RMsType::Record);
	if (nn < n * sizeof(rms_ptr_t))
		throw std::bad_array_new_length();
	const auto rp = AllocRP(RMsType::Record, n * sizeof(rms_ptr_t));
	std::memset(rp2xp(rp), 0, nn);
	return rp;
}

//	(workhorses for Marshal()... as noted below, will NOT see nullptr_t!)
static constexpr void rmscpy(rms_ptr_t rp, rms_num_type auto d) noexcept {
	*(decltype(d)*)rp2xp(rp) = d;
}

static void rmscpy(rms_ptr_t rp, string_view d) {
	memcpy(rp2xp(rp), d.data(), d.size());
}

/*
	Allocate a new RMs ptr of the specified type and copy the data.

	Note that we will ONLY see values matching the concept "rms_num_type"
	or actual std::string_views... NO nullptr_t values.
*/
rms_ptr_t RMsRoot::Marshal(rms_mid_type auto d)
{
	const rms_ptr_t rp{ AllocRP(publisher::ty_of(d), publisher::n_of(d)) };
	rmscpy(rp, d);
	return rp;
}

/*
	Remove the supplied page from the list of active [subscription] queues.

	(Also removes any "matches" referencing this queue from the cache.)
*/
void RMsRoot::RemoveQueue(int pg) noexcept
{
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
}

/*
	Perform any needed shutdown / resource-freeing operations on RMsQueue object
	after first removing from the set of active queue objects.
*/
RMsQueue::~RMsQueue()
{
	const auto pg = xp2pg(this);
	rmsRoot->RemoveQueue(pg);
	Flush();
	for (auto i = 0; i < pages; ++i)
		rmsRoot->FreePage(pageE[i]);
	rmsRoot->FreeRP(pattern);
	rmsRoot->FreePage(pg);
}

/*
	Publish tag/data to this subscription queue (I) - perform final parameter
	validation, and all the higher-level logical checks, allocations, and
	data copying to prepare the "td pair" for actual enqueuing.

	N.B. - no need to acquire the mutex lock at THIS level of processing
*/
void RMsQueue::Append(std::string_view tag, rms_mid_type auto d)
{
	const auto tRP = rmsRoot->Marshal(tag);
	rms_ptr_t dRP{};
	if (publisher::n_of(d) > 0)
		dRP = rmsRoot->Marshal(d);
	append(tRP, dRP);
}

/*
	Append [overload] supporting publisher::put_tag.
*/
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
	const td_pair_t td{ tag, data };
	auto guard = sg::make_scope_guard([td]() noexcept -> void { rmsRoot->FreePair(td); });
	if (state)
		return;	// early out; "signaled"
	else {
		std::lock_guard acquire(spin);
		const auto [is_quick, pg, pi] = checkWrite();
		(is_quick ? quickE[write] : ((pq_pag_t*)pg2xp(pg))->pqTD[pi]) = td,
			++write, guard.dismiss();
	}
	semaphore.signal(); // (ONLY happens if there were NO throws in allocations)
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
			throw std::bad_alloc();
		pageE[pages++] = rmsRoot->AllocPage();
	}
	return { false, pageE[p], qp2pi(write) };
}

/*
	Fully shut down this queue, releasing all resources, including any undelivered
	tag/data pairs and their associated storage.
*/
void RMsQueue::Close() noexcept
{
	state |= RMsStatusClosing;
	if (magic.exchange(0))
		this->~RMsQueue(); // (we weren't ALREADY closed)
}

/*
	Perform non-constructor setup of RMsQueue data structure (I) - this is a
	static method, so we act like an RMsQueue "factory".
*/
int RMsQueue::Create(string_view pattern)
{
	const auto pg = rmsRoot->AllocPage();
	auto guard = sg::make_scope_guard([pg]() noexcept -> void { rmsRoot->FreePage(pg); });
	// use "placement" new!
	auto qp = (RMsQueue*)pg2xp(pg);
	new(qp) RMsQueue();
	if (qp->initialize(pattern))
		return guard.dismiss(), rmsRoot->AddQueue(pg);
	else
		throw std::invalid_argument("Invalid pattern for RMsQueue::Create()");
}

/*
	Free any "undelivered" tag/data pairs in queue, as well as any associated
	data, finally setting the queue to a LOGICAL "empty" state.
*/
void RMsQueue::Flush() noexcept
{
	std::lock_guard acquire(spin);
	while (read < write)
		rmsRoot->FreePair(next_pair()), ++read;
	read = 0, write = 0, state.store(0);
	semaphore.reset(); // (IMPORTANT in case we use this queue again!)
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

bool RMsQueue::Validate() const noexcept
{
	if constexpr (qa_check_queue || qa_check_queue_full)
		if (magic != QueueMagic)
			return false;	// we're OUTTA here!
	// figure out more tests...
	return true;	// indicate success
}

/*
	Supports the older-style explicit "data and/or tag" -style get, which is
	still used by some of the subscriber class get variants for simple vals.
*/
template<rms_out_type T>
int RMsQueue::wait_T(string& tag, T& data, int flags, std::stop_token* st)
{
	td_pair_t td{};
	if (check_early_exit_and_wait(st, [this, &td]() { td = get_next_pair_and_remove(); }))
		return RMsStatusSignaled;	// early out; indicate "signaled"
	if (flags & RMsGetTag)
		tag = getRValue<string>(td.tag);
	if (flags & RMsGetData)
		data = getRValue<T>(td.data);
	// "consume" element
	rmsRoot->FreePair(td);
	return 0;
}

// [explicitly] instantiate wait_T for ALL supported data types!
template int RMsQueue::wait_T(string&, rms_int32&, int, std::stop_token*);
template int RMsQueue::wait_T(string&, rms_int64&, int, std::stop_token*);
template int RMsQueue::wait_T(string&, rms_ieee&, int, std::stop_token*);
template int RMsQueue::wait_T(string&, string&, int, std::stop_token*);

/*
	Supports the sometimes-convoluted accessing of queue elements as "rms_any"
	variant values, which definitions are enabled by the use of rva::variant.
*/
int RMsQueue::wait_any(string& tag, rms_any& data, std::stop_token* st)
{
	td_pair_t td{};
	if (check_early_exit_and_wait(st, [this, &td]() { td = get_next_pair_and_remove(); }))
		return RMsStatusSignaled;	// early out; indicate "signaled"
	switch (rp2ty(td.data)) {
	case RMsType::Int32: data = getRValue<rms_int32>(td.data); break;
	case RMsType::Int64: data = getRValue<rms_int64>(td.data); break;
	case RMsType::Ieee: data = getRValue<rms_ieee>(td.data); break;
	case RMsType::Record: {
		data = rms_vec();
		auto& vr = std::get<rms_vec>(data);
		const auto rec = (rms_ptr_t*)rp2xp(td.data);
		const auto n = rp2c(td.data);
		vr.reserve(n);
		for (std::remove_const_t<decltype(n)> i = 0; i < n; ++i)
			switch (const auto v = rec[i]; rp2ty(v)) {
			case RMsType::Int32: vr.emplace_back(getRValue<rms_int32>(v)); break;
			case RMsType::Int64: vr.emplace_back(getRValue<rms_int64>(v)); break;
			case RMsType::Ieee: vr.emplace_back(getRValue<rms_ieee>(v)); break;
			// 2nd-level recursion ("record of record") DISALLOWED [for now]!
			case RMsType::Record: vr.emplace_back(string()); break;
			default:
				vr.emplace_back(getRValue<string>(v)); break;
			}
		break;
	}
	default:
		data = getRValue<string>(td.data); break;
	}
	tag = getRValue<string>(td.tag);
	// "consume" element
	rmsRoot->FreePair(td);
	return 0;
}

/*
	Supports the [new] RMs API for "records", which are simply collections of
	the standard RMs "primitive" data types, implemented as a contiguous set
	of packed "fields" in the RMs data store: the RMsType::Record.

	N.B. - this returns an rms_ptr_t to these [still-packed] fields, which the
	caller MUST pass to RMsRoot::FreeRP when it is done unpacking the values.
*/
rms_ptr_t RMsQueue::wait_rec(string& tag, std::stop_token* st)
{
	td_pair_t td{};
	if (check_early_exit_and_wait(st, [this, &td]() { td = get_next_pair_and_remove(); }))
		return RMsStatusSignaled;	// early out; indicate "signaled"
	tag = getRValue<string>(td.tag);
	// "consume" element (ONLY the tag just now, we are RETURNING the data)
	rmsRoot->FreeRP(td.tag);
	return td.data; // return the RECORD RMs ptr, MUST BE FREED BY CALLER!
}
