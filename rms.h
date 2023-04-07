/*
	rms.h - interface of the RMs messaging system

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

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <thread>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#ifdef RMSDLL_EXPORTS
#define RMS_EXPORT extern "C" __declspec(dllexport)
#else
#define RMS_EXPORT extern "C"
#endif

//	the RMs "primitive" data types
using rms_ieee = double;
using rms_int32 = int;
using rms_int64 = long long;
using rms_intptr = std::intptr_t; // (this MUST resolve to either 'int' or 'long long'!)

// type(s) for returning ANY queue element!
using rms_any = std::variant<rms_int32, rms_int64, rms_ieee, std::string>;

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
void rms_close(int id) noexcept(false);
RMS_EXPORT
void rms_flush(int id) noexcept(false);
RMS_EXPORT
void rms_initialize(int np) noexcept(false);
#ifdef _DEBUG
RMS_EXPORT
int rms_is_valid_queue(int id);
#endif
RMS_EXPORT
int rms_peek(int id) noexcept(false);
RMS_EXPORT
void rms_publish_bytes(std::string_view tag, const unsigned char* data, size_t n) noexcept(false);
RMS_EXPORT
void rms_publish_ieee(std::string_view tag, rms_ieee data);
RMS_EXPORT
void rms_publish_int32(std::string_view tag, rms_int32 data);
RMS_EXPORT
void rms_publish_int64(std::string_view tag, rms_int64 data);
RMS_EXPORT
void rms_publish_intptr(std::string_view tag, rms_intptr data);
RMS_EXPORT
void rms_publish_string(std::string_view tag, std::string_view data) noexcept(false);
RMS_EXPORT
void rms_signal(int id, int flags) noexcept(false);
RMS_EXPORT
int rms_subscribe(std::string_view pattern) noexcept(false);
RMS_EXPORT
int rms_wait_bytes(int id, char tag[], size_t* tagN, unsigned char data[], size_t* dataN, int flags) noexcept(false);
RMS_EXPORT
int rms_wait_ieee(int id, char tag[], size_t* tagN, rms_ieee* data, int flags) noexcept(false);
RMS_EXPORT
int rms_wait_int32(int id, char tag[], size_t* tagN, rms_int32* data, int flags) noexcept(false);
RMS_EXPORT
int rms_wait_int64(int id, char tag[], size_t* tagN, rms_int64* data, int flags) noexcept(false);
RMS_EXPORT
int rms_wait_intptr(int id, char tag[], size_t* tagN, rms_intptr* data, int flags) noexcept(false);
RMS_EXPORT
int rms_wait_string(int id, char tag[], size_t* tagN, char data[], size_t* dataN, int flags) noexcept(false);

namespace rms {

consteval auto isPtrLongLong() { return sizeof(void*) == sizeof(long long); }

enum class RMsType {
	Auto = 0,							// [input] derive type from byte length
	Int32 = 12,							// 32-bit int
	Int64 = 13,							// 64-bit int
										// 32/64 -bit int... enough for pointer
	IntPtr = isPtrLongLong() ? Int64 : Int32,
	Ieee = 14,							// IEEE double-precision float
	Reserved = 15						// (reserved for future types)
};

enum {
	RMsSigInterrupt = 1					// "wake up" any readers via an OOB signal
};

enum {
	RMsStatusSignaled = -2,				// reflect delivery of *some* signal
	RMsStatusClosing = 0x80000000		// reflect "shutdown in progress" for queue
};

enum {
	RMsGetData = 1,						// request "data" during a get*<> operation
	RMsGetTag = 2,						// request "tag" during a get*<> operation
	RMsOnlyLength = 4					// tag and/or data requested but...
};

//	# of [4KB] pages to commit at each growth
constexpr auto PageIncrement = 16;

constexpr auto RootMagic = 0x52734d52;	// ('RMsR')
constexpr auto QueueMagic = 0x51734d52;	// ('RMsQ')

using rms_ptr_t = unsigned int;			// [RMs] ptr type

//	RMs tag/data pair (data may be empty)
using td_pair_t = struct {
	rms_ptr_t tag;						// [RMs] ptr to tag
	rms_ptr_t data;						// [RMs] ptr to data
};

extern char* rmsB;						// shared memory view pointer(s)

//	RMs page -> exposed [H/W] ptr
constexpr void* pg2xp(int pg)
{
	return rmsB + (ptrdiff_t(pg) << 12);
}

//	RMs ptr -> RMs type
constexpr RMsType rp2ty(rms_ptr_t rp)
{
	return (RMsType)((rp >> 12) & 0x0f);
}

// length -> RMs type
constexpr RMsType n2ty(int nn)
{
	auto i = 1;
	for (; i < 11; ++i)
		if ((unsigned)nn <= (2u << i))
			break;
	return (RMsType)i;
}

//	RMs type -> (binary logarithm of type's [max] length) - 1
constexpr int ty2logM1(RMsType ty)
{
	// assert 0 <= ty <= 15
	return
		"\x0b"											// unk
		"\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b"	// iso
		"\x01"											// int32
		"\x02"											// int64
		"\x02"											// double
		"\x0b"[(int)ty];								// unk
}

// RMs ptr from page, RMs type, and page offset
constexpr rms_ptr_t make_rp(int pg, RMsType ty, int po)
{
	return (pg << 16) | ((int)ty << 12) | po;
}

// RMs ptr -> RMs ptr' with encoded data length n
constexpr rms_ptr_t rp2rp(rms_ptr_t rp, int nn)
{
	return (rp & ~((2u << ty2logM1(rp2ty(rp))) - 1)) | ((nn == (2u << ty2logM1(rp2ty(rp)))) ? 0 : nn);
}

//	exposed [H/W] ptr -> RMs page
constexpr int xp2pg(void* xp)
{
	return (int)((char*)xp - rmsB) >> 12;
}

//	RMs ptr -> exposed [H/W] ptr
constexpr void* rp2xp(rms_ptr_t rp)
{
	const auto pg = rp >> 16;
	const auto po = rp & (~((2u << ty2logM1(rp2ty(rp))) - 1) & 0x0fff);
	return (char*)pg2xp(pg) + po;
}

//	Simple spinlock class
class RSpinLock {
public:
	void lock() {
		// try simple lock...
		while (lock_.test_and_set(std::memory_order_acquire))
			// ... nope, release time slice and keep trying
			std::this_thread::yield();
	}
	void unlock() { lock_.clear(std::memory_order_release); }

private:
	std::atomic_flag lock_ ATOMIC_FLAG_INIT;
};

//	Recursive spinlock class
class RSpinLockEx {
public:
	RSpinLockEx() : nobody{}, owner{ nobody } {}

	void lock() {
		auto current = std::this_thread::get_id();
		auto id = nobody;
		// try simple lock (for current thread)...
		if (owner != current && !owner.compare_exchange_strong(id, current))
			do
				// ... nope, release time slice and keep trying
				std::this_thread::yield(), id = nobody;
			while (!owner.compare_exchange_weak(id, current));
		// ... we HAVE the lock, increment our "recursion" count
		++count_;
	}
	void unlock() {
		// with thread's final "unlock", show owner as "no thread"
		if (--count_ == 0)
			owner.store(nobody, std::memory_order_release);
	}

private:
	const std::thread::id nobody;		// value matching NO thread
	std::atomic<std::thread::id> owner;	// thread ID of lock holder (synch object)
	int count_{ 0 };					// # of [recursive] locks held
};

//	Simple semaphore class
class RSemaphore {
public:
	RSemaphore() {}
	~RSemaphore() {
		count_ = 0;
		cv.notify_all();
	}

	void signal(int n = 1) {
		{
			std::lock_guard<std::mutex> lock(mt);
			count_ += n;
		}
		cv.notify_one();
	}
	void wait() {
		std::unique_lock<std::mutex> lock(mt);
		cv.wait(lock, [this]() { return count_ > 0; });
		--count_;
	}
	// EXPERIMENTAL: "non-consuming" wait
	// N.B. - ONLY meaningful in "single-consumer" (per semaphore) model!
	void block() {
		std::unique_lock<std::mutex> lock(mt);
		cv.wait(lock, [this]() { return count_ > 0; });
	}

private:
	std::atomic<int> count_{ 0 };		// synch object
	std::condition_variable cv;			// condition variable
	std::mutex mt;						// mutex for above
};

/*
	RMsRoot is the central controlling object of the RMs message system, with
	the primary tasks of allocating and freeing all memory from our page pool,
	as well as distributing published messages on behalf of the higher levels.

	A single [recursive] mutex object is used to synchronize all access to the
	root data structures and elements.

	N.B. - There will be exactly ONE instance of RMsRoot.
*/
class RMsRoot {
public:
	RMsRoot() {}
	~RMsRoot() {}

	int AddQueue(int pg);
	int AllocPage();
	rms_ptr_t AllocRP(RMsType ty, size_t n);
	int CheckAlloc(int pg) const;
	int CheckQueue(int pg) const;
	void Distribute(std::string_view tag, const void* data, size_t n, RMsType ty = RMsType::Auto);
	void FreePage(int pg);
	void FreePair(td_pair_t p);
	void FreeRP(rms_ptr_t rp);
	bool Initialize(int np);
	void RemoveQueue(int pg);

private:
	friend bool isValidQueue(int pg);

	// Link RMs ptr at front of typed free list
	// N.B. - Call with RMsRoot mutex LOCKED!
	void free_rp(rms_ptr_t rp) {
		const auto ty = rp2ty(rp);
		*(int*)rp2xp(rp) = typeFree[(int)ty], typeFree[(int)ty] = rp;
	}
	// N.B. - Call with RMsRoot mutex LOCKED!
	void initTypedPageAsFree(int pg, RMsType ty);

	int magic{ RootMagic };				// root magic number ('RMsR')
	RSpinLockEx spin;					// root [recursive] spinlock
	volatile int pages { 0 };			// # of [4kb] pages RESERVED
	volatile int committed{ 0 };		// # of [4kb] pages COMMITTED
	volatile int high{ 0 };				// # used / next available
	volatile int pageFree{ 0 };			// chain of free pages
	volatile int queueHead{ 0 };		// doubly-linked list of queues
	volatile int queueTail{ 0 };
	rms_ptr_t typeFree[16]{};			// "typed" (sized) free chains
};

extern RMsRoot* rmsRoot;				// shared "root" object

//	RMs ptr -> actual data length
constexpr size_t rp2n(rms_ptr_t rp)
{
	const auto i = ty2logM1(rp2ty(rp));
	const auto n = rp & ((2u << i) - 1);
	return n ? n : (2u << i);
}

//	construct and return the appropriate "rvalue" from RMs ptr
template<typename U>
inline U getRValue(rms_ptr_t rp)
{
	return *(U*)rp2xp(rp);
}
template<>
inline std::string getRValue(rms_ptr_t rp)
{
	// since an empty string is written out as a ZERO rms_ptr_t...
	return rp ? std::string((const char*)rp2xp(rp), rp2n(rp)) : std::string();
}

/*
	RMsQueue is the controlling object for a "subscription", orchestrating all
	lower-level publication/subscription activities.

	A single [non-recursive] mutex is used PER-QUEUE to synchronize all access
	to the queue data structures and elements, with a single semaphore object
	(also PER-QUEUE) used to synchronize the reading and writing of the tag /
	data pairs that are the subscription-level queue elements.

	N.B. - There will be an RMsQueue instance for EVERY active subscription.
*/
class RMsQueue {
	using pq_pag_t = struct {
		td_pair_t pqTD[512];			// [4kb] page of [RMs] td_pair_t
	};

public:
	RMsQueue() {}
	~RMsQueue();

	void Append(std::string_view tag, const void* data, size_t n, RMsType ty = RMsType::Auto);
	bool Block() { return semaphore.block(), true; }
	void Close();
	void Flush();
	int Match(std::string_view tag) const;
	int Peek() const { return write - read; }
	void Signal(int flags);
	bool Validate() const;
	int State() const { return state; }
	int Wait(char* tag, size_t* tagN, void* data, size_t* dataN, int flags);

	template<typename T>
	int Wait2(std::string& tag, T& data, int flags)
	{
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

	int Wait3(std::string& tag, rms_any& data) {
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

	static int Create(std::string_view pattern);

private:
	friend class RMsRoot;
	friend bool isValidQueue(int pg);
	friend void ::rms_initialize(int np);

	/*
		NQuick is the number of items PER-QUEUE that can be "published" (but
		not yet "consumed") before needing to allocate an "indirect" page of
		queue entries.

		NQPage is the number of pages PER-QUEUE that can hold "published" but
		not yet "consumed" queue entries - these needed to be allocated as/when
		needed, but will not be returned to the RMsRoot-controlled page pool
		until this queue is closed.

		The maximum number of "published but not yet consumed" queue items is

		NQuick + (NQPage * 512) = 976,915 ... again, PER-QUEUE.
	*/
	enum {
		NQuick = 19					// # of "quick" ("direct") queue entries
	};

	void append(rms_ptr_t tag, rms_ptr_t data);
	std::tuple<bool, int, int> checkWrite();
	bool initialize(std::string_view pattern);
	// RMs queue read/write pointer -> queue indirect page #
	constexpr int qp2pq(int p) const { return (p - NQuick) >> 9; }
	// RMs queue read/write pointer -> queue indirect page index
	constexpr int qp2pi(int p) const { return (p - NQuick) & 0x1ff; }

	std::atomic<int> magic{ QueueMagic };// our magic number ('RMsQ')
	RSpinLock spin;						// our spinlock
	RSemaphore semaphore;				// our semaphore
	rms_ptr_t pattern;					// our [compiled] pattern
	std::atomic<int> state{ 0 };		// our "state"
	volatile int read{ 0 }, write{ 0 };	// [current] read, write ptrs
	volatile int pages{ 0 };			// # of [4kb] indirect queue entry pages
	volatile int prev{ 0 }, next{ 0 };	// previous, next queues
	td_pair_t quickE[NQuick]{};			// "quick" queue entries
	unsigned short pageE[4]{};			// [pages of] queue entries (computed #)
};

/*
	Primary setup call for RMs messaging system, supplying the number of 4 KiB
	pages to use for all control and data storage needs (maximum is 65,536).
*/
void initialize(int np);

/*
	Contains static "publish" methods for all supported data and tag types -
	note that "data-less" tags may be published, but publication of "tag-less"
	data is NOT allowed, as there would be no way to match with subscriptions
	for delivery.

	N.B. - there is a fundamental asymmetry when publishing using [the new]
	std::string_view as a data source... while it is a convenient and performant
	data model to use for "output", in the general case the reverse is not true,
	as there are serious issues raised with respect to "ownership" and lifetime
	of the underlying character data being "viewed".
*/
class publisher {
public:
	// publish "tag/data pair" to any subscription queues with matching patterns
	static void put_with_tag(std::string_view d, std::string_view t) { rms_publish_string(t, d); }
	static void put_with_tag(const unsigned char* d, size_t n, std::string_view t) { rms_publish_bytes(t, d, n); }
	static void put_with_tag(rms_ieee d, std::string_view t) { rms_publish_ieee(t, d); }
	static void put_with_tag(rms_int32 d, std::string_view t) { rms_publish_int32(t, d); }
	static void put_with_tag(rms_int64 d, std::string_view t) { rms_publish_int64(t, d); }
	// put_with_tag(rms_intptr d, ... will be implemented by EITHER rms_int32 OR rms_int64

	// publish tag ONLY to any subscription queues with matching patterns
	static void put_tag(std::string_view t) { rms_publish_bytes(t, nullptr, 0); }
};

// alias template for RMs data/tag pairs (tag is ALWAYS a std::string)
template<typename T>
using rms_pair = std::pair<T, std::string>;

/*
	The subscription is the object used by all consumers of published data in
	the RMs messaging system, allowing for "subscribing" to (or "expressing an
	interest" in) data having particular values or forms of "tag" information.

	After the subscription is created via the constructor or the "subscribe"
	method, clients then only need to do a typed "get" or "get_with_tag", or a
	"get_tag", depending on whether they want the data only, the data with the
	associated tag, or the tag only.

	In addition to the "functional" interface for retrieving data and/or tags
	from a subscription, there are also the "extraction operator" forms - in
	C++ standard library terminology... instead of using instance methods on a
	subscription to perform "get*" operations, we can instead use [overloaded]
	versions of operator>> (and operator>=).  A key difference is that where we
	might use something like

	auto td = sub.get_with_tag();

	to get the next data-and-tag pair from sub, we could instead do this:

	rms_pair<int> td;
	sub >> td;

	While this *might* appear more cumbersome (having to pre-declare the data),
	consider the case where we want to get multiple data (and/or tags) from the
	subscription... where we would be required to perform an assignment per each
	desired item, consider that with the "extraction op" version, we can say:

	rms_pair<int> tx, ty, tz;
	sub >> tx >> ty >> tz;

	... which most would agree is more concise.

	Finally, we have the "higher-order access" template functions for supporting
	"smart" access to subscription data-and-tag streams.  The simpler forms here
	are the try_get* [mostly] template functions for doing NON-BLOCKING accesses
	to the subscription queue, first testing for "empty" and/or "end-of-data" on
	the queue, and then only returning data if it is available AND valid.

	We wrap up these "higher-order access" functions with the for_each* set of
	queries... note that these perform the same data presence and validity tests
	of the try_get* group, but also obviate the need for you to use any explicit
	"looping" constructs to consume the messages in a queue!  Just start with an
	active subscription, supply a lambda implementing the desired functionality,
	and this will be SYNCHRONOUSLY invoked for each data, tag, or data/tag pair
	available in the queue.

	N.B. - The RMs model is to have only a SINGLE "consuming" thread per/for a
	given subscription queue... if multiple "reader" threads are desired, then
	multiple subscription objects should be created using the same pattern. On
	the other hand, multiple publishers / "writers" are [of course] supported.

	There are other more subtle implications that are influenced by the choice
	of "threading model"... in general, when only a single thread is involved in
	sending and receiving messages for a given queue (admittedly a trivial case)
	it is "safe" to use the [lower-level] get* or the "extraction op" forms for
	accessing that queue.  However, if multiple threads are interacting in what
	is fundamentally an ASYNCHRONOUS relationship, then the consuming thread is
	REQUIRED to check for a queue being in a "signaled" state (reflected in the
	"end of data" condition) before reading or "trusting" any new data - THIS is
	where the [higher-order] try_get* and for_each* come into their own, as they
	implement the required semantics in their accessing logic.
*/
class subscription {
public:
	subscription() {}
	// create a subscription queue that will receive messages matching pattern
	subscription(std::string_view pattern) { subscribe(pattern); }
	~subscription() { close(); }

	// do NON-CONSUMING wait for message, returning queue validity (blocking)
	auto block() { return id ? ((RMsQueue*)pg2xp(id))->Block() : false; }
	// force shutdown of queue - usually better to leave to destructor
	void close() {
		if (const auto id_ = id.exchange(0); id_)
			((RMsQueue*)pg2xp(id_))->Close();
	}
	// return whether queue has reached "end of data"
	bool eod() const { return id ? ((RMsQueue*)pg2xp(id))->State() != 0 : true; }
	// force discarding of all undelivered messages in queue
	void flush() { if (id) ((RMsQueue*)pg2xp(id))->Flush(); }
	// return whether there is a message waiting (i.e., would a get* block?)
	bool empty() const { return id ? ((RMsQueue*)pg2xp(id))->Peek() == 0 : true; }
	// compose "eod" and "empty" tests
	bool eod_or_empty() const {
		if (id) {
			auto q = (RMsQueue*)pg2xp(id);
			return q->State() != 0 || q->Peek() == 0;
		}
		return true;
	}
	// send an OOB (out of band) signal to any waiting reader
	void signal(int flags) { if (id) ((RMsQueue*)pg2xp(id))->Signal(flags); }

	// sign up to receive any published messages with tag matched by pattern
	void subscribe(std::string_view pattern) { close(), id = RMsQueue::Create(pattern); }

	// get next typed DATA item in queue (blocking)
	template<typename T>
	T get() {
		std::string tag;
		T data{};
		if (id)
			((RMsQueue*)pg2xp(id))->Wait2(tag, data, RMsGetData);
		return data;
	}
	// get next <any> DATA item from queue (blocking)
	// N.B. - typically used in "logging" queue readers or RMs maintenance
	template<>
	rms_any get() {
		std::string tag;
		rms_any data;
		if (id)
			((RMsQueue*)pg2xp(id))->Wait3(tag, data);
		return data;
	}
	// get next TAG item in queue (blocking)
	std::string get_tag() {
		std::string tag;
		int data;
		if (id)
			((RMsQueue*)pg2xp(id))->Wait2(tag, data, RMsGetTag);
		return tag;
	}
	// get next typed DATA item / TAG pair from queue (blocking)
	template<typename T>
	rms_pair<T> get_with_tag() {
		rms_pair<T> rp;
		if (id)
			((RMsQueue*)pg2xp(id))->Wait2(rp.second, rp.first, RMsGetTag | RMsGetData);
		return rp;
	}
	// get next <any> DATA item / tag pair from queue (blocking)
	// N.B. - typically used in "logging" queue readers or RMs maintenance
	template<>
	rms_pair<rms_any> get_with_tag() {
		rms_pair<rms_any> rp;
		if (id)
			((RMsQueue*)pg2xp(id))->Wait3(rp.second, rp.first);
		return rp;
	}

	// get next typed DATA item in queue (extraction operator >>)
	template<typename T>
	subscription& operator>>(T& data) { data = get<T>(); return *this; }
	// get next TAG item in queue (extraction operator >=)
	subscription& operator>=(std::string& tag) { tag = get_tag(); return *this; }
	// get next typed DATA item / TAG pair from queue (extraction operator >>)
	template<typename T>
	subscription& operator>>(rms_pair<T>& p) { p = get_with_tag<T>(); return *this; }

	// get next typed DATA item in queue IF POSSIBLE (non-blocking)
	template<typename T>
	bool try_get(T& data) { return !eod_or_empty() ? data = get<T>(), true : false; }
	// get next TAG item in queue IF POSSIBLE (non-blocking)
	bool try_get_tag(std::string& tag) { return !eod_or_empty() ? tag = get_tag(), true : false; }
	// get next typed DATA item / TAG pair from queue IF POSSIBLE (non-blocking)
	template<typename T>
	bool try_get_with_tag(rms_pair<T>& p) { return !eod_or_empty() ? p = get_with_tag<T>(), true : false; }

	// get and apply f to FIRST typed DATA item in queue (blocking)
	template<typename T, class UnaryFunction>
	void for_first(UnaryFunction f) { if (T data = get<T>(); !eod()) f(data); }
	// get and apply f to FIRST TAG item in queue (blocking)
	template<class UnaryFunction>
	void for_first_tag(UnaryFunction f) { if (std::string tag = get_tag(); !eod()) f(tag); }
	// get and apply f to FIRST typed DATA item / TAG pair in queue (blocking)
	template<typename T, class UnaryFunction>
	void for_first_with_tag(UnaryFunction f) { if (rms_pair<T> pair = get_with_tag<T>(); !eod()) f(pair); }

	// iterate and apply f to ALL typed DATA items in queue (blocking)
	template<typename T, class UnaryFunction>
	void for_each(UnaryFunction f) {
		T data{};
		while (data = get<T>(), !eod())
			f(data);
	}
	// iterate and apply f to ALL TAG items in queue (blocking)
	template<class UnaryFunction>
	void for_each_tag(UnaryFunction f) {
		std::string tag;
		while (tag = get_tag(), !eod())
			f(tag);
	}
	// iterate and apply f to ALL typed DATA item / TAG pairs in queue (blocking)
	template<typename T, class UnaryFunction>
	void for_each_with_tag(UnaryFunction f) {
		rms_pair<T> pair;
		while (pair = get_with_tag<T>(), !eod())
			f(pair);
	}

private:
	std::atomic<int> id{ 0 };			// our subscription queue ID
};

}
