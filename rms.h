/*
	rms.h - interface of the RMs messaging system

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

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <thread>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <map>
#include <vector>
#include <cstdint>
#include "variant.hpp"
#include "scope_guard.hpp"

//	the RMs "primitive" data types
using rms_ieee = double;
// make SURE that *our* expected 32-bit ints really ARE!
using rms_int32 = int32_t;
// retain "simple" SAFE access to 32-bit ints, for user/client code ONLY
using rms_int = rms_int32;
// we EXPECT that rms_int64 is ALWAYS equivalent to a long long
using rms_int64 = long long;
using rms_intptr = std::intptr_t; // (this MUST resolve to either 'int' or 'long long'!)

/*
	type(s) for returning ANY queue element!

	Note the use of rva::variant instead of std:: version for RECURSIVE def!

	This extremely useful tool allows us to use the "natural" solution here,
	which is to view variants as modeling "sum types"... unfortunately, these
	[in their full recursive form] are unsupported in the standard C++ library.

	Luckily for us, the Recursive Variant Authority provides a solution:

	https://github.com/codeinred/recursive-variant
*/
using rms_any = rva::variant<rms_int32, rms_int64, rms_ieee, std::string, std::vector<rva::self_t>>;
using rms_vec = std::vector<rms_any>; // (use OUTSIDE of recursive definition)

//	(allow "our" enums, i.e., based on underlying rms_int32, as well as bools)
template<class T>
concept rms_int_like =
(std::is_enum_v<T> &&
std::same_as<std::underlying_type_t<T>, rms_int32>) ||
std::same_as<T, bool>;

//	(allow "our" numbers)
template<class T>
concept rms_num_type =
std::same_as<T, rms_ieee> || rms_int_like<T> ||
(std::is_integral_v<T> && std::same_as<std::make_signed_t<T>, rms_int32>) ||
(std::is_integral_v<T> && std::same_as<std::make_signed_t<T>, rms_int64>);

//	(allow "our" string_views and DISALLOW nullptrs)
//	N.B. - the "nullptr problem" has been "fixed" in c++23; it already made no
//	sense to construct strings or string_views from nullptrs - now it is ILLEGAL
template<class T>
concept rms_string_view =
!std::same_as<T, nullptr_t> &&
std::convertible_to<T, std::string_view>;

//	(allow either "our" numbers or string_views)
template<class T>
concept rms_mid_type =
rms_num_type<T> ||
rms_string_view<T>;

//	(allow "our" numbers, string_views, OR nullptrs)
template<class T>
concept rms_max_type =
rms_mid_type<T> ||
std::same_as<T, std::nullptr_t>;

//	(allow OUTPUT - via get() et al - of "our" numbers or strings)
template<class T>
concept rms_out_type =
rms_num_type<T> || std::same_as<T, std::string>;

namespace rms {

consteval auto isPtrLongLong() { return sizeof(void*) == sizeof(long long); }

enum class RMsType {
	Auto = 0,							// [input] derive type from byte length
	Int32 = 12,							// 32-bit int
	Int64 = 13,							// 64-bit int
										// 32/64 -bit int... enough for pointer
	IntPtr = isPtrLongLong() ? Int64 : Int32,
	Ieee = 14,							// IEEE double-precision float
	Record = 15							// Record made of RMs "primitive" types
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

#ifdef _DEBUG
extern int is_valid_queue(int id);
#endif

//	RMs ptr -> RMs type
constexpr RMsType rp2ty(rms_ptr_t rp) noexcept { return (RMsType)((rp >> 12) & 0x0f); }

//	[byte] length -> RMs [string] type coding
constexpr RMsType n2ty(int nn) noexcept {
	auto i = 1;
	for (; i < 11; ++i)
		if ((unsigned)nn <= (2u << i))
			break;
	return (RMsType)i;
}

//	RMs type -> (binary logarithm of type's [max] length) - 1
constexpr int ty2logM1(RMsType ty) noexcept {
	// assert 0 <= ty <= 15
	return
		"\x0b"											// unk
		"\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b"	// iso
		"\x01"											// int32
		"\x02"											// int64
		"\x02"											// double
		"\x05"[(int)ty];								// record (1-16 fields)
}

//	RMs ptr from page, RMs type, and page offset
constexpr rms_ptr_t make_rp(int pg, RMsType ty, int po) noexcept { return (pg << 16) | ((int)ty << 12) | po; }

//	RMs ptr -> RMs ptr' with encoded data length nn
constexpr rms_ptr_t rp2rp(rms_ptr_t rp, int nn) noexcept {
	const auto pl2m1 = ty2logM1(rp2ty(rp)); // (pseudo "log2 - 1")
	const auto max_n = 2u << pl2m1;
	return (rp & ~(max_n - 1)) | (nn == max_n ? 0 : nn);
}

//	RMs ptr -> actual data length
constexpr size_t rp2n(rms_ptr_t rp) noexcept {
	const auto i = ty2logM1(rp2ty(rp));
	const auto n = rp & ((2u << i) - 1);
	return n ? n : (2u << i);
}

//	RMs ptr -> # of in-use RECORD slots
constexpr size_t rp2c(rms_ptr_t rp) noexcept { return rp2n(rp) / sizeof(rms_ptr_t); }

//	exposed [H/W] ptr -> RMs page
inline int xp2pg(void* xp) noexcept { return (int)((char*)xp - rmsB) >> 12; }

//	RMs page -> exposed [H/W] ptr
inline void* pg2xp(int pg) noexcept { return rmsB + (ptrdiff_t(pg) << 12); }

//	RMs ptr -> exposed [H/W] ptr
inline void* rp2xp(rms_ptr_t rp) noexcept {
	const auto pg = rp >> 16;
	const auto po = rp & (~((2 << ty2logM1(rp2ty(rp))) - 1) & 0x0fff);
	return (char*)pg2xp(pg) + po;
}

//	Simple spinlock class
class RSpinLock {
	std::atomic_flag lock_{};

public:
	bool test() const noexcept { return lock_.test(); }
	void lock() noexcept {
		// try simple lock...
		while (lock_.test_and_set(std::memory_order_acquire))
			// ... nope, release time slice and keep trying
			std::this_thread::yield();
	}
	void unlock() noexcept { lock_.clear(std::memory_order_release); }
};

//	Recursive spinlock class
class RSpinLockEx {
	static inline const std::thread::id nobody{};

public:
	RSpinLockEx() : owner{ nobody } {}

	void lock() noexcept {
		const auto current = std::this_thread::get_id();
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
	void unlock() noexcept {
		// with thread's final "unlock", show owner as "no thread"
		if (--count_ == 0)
			owner.store(nobody, std::memory_order_release);
	}

private:
	std::atomic<std::thread::id> owner;	// thread ID of lock holder (synch object)
	int count_{ 0 };					// # of [recursive] locks held
};

//	Simple semaphore class
class RSemaphore {
public:
	RSemaphore() {}
	~RSemaphore() { reset(); }

	void signal(int n = 1) noexcept {
		{
			std::lock_guard lock(mt);
			count_ += n;
		}
		cv.notify_one();
	}
	// N.B. - NullaryFn should be used to take the newly-available resource
	template<class NullaryFn>
	void wait(NullaryFn f = []() {}) {
		std::unique_lock lock(mt);
		cv.wait(lock, [this]() { return count_ > 0; });
		--count_, f();
	}
	// as of c++20, we can use the "std::stop_token" wait form... so
	// std::condition_variable => std::condition_variable_any to support this
	// N.B. - NullaryFn should be used to take the newly-available resource
	template<class NullaryFn>
	void wait(std::stop_token st, NullaryFn f = []() {}) {
		std::unique_lock lock(mt);
		if (cv.wait(lock, st, [this]() { return count_ > 0; }))
			--count_, f();
	}
	// "clear" or "reset" the semaphore
	// N.B. - "wipes" previous signals, does NOT release waiting threads!
	void reset() noexcept { count_ = 0; }

private:
	int count_{ 0 };					// synch object
	std::condition_variable_any cv;		// condition variable
	RSpinLock mt;						// mutex for above
};

static bool isValidQueue(int pg);

/*
	RMsRoot is the central controlling object of the RMs message system, with
	the primary tasks of allocating and freeing all memory from our page pool,
	as well as distributing published messages on behalf of the higher levels.

	A single [recursive] mutex object is used to synchronize all access to the
	root data structures and elements.

	N.B. - There will be exactly ONE [singleton] instance of RMsRoot.
*/
class RMsRoot {
public:
	RMsRoot() {}
	~RMsRoot() {}

	int AddQueue(int pg) noexcept;
	int AllocPage();
	rms_ptr_t AllocRP(RMsType ty, size_t n);
	bool CheckAlloc(int pg) noexcept;
	bool CheckQueue(int pg) noexcept;
	void Distribute(std::string_view tag, rms_max_type auto d);
	void FreePage(int pg) noexcept;
	void FreePair(td_pair_t p) noexcept;
	void FreeRP(rms_ptr_t rp) noexcept;
	bool Initialize(size_t np) noexcept;
	rms_ptr_t MakeRecord(size_t n);
	rms_ptr_t Marshal(rms_mid_type auto d);
	void RemoveQueue(int pg) noexcept;

private:
	friend class publisher;
	friend bool isValidQueue(int pg);

	// Link RMs ptr at front of typed free list
	// N.B. - Call with RMsRoot mutex LOCKED!
	void inline free_rp(rms_ptr_t rp) noexcept {
		const auto ty = rp2ty(rp);
		*(int*)rp2xp(rp) = typeFree[(int)ty], typeFree[(int)ty] = rp;
	}
	// INDIVIDUALLY free RMs ptrs in active RECORD slots
	// N.B. - Call with RMsRoot mutex LOCKED!
	void inline free_rec(rms_ptr_t rp) noexcept {
		const auto rec = (rms_ptr_t*)rp2xp(rp);
		const auto n = rp2c(rp);
		for (std::remove_const_t<decltype(n)> i = 0; i < n; ++i)
			free_rp(rec[i]);
	}
	// N.B. - Call with RMsRoot mutex LOCKED!
	void initTypedPageAsFree(int pg, RMsType ty) noexcept;

	int magic{ RootMagic };				// root magic number ('RMsR')
	RSpinLockEx spin;					// root [recursive] spinlock
										// accelerator for tag->lookups
	std::multimap<std::string, int, std::less<>> matches;
	volatile int pages { 0 };			// # of [4kb] pages RESERVED
	volatile int committed{ 0 };		// # of [4kb] pages COMMITTED
	volatile int high{ 0 };				// # used / next available
	volatile int pageFree{ 0 };			// chain of free pages
	volatile int queueHead{ 0 };		// doubly-linked list of queues
	volatile int queueTail{ 0 };
	rms_ptr_t typeFree[16]{};			// "typed" (sized) free chains

	// N.B. - Call with RMsRoot mutex LOCKED!
	auto get_matches(std::string_view tag) -> decltype(matches.equal_range(tag));
};

extern RMsRoot* rmsRoot;				// shared "root" object

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

	// construct and return the appropriate "rvalue" from RMs ptr
	template<rms_out_type U>
	static inline constexpr U getRValue(rms_ptr_t rp) { return *(U*)rp2xp(rp); }
	template<>
	static inline constexpr std::string getRValue(rms_ptr_t rp) {
		// since an empty string is written out as a ZERO rms_ptr_t...
		return rp ? std::string((const char*)rp2xp(rp), rp2n(rp)) : std::string();
	}

public:
	RMsQueue() {}
	~RMsQueue();

	void Append(std::string_view tag, rms_mid_type auto d);
	void Append(std::string_view tag, nullptr_t d);
	void Append(std::string_view tag, rms_ptr_t dRP);
	void Close() noexcept;
	void Flush() noexcept;
	bool Match(std::string_view tag) const;
	// N.B. - DIRECTLY returns a [possibly negative] difference, use abs as needed
	constexpr int Peek() const noexcept { return write - read; }
	// Perform out - of - band "signaling" of our queue - typically used to set an
	// "end of data" state when we want to shut down the queue, and let any readers
	// know that no additional data will be forthcoming.
	void Signal(int flags) noexcept { state |= flags, semaphore.signal(); }
	bool Validate() const noexcept;
	int State() const noexcept { return state; }

	static int Create(std::string_view pattern);

private:
	friend class RMsRoot;
	friend class subscription;
	friend bool isValidQueue(int pg);
	friend void initialize(int np);

	/*
		NQuick is the number of items PER-QUEUE that can be "published" (but
		not yet "consumed") before needing to allocate an "indirect" page of
		queue entries.

		NQPage is the number of pages PER-QUEUE that can hold "published" but
		not yet "consumed" queue entries - these need to be allocated as/when
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
	constexpr int qp2pq(int p) const noexcept { return (p - NQuick) >> 9; }
	// RMs queue read/write pointer -> queue indirect page index
	constexpr int qp2pi(int p) const noexcept { return (p - NQuick) & 0x1ff; }
	// boilerplate code used by all "wait_*" fns
	// N.B. - executes "take resource" fn while still under semaphore lock!
	template<class TakeResource>
	auto check_early_exit_and_wait(std::stop_token* st, TakeResource f) noexcept {
		if (state != 0)
			return true;
		else if (st != nullptr)
			semaphore.wait(*st, f);
		else
			semaphore.wait(f);
		return state != 0 || (st != nullptr && st->stop_requested());
	}
	// more boilerplate, used whenever queue PAIRs are pulled off and returned
	// N.B. - Call with [our] RMsQueue mutex LOCKED!
	constexpr auto next_pair() const noexcept {
		return read < NQuick ?
			quickE[read] :
			((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)];
	}
	// still more boilerplate, take next pair AND tidy up [under lock]
	auto get_next_pair_and_remove() noexcept {
		std::lock_guard acquire(spin);
		const auto td = next_pair();
		if (++read == write)
			read = 0, write = 0; // reset to "quick" entries when we can
		return td;
	}
	// ("generic" single-value wait)
	template<rms_out_type T>
	int wait_T(std::string& tag, T& data, int flags, std::stop_token* st = nullptr);
	// (specialized wait used ONLY for rms_any)
	int wait_any(std::string& tag, rms_any& data, std::stop_token* st = nullptr);
	// (specialized wait used ONLY for records)
	rms_ptr_t wait_rec(std::string& tag, std::stop_token* st = nullptr);

	std::atomic<int> magic{ QueueMagic };// our magic number ('RMsQ')
	RSpinLock spin;						// our spinlock
	RSemaphore semaphore;				// our semaphore
	rms_ptr_t pattern{};				// our [compiled] pattern
	std::atomic<int> state{ 0 };		// our "state"
	volatile int read{ 0 }, write{ 0 };	// [current] read, write ptrs
	volatile int pages{ 0 };			// # of [4kb] indirect queue entry pages
	volatile int prev{ 0 }, next{ 0 };	// previous, next queues
	td_pair_t quickE[NQuick]{};			// "quick" queue entries
	unsigned short pageE[4]{};			// [pages of] queue entries (computed #)

	static int NQPage;					// # of pages of RMsQueue "extras"
};

/*
	Primary setup call for RMs messaging system, supplying the number of 4 KiB
	pages to use for all control and data storage needs (maximum is 65,536).
*/
void initialize(int np);

/*
	Create a string representation of the supplied rms_any [recursive variant].
*/
std::string to_string(const rms_any& v);

/*
	Contains static "publish" methods for all supported data and tag types -
	note that "data-less" tags may be published, but publication of "tag-less"
	data is NOT allowed, as there would be no way to match with subscriptions
	for delivery.

	N.B. - put(...) and put_with_tag(...) public methods are EXACTLY equivalent,
	but ...with_tag versions MAY get deprecated, as we add new methods / syntax
	supporting "record" pub/sub operations.
*/
class publisher {
	friend class RMsRoot;
	friend class RMsQueue;

	// return RMs "type" from associated C/C++ language type
	template<typename T>
	static constexpr auto ty_of(T v) noexcept { return RMsType::Auto; }
	template<>
	static constexpr auto ty_of(rms_int32 v) noexcept { return RMsType::Int32; }
	template<>
	static constexpr auto ty_of(rms_int64 v) noexcept { return RMsType::Int64; }
	template<>
	static constexpr auto ty_of(rms_ieee v) noexcept { return RMsType::Ieee; }
	//rms_intptr already handled above as EITHER rms_int32 OR rms_int64

	// (n_of() will NOT see a nullptr_t)
	static constexpr auto n_of(rms_num_type auto v) noexcept { return sizeof v; }

	static constexpr auto n_of(std::string_view v) noexcept { return v.size(); }

	// (make SURE any marshaling will store enums / bools as rms_int32 values)
	static constexpr auto coerce(rms_int_like auto v) noexcept { return (rms_int32)v; }

	// (make SURE any marshaling will store integrals as rms_int32/rms_int64!)
	static constexpr auto coerce(rms_num_type auto v) noexcept {
		using t = decltype(v);
		if constexpr (std::same_as<t, rms_ieee>)
			return v;
		else
			return (std::make_signed_t<t>)v;
	}

	static constexpr auto coerce(std::string_view v) noexcept { return v; }

	static void publish(std::string_view t, rms_num_type auto d) { rmsRoot->Distribute(t, coerce(d)); }

	static void publish(std::string_view t, std::string_view d) { rmsRoot->Distribute(t, d); }

	static void publish(std::string_view t, nullptr_t d) { rmsRoot->Distribute(t, nullptr); }

	static inline void publish_rec(std::string_view t, rms_mid_type auto&& ...d) {
		std::lock_guard acquire(rmsRoot->spin);
		auto tqp = rmsRoot->get_matches(t);
		// deliver to ALL cached tag->queue pairs which [now] match
		for (auto i = tqp.first; i != tqp.second; ++i) {
			const auto rec = rmsRoot->MakeRecord(sizeof...(d));
			auto e = (rms_ptr_t*)rp2xp(rec);
			((*e++ = rmsRoot->Marshal(d)), ...);
			((RMsQueue*)pg2xp(i->second))->Append(t, rec);
		}
	}

public:
	// publish "tag/data pair" to any subscription queues with matching patterns
	static void put_with_tag(rms_num_type auto d, std::string_view t) { publish(t, d); }

	static void put_with_tag(rms_string_view auto d, std::string_view t) { publish(t, d); }

	// publish "tag/data pair" to any subscription queues with matching patterns
	static void put(std::string_view t, rms_num_type auto d) { publish(t, d); }

	static void put(std::string_view t, rms_string_view auto d) { publish(t, d); }

	// publish "tag/RECORD pair" to any subscription queues with matching patterns
	static constexpr void put_rec(std::string_view t, rms_mid_type auto ...d) { publish_rec(t, coerce(d)...); }

	// publish tag ONLY to any subscription queues with matching patterns
	static void put_tag(std::string_view t) { publish(t, nullptr); }
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

	* NEW for v3.0: to deal with the ugly issue of *potentially* asynchronous
	interleaving of the multiple "fields" in a logical "record", RMs has added...
	records!

	In addition to safe ("atomic") grouping of the fields in a record, RMs now
	also lets you "have it your way": in addition to the usual RMs primitive types,
	when publishing a record [to a subscription queue], you can also use bools and
	enums, e.g.

	enum class MyEnum { first, second };
	publisher::put_rec("tag", 42, 347.2, "string", true, MyEnum::second);

	... to retrieve

	auto rec = q.get_rec();
	auto [a, b, c, d, e] = q.unpack_rec<int, double, string, bool, MyEnum>(rec);

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

	* NEW for v3.5: with some revisions to the semaphore accessing logic as well
	as in the suggested usage model, RMs now allows BOTH multiple queue writers
	AND multiple queue readers!  See the implementations in RMsCore.cpp of the
	support routines RMsQueue::{wait_T,wait_any,wait_rec} for actual code, but
	the key idea is that when you wait on a queue's semaphore, you pass a lambda
	to it of the [usually small but] important code you want executed when the
	resource becomes available, BUT STILL UNDER THE SEMAPHORE'S LOCK!

	In case the above is obscure, it is now trivial to create multiple jthreads
	where each has a body doing something like

	q.for_each_rec([&](auto rec) {
		const auto [a, b, c] = q.unpack_rec<int, double, long long>(rec);
		// ... do some work parameterized by the above record fields...
	});
*/
class subscription {
public:
	subscription() {}
	// create a subscription queue that will receive messages matching pattern
	subscription(std::string_view pattern) { subscribe(pattern); }
	~subscription() { close(); }

	// force shutdown of queue - usually better to leave to destructor
	void close() noexcept {
		if (const auto id_ = id.exchange(0); id_)
			((RMsQueue*)pg2xp(id_))->Close();
	}
	// return whether queue has reached "end of data"
	constexpr bool eod() const noexcept { return id ? ((RMsQueue*)pg2xp(id))->State() != 0 : true; }
	// force discarding of all undelivered messages in queue
	void flush() noexcept { if (id) ((RMsQueue*)pg2xp(id))->Flush(); }
	// expose the actual *count* present in the underlying queue object
	// N.B. - DIRECTLY returns a [possibly negative] difference, use abs as needed
	constexpr auto peek() const noexcept { return id ? ((RMsQueue*)pg2xp(id))->Peek() : 0; }
	// return whether there is a message waiting (i.e., would a get* block?)
	constexpr auto empty() const noexcept { return peek() == 0; }
	// compose "eod" and "empty" tests
	constexpr bool eod_or_empty() const noexcept { return eod() || empty(); }
	// send an OOB (out of band) signal to any waiting reader
	void signal(int flags) noexcept { if (id) ((RMsQueue*)pg2xp(id))->Signal(flags); }
	// EXPERIMENTAL: append data DIRECTLY to queue, bypassing usual publication
	// N.B. - useful for maintenance / "Quis custodiet ipsos custodes?" problem
	void push_back(std::string_view t, std::string_view d) { if (id) ((RMsQueue*)pg2xp(id))->Append(t, d); }

	// sign up to receive any published messages with tag matched by pattern
	void subscribe(std::string_view pattern) { close(), id = RMsQueue::Create(pattern); }

	// get next typed DATA item in queue (blocking)
	template<typename T>
	constexpr T get(std::stop_token* st = nullptr) {
		T data{};
		if (std::string tag; id)
			((RMsQueue*)pg2xp(id))->wait_T(tag, data, RMsGetData, st);
		return data;
	}
	// get next <any> DATA item from queue (blocking)
	// N.B. - typically used in "logging" queue readers or for "packed" RECORDs
	template<>
	rms_any get(std::stop_token* st) {
		rms_any data{};
		if (std::string tag; id)
			((RMsQueue*)pg2xp(id))->wait_any(tag, data, st);
		return data;
	}
	// get next TAG item in queue (blocking)
	std::string get_tag(std::stop_token* st = nullptr) {
		std::string tag;
		if (int data; id)
			((RMsQueue*)pg2xp(id))->wait_T(tag, data, RMsGetTag, st);
		return tag;
	}
	// get next typed DATA item / TAG pair from queue (blocking)
	template<typename T>
	constexpr rms_pair<T> get_with_tag(std::stop_token* st = nullptr) {
		rms_pair<T> rp{};
		if (id)
			((RMsQueue*)pg2xp(id))->wait_T(rp.second, rp.first, RMsGetTag | RMsGetData, st);
		return rp;
	}
	// get next <any> DATA item / tag pair from queue (blocking)
	// N.B. - typically used in "logging" queue readers or for "packed" RECORDs
	template<>
	rms_pair<rms_any> get_with_tag(std::stop_token* st) {
		rms_pair<rms_any> rp{};
		if (id)
			((RMsQueue*)pg2xp(id))->wait_any(rp.second, rp.first, st);
		return rp;
	}
	// get next INTERNAL RECORD object from queue (blocking)
	// N.B. - SHOULD not be returned to "user" code
	// N.B.2 - MOST useful if "expanded" into a tuple by unpack_rec [see below]
	// N.B.3 - you MUST use unpack_rec... otherwise, RMs objects WILL be leaked
	rms_ptr_t get_rec(std::stop_token* st = nullptr) {
		if (std::string tag; id)
			if (const auto rec = ((RMsQueue*)pg2xp(id))->wait_rec(tag, st); rec != RMsStatusSignaled)
				return rec;
		return 0; // (error case: queue isn't open)
	}

	/*
		utility fn to "explode" an internal RECORD object into a tuple
		- upon completion, the internal RECORD object will be properly freed

		Note the use of a "scope guard", one of those super-useful idioms that
		hasn't yet made it into the standard (after over 20 years)... this one
		is particularly lightweight, flexible, and simple to use, from

		https://github.com/ricab/scope_guard
	*/
	template<typename ...T>
	static constexpr std::tuple <T...> unpack_rec(rms_ptr_t rec) {
		if (rec != 0) {
			auto guard = sg::make_scope_guard([rec]() noexcept -> void { rmsRoot->FreeRP(rec); });
			auto e = (rms_ptr_t*)rp2xp(rec);
			// (scope guard will free the RMs record AFTER unpacking is complete)
			return { RMsQueue::getRValue<T>(*e++)... };
		}
		return {}; // (error case: rec is from unopened queue or just invalid)
	}

	// utility fn to "explode" a vec of rms_any values into a tuple
	template<typename ...T>
	static constexpr std::tuple <T...> unpack_any(const rms_vec& vec) {
		auto e = vec.cbegin();
		return { std::get<T>(*e++)... };
	}

	// get next typed DATA item in queue (extraction operator >>)
	template<typename T>
	constexpr subscription& operator>>(T& data) { data = get<T>(); return *this; }
	// get next TAG item in queue (extraction operator >=)
	constexpr subscription& operator>=(std::string& tag) { tag = get_tag(); return *this; }
	// get next typed DATA item / TAG pair from queue (extraction operator >>)
	template<typename T>
	constexpr subscription& operator>>(rms_pair<T>& p) { p = get_with_tag<T>(); return *this; }

	// get next typed DATA item in queue IF POSSIBLE (non-blocking)
	template<typename T>
	constexpr bool try_get(T& data) { return !eod_or_empty() ? data = get<T>(), true : false; }
	// get next TAG item in queue IF POSSIBLE (non-blocking)
	constexpr bool try_get_tag(std::string& tag) { return !eod_or_empty() ? tag = get_tag(), true : false; }
	// get next typed DATA item / TAG pair from queue IF POSSIBLE (non-blocking)
	template<typename T>
	constexpr bool try_get_with_tag(rms_pair<T>& p) { return !eod_or_empty() ? p = get_with_tag<T>(), true : false; }

	// get and apply f to FIRST typed DATA item in queue (blocking)
	template<typename T, class UnaryFn>
	constexpr void for_first(UnaryFn f) { if (T data = get<T>(); !eod()) f(data); }
	// get and apply f to FIRST TAG item in queue (blocking)
	template<class UnaryFn>
	constexpr void for_first_tag(UnaryFn f) { if (std::string tag = get_tag(); !eod()) f(tag); }
	// get and apply f to FIRST typed DATA item / TAG pair in queue (blocking)
	template<typename T, class UnaryFn>
	constexpr void for_first_with_tag(UnaryFn f) { if (rms_pair<T> pair = get_with_tag<T>(); !eod()) f(pair); }

	// iterate and apply f to ALL typed DATA items in queue (blocking)
	template<typename T, class UnaryFn>
	constexpr void for_each(UnaryFn f, std::stop_token* st = nullptr) {
		auto stop_requested = [](auto st) {
			return st != nullptr ? st->stop_requested() : false;
		};
		T data;
		while (data = get<T>(st), !stop_requested(st) && !eod())
			f(data);
	}
	// iterate and apply f to ALL internal RECORD objects in queue (blocking)
	template<class UnaryFn>
	constexpr void for_each_rec(UnaryFn f, std::stop_token* st = nullptr) {
		auto stop_requested = [](auto st) {
			return st != nullptr ? st->stop_requested() : false;
		};
		rms_ptr_t rec{};
		while (rec = get_rec(st), !stop_requested(st) && !eod())
			f(rec);
	}
	// iterate and apply f to ALL TAG items in queue (blocking)
	template<class UnaryFn>
	constexpr void for_each_tag(UnaryFn f, std::stop_token* st = nullptr) {
		auto stop_requested = [](auto st) {
			return st != nullptr ? st->stop_requested() : false;
		};
		std::string tag;
		while (tag = get_tag(st), !stop_requested(st) && !eod())
			f(tag);
	}
	// iterate and apply f to ALL typed DATA item / TAG pairs in queue (blocking)
	template<typename T, class UnaryFn>
	constexpr void for_each_with_tag(UnaryFn f, std::stop_token* st = nullptr) {
		auto stop_requested = [](auto st) {
			return st != nullptr ? st->stop_requested() : false;
		};
		rms_pair<T> pair;
		while (pair = get_with_tag<T>(st), !stop_requested(st) && !eod())
			f(pair);
	}

private:
	std::atomic<int> id{ 0 };			// our subscription queue ID
};

}
