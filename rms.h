/*
	rms.h - interface of the RMs messaging system

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

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <thread>
#include <string>
#include <utility>

#ifdef RMSDLL_EXPORTS
#define RMS_EXPORT extern "C" __declspec(dllexport)
#else
#define RMS_EXPORT extern "C"
#endif

//	the RMs "primitive" data types
typedef double rms_ieee;
typedef int rms_int32;
typedef long long rms_int64;

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
int rms_close(int id);
RMS_EXPORT
int rms_flush(int id);
RMS_EXPORT
int rms_initialize(int np);
#ifdef _DEBUG
RMS_EXPORT
int rms_is_valid_queue(int id);
#endif
RMS_EXPORT
int rms_peek(int id);
RMS_EXPORT
int rms_publish_bytes(const char* tag, const unsigned char* data, int n);
RMS_EXPORT
int rms_publish_ieee(const char* tag, rms_ieee data);
RMS_EXPORT
int rms_publish_int32(const char* tag, rms_int32 data);
RMS_EXPORT
int rms_publish_int64(const char* tag, rms_int64 data);
RMS_EXPORT
int rms_publish_string(const char* tag, const char* data);
RMS_EXPORT
int rms_signal(int id, int flags);
RMS_EXPORT
int rms_subscribe(const char* pattern);
RMS_EXPORT
int rms_wait_bytes(int id, char tag[], int* tagN, unsigned char data[], int* dataN, int flags);
RMS_EXPORT
int rms_wait_ieee(int id, char tag[], int* tagN, rms_ieee* data, int flags);
RMS_EXPORT
int rms_wait_int32(int id, char tag[], int* tagN, rms_int32* data, int flags);
RMS_EXPORT
int rms_wait_int64(int id, char tag[], int* tagN, rms_int64* data, int flags);
RMS_EXPORT
int rms_wait_string(int id, char tag[], int* tagN, char data[], int* dataN, int flags);

namespace rms {

enum {
	RMsTypeAuto = 0,					// derive type from byte length
	RMsTypeInt32 = 12,					// 32-bit int
	RMsTypeInt64 = 13,					// 64-bit int
	RMsTypeIeee = 14					// IEEE double-precision float
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
	RMsGetTag = 2						// request "tag" during a get*<> operation
};

//	# of [4KB] pages to commit at each growth
const int PageIncrement = 16;

const int RootMagic = 0x52734d52;		// ('RMsR')
const int QueueMagic = 0x51734d52;		// ('RMsQ')

//#define DUMP_ALL
#if defined(_DEBUG) || defined(DUMP_ALL)
//#define CHECK_ALLOC_FULL
//#define CHECK_QUEUE_FULL
//#define DUMP_EXPORTED
#define DUMP_QUEUE
#define DUMP_ROOT
#else
#define CHECK_NOTHING
#endif /* defined(_DEBUG) || defined(DUMP_ALL) */

typedef unsigned int rms_ptr_t;			// [RMs] ptr type

//	RMs tag/data pair (data may be empty)
typedef struct {
	rms_ptr_t tag;						// [RMs] ptr to tag
	rms_ptr_t data;						// [RMs] ptr to data
} td_pair_t;

extern char* rmsB;						// shared memory view pointer(s)

//	RMs page -> exposed [H/W] ptr
inline void* pg2xp(int pg)
{
	return rmsB + (pg << 12);
}

//	RMs ptr -> RMs type
inline int rp2ty(rms_ptr_t rp)
{
	return (rp >> 12) & 0x0f;
}

//	RMs type -> (binary logarithm of type's [max] length) - 1
inline int ty2logM1(int ty)
{
	// assert 0 <= ty <= 15
	return
		"\x0b"											// unk
		"\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b"	// iso
		"\x01"											// int32
		"\x02"											// int64
		"\x02"											// double
		"\x0b"[ty];										// unk
}

//	exposed [H/W] ptr -> RMs page
inline int xp2pg(void* xp)
{
	return (int)((char*)xp - rmsB) >> 12;
}

//	RMs ptr -> exposed [H/W] ptr
inline void* rp2xp(rms_ptr_t rp)
{
	const int pg = rp >> 16;
	const int po = rp & (~((2 << ty2logM1(rp2ty(rp))) - 1) & 0x0fff);
	return (char*)pg2xp(pg) + po;
}

//	Simple spinlock class
class RSpinLock {
public:
	inline void lock()
	{
		// try simple lock...
		while (lock_.test_and_set(std::memory_order_acquire))
			// ... nope, release time slice and keep trying
			std::this_thread::yield();
	}
	inline void unlock() { lock_.clear(std::memory_order_release); }

private:
	std::atomic_flag lock_ = ATOMIC_FLAG_INIT;
};

//	Recursive spinlock class
class RSpinLockEx {
public:
	RSpinLockEx() : nobody(std::thread::id::id()), owner(nobody) {}

	inline void lock() {
		auto current = std::this_thread::get_id();
		auto id = nobody;
		// try simple lock (for current thread)...
		if (owner != current && !owner.compare_exchange_strong(id, current))
			do
				// ... nope, release time slice and keep trying
				std::this_thread::yield(), id = nobody;
			while (!owner.compare_exchange_weak(id, current));
		// ... we HAVE the lock, increment our "recursion" count
		count++;
	}
	inline void unlock() {
		// with thread's final "unlock", show owner as "no thread"
		if (--count == 0)
			owner.store(nobody, std::memory_order_release);
	}

private:
	const std::thread::id nobody;		// value matching NO thread
	std::atomic<std::thread::id> owner;	// thread ID of lock holder (synch object)
	int count = 0;						// # of [recursive] locks held
};

//	Helper class supporting "RAII-style" locking
template<class T>
class lock_guard {
public:
	lock_guard<T>(T& o) : lock_(o) { lock_.lock(); }
	~lock_guard() { lock_.unlock(); }

private:
	T& lock_;							// Lock object supporting lock() and unlock()
};

//	Simple semaphore class
class RSemaphore {
public:
	RSemaphore() {}
	~RSemaphore() {
		count_ = 0;
		cv.notify_all();
	}

	inline void signal(int n = 1) {
		if (count_.fetch_add(n) < 0)
			cv.notify_one();
	}
	inline void wait() {
		if (count_.fetch_sub(1) > 0)
			return; // got it!
		std::unique_lock<std::mutex> lock(mt);
		cv.wait(lock);
	}

private:
	std::atomic<int> count_ = 0;		// synch object
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

	int AddQueue(int pg);
	int AllocPage();
	rms_ptr_t AllocRP(int ty, int n);
#if defined(CHECK_ALLOC) || defined(CHECK_ALLOC_FULL)
	int CheckAlloc(int pg);
#endif
#if defined(CHECK_QUEUE) || defined(CHECK_QUEUE_FULL)
	int CheckQueue(int pg);
#endif
	int Distribute(const char* tag, const void* data, int n, int ty = 0);
	void FreePage(int pg);
	void FreePair(td_pair_t p);
	void FreeRP(rms_ptr_t rp);
	int Initialize(int np);
	void RemoveQueue(int pg);

private:
	friend int isValidQueue(int pg);

	// Link RMs ptr at front of typed free list
	// N.B. - Call with RMsRoot mutex LOCKED!
	inline void free_rp(rms_ptr_t rp) {
		const int ty = rp2ty(rp);
		*(int*)rp2xp(rp) = typeFree[ty], typeFree[ty] = rp;
	}
	void initTypedPageAsFree(int pg, int ty);
	// RMs ptr from page, RMs type, and page offset
	inline rms_ptr_t make_rp(int pg, int ty, int po) const
	{
		return (pg << 16) | (ty << 12) | po;
	}
	// length -> RMs type
	inline int n2ty(int n) const
	{
		int i = 1;
		for (; i < 11; i++)
			if (n <= (2 << i))
				break;
		return i;
	}
	// RMs ptr -> RMs ptr' with encoded data length n
	inline rms_ptr_t rp2rp(rms_ptr_t rp, int n) const
	{
		const int i = ty2logM1(rp2ty(rp));
		const int N = (n == (2 << i)) ? 0 : n;
		return (rp & ~((2 << i) - 1)) | N;
	}

	int magic = RootMagic;				// root magic number ('RMsR')
	RSpinLockEx spin;					// root [recursive] spinlock
	volatile int pages;					// # of [4kb] pages RESERVED
	volatile int committed;				// # of [4kb] pages COMMITTED
	volatile int high;					// # used / next available
	volatile int pageFree = 0;			// chain of free pages
	volatile int queueHead = 0;			// doubly-linked list of queues
	volatile int queueTail = 0;
	rms_ptr_t typeFree[16] = {};		// "typed" (sized) free chains
};

extern RMsRoot* rmsRoot;				// shared "root" object

//	RMs ptr -> actual data length
inline int rp2n(rms_ptr_t rp)
{
	const int i = ty2logM1(rp2ty(rp));
	const int n = rp & ((2 << i) - 1);
	return n ? n : (2 << i);
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
	return std::string((const char*)rp2xp(rp), rp2n(rp));
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
	typedef struct {
		td_pair_t pqTD[512];			// [4kb] page of [RMs] td_pair_t
	} pq_pag_t;

public:
	RMsQueue() {}

	int Append(const char* tag, const void* data, int n, int ty = 0);
	int Close();
	int Flush();
	int Match(const char* tag) const;
	int Peek() const { return write - read; }
	int Signal(int flags);
#if defined(CHECK_QUEUE) || defined(CHECK_QUEUE_FULL)
	int Validate();
#endif
	int State() const { return state; }
	int Wait(char* tag, int* tagN, void* data, int* dataN, int flags);

	template<typename T>
	int Wait2(std::string& tag, T& data, int flags)
	{
		if (state)
			return RMsStatusSignaled;	// early out; indicate "signaled"
		semaphore.wait();
		if (state)
			return RMsStatusSignaled;	// early out; indicate "signaled"
		const td_pair_t td = read < NQuick ?
			quickE[read] :
			((pq_pag_t*)pg2xp(pageE[qp2pq(read)]))->pqTD[qp2pi(read)];
		if (flags & RMsGetTag)
			tag = getRValue<std::string>(td.tag);
		if (flags & RMsGetData)
			data = getRValue<T>(td.data);
		// "consume" element
		rmsRoot->FreePair(td);
		lock_guard<RSpinLock> acquire(spin);
		if (++read == write)
			read = 0, write = 0; // reset to "quick" entries when we can
		return flags;
	}

	static int Create(const char* pattern);

private:
	friend class RMsRoot;
	friend int isValidQueue(int pg);
	friend int ::rms_initialize(int np);

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

	int append(rms_ptr_t tag, rms_ptr_t data);
	int checkWrite(int& pg, int& pi);
	int initialize(const char* pattern);
	// RMs queue read/write pointer -> queue indirect page #
	inline int qp2pq(int p) const { return (p - NQuick) >> 9; }
	// RMs queue read/write pointer -> queue indirect page index
	inline int qp2pi(int p) const { return (p - NQuick) & 0x1ff; }

	std::atomic<int> magic = QueueMagic;// our magic number ('RMsQ')
	RSpinLock spin;						// our spinlock
	RSemaphore semaphore;				// our semaphore
	rms_ptr_t pattern;					// our pattern
	std::atomic<int> state = 0;			// our "state"
	volatile int read = 0, write = 0;	// [current] read, write ptrs
	volatile int pages = 0;				// # of [4kb] indirect queue entry pages
	volatile int prev = 0, next = 0;	// previous, next queues
	td_pair_t quickE[NQuick] = {};		// "quick" queue entries
	unsigned short pageE[4] = {};		// [pages of] queue entries (computed #)
};

/*
	Primary setup call for RMs messaging system, supplying the number of 4 KiB
	pages to use for all control and data storage needs (maximum is 65,536).
*/
int initialize(int np);

/*
	Contains function template versions of static "publish" methods for all
	supported data and tag types - note that "data-less" tags may be published,
	but publication of "tag-less" data is NOT allowed, as there would be no way
	to match with subscriptions for delivery.
*/
class publisher {
	static inline const char* tagValue(const char* t) { return t; }
	static inline const char* tagValue(const std::string& t) { return t.c_str(); }

public:
	// publish "tag/data pair" to any subscription queues with matching patterns
	template<typename T>
	static int put_with_tag(const char* d, T t) { return rms_publish_string(tagValue(t), d); }
	template<typename T>
	static int put_with_tag(const std::string& d, T t) { return rms_publish_bytes(tagValue(t), (const unsigned char*)d.data(), (int)d.length()); }
	template<typename T>
	static int put_with_tag(const unsigned char* d, int n, T t) { return rms_publish_bytes(tagValue(t), d, n); }
	template<typename T>
	static int put_with_tag(rms_ieee d, T t) { return rms_publish_ieee(tagValue(t), d); }
	template<typename T>
	static int put_with_tag(rms_int32 d, T t) { return rms_publish_int32(tagValue(t), d); }
	template<typename T>
	static int put_with_tag(rms_int64 d, T t) { return rms_publish_int64(tagValue(t), d); }

	// publish tag ONLY to any subscription queues with matching patterns
	template<typename T>
	static int put_tag(T t) { return rms_publish_bytes(tagValue(t), NULL, 0); }
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
	subscription(const std::string& pattern) { subscribe(pattern); }
	subscription(const char* pattern) { subscribe(pattern); }
	~subscription() { close(); }

	// force shutdown of queue - usually better to leave to destructor
	int close() {
		const int id_ = id.exchange(0);
		return id_ ? ((RMsQueue*)pg2xp(id_))->Close() : 0;
	}
	// return whether queue has reached "end of data"
	bool eod() const { return id ? ((RMsQueue*)pg2xp(id))->State() != 0 : true; }
	// force discarding of all undelivered messages in queue
	int flush() { return id ? ((RMsQueue*)pg2xp(id))->Flush() : 0; }
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
	int signal(int flags) { return id ? ((RMsQueue*)pg2xp(id))->Signal(flags) : 0; }

	// sign up to receive any published messages with tag matched by pattern
	void subscribe(const std::string& pattern) { close(), id = RMsQueue::Create(pattern.c_str()); }
	void subscribe(const char* pattern) { close(), id = RMsQueue::Create(pattern); }

	// get next typed DATA item in queue
	template<typename T>
	T get() {
		std::string tag;
		T data;
		if (id)
			((RMsQueue*)pg2xp(id))->Wait2(tag, data, RMsGetData);
		return data;
	}
	// get next TAG item in queue
	std::string get_tag() {
		std::string tag;
		int data;
		if (id)
			((RMsQueue*)pg2xp(id))->Wait2(tag, data, RMsGetTag);
		return tag;
	}
	// get next typed DATA item / TAG pair from queue
	template<typename T>
	rms_pair<T> get_with_tag() {
		std::string tag;
		T data;
		if (id)
			((RMsQueue*)pg2xp(id))->Wait2(tag, data, RMsGetTag | RMsGetData);
		return std::make_pair(data, tag);
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

	// get ALL typed DATA items in queue (blocking)
	template<typename T, class UnaryFunction>
	void for_each(UnaryFunction f) {
		T data;
		while (data = get<T>(), !eod())
			f(data);
	}
	// get ALL TAG items in queue (blocking)
	template<class UnaryFunction>
	void for_each_tag(UnaryFunction f) {
		std::string tag;
		while (tag = get_tag(), !eod())
			f(tag);
	}
	// get ALL typed DATA item / TAG pairs in queue (blocking)
	template<typename T, class UnaryFunction>
	void for_each_with_tag(UnaryFunction f) {
		rms_pair<T> pair;
		while (pair = get_with_tag<T>(), !eod())
			f(pair);
	}

private:
	std::atomic<int> id = 0;			// our subscription queue ID
};

}
