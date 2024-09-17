/*
	rglob.h - interface (AND implementation) of the RGlob "glob" pattern-matcher

	Copyright(c) 2016-2024, Robert Roessler
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

#include <string>
#include <string_view>
#include <bitset>
#include <algorithm>
#include <exception>
#include <iostream>
#include <iomanip>

using std::string;
using std::string_view;

/*
	The rglob namespace contains the classes compiler, matcher, and glob, the
	last of which inherits from the first two to provide complete "glob"-style
	pattern matching over UTF-8 -encoded text.

	In addition, an iterator template class (basic_utf8iterator) plus a handful
	of helper functions for assisting with processing of UTF-8 -encoded Unicode
	text are made available.
*/
namespace rglob {

/*
	Make sure we have this - Just In Case(tm).
*/
#ifndef isascii
#define isascii(c) ((int)(c) >= 0 && (int)(c) < 128)
#endif

constexpr size_t LengthSize = 2;		// # of chars in [base64] encoded length
constexpr size_t AllowedMaxFSM = 4096-1;// limit [compiled] finite state machine

// (internally used definitions not intended to appear in the rglob namespace)
namespace detail {
	/*
		sizeOfUTF8CodePoint returns the length in bytes of a UTF-8 code point, based
		on being passed the [presumed] first byte.

		N.B. - if the passed value does NOT represent [the start of] a well-formed
		UTF-8 code point, the returned length is ZERO, which means this should most
		likely be used at least initially in a "validation" capacity.

		Conceptually, this is the logic:

		return
			isascii(c)                     ? 1 :
			(c & 0b11100000) == 0b11000000 ? 2 :
			(c & 0b11110000) == 0b11100000 ? 3 :
			(c & 0b11111000) == 0b11110000 ? 4 :
			0; // (caller(s) should NOTICE this)
	*/
	constexpr size_t sizeOfUTF8CodePoint(char32_t c) noexcept
	{
		return
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 00-0f 1-byte UTF-8/ASCII
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 10-1f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 20-2f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 30-3f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 40-4f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 50-5f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 60-6f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 70-7f

			"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// 80-8f <illegal>
			"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// 90-9f
			"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// a0-af
			"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// b0-bf

			"\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2"	// c0-cf 2-byte UTF-8
			"\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2"	// d0-df

			"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3"	// e0-ef 3-byte UTF-8

			"\4\4\4\4\4\4\4\4"					// f0-f7 4-byte UTF-8

			"\0\0\0\0\0\0\0\0"					// f8-ff <illegal>
			[c & 0xff];
	}

	/*
		validateUTF8String evaluates the [NUL-terminated] sequence of chars supplied
		for "valid" UTF-8 encoding - structurally, NOT in terms of specific values
		of code points / combinations.

		Returns the result of this evaluation.

		N.B. - a "false" return should probably NOT be ignored.
	*/
	constexpr auto validateUTF8String(string_view v)
	{
		for (auto i = v.cbegin(); i != v.cend();)
			switch (auto n = sizeOfUTF8CodePoint(*i++); n) {
			case 0:
				// invalid "lead char" of UTF-8 Unicode sequence
				return false;
			case 1:
				// ASCII char
				break;
			default:
				// multi-byte UTF-8 Unicode sequence...
				while (--n && i != v.cend())
					if (const auto c = *i++; (c & 0b11000000) != 0b10000000)
						// invalid "following char" of UTF-8 Unicode sequence
						return false;
				if (n && i == v.cend())
					// you are NOT paranoid if the UTF-8 really IS malformed!
					return false;
			}
		return true;
	}

	/*
		codePointToUTF8 is a template function providing flexible output options for
		the encoded UTF-8 chars representing the supplied Unicode code point.

		It is a template function so you can choose to store the output UTF-8 stream
		either like this

		char buf[80], * s = buf;
		codePointToUTF8(c, [&](char x) { *s++ = x; })

		or this

		std::string buf;
		codePointToUTF8(c, [&](char x) { buf.push_back(x); })

		... where c is a Unicode code point in a char32_t.
	*/
	template<class CharOutput>
	constexpr void codePointToUTF8(char32_t c, CharOutput f) noexcept
	{
		if (c < 0x80)
			f((char)c);
		else if (c < 0x800)
			f((char)(0b11000000 | (c >> 6))),
			f((char)((c & 0b111111) | 0b10000000));
		else if (c < 0x10000)
			f((char)(0b11100000 | (c >> 12))),
			f((char)(((c >> 6) & 0b111111) | 0b10000000)),
			f((char)((c & 0b111111) | 0b10000000));
		else
			f((char)(0b11110000 | (c >> 18))),
			f((char)(((c >> 12) & 0b111111) | 0b10000000)),
			f((char)(((c >> 6) & 0b111111) | 0b10000000)),
			f((char)((c & 0b111111) | 0b10000000));
	}
}

/*
	basic_utf8iterator is an iterator adaptor template class providing read-only
	operations over a "base" character iterator type on text containing Unicode
	represented in the UTF-8 encoding - typically a std::string::const_iterator
	or a "bare" const char* is used as this underlying character iterator type.

	C++17 Update: based on this new iteration of the C++ language standard, we
	have a) quit deriving from std::iterator, as this was not required and is
	now deprecated, and b) switched to "expecting" a std::string_view -based
	iterator as a typical template param, given the new [string_view] order.

	While this is a functional "bidirectional iterator" - and is almost able to
	be a "random access iterator" (BUT, no subscripting), it must be noted that

	1) The "difference type" supplied to the arithmetic operators represents a
	BYTE offset in "UTF-8 space" - it is NOT in "Unicode space", i.e., it is NOT
	true that It++ is guaranteed to result in the same iterator value as the
	result of It += 1... the former WILL advance to the next code point, while
	the latter [in general] will not.

	In practice, this isn't a problem or limitation at all, since the difference
	values being used in the arithmetic operators should be the result of other
	operations that yield them, or "known" BYTE lengths of UTF-8 sequences.

	2) A possibly trivial issue is that, due to fundamental limitations of the
	UTF-8 encoding, the "operator--" implementation is not [strictly speaking]
	quite "constant time" - for those worried about such things.
*/
template<class BaseIteratorType>
class basic_utf8iterator
{
	// provide [internal] short-hand access to our value and instantiated types
	using _Ty = char32_t;
	using T = basic_utf8iterator<BaseIteratorType>;

	BaseIteratorType base;						// our actual "base" iterator

	// codePointFromUTF8 assembles and returns a full [32-bit] Unicode code point
	// from a UTF-8 encoded sequence of bytes
	//
	// N.B. - as it computes the length of the encoded representation from the data
	// itself - as well as "trusting" the bit patterns contained therein - it will
	// ONLY work with well-formed UTF-8 encoded data!
	constexpr _Ty codePointFromUTF8() const {
		const _Ty c = base[0];
		switch (detail::sizeOfUTF8CodePoint(c)) {
		case 1: return c;
		case 2: return (c & 0b11111) << 6 | (base[1] & 0b111111);
		case 3: return (c & 0b1111) << 12 | (base[1] & 0b111111) << 6 | (base[2] & 0b111111);
		case 4: return (c & 0b111) << 18 | (base[1] & 0b111111) << 12 | (base[2] & 0b111111) << 6 | (base[3] & 0b111111);
		}
		return 0; // ("can't happen")
	}

	// sizeOfPreviousUTF8CodePoint is the basis for our not-quite-constant-time
	//	"decrement" operator
	//
	// N.B. - depends on BOTH well-formed UTF-8 encoded data AND caller not
	// attempting to position iterator prior to beginning of container!
	constexpr size_t sizeOfPreviousUTF8CodePoint() const {
		return
			(*(base - 1) & 0b11000000) != 0b10000000 ? 1 :
			(*(base - 2) & 0b11000000) != 0b10000000 ? 2 :
			(*(base - 3) & 0b11000000) != 0b10000000 ? 3 :
			(*(base - 4) & 0b11111000) == 0b11110000 ? 4 :
			0; // happens only for ILLEGAL UTF-8 encoding!
	}

public:
	// provide public access to our constituent types (now required given that
	// we are no longer deriving from std::iterator)
	using iterator_category = std::bidirectional_iterator_tag;
	using value_type = _Ty;
	using difference_type = ptrdiff_t;
	using pointer = value_type *;
	using reference = value_type &;

	// provide public access to our instantiated base type
	using base_type = BaseIteratorType;

	// define "standard" constructors/destructor for iterators... note that as
	// there isn't a good, generic default "uninitialized" value for iterators
	// - this is a C++ language/stdlib issue - we delete the default ctor, and
	// require apps to explictily employ only valid copy-constructor exprs
	basic_utf8iterator() = delete;
	basic_utf8iterator(const T& u) : base(u.base) {}
	basic_utf8iterator(base_type i) : base(i) {}
	~basic_utf8iterator() {}

	// provide [expert] access to "base" iterator member
	constexpr operator base_type() const { return base; }

	// define "copy assignment" operator for iterators
	constexpr T& operator=(const T& u) { base = u.base; return *this; }

	// define "dereferencing" operator for iterators
	constexpr value_type operator*() const { return codePointFromUTF8(); }

	// define "pre- and post- increment/decrement" operators for iterators
	constexpr T& operator++() { base += detail::sizeOfUTF8CodePoint(*base); return *this; }
	constexpr T operator++(int) { auto u = *this; ++(*this); return u; }
	constexpr T& operator--() { base -= sizeOfPreviousUTF8CodePoint(); return *this; }
	constexpr T operator--(int) { auto u = *this; --(*this); return u; }

	// define "arithmetic" operators for iterators
	//
	// N.B. - based on char/byte ptrdiff_t, NOT code point "distance"!
	constexpr T operator+(difference_type d) const { return T(base + d); }
	constexpr T operator-(difference_type d) const { return T(base - d); }
	constexpr T& operator+=(difference_type d) { base += d; return *this; }
	constexpr T& operator-=(difference_type d) { base -= d; return *this; }

	// define "differencing" operators for iterators in same container
	constexpr difference_type operator-(const T& u) const { return base - u.base; }
	constexpr difference_type operator-(const base_type& b) const { return base - b; }

	// define "relational" operators for iterators in same container
	constexpr bool operator==(const T& u) const { return base == u.base; }
	constexpr bool operator==(const base_type& b) const { return base == b; }
	constexpr 	bool operator!=(const T& u) const { return base != u.base; }
	constexpr bool operator!=(const base_type& b) const { return base != b; }

	constexpr bool operator>(const T& u) const { return base > u.base; }
	constexpr bool operator>(const base_type& i) const { return base > i; }
	constexpr bool operator<(const T& u) const { return base < u.base; }
	constexpr bool operator<(const base_type& i) const { return base < i; }

	constexpr bool operator>=(const T& u) const { return base >= u.base; }
	constexpr bool operator>=(const base_type& i) const { return base >= i; }
	constexpr bool operator<=(const T& u) const { return base <= u.base; }
	constexpr bool operator<=(const base_type& i) const { return base <= i; }
};

/*
	Create the 2 UTF-8 iterators used by the rglob compiler and matcher classes.
*/
typedef basic_utf8iterator<string_view::const_iterator> utf8iterator;
typedef basic_utf8iterator<const char*> utf8iteratorBare;

/*
	The compiler class is composed of a primary function - compile - which takes
	a "pattern" specification in the style of the "glob" patterns of Unix/Linux,
	and machine, a "payload" function that returns the now-compiled pattern for
	subsequent display or execution by the matcher class.

	All text is expected to be in UTF-8 representation, which is usable and at
	least minimally supported by modern C++ compilers... without attempting to
	be a tutorial on the UTF-8 Unicode encoding, we can observe the following:

	* Unicode is a set of over a million characters ("everything"), and UTF-8
	is a way of representing Unicode "code points" as 1, 2, 3, or 4 bytes

	* 7-bit ASCII is the first 128 characters of Unicode, and appears unchanged
	in UTF-8... another way to say this is that ASCII chars ARE UTF-8 chars

	* 2nd, 3rd, or 4th chars in Unicode code points will NEVER look like an
	ASCII char, so one can still perform "normal" text comparisons or even
	make use of the standard C++ library with UTF-8 text

	* if you want to include non-ASCII text literals in C++ source, you have
	the choice of doing it the hard way with hex/binary escaped sequences in
	strings, \unnnn sequences in strings for 2/3-byte code points, \Unnnnnnnn
	sequences in strings for 3-byte or 4-byte code points, or [easiest] using
	the new UTF-8 string literals: u8"This is a UTF-8 string!"

	* to see FAR more detail on this, visit http://utf8everywhere.org/

	Patterns supported by the rglob::compiler and rglob::matcher classes are
	made up of combinations of the following elements:

	?		any SINGLE UTF-8 code point (again, this could be an ASCII char)

	*		any sequence of ZERO OR MORE UTF-8 code points

	[abc]	"ONE OF" the supplied set of UTF-8 code points

	[a-c]	"ONE OF" the specified range of UTF-8 code points

	[a-cYZ]	"ONE OF" either the range OR set of UTF-8 code points

	abcdef	the EXACT SEQUENCE of UTF-8 code points

	More details on patterns:

	* mixing and matching is fine, so "*[abc]?[A-Z]hello" is a valid pattern
	that matches
		1 ZERO OR MORE UTF-8 code points, followed by
		2 ONE OF a, b, or c, followed by
		3 any SINGLE UTF-8 code point, followed by
		4 ONE OF any character in the range A-Z, followed by
		5 the EXACT SEQUENCE hello

	* [...] pattern elements are basically simplified versions of the "character
	classes" found in regular expressions

	* if the FIRST char in the class is '!' or '^', then that changes that class
	to mean "any UTF-8 code point EXCEPT for the ones specified in this class"

	* to include the "special" characters ']', '-', '!', or '^' IN a character
	class, do the following
		]	use as the FIRST char (but AFTER either '!' or '^')
		-	use as the LAST char
		!	use as anything BUT the first char
		^	use as anything BUT the first char
*/
class compiler
{
	string fsm;							// compiled fsm for current glob pattern

	static constexpr auto base64Digit(int n) noexcept {
		return							// RFCs 2045/3548/4648/4880 et al
			"ABCDEFGHIJKLMNOPQRSTUVWXYZ"//  0-25
			"abcdefghijklmnopqrstuvwxyz"// 26-51
			"0123456789"				// 52-61
			"+/"						// 62-63
			[n & 0x3f];
	}
	static constexpr auto hexDigit(int n) noexcept { return "0123456789abcdef"[n & 0xf]; }
	constexpr void emit(char c) { fsm.push_back(c); }
	constexpr void emit(string_view v) { fsm.append(v); }
	constexpr void emitAt(size_t i, char c) { fsm[i] = c; }
	/*
		emitPackedBitset inserts a representation of the just-processed "fast path"
		character class into the current finite state machine definition.

		The actual form of this data is dictated by two considerations:

		1) The stdlib bitset implementation only has convenient "[de-]serialization"
		options for up-to 64-element sets - the "1 character per bit" form is just a
		bit too voluminous for our purposes, so we use our own 32 "ASCII/hex" char
		string for the 128-bit sets used by the "fast path" logic.

		2) Additionally, it was desirable to employ a format that permits fairly
		efficient queries of individual bits WITHOUT having to "de-serialize" the
		entire bitset.
	*/
	constexpr void emitPackedBitset(const std::bitset<128>& b) {
		// output the 128-bit bitset in a 4-bits-per-ASCII/hex-character format.
		for (auto c = 128 - 4; c >= 0; c -= 4)
			emit(hexDigit(
				(b.test((size_t)c + 0) ? 1 : 0) |
				(b.test((size_t)c + 1) ? 2 : 0) |
				(b.test((size_t)c + 2) ? 4 : 0) |
				(b.test((size_t)c + 3) ? 8 : 0)));
	}
	constexpr void emitLengthAt(size_t i, size_t n) {
		emitAt(i + 0, base64Digit((n & 0xfc) >> 6)), emitAt(i + 1, base64Digit((n & 0x3f)));
	}
	constexpr void emitPadding(int n, char c = '_') { while (n-- > 0) emit(c); }
	constexpr auto emitted() const noexcept { return fsm.size(); }
	constexpr void emitUTF8CodePoint(char32_t c) { detail::codePointToUTF8(c, [this](char x) { emit(x); }); }
	constexpr auto peek(string_view::const_iterator i) const { return *++i; }
	auto peek(utf8iterator u) const { return *++u; }

	/*
		compileClass processes a single "character class" string from a glob pattern
		- after first determining whether the sequence is well-formed - an exception
		(invalid_argument) will be thrown if it fails this test.

		While evaluating the legality of the character class, two "special cases" of
		leading character class metachars are checked, and then the presence of non-
		ASCII is tested... if none are found, then the entire class will be handled
		by "fast path" logic, and represented as a "bitset" - in which case, class
		membership (matching) can be tested by a single "lookup" per target char.

		In the general [non-ASCII] case, the match-time evaluation of matches in the
		character class will be done by evaluating a number of either single-char or
		char-range expressions serially... if at least one matches the "target"/test
		char, then the class is matched - otherwise, the class match fails.

		The number of chars/BYTEs consumed is returned.
	*/
	auto compileClass(string_view pattern, string_view::const_iterator p) {
		const auto base = p++;
		const auto pos = emitted();
		// check for "inversion" of character class metacharacter
		auto invert = false;
		if (*p == '!' || *p == '^')
			invert = true, ++p;
		// NOW check for "close" metacharacter as the first class member
		auto leadingCloseBracket = false;
		// NOW look for the end of the character class specification...
		if (*p == ']')
			leadingCloseBracket = true, ++p;
		const auto o = p - pattern.cbegin();
		const auto close = pattern.find_first_of(']', o);
		// ... and throw if we don't see one
		if (close == string::npos)
			throw std::invalid_argument(string("Missing terminating ']' for character class @ ") + string(pattern.substr(base - pattern.cbegin())));
		if (std::all_of(p, p + (close - o), [](char c) { return isascii(c); })) {
			// the character class is ALL ASCII chars, so we can use the "fast path"
			emit('{');
			// (neither "invert" flag nor "length" field are needed for "fast path")
			std::bitset<128> b;
			// "fast path" (bitset) invert is easy
			if (invert)
				b.set();
			if (leadingCloseBracket)
				b.flip(']');
			// process all class members by "flipping" corresponding bits...
			while (*p != ']')
				if (const auto c1 = *p++, c2 = *p; c2 == '-' && peek(p) != ']') {
					const auto c3 = *++p;
					for (auto c = c1; c <= c3; c++)
						b.flip(c);
					++p;
				} else
					b.flip(c1);
			// ... finish up by copying the [packed] bitset to finite state machine
			emitPackedBitset(b), ++p;
			return p - base;
		} else {
			// "general case" character class, output single and range match exprs
			emit('[');
			emit(hexDigit(invert ? 1 : 0));
			// initialize and "remember" location of length (to be filled in later)
			const auto lenPos = emitted();
			emitPadding(LengthSize);
			if (leadingCloseBracket)
				emit('+'), emit(']');
			// NOW switch to full UTF-8 (Unicode) processing...
			utf8iterator u = p;
			// ... and process all class members by outputting match-time operators
			while (*u != ']')
				if (const auto c1 = *u++, c2 = *u; c2 == '-' && peek(u) != ']') {
					// (generate "char range" matching operator)
					const auto c3 = *++u;
					emit('-'), emitUTF8CodePoint(c1), emitUTF8CodePoint(c3), ++u;
				} else
					// (generate "single char" matching operator)
					emit('+'), emitUTF8CodePoint(c1);
			// finish up by generating the "NO match" operator...
			emit(']'), ++u;
			// ... and output the length of the character class "interpreter" logic
			emitLengthAt(lenPos, emitted() - pos - (1 + 1 + LengthSize + 1));
			return u - base;
		}
	}
	/*
		compileString processes a single "exact match" string from a glob pattern...
		this will extend until either the next glob metacharacter or the pattern end
		- there is no "invalid" case.

		The number of chars/BYTEs consumed is returned.
	*/
	auto compileString(string_view pattern, string_view::const_iterator p) {
		emit('=');
		// initialize and "remember" location of length (to be filled in later)
		const auto lenPos = emitted();
		emitPadding(LengthSize);
		// determine length...
		const auto o = p - pattern.cbegin();
		const auto i = pattern.find_first_of("?*[", o);
		const auto n = i != string::npos ? i - o : pattern.size() - o;
		// ... and copy "exact match" string to finite state machine
		emit(pattern.substr(p - pattern.cbegin(), n));
		emitLengthAt(lenPos, n);
		return n;
	}

public:
	compiler() {
		// N.B. - it is IMPORTANT to do this in the constructor!
		// (needed when composed with the matcher class in rglob::glob class)
		fsm.reserve(AllowedMaxFSM);
	}

	/*
		compile accepts a pattern following the rules detailed in the class
		documentation, and "compiles" it to a representation enabling faster
		subsequent matching: a "finite state machine" able to recognize text
		matching the supplied [UTF-8] pattern.

		invalid_argument if the pattern string is NOT valid UTF-8

		invalid_argument if pattern string has an unterminated character class

		length_error if the compiled pattern is > 4 KB (implementation limit)

		In all cases, an explanatory text message is included, with position
		information if applicable.
	*/
	void compile(string_view pattern) {
		// make SURE pattern is *structurally* valid UTF8
		if (!detail::validateUTF8String(pattern))
			throw std::invalid_argument("Pattern string is not valid UTF-8.");
		fsm.clear();
		// prep for filling in compiled length of pattern later
		emit('#'), emitPadding(2);
		ptrdiff_t incr = 1;
		// iterate over, compile, and consume pattern elements
		for (auto pi = pattern.cbegin(); pi != pattern.cend(); pi += incr) {
			switch (*pi) {
			case '?':
				emit(*pi), incr = 1;
				break;
			case '*':
				emit(*pi), incr = 1;
				break;
			case '[':
				incr = compileClass(pattern, pi);
				break;
			default:
				incr = compileString(pattern, pi);
			}
			if (emitted() > AllowedMaxFSM)
				throw std::length_error(string("Exceeded allowed compiled pattern size @ ") + string(pattern.substr(pi - pattern.cbegin())));
		}
		// NOW fill in length of compiled pattern... IFF there is any actual pattern
		if (const auto n = emitted(); n > 1 + LengthSize)
			emitLengthAt(1, n - (1 + LengthSize));
		else
			fsm.clear();
	}

	/*
		machine returns the compiled form of the [valid] glob pattern supplied
		to compile... note that while this is "human-readable", the matcher
		class's pretty_print does a better job of displaying this information.
	*/
	string_view machine() const noexcept { return fsm; }
};

/*
	The matcher class accepts (via its constructor) the compiled representation
	of a "glob" pattern from the compiler class above, and can then be used to
	match targets against this pattern with its match function, or to output it
	in "pretty-printed" form to a supplied stream with pretty_print.
*/
class matcher
{
	string_view fsm;					// compiled fsm for current glob pattern

	static constexpr int base64Value(char c) noexcept {
		return
			"\x00\x00\x00\x00\x00\x00\x00\x00"	// 00-0f <illegal>
			"\x00\x00\x00\x00\x00\x00\x00\x00"
			"\x00\x00\x00\x00\x00\x00\x00\x00"	// 10-1f <illegal>
			"\x00\x00\x00\x00\x00\x00\x00\x00"

			"\x00\x00\x00\x00\x00\x00\x00\x00"	// 20-2f <illegal>,+,/
			"\x00\x00\x00\x3e\x00\x00\x00\x3f"
			"\x34\x35\x36\x37\x38\x39\x3a\x3b"	// 30-3f 0-9,<illegal>
			"\x3c\x3d\x00\x00\x00\x00\x00\x00"

			"\x00\x00\x01\x02\x03\x04\x05\x06"	// 40-4f <illegal>,A-O
			"\x07\x08\x09\x0a\x0b\x0c\x0d\x0e"
			"\x0f\x10\x11\x12\x13\x14\x15\x16"	// 50-5f P-Z,<illegal>
			"\x17\x18\x19\x00\x00\x00\x00\x00"

			"\x00\x1a\x1b\x1c\x1d\x1e\x1f\x20"	// 60-6f <illegal>,a-o
			"\x21\x22\x23\x24\x25\x26\x27\x28"
			"\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30"	// 70-7f p-z,<illegal>
			"\x31\x32\x33\x00\x00\x00\x00\x00"
			[c & 0x7f];
	}
	static constexpr int hexValue(char c) noexcept {
		return
			"\x00\x00\x00\x00\x00\x00\x00\x00"	// 00-0f <illegal>
			"\x00\x00\x00\x00\x00\x00\x00\x00"
			"\x00\x00\x00\x00\x00\x00\x00\x00"	// 10-1f <illegal>
			"\x00\x00\x00\x00\x00\x00\x00\x00"

			"\x00\x00\x00\x00\x00\x00\x00\x00"	// 20-2f <illegal>
			"\x00\x00\x00\x00\x00\x00\x00\x00"
			"\x00\x01\x02\x03\x04\x05\x06\x07"	// 30-3f 0-9
			"\x08\x09\x00\x00\x00\x00\x00\x00"	//  ...  <illegal>

			"\x00\x0a\x0b\x0c\x0d\x0e\x0f\x00"	// 40-4f <illegal>,A-F
			"\x00\x00\x00\x00\x00\x00\x00\x00"	//  ...  <illegal>
			"\x00\x00\x00\x00\x00\x00\x00\x00"	// 50-5f <illegal>
			"\x00\x00\x00\x00\x00\x00\x00\x00"

			"\x00\x0a\x0b\x0c\x0d\x0e\x0f\x00"	// 60-6f <illegal>,a-f
			"\x00\x00\x00\x00\x00\x00\x00\x00"	//  ...  <illegal>
			"\x00\x00\x00\x00\x00\x00\x00\x00"	// 70-7f <illegal>
			"\x00\x00\x00\x00\x00\x00\x00\x00"
			[c & 0x7f];
	}
	auto opAt(utf8iteratorBare i) const { return *(utf8iteratorBare::base_type)i; }
	auto decodeLengthAt(utf8iteratorBare i) const { return base64Value(opAt(i + 0)) * 64 + base64Value(opAt(i + 1)); }
	auto decodeModifierAt(utf8iteratorBare i) const { return hexValue(opAt(i)); }
	constexpr int packedBitsetMask(int b) const noexcept { return "\x8\4\2\1"[(127 - b) & 0b11]; }
	auto packedBitsetNibbleAt(utf8iteratorBare i, int b) const { return hexValue(((utf8iteratorBare::base_type)i)[(127 - b) >> 2]); }
	auto testPackedBitsetAt(utf8iteratorBare i, int b) const { return (packedBitsetNibbleAt(i, b) & packedBitsetMask(b)) != 0; }

	utf8iteratorBare cbegin() const noexcept { return fsm.data(); }
	// (N.B. - fsm.size() MAY not be useful, but fsm.data() WILL point to text)
	utf8iteratorBare cend() const { return fsm.data() + (fsm[0] == '#' ? 1 + LengthSize + decodeLengthAt(fsm.data() + 1) : std::strlen(fsm.data())); }

public:
	/*
		Using the rglob::matcher constructor is considered an "expert" level of
		use of the rglob system... it is far more likely that you will be using
		the rglob::glob class - it's easier and really made for most situations.

		That said, if you DO choose to access rglob functionality at the lower
		level of using the compiler and matcher classes directly, note that the
		ONLY supported values for the matcher constructor(s) are those returned
		from the compiler::machine function... which by definition only returns
		well-formed finite state machines, composed of valid sequences.

		This last bit is really just a disclaimer saying "we trust our own data
		and may therefore be a bit relaxed in our internal error-checking".
	*/
	matcher() = delete;
	matcher(string_view m) : fsm(m) {}

	/*
		match accepts a [UTF-8] "target" string and attempts to match it to the
		pattern that was previously processed by compiler::compile, reflecting
		the match success/failure as its return value.

		invalid_argument if the target string is NOT valid UTF-8
	*/
	bool match(string_view target) const {
		// make SURE target is *structurally* valid UTF8
		if (!detail::validateUTF8String(target))
			throw std::invalid_argument("Target string is not valid UTF-8.");
		auto anchored = true, invert = false;
		utf8iteratorBare next = nullptr;
		utf8iterator ti = target.cbegin();
		// iterate over the previously compiled pattern representation, consuming
		// recognized (matched) elements of the target text
		for (auto first = cbegin(), mi = first, last = cend(); mi != last;)
			switch (*mi++) {
			case '#':
				// "no-op" from the perspective of matching
				mi += LengthSize;
				break;
			case '?':
				// accept ("match") single target code point
				anchored = true, ++ti;
				break;
			case '*':
				// set "free" or "floating" match meta state; this MAY involve
				// "skipping over" zero or more target code points
				anchored = false;
				break;
			case '[':
				// prep for full "interpreted" UTF-8 character class recognition
				invert = decodeModifierAt(mi) != 0;
				next = mi + 1 + LengthSize + decodeLengthAt(mi + 1) + 1, mi += 1 + LengthSize;
				break;
			case '{':
				// perform "fast path" (all-ASCII) character class match
				if (anchored) {
					if (const auto tx = *ti; !(isascii(tx) && testPackedBitsetAt(mi, tx)))
						return false;
					// (consume target code point(s) and skip to after the ']')
					++ti, mi += 32;
				} else {
					auto i = find_if(ti, utf8iterator(target.cend()), [=](char32_t tx) { return isascii(tx) && testPackedBitsetAt(mi, tx); });
					if (i == target.cend())
						return false;
					// (consume target code point(s) and skip to after the ']')
					ti = ++i, anchored = true, mi += 32;
				}
				break;
			case '+': {
				// attempt to match single "interpreted" character class code point
				const auto p = *mi++;
				if (anchored) {
					if (const auto tx = *ti; (p == tx) == !invert)
						// (consume target code point(s) and skip to after the ']')
						++ti, mi = next;
				} else {
					auto i = find_if(ti, utf8iterator(target.cend()), [=](char32_t tx) { return (p == tx) == !invert; });
					if (i != target.cend())
						// (consume target code point(s) and skip to after the ']')
						ti = ++i, anchored = true, mi = next;
				}
				break;
			}
			case '-': {
				// attempt to match "interpreted" character class "range" code point
				const auto p1 = *mi++, p2 = *mi++;
				if (anchored) {
					const auto tx = *ti;
					if ((p1 <= tx && tx <= p2) == !invert)
						// (consume target code point(s) and skip to after the ']')
						++ti, mi = next;
				} else {
					auto i = find_if(ti, utf8iterator(target.cend()), [=](char32_t tx) { return (p1 <= tx && tx <= p2) == !invert; });
					if (i != target.cend())
						// (consume target code point(s) and skip to after the ']')
						ti = ++i, anchored = true, mi = next;
				}
				break;
			}
			case ']':
				// end of "interpreted" UTF-8 character class... if we get here, it
				// means we did NOT match ANY target code point - i.e., "failure"
				return false;
			case '=': {
				// attempt an exact sequence of UTF-8 code points match
				const auto n = decodeLengthAt(mi);
				const auto o = ti - target.cbegin();
				const auto i = target.find(mi + LengthSize, o, n);
				// (below means "not found" OR "found, but not where expected")
				if (i == string::npos || (anchored && i != o))
					return false;
				ti = anchored ? ti + n : utf8iterator(target.cbegin() + i + n), anchored = true, mi += LengthSize + n;
				break;
			}
			}
		// return whether we [successfully] consumed ALL target text OR the pattern
		// ended in a "free" or "floating" match state (e.g.,  "ab*" matches "abZ")
		return ti == target.cend() || !anchored;
	}

	/*
		pretty_print outputs a formatted representation of the current finite
		state machine produced by compiler::compile to the supplied ostream
		(with optional layout "prefix" per line).
	*/
	void pretty_print(std::ostream& s, string_view pre = "") const {
		// (local fn to compute width for Unicode representation)
		auto w = [](char32_t c) { return c < 0x010000 ? 4 : c < 0x100000 ? 5 : 6; };
		// (local fn to show Unicode char as ASCII if we can, else use "U+..." form)
		auto a = [&](char32_t c) -> std::ostream& {
			return
				isascii(c) ?
				s << (char)c :
				s << "U+" << std::hex << std::uppercase << std::setfill('0') << std::setw(w(c))
				<< (int)c
				<< std::dec << std::setfill(' ');
		};
		// iterate over each element of finite state machine...
		for (auto first = cbegin(), mi = first, last = cend(); mi != last;) {
			const auto op = *mi++;
			s << pre << "[" << std::setw(4) << (mi - first - 1) << "] op: " << (char)op;
			switch (op) {
			case '#':
				// display length of compiled pattern
				s << " len: " << decodeLengthAt(mi);
				mi += LengthSize;
				break;
			case '[':
				// display control metadata from "interpreted" character class
				s << " mod: " << (char)*mi << " len: " << decodeLengthAt(mi + 1);
				mi += 1 + LengthSize;
				break;
			case '{':
				// display bitset from "fast path" character class
				s << " val: ";
				std::copy((utf8iteratorBare::base_type)mi, (utf8iteratorBare::base_type)mi + 32, std::ostreambuf_iterator<char>(s));
				mi += 32;
				break;
			case '+':
				// display SINGLE match case from "interpreted" character class
				s << " val: ", a(*mi++);
				break;
			case '-':
				// display RANGE match case from "interpreted" character class
				s << " val: ", a(*mi++) << ' ', a(*mi++);
				break;
			case '=': {
				// display "exact match" string from glob pattern
				const auto n = decodeLengthAt(mi);
				s << " len: " << n << " val:";
				// "leading space" rules: NEVER show ASCII sequences with embedded
				// spaces, ALWAYS show [multi-byte] Unicode code points as " U+..."
				// for each, and ALWAYS insert a space when switching between them.
				enum LeadingSpace { None, Ascii, Unicode } state = None;
				std::for_each(mi + LengthSize, mi + LengthSize + n, [&](char32_t c) {
					if (const auto newState = isascii(c) ? Ascii : Unicode; newState != state || state == Unicode)
						s << ' ', state = newState;
					a(c);
				});
				mi += LengthSize + n;
				break;
			}
			}
			s << std::endl;
		}
	}
};

/*
	The glob class is a "glue" class that composes a compiler and a matcher for
	specifying and subsequently recognizing "glob" -style patterns over text in
	UTF-8 form.

	While certain specialized applications may find it convenient to separately
	compile and match patterns - and will thus directly make use of the compiler
	and matcher classes, the expected typical usage is to use the composed class
	glob to handle all compiling, matching, and pretty-printing functionality.

	Note that when using glob, there is no need to refer to or use the compiler
	or matcher classes (or their constructors) at all: just create a glob object
	and invoke its compile and match (or pretty_print) functions directly.
*/
class glob : public compiler, public matcher
{
public:
	glob() : compiler(), matcher(compiler::machine()) {}
};

}
