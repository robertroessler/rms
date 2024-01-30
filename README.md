# RMs

[![Build status](https://ci.appveyor.com/api/projects/status/kyapnn52369psutv?svg=true)](https://ci.appveyor.com/project/robertroessler/rms)

The RMs (rms) project implements a [reasonably] high-performance multi-threaded
"pub/sub" messaging system, designed around a shared-memory approach to storing
messages awaiting delivery.  This was chosen so that multi-process publication
and distribution would be simplified - other than the discovered (or allocated)
per-process "base" pointer to the shared memory segment, there are NO pointers
stored in any in-memory structures.  This would make sharing - even if mapped to
different virtual address ranges in different processes - quite simple.

HINT: think "indices, all the way down". :sunglasses:

As things actually worked out, over the decades this package has been in use, the
"multi-process" aspects of this architecture were never built out, both because
of the substantially higher costs of resource coordination/locking INTER-process
*vs* INTRA-process, but more subtly, the paradigm of "many cooperating lightweight
threads" ultimately seemed more useful than one of "many cooperating heavyweight
processes" - at least on a single machine.

An additional "novel" aspect of RMs (rms) is the actual representation used for
"pointers" to stored messages and metadata - given the goal above of being able
to share in-memory structures - possibly mapped to different addresses while in
use - a 32-bit format was employed that simultaneously encodes location, typing,
AND length information - really(*).  This design has provided high-performance
allocation, releasing, and management of resources for the rest of the system.

- a *possible* trade-off here has been the "limitation" of message buffer memory
to 256 MB (total), as well as individual message "payloads" to 4 KB... see below
for possible future work in this area.

It is written in fairly idiomatic modern C++ 17/20, which is to say that it has
mostly been updated to C\+\+17, with *some* C\+\+20 (like std::format)... although
since it was in its original version built from "old-school" C/C++, there may be
portions of the code that are still awaiting a "re-imagining" (again, see below).

A final "stylistic" comment is that virtually no C++ library data structures are
used for *long-term* storage - all use of things like std::string and std::variant
happens only at the boundaries: when publishing (sending) and receiving messages.

The primary "user" (as well as "developer") documentation for rms is present
in the rms.h header file.  Note that there is a single [non-system] dependency:

[UTF-8 Pattern-matcher "sibling" project __rglob__](https://github.com/robertroessler/rglob)

... the VS2022 project files are expecting RGlob/RMs to be in "sibling" folders,
with rglob in a folder [roughly] at "../RGlob" - but of course, feel free
to re-arrange any of the project/folder layout.  And, due to its size, the rglob
library should (as in the included VS files) likely be kept as a "static" lib.

UPDATE: as of the 3.5 release of rms, the rglob library "dependency" is now a
"header-only" library, so the above [historical] remarks about "static " libs
are no longer relevant.

Besides being "pure" C++, the code is believed to be both 32/64 -bit "safe", and
contains only 2 dependencies on Windows (easily "ported" to other environments):

* ::rms_initialize of course needs to set up an area of mapped/shared memory,
principally handled in Windows by ::MapViewOfFile and ::VirtualAlloc

* the dump{Root,Queue,Exported,Check} variadic template static diagnostic functions
expect a high-perf / non-blocking logging sink (::OutputDebugString in Windows)

... again, both of the above are [almost] trivially re-hosted in modern OSes.
And, in fact, this has already happened, with non-Windows [building and] use being
supported since very early versions.

## ToDo (or, "The Future")

Possible items to work on - for myself or collaborators include (*check* means DONE)

- [ ] while the low-level *public* interface functions (*e.g.*, rms_publish_bytes(),
rms_peek()) have been unofficially deprecated for some time now, their end-time is
[now] likely to come sooner rather than later... with most of these going away in
favor of the more modern template-based functions in the rms\:\:subscription and
rms\:\:publisher classes

- [x] add the ability to side-step the usual data typing and *receive* messages from
queues of *any* type: using rms\:\:rms_any (implemented with the C++ std::variant), a
"reader" may be constructed that can read any message, without knowing the expected
data type in advance... useful for things like *loggers* or recording / auditing **all**
rms message traffic

- [x] [really "blue-sky"] support some form of either *transactions* or *records* to deal
with multiple messages that are logically "connected" being interspersed when delivered
by multiple publishers [to the same message queue]

- [x] additional (and properly laid out) test cases, both to serve as actual tests
and to show examples of usage - PARTICULARLY displaying multi-threaded use

- [x] investigating a lightweight (lambda-based?) form of "subscription targets"

- [ ] investigating (probably in a "version 2+") a 64-bit version for RMs "pointers"

- [ ] as only Visual Studio 2022 project and solution files are initially present,
control files for building in non-Windows environments could be useful

## ProbablyNot

Things that most likely should NOT happen include

* any attempt to return to or re-emphasize the ancient "version 0.5" lower-level
interface... really, any work on the interface(s) should be in extending the
"modern" template-based version, and/or taking advantage of any new RMs pointers
