# RMs

The RMs (rms) project implements a [reasonably] high-performance multi-threaded
"pub/sub" messaging system, designed around a shared-memory approach to storing
messages awaiting delivery.  This was chosen so that multi-process publication
and distribution would be simplified - other than the discovered (or allocated)
per-process "base" pointer to the shared memory segment, there are NO pointers
stored in any in-memory structures.  This would make sharing - even if mapped to
different virtual address ranges in different processes - quite simple.

HINT: think "indices, all the way down". :sunglasses:

As things actually worked out, over the decade this package has been in use, the
"multi-process" aspects of this architecture were never built out, both because
of the substantially higher costs of resource coordination/locking INTER-process
*vs* INTRA-process, but more subtly, the paradigm of "many cooperating lightweight
threads" utlimately seemed more useful than one of "many cooperating heavyweight
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

It is written in fairly idiomatic modern C++ 17... although since it was in
its original version built from "old-school" C/C++, there may be portions of the
code that are still awaiting a "re-imagining" (again, see below).

The primary "user" (as well as "developer") documentation for rms is present
in the rms.h header file.  Note that there is a single [non-system] dependency:

[UTF-8 Pattern-matcher "sibling" project](https://github.com/robertroessler/rglob)

... the VS2019 project files are expecting RGlob/RMs to be in "sibling" folders,
with rglob in a folder [roughly] at "../RGlob-trunk" - but of course, feel free
to re-arrange any of the project/folder layout.  And, due to its size, the rglob
library should (as in the included VS files) likely be kept as a "static" lib.

Besides being "pure" C++, the code is believed to be both 32/64 -bit "safe", and
contains only 2 dependencies on Windows (easily "ported" to other environments):

* ::rms_initialize of course needs to set up an area of mapped/shared memory,
principally handled in Windows by ::MapViewOfFile and ::VirtualAlloc

* the ::dump{Root,Queue,Exported,Check} variadic template diagnostic functions
expect a high-perf / non-blocking logging sink (::OutputDebugString in Windows)

... again, both of the above are [almost] trivially re-hosted in modern OSes.

## ToDo

Possible items to work on - for myself or collaborators include

* [DONE!] additonal (and properly laid out) test cases, both to serve as actual tests
and to show examples of usage - PARTICULARLY displaying multi-threaded use

* [DONE!] investigating a lightweight (lambda-based?) form of "subscription targets"

* investigating (probably in a "version 2") a 64-bit version for RMs "pointers"

* as only Visual Studio 2019 project and solution files are initially present,
control files for building in non-Windows environments could be useful

## ProbablyNot

Things that most likely should NOT happen include

* any attempt to return to or re-emphasize the ancient "version 0.5" lower-level
interface... really, any work on the interface(s) should be in extending the
"modern" template-based version, and/or taking advantage of any new RMs pointers
