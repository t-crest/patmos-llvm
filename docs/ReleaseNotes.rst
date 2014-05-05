======================
LLVM 3.5 Release Notes
======================

.. contents::
    :local:

.. warning::
   These are in-progress notes for the upcoming LLVM 3.5 release.  You may
   prefer the `LLVM 3.4 Release Notes <http://llvm.org/releases/3.4/docs
   /ReleaseNotes.html>`_.


Introduction
============

This document contains the release notes for the LLVM Compiler Infrastructure,
release 3.5.  Here we describe the status of LLVM, including major improvements
from the previous release, improvements in various subprojects of LLVM, and
some of the current users of the code.  All LLVM releases may be downloaded
from the `LLVM releases web site <http://llvm.org/releases/>`_.

For more information about LLVM, including information about the latest
release, please check out the `main LLVM web site <http://llvm.org/>`_.  If you
have questions or comments, the `LLVM Developer's Mailing List
<http://lists.cs.uiuc.edu/mailman/listinfo/llvmdev>`_ is a good place to send
them.

Note that if you are reading this file from a Subversion checkout or the main
LLVM web page, this document applies to the *next* release, not the current
one.  To see the release notes for a specific release, please see the `releases
page <http://llvm.org/releases/>`_.

Non-comprehensive list of changes in this release
=================================================

* All backends have been changed to use the MC asm printer and support for the
  non MC one has been removed.

* Clang can now successfully self-host itself on Linux/Sparc64 and on
  FreeBSD/Sparc64.

* LLVM now assumes the assembler supports ``.loc`` for generating debug line
  numbers. The old support for printing the debug line info directly was only
  used by ``llc`` and has been removed.

* All inline assembly is parsed by the integrated assembler when it is enabled.
  Previously this was only the case for object-file output. It is now the case
  for assembly output as well. The integrated assembler can be disabled with
  the ``-no-integrated-as`` option,

* llvm-ar now handles IR files like regular object files. In particular, a
  regular symbol table is created for symbols defined in IR files.

* llvm now requires assemblers that support cfi directives.

.. NOTE
   For small 1-3 sentence descriptions, just add an entry at the end of
   this list. If your description won't fit comfortably in one bullet
   point (e.g. maybe you would like to give an example of the
   functionality, or simply have a lot to talk about), see the `NOTE` below
   for adding a new subsection.

* ... next change ...

.. NOTE
   If you would like to document a larger change, then you can add a
   subsection about it right here. You can copy the following boilerplate
   and un-indent it (the indentation causes it to be inside this comment).

   Special New Feature
   -------------------

   Makes programs 10x faster by doing Special New Thing.

Changes to the ARM Backend
--------------------------

Since release 3.3, a lot of new features have been included in the ARM
back-end but weren't production ready (ie. well tested) on release 3.4.
Just after the 3.4 release, we started heavily testing two major parts
of the back-end: the integrated assembler (IAS) and the ARM exception
handling (EHABI), and now they are enabled by default on LLVM/Clang.

The IAS received a lot of GNU extensions and directives, as well as some
specific pre-UAL instructions. Not all remaining directives will be
implemented, as we made judgement calls on the need versus the complexity,
and have chosen simplicity and future compatibility where hard decisions
had to be made. The major difference is, as stated above, the IAS validates
all inline ASM, not just for object emission, and that cause trouble with
some uses of inline ASM as pre-processor magic.

So, while the IAS is good enough to compile large projects (including most
of the Linux kernel), there are a few things that we can't (and probably
won't) do. For those cases, please use ``-fno-integrated-as`` in Clang.

Exception handling is another big change. After extensive testing and
changes to cooperate with Dwarf unwinding, EHABI is enabled by default.
The options ``-arm-enable-ehabi`` and ``-arm-enable-ehabi-descriptors``,
which were used to enable EHABI in the previous releases, are removed now.

This means all ARM code will emit EH unwind tables, or CFI unwinding (for
debug/profiling), or both. To avoid run-time inconsistencies, C code will
also emit EH tables (in case they interoperate with C++ code), as is the
case for other architectures (ex. x86_64).

External Open Source Projects Using LLVM 3.5
============================================

An exciting aspect of LLVM is that it is used as an enabling technology for
a lot of other language and tools projects. This section lists some of the
projects that have already been updated to work with LLVM 3.5.

LibBeauty
---------

The `LibBeauty <http://www.libbeauty.com>`_ decompiler and reverse
engineering tool currently utilises the LLVM disassembler and the LLVM IR
Builder. The current aim of the project is to take a x86_64 binary ``.o`` file
as input, and produce an equivalent LLVM IR ``.bc`` or ``.ll`` file as
output. Support for ARM binary ``.o`` file as input will be added later.

Likely
------

`Likely <http://www.liblikely.org/>`_ is an open source domain specific
language for image recognition.  Algorithms are just-in-time compiled using
LLVM's MCJIT infrastructure to execute on single or multi-threaded CPUs as well
as OpenCL SPIR or CUDA enabled GPUs. Likely exploits the observation that while
image processing and statistical learning kernels must be written generically
to handle any matrix datatype, at runtime they tend to be executed repeatedly
on the same type.

Portable Computing Language (pocl)
----------------------------------

In addition to producing an easily portable open source OpenCL
implementation, another major goal of `pocl <http://portablecl.org/>`_
is improving performance portability of OpenCL programs with
compiler optimizations, reducing the need for target-dependent manual
optimizations. An important part of pocl is a set of LLVM passes used to
statically parallelize multiple work-items with the kernel compiler, even in
the presence of work-group barriers. This enables static parallelization of
the fine-grained static concurrency in the work groups in multiple ways. 

Portable Native Client (PNaCl)
------------------------------

`Portable Native Client (PNaCl) <http://www.chromium.org/nativeclient/pnacl>`_
is a Chrome initiative to bring the performance and low-level control of native
code to modern web browsers, without sacrificing the security benefits and
portability of web applications. PNaCl works by compiling native C and C++ code
to an intermediate representation using the LLVM clang compiler. This
intermediate representation is a subset of LLVM bytecode that is wrapped into a
portable executable, which can be hosted on a web server like any other website
asset. When the site is accessed, Chrome fetches and translates the portable
executable into an architecture-specific machine code optimized directly for
the underlying device. PNaCl lets developers compile their code once to run on
any hardware platform and embed their PNaCl application in any website,
enabling developers to directly leverage the power of the underlying CPU and
GPU.

TTA-based Co-design Environment (TCE)
-------------------------------------

`TCE <http://tce.cs.tut.fi/>`_ is a toolset for designing new
exposed datapath processors based on the Transport triggered architecture (TTA). 
The toolset provides a complete co-design flow from C/C++
programs down to synthesizable VHDL/Verilog and parallel program binaries.
Processor customization points include the register files, function units,
supported operations, and the interconnection network.

TCE uses Clang and LLVM for C/C++/OpenCL C language support, target independent 
optimizations and also for parts of code generation. It generates
new LLVM-based code generators "on the fly" for the designed processors and
loads them in to the compiler backend as runtime libraries to avoid
per-target recompilation of larger parts of the compiler chain. 

WebCL Validator
---------------

`WebCL Validator <https://github.com/KhronosGroup/webcl-validator>`_ implements
validation for WebCL C language which is a subset of OpenCL ES 1.1. Validator
checks the correctness of WebCL C, and implements memory protection for it as a
source-2-source transformation. The transformation converts WebCL to memory
protected OpenCL. The protected OpenCL cannot access any memory ranges which
were not allocated for it, and its memory is always initialized to prevent
information leakage from other programs.


Additional Information
======================

A wide variety of additional information is available on the `LLVM web page
<http://llvm.org/>`_, in particular in the `documentation
<http://llvm.org/docs/>`_ section.  The web page also contains versions of the
API documentation which is up-to-date with the Subversion version of the source
code.  You can access versions of these documents specific to this release by
going into the ``llvm/docs/`` directory in the LLVM tree.

If you have any questions or comments about LLVM, please feel free to contact
us via the `mailing lists <http://llvm.org/docs/#maillist>`_.
