======================
LLVM 3.8 Release Notes
======================

.. contents::
    :local:


Introduction
============

This document contains the release notes for the LLVM Compiler Infrastructure,
release 3.8.  Here we describe the status of LLVM, including major improvements
from the previous release, improvements in various subprojects of LLVM, and
some of the current users of the code.  All LLVM releases may be downloaded
from the `LLVM releases web site <http://llvm.org/releases/>`_.

For more information about LLVM, including information about the latest
release, please check out the `main LLVM web site <http://llvm.org/>`_.  If you
have questions or comments, the `LLVM Developer's Mailing List
<http://lists.llvm.org/mailman/listinfo/llvm-dev>`_ is a good place to send
them.

Non-comprehensive list of changes in this release
=================================================
* With this release, the minimum Windows version required for running LLVM is
  Windows 7. Earlier versions, including Windows Vista and XP are no longer
  supported.

* With this release, the autoconf build system is deprecated. It will be removed
  in the 3.9 release. Please migrate to using CMake. For more information see:
  `Building LLVM with CMake <CMake.html>`_

* The C API function LLVMLinkModules is deprecated. It will be removed in the
  3.9 release. Please migrate to LLVMLinkModules2. Unlike the old function the
  new one

   * Doesn't take an unused parameter.
   * Destroys the source instead of only damaging it.
   * Does not record a message. Use the diagnostic handler instead.

* The C API functions LLVMParseBitcode, LLVMParseBitcodeInContext,
  LLVMGetBitcodeModuleInContext and LLVMGetBitcodeModule have been deprecated.
  They will be removed in 3.9. Please migrate to the versions with a 2 suffix.
  Unlike the old ones the new ones do not record a diagnostic message. Use
  the diagnostic handler instead.

* The deprecated C APIs LLVMGetBitcodeModuleProviderInContext and
  LLVMGetBitcodeModuleProvider have been removed.

* The deprecated C APIs LLVMCreateExecutionEngine, LLVMCreateInterpreter,
  LLVMCreateJITCompiler, LLVMAddModuleProvider and LLVMRemoveModuleProvider
  have been removed.

* With this release, the C API headers have been reorganized to improve build
  time. Type specific declarations have been moved to Type.h, and error
  handling routines have been moved to ErrorHandling.h. Both are included in
  Core.h so nothing should change for projects directly including the headers,
  but transitive dependencies may be affected.

* llvm-ar now suports thin archives.

* llvm doesn't produce .data.rel.ro.local or .data.rel sections anymore.

* aliases to available_externally globals are now rejected by the verifier.

* the IR Linker has been split into IRMover that moves bits from one module to
  another and Linker proper that decides what to link.

* Support for dematerializing has been dropped.

* RegisterScheduler::setDefault was removed. Targets that used to call into the
  command line parser to set the DAGScheduler, and that don't have enough
  control with setSchedulingPreference, should look into overriding the
  SubTargetHook "getDAGScheduler()".

* ``ilist_iterator<T>`` no longer has implicit conversions to and from ``T*``,
  since ``ilist_iterator<T>`` may be pointing at the sentinel (which is usually
  not of type ``T`` at all).  To convert from an iterator ``I`` to a pointer,
  use ``&*I``; to convert from a pointer ``P`` to an iterator, use
  ``P->getIterator()``.  Alternatively, explicit conversions via
  ``static_cast<T>(U)`` are still available.

* ``ilist_node<T>::getNextNode()`` and ``ilist_node<T>::getPrevNode()`` now
  fail at compile time when the node cannot access its parent list.
  Previously, when the sentinel was was an ``ilist_half_node<T>``, this API
  could return the sentinal instead of ``nullptr``.  Frustrated callers should
  be updated to use ``iplist<T>::getNextNode(T*)`` instead.  Alternatively, if
  the node ``N`` is guaranteed not to be the last in the list, it is safe to
  call ``&*++N->getIterator()`` directly.

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


Changes to the ARM Backends
---------------------------

During this release the AArch64 target has:

* Added support for more sanitizers (MSAN, TSAN) and made them compatible with
  all VMA kernel configurations (currently tested on 39 and 42 bits).
* Gained initial LLD support in the new ELF back-end
* Extended the Load/Store optimiser and cleaned up some of the bad decisions
  made earlier.
* Expanded LLDB support, including watchpoints, native building, Renderscript,
  LLDB-server, debugging 32-bit applications.
* Added support for the ``Exynos M1`` chip.

During this release the ARM target has:

* Gained massive performance improvements on embedded benchmarks due to finally
  running the stride vectorizer in full form, incrementing the performance gains
  that we already had in the previous releases with limited stride vectorization.
* Expanded LLDB support, including watchpoints, unwind tables
* Extended the Load/Store optimiser and cleaned up some of the bad decisions
  made earlier.
* Simplified code generation for global variable addresses in ELF, resulting in
  a significant (4% in Chromium) reduction in code size.
* Gained some additional code size improvements, though there's still a long road
  ahead, especially for older cores.
* Added some EABI floating point comparison functions to Compiler-RT
* Added support for Windows+GNU triple, +features in -mcpu/-march options.


Changes to the MIPS Target
--------------------------

During this release the MIPS target has:

* Significantly extended support for the Integrated Assembler. See below for
  more information
* Added support for the ``P5600`` processor.
* Added support for the ``interrupt`` attribute for MIPS32R2 and later. This
  attribute will generate a function which can be used as a interrupt handler
  on bare metal MIPS targets using the static relocation model.
* Added support for the ``ERETNC`` instruction found in MIPS32R5 and later.
* Added support for OpenCL. See http://portablecl.org/.

  * Address spaces 1 to 255 are now reserved for software use and conversions
    between them are no-op casts.

* Removed the ``mips16`` value for the -mcpu option since it is an :abbr:`ASE
  (Application Specific Extension)` and not a processor. If you were using this,
  please specify another CPU and use ``-mips16`` to enable MIPS16.
* Removed ``copy_u.w`` from 32-bit MSA and ``copy_u.d`` from 64-bit MSA since
  they have been removed from the MSA specification due to forward compatibility
  issues.  For example, 32-bit MSA code containing ``copy_u.w`` would behave
  differently on a 64-bit processor supporting MSA. The corresponding intrinsics
  are still available and may expand to ``copy_s.[wd]`` where this is
  appropriate for forward compatibility purposes.
* Relaxed the ``-mnan`` option to allow ``-mnan=2008`` on MIPS32R2/MIPS64R2 for
  compatibility with GCC.
* Made MIPS64R6 the default CPU for 64-bit Android triples.

The MIPS target has also fixed various bugs including the following notable
fixes:

* Fixed reversed operands on ``mthi``/``mtlo`` in the DSP :abbr:`ASE
  (Application Specific Extension)`.
* The code generator no longer uses ``jal`` for calls to absolute immediate
  addresses.
* Disabled fast instruction selection on MIPS32R6 and MIPS64R6 since this is not
  yet supported.
* Corrected addend for ``R_MIPS_HI16`` and ``R_MIPS_PCHI16`` in MCJIT
* The code generator no longer crashes when handling subregisters of an 64-bit
  FPU register with undefined value.
* The code generator no longer attempts to use ``$zero`` for operands that do
  not permit ``$zero``.
* Corrected the opcode used for ``ll``/``sc`` when using MIPS32R6/MIPS64R6 and
  the Integrated Assembler.
* Added support for atomic load and atomic store.
* Corrected debug info when dynamically re-aligning the stack.

Integrated Assembler
^^^^^^^^^^^^^^^^^^^^
We have made a large number of improvements to the integrated assembler for
MIPS. In this release, the integrated assembler isn't quite production-ready
since there are a few known issues related to bare-metal support, checking
immediates on instructions, and the N32/N64 ABI's. However, the current support
should be sufficient for many users of the O32 ABI, particularly those targeting
MIPS32 on Linux or bare-metal MIPS32.

If you would like to try the integrated assembler, please use
``-fintegrated-as``.

Changes to the PowerPC Target
-----------------------------

 During this release ...


Changes to the X86 Target
-----------------------------

 During this release ...

* TLS is enabled for Cygwin as emutls.

* Smaller code for materializing 32-bit 1 and -1 constants at ``-Os``.

* More efficient code for wide integer compares. (E.g. 64-bit compares
  on 32-bit targets.)

* Tail call support for ``thiscall``, ``stdcall`, ``vectorcall``, and
  ``fastcall`` functions.

Changes to the AVR Target
-------------------------

Slightly less than half of the AVR backend has been merged in at this point. It is still
missing a number large parts which cause it to be unusable, but is well on the
road to being completely merged and workable.

Changes to the OCaml bindings
-----------------------------

 During this release ...

* The ocaml function link_modules has been replaced with link_modules' which
  uses LLVMLinkModules2.


External Open Source Projects Using LLVM 3.8
============================================

An exciting aspect of LLVM is that it is used as an enabling technology for
a lot of other language and tools projects. This section lists some of the
projects that have already been updated to work with LLVM 3.8.

LDC - the LLVM-based D compiler
-------------------------------

`D <http://dlang.org>`_ is a language with C-like syntax and static typing. It
pragmatically combines efficiency, control, and modeling power, with safety and
programmer productivity. D supports powerful concepts like Compile-Time Function
Execution (CTFE) and Template Meta-Programming, provides an innovative approach
to concurrency and offers many classical paradigms.

`LDC <http://wiki.dlang.org/LDC>`_ uses the frontend from the reference compiler
combined with LLVM as backend to produce efficient native code. LDC targets
x86/x86_64 systems like Linux, OS X and Windows and also PowerPC (32/64 bit)
and ARM. Ports to other architectures like AArch64 and MIPS64 are underway.

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
