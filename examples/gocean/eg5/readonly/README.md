# PSyclone GOcean PSyData - ReadOnly-Verification Example

**Author:** J. Henrichs, Bureau of Meteorology

## Introduction

This is a simple test that shows how to use the read-only verification
support in PSyclone. It is a stand-alone program that can be compiled
and run. 

## Compilation
A makefile is provided to compile this example. If required
it will compile the dl_esm_inf library and the read-only verification
wrapper library. By default, the compilation uses the version
of the dl_esm_inf library provided as a git submodule (under
``../../../../external/dl_esm_inf/finite_difference``- see
https://psyclone-dev.readthedocs.io/en/latest/working_practises.html)
within the PSyclone repository. You can set the environment variable
``INF_DIR`` for the ``make`` command to pick a different version.

The makefile here invokes psyclone with the script ``read_only_transform.py``.
This script uses PSyclone's ``ReadOnlyVerifyTrans`` to instrument the two
invokes in the ``test.x90`` source file.

The source code overwrites the two read-only variables
``b_fld`` and ``z`` in the kernel, even though these variables are
declared as read-only in the PSyclone meta-data as well as being
declared with ``intent(in)``.  This is done by using out-of-bounds accesses
in one array that is declared to be writable. This code will trigger an
exception if the compiler adds array-bounds-check!

## Running
```
$ ./read_only_test
...
Allocating C-T field with bounds: (1: 12,1:  6)
Internal region is:(2:  4,2:  4)
Grid has bounds:  (1: 12,1:  6)
 ------------------- PSyData -------------------
 Double precision field b_fld has been modified in main : update
 Original checksum:   4611686018427387904
 New checksum:        4638355772470722560
 ------------------- PSyData -------------------
 ------------- PSyData -------------------------
 real(kind=real64) variable z has been modified in main : update
 Original value:    1.0000000000000000     
 New value:         123.00000000000000     
 ------------- PSyData -------------------------
```
After calling the kernel ``update``, two warnings are being printed:
one indicating that the field ``b_fld`` has been modified (including
the checksum computed before and after), and a second one indicating
that the value of the scalar variable ``z`` has changed from 1.0 to
123.0.


## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2020-2021, Science and Technology Facilities Council.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

------------------------------------------------------------------------------
