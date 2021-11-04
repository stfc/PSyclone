# PSyclone GOcean PSyData NaN-check Example

**Author:** J. Henrichs, Bureau of Meteorology

## Introduction

This is a simple example that shows how to use the NaN verification
support in PSyclone. It is a stand-alone program that can be compiled
and run. 

## Compilation
A makefile is provided to compile this example. If required,
it will compile the dl_esm_inf library and the nan_test
wrapper library. By default, the compilation uses the version
of the dl_esm_inf library provided as a git submodule (under
``../../../external/dl_esm_inf/finite_difference``- see
https://psyclone-dev.readthedocs.io/en/latest/working_practises.html)
within the PSyclone repository. You can set the environment variable
``INF_DIR`` for the ``make`` command to pick a different version.

The makefile here invokes psyclone with the script ``test_nan_transform.py``.
This script uses PSyclone's ``NanTestTrans`` to instrument the two
invokes in the ``test.x90`` source file.

The source code computes divisions by 0 on the diagonals, resulting in
invalid numbers (Infinity).

## Running
```
$ ./nan_test
...
Allocating C-T field with bounds: (1:   6,1:   6), internal region is (2:   4,2:   4)
PSyData: Variable a_fld has the invalid value Inf at index/indices 1 1 in module 'main' region 'update'.
PSyData: Variable a_fld has the invalid value Inf at index/indices 2 2 in module 'main' region 'update'.

```
After calling the kernel ``mainupdate``, five warnings are printed,
indicating that the field ``a_fld`` has a value of infinity on the diagonals
(i.e. indices 1 1, ..., 5 5).

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2021, Science and Technology Facilities Council.
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
