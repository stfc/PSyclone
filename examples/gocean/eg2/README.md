# PSyclone GOcean Example 2

**Authors:** A. R. Porter, STFC Daresbury Lab

## Introduction

This is a very simple, single-kernel example of the use of PSyclone to
generate an application that will run on a GPU. A second
transformation script is also provided that will add profiling
instrumentation to the PSyclone-generated code. The kernel itself
simply increments a field by adding the current value of the
time-stepping loop index

The Makefile and this version of PSyclone work with version 1.0 of the GOcean
API.

In order to use PSyclone you must first install it, ideally with pip.
See `../../../README.md` for more details.

## Compilation

In order to compile and run this example you will need the dl_esm_inf
infrastructure library installed ([github.com/stfc/dl_esm_inf](https://github.com/stfc/dl_esm_inf)). The
directory containing the dl_esm_inf directory can be specified in the
Makefile (SHARED_DIR) but defaults to the location of the git submodle
(`../../../external/dl_esm_inf`). The compiler and flags to use are
picked up from environment variables (and must be the same as used for
the dl_esm_inf library). e.g. to use the PGI compiler for an NVIDIA
Volta:

```sh
export F90=pgf90
export F90FLAGS="-O3 -Minfo=all -acc -ta=tesla,cc70"
export LDFLAGS="-acc -ta=tesla,cc70"
```

## Profiling

Building the `profile` target causes PSyclone to use the `acc_prof_transform.py`
script. This uses `acc_transform.py` to do the same OpenACC transformations but
then adds a profiling region around the body of the whole PSy layer routine.
Compilation of this example requires that the CUDA toolkit be installed. The
location of this toolkit can be passed to the Makefile by setting the `NVTX_DIR`
variable, e.g.:

```sh
make NVTX_DIR=/usr/local/lib64/cuda10.0 profile
```

This should produce a `single_prof.exe` executable which may be run
with NVIDIA's profiling tools, either `nvprof` or `nvvp`.

## Notes

PSyclone transforms the kernel source and adds a
`!$acc routine` to it to instruct the compiler to build it for the
accelerator device. An alternative solution (at least, for this simple
example) would be to use the '-Mipa=inline:reshape' compiler flag.
(Currently PSyclone's module-inline transformation cannot be used on
a kernel that has been transformed for use with OpenACC -
[issue #229](https://github.com/stfc/PSyclone/issues/229).)

Also, the kernel currently has the extents of the field array
as explicit arguments. This is because PGI does not support
assumed-size arrays within compute regions. Once PSyclone has been
extended to automatically provide this information - [issue #230](https://github.com/stfc/PSyclone/issues/230),
these arguments can be removed from the kernel meta-data and algorithm
layer.

## Licence:

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2018-2019, Science and Technology Facilities Council.
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
