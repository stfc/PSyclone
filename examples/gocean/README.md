# PSyclone GOCean Examples

**Authors:** A. R. Porter and S. Siso, STFC Daresbury Lab

The sub-directories present in the directory containing this README provide
examples of the use of PSyclone with the GOcean 1.0 API.

## Example 1

Contains a version of the Shallow benchmark with a subset of the kernels
called from within invoke()'s. Contains example scripts showing the use
of PSyclone for adding OpenMP or OpenACC and for performing loop fusion.

## Example 2

A single-kernel example demonstrating the use of PSyclone in generating
a compilable and executable OpenACC code. Note that compiling this
example requires that the dl_esm_inf library ([github.com/stfc/dl_esm_inf](https://github.com/stfc/dl_esm_inf))
be installed first.

## Example 3

Illustrates the use of PSyclone to generate an OpenCL driver layer for
a four-kernel invoke and matching OpenCL kernels. Some useful optimisations
for OpenCL are applied to each kernel.

## Example 4

Examples of the application of kernel transforms to kernels that access
data and/or routines from other Fortran modules. Note that this is not
yet fully supported and is the subject of Issue #342.

## Example 5

Illustrates the use of the profiling support in PSyclone. The resulting
code may be compiled and executed.

## Example 6

Illustrates the use of the kernel-data extraction support in PSyclone. The
resulting code may be compiled and executed (requires a netcdf installation).

## Example 7

Illustrates the use of the read-only verification in PSyclone. The
resulting code may be compiled and executed to show warnings printed
by the read-only verification.

## Example 8

Informs the development of the code generation of PSy-layer code using the
PSyIR language backends.

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2017-2020, Science and Technology Facilities Council.
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

-----------------------------------------------------------------------------
