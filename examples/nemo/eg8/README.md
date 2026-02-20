# PSyclone NEMO Example 8

**Author:** T. H. Gibson, Advanced Micro Devices, Inc.

This example demonstrates a simple profiling workflow for OpenMP target
offloading, using the tracer advection demo. It processes `../code/tra_adv.F90` and
generates `traadv_instrumented.F90` with OpenMP target offload directives plus
profiling hooks. The transformation script `omp_gpu_profile_trans.py` is a
small local transform script that uses shared helpers from `../scripts` and
inserts profile regions around *all* OpenMP target regions.

## Running

```sh
make transform
```

or explicitly:

```sh
ENABLE_PROFILING=1 ${PSYCLONE} -s ./omp_gpu_profile_trans.py ../code/tra_adv.F90 -o traadv_instrumented.F90
```

This emits transformed Fortran code with PSyData profiling around OpenMP target
regions.

## Compiling and Running

This example supports compilation and execution using the AMD ROCTx profiling
wrapper in `../../../lib/profiling/amd` and the ROCm profiler (`rocprofv3`)
by default. It can also be tested with NVIDIA tooling by overriding the relevant
Makefile variables (compiler/flags and profiling wrapper variables such as
`PSYCLONE_PROFILING_DIR`, `PSYCLONE_PROFILING_LIB`, and
`PSYCLONE_PROFILING_LIBS`).

Typical compiler settings for AMD GPU offloading are:

```sh
export F90=amdflang
export F90FLAGS="-O3 -fopenmp --offload-arch=<arch>"
export LDFLAGS="-fopenmp --offload-arch=<arch> -L${ROCM_PATH}/lib -lrocprofiler-sdk-roctx"
```

Then build and run:

```sh
make compile
make run
```

For more information on profiling wrappers and profiler-specific options, see the
[profiling wrappers README](../../../lib/profiling/README.md).

## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2026, Science and Technology Facilities Council.
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
