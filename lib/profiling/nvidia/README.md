# NVTX Wrapper

This is a wrapper library that maps the [PSyclone profiling API](
https://psyclone.readthedocs.io/en/stable/profiling.html#profiling) to the
NVIDIA Tools Extension library (NVTX). Unlike some of the other
profiling tools, the use of this library does *not* require that calls
to ``profile_PSyDataInit()`` and ``profile_PSyDataShutdown()`` be inserted
into the application.

This wrapper supports the ``profile_PSyDataStart()`` and
``profile_PSyDataStop()`` API calls that may be used in order to limit
the region of code that is profiled at runtime. If so, the application
must be linked against CUDA (``-Mcuda`` flag to the PGI compiler). This
functionality is often used in combination with disabling profiling at
application startup (e.g. flag ``'--profile-from-start off'`` to ``nvprof``).

## Dependencies

NVTX is a part of the CUDA toolkit which may be freely downloaded from
https://developer.nvidia.com/cuda-toolkit. However, it is not required
to build this wrapper - it is only needed when doing the final linking
of the application to be profiled. Since the NVTX library is in C,
this wrapper uses the Fortran ISO C Binding and is heavily based on
the example module provided by Massimiliano Fatica at
https://devblogs.nvidia.com/customize-cuda-fortran-profiling-nvtx/.

## Compilation

A ``Makefile`` is provided and just executing `make` should build the wrapper
library. By default the ``gfortran`` compiler is used but you will probably
want to use PGI if working with OpenACC, i.e. ``make F90=pgf90``. This will
produce ``libnvtx_prof.a`` and ``profile_mod.mod``.

When compiling the application that has been instrumented for
profiling, the location of the ``profile_mod.mod`` file must be provided
as an include/module path, e.g. ``-I/path/to/psyclone/lib/profiling/nvidia``.

### Linking the wrapper library

Finally, at the link stage the location of the wrapper *and* NVTX
libraries must be provided, e.g.:

```shell
pgf90 <my object files> -Mcuda -L<PATH-TO-PSYCLONE>/lib/profiling/nvidia -lnvtx_prof -L<CUDA_LIB_DIR> -lnvToolsExt
```

where ``<CUDA_LIB_DIR>`` will depend upon your system but is likely to be
something like ``/apps/packages/cuda/10.0/lib64``.

**Note**, The ``<PATH-TO-PSYCLONE>`` differs depending on whether the
wrapper library is compiled in a clone of PSyclone repository or in a
PSyclone [installation](./../../README.md#installation).

Once the application has been built, it may be profiled using ``nvvp``,
[NVIDIA's Visual Profiler](
https://developer.nvidia.com/nvidia-visual-profiler) (see the
[User's Guide](https://docs.nvidia.com/cuda/profiler-users-guide/index.html)
for more information).

## Notes

Currently the wrapper library is configured with seven distinct
colours and assigns these to profile regions in a "round-robin" fashion.
That is, each new region that is created by a call to ``ProfileStart()``
causes a new colour to be chosen from the list.  This colour and the
name of the region are stored in the persistent ``ProfileData`` structure
associated with that region for use in subsequent profiling calls. Once the
end of the list of available colours is reached, the library simply goes
back to the first entry in the list.

<!--
## Licence

-------------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2019-2021, Science and Technology Facilities Council.
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

-------------------------------------------------------------------------------
Authors: A. R. Porter, STFC Daresbury Lab,
         I. Kavcic, Met Office
-->
