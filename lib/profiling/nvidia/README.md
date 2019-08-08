# NVTX Wrapper #

This is a wrapper library that maps the PSyclone profiling API to the
NVIDIA Tools Extension library (NVTX). Unlike some of the other
profiling tools, the use of this library does *not* require that calls
to `ProfileInit()` and `ProfileFinalise()` be inserted into the
application.

## Dependencies ##

NVTX is a part of the CUDA toolkit which may be freely downloaded from
https://developer.nvidia.com/cuda-toolkit. However, it is not required
to build this wrapper - it is only needed when doing the final linking
of the application to be profiled. Since the NVTX library is in C,
this wrapper uses the Fortran ISO C Binding and is heavily based on
the example module provided by Massimiliano Fatica at
https://devblogs.nvidia.com/customize-cuda-fortran-profiling-nvtx/.

## Compilation ##

A Makefile is provided and just executing `make` should build the wrapper
library. By default the gfortran compiler is used but you will probably
want to use PGI if working with OpenACC, i.e. `make F90=pgf90`. This will
produce `libnvtx_prof.a` and `profile_mod.mod`.

When compiling the application that has been instrumented for
profiling, the location of the `profile_mod.mod` file must be provided
as an include/module path, e.g. `-I/path/to/psyclone/lib/profiling/nvidia`.

Finally, at the link stage the location of the wrapper *and* NVTX
libraries must be provided, e.g.:

    pgf90 <my object files> -L/path/to/psyclone/lib/profiling/nvidia -lnvtx_prof -L<CUDA_LIB_DIR> -lnvToolsExt

where <CUDA_LIB_DIR> will depend upon your system but is likely to be
something like `/apps/packages/cuda/10.0/lib64`.

Once the application has been built, it may be profiled using `nvvp`,
NVIDIA's Visual Profiler.

## Notes ##

Currently the wrapper library is configured with seven distinct
colours and assigns these to profile regions in a round-robin
fashion. i.e. each new region that is created by a call to `ProfileStart()`
causes a new colour to be chosen from the list.  This colour and the
name of the region are stored in the persistent `ProfileData` structure
associated with that region for use in subsequent profiling calls. Once the
end of the list of available colours is reached, the library simply goes
back to the first entry in the list.


