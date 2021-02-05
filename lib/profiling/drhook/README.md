# PSyclone Wrapper Library for `DrHook`

This is a wrapper library that maps the PSyclone PSyData API
to the DrHook API.


## Dependencies

The Dr Hook library  must be installed. Note that this is currently
not available on any public repository, you need to contact ECMWF to
obtain a copy of DrHook.

It uses the ProfileData type to store DrHook's handle for each region.

## Compilation
A makefile is provided for compilation. The environment variables
``$F90`` and ``$F90FLAGS`` can be set to point to the Fortran compiler
and flags to use. They default to ``gfortran`` and the empty string.
The application needs to provide the DrHook directory using the variable
``$DRHOOK_ROOT``, which defaults to ``../../../../drhook`` (i.e.
it assumes DrHook is installed next to PSyclone):

```sh
DRHOOK_ROOT=path_to_drhook make
```

In order to use the wrapper with your application, you must provide the
location of the wrapper as an include path (so that the module file is found),
and link first with the wrapper library, then the DrHook library:

```sh
gfortran -c  ... -I PATH-TO-PSYCLONE/lib/profiling/drhook somefile.f90
gfortran -o a.out ... -L PATH-TO-PSYCLONE/lib/profiling/drhook -ldrhook_psy
         -L PATH-TO-DRHOOK -ldrhook
```
Note that the name of the DrHook library might depend on the way it was compiled.


Example output:

```
Profiling information for program='./profile_test.drhook', proc#1:
        No. of instrumented routines called : 2
        Instrumentation started : 20200929 161905
        Instrumentation   ended : 20200929 161905
        Instrumentation overhead: 12.12%
        Memory usage : 70 MBytes (heap), 70 MBytes (rss), 0 MBytes (stack), 0 (paging)
        Wall-time is 0.00 sec on proc#1 (1 procs, 4 threads)
        Thread#1:        0.00 sec (100.00%)
        Thread#2:        0.00 sec (0.00%)
        Thread#3:        0.00 sec (0.00%)
        Thread#4:        0.00 sec (0.00%)

    #  % Time         Cumul         Self        Total     # of calls        Self       Total    Routine@<thread-id>
                                                                             (Size; Size/sec; Size/call; MinSize; MaxSize)
        (self)        (sec)        (sec)        (sec)                    ms/call     ms/call

    1    78.38        0.000        0.000        0.000              1        0.01        0.01    psy_test:invoke_0:r0@1
    2    21.62        0.000        0.000        0.000              1        0.00        0.00    psy_test:invoke_1_update_field:update_field_code:r0@1

```
