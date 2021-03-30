# PSyclone Wrapper Library for `dl_timer`

This is a wrapper library that maps the PSyclone profiling API
to the dl_timer API. This library is thread-safe.


## Dependencies
The library dl_timer must be installed, which can be downloaded from
https://bitbucket.org/apeg/dl_timer.
This PSyData-based profiling library is based on the PSyData base class,
which is included in PSyclone as a Jinja template (see the
[developer's guide](https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#psydata-base-class)
). Since the profiling API does not need access to any fields or variables,
only the static subroutines and ``PreStart`` and ``PostEnd`` are implemented,
the ``PreDeclare`` and ``ProvideVariable`` methods are not created at all.

It uses the ProfileData type and dl_timer's timer_register function
to store the module/region name (done by the base class) and the index used
by dl_timer.

## Compilation
A makefile is provided for compilation. The environment variables
``$F90`` and ``$F90FLAGS`` can be set to point to the Fortran compiler
and flags to use. They default to ``gfortran`` and the empty string.
You need to provide the location of the dl_timer library using the
variable ``$DL_TIMER_ROOT``, which defaults to ``../../../../dl_timer``
(i.e. it assumes dl_timer is installed next to PSyclone):

```sh
DL_TIMER_ROOT=path_to_dl_timer make
```

In order to use the wrapper with your application, you must provide the
location of the wrapper as an include path (so that the module file is found),
and link first with the wrapper library, then the dl_timer library:

```sh
gfortran -c ... -I PATH-TO-PSYCLONE/lib/profiling/dl_timer somefile.f90
gfortran -o a.out ... -L PATH-TO-PSYCLONE/lib/profiling/dl_timer -ldl_timer_psy
         -L PATH-TO-DLTIMER -ldltimer
```
The name of the dl_timer library will depend on the way it was compiled
(shared memory or distributed memory parallel).

Example output:

```
=============================== Timing report ===============================
Timed using POSIX timer. Units are seconds.
Reported resolution =  0.1000E-08 (s)
Effective clock granularity =  0.25997E-07 (s)
Measured systematic error in dl_timer API =  0.37790E-07 +/- 0.789E-09 (s)
Measured overhead in calling start/stop =  0.9411E-07 (s)
Measured overhead in calling start/stop for registered timer =  0.4725E-07 (s)
-----------------------------------------------------------------------------
Region                          Counts     Total       Average*     Std Err
-----------------------------------------------------------------------------
psy_inputoutput:eliminate_one_no     1  0.12603E+00   0.12603E+00  0.00E+00
psy_time_step_mod:swlon_adjust_c    11  0.12201E+01   0.11092E+00  0.28E-02
psy_time_step_mod:swlon_code        11  0.44050E+01   0.40046E+00  0.25E-02
psy_time_step_mod:swlon_update_c    11  0.18761E+01   0.17056E+00  0.45E-03
psy_time_step_mod:swlat_adjust_c    11  0.12325E+01   0.11204E+00  0.53E-03
psy_time_step_mod:swlat_code        11  0.50031E+01   0.45483E+00  0.26E-02
psy_time_step_mod:swlat_update_c    11  0.19000E+01   0.17272E+00  0.24E-02
-----------------------------------------------------------------------------
* corrected for systematic error
=============================================================================
```
