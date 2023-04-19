# PSyclone Wrapper Library for ``dl_timer``

This is a wrapper library that maps the [PSyclone profiling API](
https://psyclone.readthedocs.io/en/stable/profiling.html#profiling) to the
dl_timer API. This library is **thread-safe**.

## Dependencies

The library dl_timer must be installed, which can be downloaded from
https://bitbucket.org/apeg/dl_timer.

This profiling library uses the [PSyData API](
https://psyclone.readthedocs.io/en/stable/psy_data.html) to interface with
the application. The library is based on the [PSyData base class](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#psydata-base-class),
which is included in PSyclone as a Jinja template, ``psy_data_base.jinja``.
Full documentation on using this template is provided in the PSyclone
[Developer Guide](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja).

Since the profiling API does not need access to any fields or variables,
only the static subroutines and ``PreStart`` and ``PostEnd`` are implemented,
the ``PreDeclare`` and ``ProvideVariable`` methods are not created at all.

The library uses the ``ProfileData`` type and dl_timer's
timer_register function to store the module/region name (done by the base
class) and the index used by dl_timer.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string.

To compile the PSyclone wrapper library for dl_timer, one of the
following two ``Makefile`` variables must be set to specify the path to
the dl_timer installation:

- ``DL_TIMER_ROOT``, the path to the ``apeg-dl_timer`` directory in which
  dl_timer is compiled. It defaults to ``./../../../../dl_timer`` in
  the ``Makefile`` (i.e., it assumes dl_timer is installed next to a
  PSyclone clone).

- ``DL_TIMER_INCLUDE``, the path to the dl_timer ``include`` directory
  (i.e. ``src`` directory). This defaults to ``$DL_TIMER_ROOT/src``.

For instance, compiling the wrapper library with the default compiler
flags may look something like:

```shell
DL_TIMER_ROOT=<path_to_dl_timer> make
```

The location of the PSyData base class Jinja template,
``psy_data_base.jinja`` is specified using the environment variable
``$PSYDATA_LIB_DIR``. It defaults to the relative path to the
top-level [``lib``](./../../) directory.

The compilation process will create the wrapper library ``libdl_timer_psy.a``.

### Linking the wrapper library

In order to use the wrapper with your application, the location of this
library must be provided as an ``include`` path (so that the module file
is found), and linked first with the wrapper library, ``dl_timer_psy``,
and then with the dl_timer library:

```shell
$(F90) -c ... -I $(PSYDATA_LIB_DIR)/profiling/dl_timer somefile.f90
$(F90) -o a.out ... -L $(PSYDATA_LIB_DIR)/profiling/dl_timer -ldl_timer_psy \
       -L PATH-TO-DLTIMER -ldltimer
```
The name of the dl_timer library will depend on the way it is compiled
(shared-memory or distributed-memory parallel).

## Output

An example output of the profiling report is below:

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
Authors: J. Henrichs, Bureau of Meteorology,
         I. Kavcic, Met Office
-->
