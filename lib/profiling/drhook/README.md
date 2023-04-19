# PSyclone Wrapper Library for Dr Hook

This is a wrapper library that maps the [PSyclone profiling API](
https://psyclone.readthedocs.io/en/stable/profiling.html#profiling)
to the Dr Hook API.

## Dependencies

The Dr Hook library  must be installed. Note that this is currently
not available on any public repository and the prospective user needs
to contact ECMWF to obtain a copy of Dr Hook.

This profiling library uses the [PSyData API](
https://psyclone.readthedocs.io/en/stable/psy_data.html) to interface with
the application. The library is based on the [PSyData base class](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#psydata-base-class),
which is included in PSyclone as a Jinja template, ``psy_data_base.jinja``.
Full documentation on using this template is provided in the PSyclone
[Developer Guide](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja).

The library uses the ``ProfileData`` type to store Dr Hook's handle for each
region.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string.

In the version tested here, 1.0.0, it appears that the installation target
in the Dr Hook distribution is broken, so the following instructions and
the default values assume that Dr Hook is compiled in a ``drhook``
subdirectory called ``build`` and not installed.

To compile the PSyclone wrapper library for Dr Hook, one of the following
two ``Makefile`` variables must be set to specify the path to the Dr Hook
installation:

- ``DRHOOK_ROOT``, the path to the Dr Hook root directory in which
  Dr Hook is compiled. It defaults to ``./../../../../drhook`` in the
  ``Makefile`` (i.e., it assumes Dr Hook is installed next to a PSyclone
  repository clone). This will set ``DRHOOK_INCLUDE`` to
  ``.../drhook/include`` and ``DRHOOK_MODULES`` to
  ``.../drhook/build/module``, so that the ``*.mod`` files created in the
  build processes are found.

- ``DRHOOK_INCLUDE`` and ``DRHOOK_MODULES``: Setting these environment
  variables explicitly will allow to flexibly point to an existing
  Dr Hook installation.

**Note**, it is not known whether a proper Dr Hook installation will
install ``include`` and ``*.mod`` files in separate directories.

For instance, compiling the wrapper library with the default compiler
flags may look something like:

```shell
DRHOOK_ROOT=<path_to_drhook> make
```

The compilation process will create the wrapper library ``libdrhook_psy.a``.

### Linking the wrapper library

In order to use the wrapper with your application, you must provide the
location of the wrapper as an ``include`` path (so that the module file is found),
and link first with the wrapper library, then the DrHook library:

In order to use the wrapper with your application, the location of this
library must be provided as an ``include`` path (so that the module file
is found), and linked first with the wrapper library, ``drhook_psy``,
and then with the Dr Hook library:

```shell
$(F90) -c  ... -I <PATH-TO-PSYCLONE>/lib/profiling/drhook somefile.f90
$(F90) -o a.out ... -L <PATH-TO-PSYCLONE>/lib/profiling/drhook -ldrhook_psy \
       -L <PATH-TO-DRHOOK> -ldrhook
```

**Note:**

- The name of the Dr Hook library might depend on the way it is compiled.

- The ``<PATH-TO-PSYCLONE>`` differs depending on whether the wrapper
  library is compiled in a clone of PSyclone repository or in a PSyclone
  [installation](./../../README.md#installation).

## Output

An example output of the profiling report is below:

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
