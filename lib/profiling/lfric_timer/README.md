# PSyclone Wrapper Library for LFRic

This is a wrapper library that maps PSyclone's [PSyData](
https://psyclone.readthedocs.io/en/stable/psy_data.html) [profiling API](
https://psyclone.readthedocs.io/en/stable/profiling.html#profiling) to the
timer functionality provided in LFRic (see the [LFRic (Dynamo 0.3) API](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html) documentation
on how to access and use the LFRic code).

## Dependencies

While this wrapper library uses LFRic infrastructure code for the timer,
``timer_mod.F90``, the library itself can be used independent of LFRic for
any code. The ``Makefile`` will create two different versions of the library:

- ``libpsy_lfric_timer.a``
  This version just contains the PSyData wrapper, but not the actual
  timer code. Hence, it needs to be linked with the LFRic infrastructure
  library, which provides the actual timer code, ``timer_mod.F90``, and
  dependencies for the timer. This version is meant to be used for LFRic,
  since the corresponding objects files are already included in the
  LFRic build.

- ``libpsy_lfric_timer_standalone.a``

  This version of the library contains all dependencies to use the
  LFRic timer and can be used with any application. This library uses
  the timer available in the pared-down LFRic infrastructure available
  in PSyclone. A runnable example using a GOcean code is included in
  [``examples/gocean/eg5/profile``](
  https://github.com/stfc/PSyclone/tree/master/examples/gocean/eg5/profile).

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use.

In order to compile the ``libpsy_lfric_timer.a`` library using already
compiled LFRic code, the following command can be used:

```shell
LFRIC_INF_DIR=$<path/to/LFRic/compiled/code> make libpsy_lfric_timer.a
```
This will pick up the module file for the LFRic timer from the specified
directory. As can be seen from the above command, the location of the LFRic
infrastructure code is specified using the environment variable
``LFRIC_INF_DIR``. For demonstration purposes, it defaults to the relative
path to location of the pared-down LFRic infrastructure located in a clone
of PSyclone repository,
``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``.
This is not available in the PSyclone [installation](
./../../README.md#installation) so the exact path
**must be specified** during the compilation process.

For an LFRic application, it is the responsibility of the user to make sure
that the module files used when compiling the LFRic timer library are
identical to the ones used when running an LFRic application.

As mentioned [above](#dependencies), the ``libpsy_lfric_timer_standalone.a``
library uses the pared-down LFRic infrastructure available in PSyclone.
As this is not available in the PSyclone [installation](
./../../README.md#installation), it is recommended to clone the PSyclone
repository and specify the exact path during the compilation process.
For instance,

```shell
LFRIC_INF_DIR=<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure make \
	libpsy_lfric_timer_standalone.a
```

The ``Makefile`` will compile the LFRic infrastructure library,
``liblfric.a``, if required, with the previously selected compiler flags.

### Linking the wrapper library

In order to link this timer library with your application, the location of
this library must be provided as an ``include`` path (so that the module
file is found). Also, the library name must be specified at link time.

For instance, linking the standalone library may look something like:

```shell
$(F90) -c  -I <PATH-TO-PSYCLONE>/lib/profiling/lfric_timer some_file.f90
$(F90) some_file.o -L <PATH-TO-PSYCLONE>/lib/profiling/lfric_timer -lpsy_lfric_timer_standalone
```
The application of the ``libpsy_lfric_timer.a`` needs to provide the path
to the LFRic compiled code as well, e.g. by adding ``-L$(LFRIC_INF_DIR) -llfric``.

**Note**, The ``<PATH-TO-PSYCLONE>`` differs depending on whether the
wrapper library is compiled in a clone of PSyclone repository or in a
PSyclone [installation](./../../README.md#installation).

## Output

The output is written to the file ``timer.txt``, which will be overwritten
if it already exists. An example output is below:

```
$ less timer.txt
||=           Routine            =||=   min time(s)     =||=   mean time(s)    =||=   max time(s)     =||=     No. calls     =||=       %time       =||= time per call(s)  =||
||            psy_test:invoke_0:r0||                 0.00||                 0.00||                 0.00||                    1||               100.00||                 0.00||
||psy_test:invoke_1_update_field:u||                 0.00||                 0.00||                 0.00||                    1||                37.11||                 0.00||

```

<!--
## Licence

-------------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
