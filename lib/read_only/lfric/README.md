# Read-only Verification Library for LFRic

This library implements the [PSyData API](
https://psyclone.readthedocs.io/en/latest/psy_data.html#read-only-verification-library-for-lfric)
to verify that variables declared read-only are not modified (overwritten) in
a kernel call for an application using the LFRic infrastructure library.

## Dependencies

This library uses the [PSyData API](
https://psyclone.readthedocs.io/en/stable/psy_data.html) to interface with
the application. The following dependencies must be available:

- The LFRic infrastructure library. A pared-down version of LFRic
  infrastructure is located in the PSyclone repository (see e.g.
  [LFRic Example 17](
  https://github.com/stfc/PSyclone/tree/master/examples/lfric/eg17), however
  it is not included in the PSyclone [installation](
  ./../../README.md#installation). See the [LFRic API](
  https://psyclone.readthedocs.io/en/stable/dynamo0p3.html) documentation
  for information on how to obtain access to the LFRic code.

- The ReadOnly (``read_only_base.jinja``) and PSyData
  (``psy_data_base.jinja``) base classes, which are included in PSyclone
  installation. These Jinja templates are processed to create the
  read-only verification code for ``integer``, 32- and 64-bit ``real``
  scalars, and 1, 2, 3, and 4-dimensional ``real`` and ``integer`` arrays.
  The generated Fortran modules, ``read_only_base.f90`` and
  ``psy_data_base.f90``, are then used by the supplied ``read_only.f90``
  module to create the wrapper library.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string.

The location of the LFRic infrastructure library is specified using the
environment variable ``LFRIC_INF_DIR``. It defaults to the relative path
to location of the pared-down LFRic infrastructure located in a clone of
PSyclone repository,
``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``.
This is not available in the PSyclone [installation](
./../../README.md#installation) so the exact path
**must be specified** during the compilation process, e.g.

```shell
F90=ifort F90FLAGS="-g -check bounds" LFRIC_INF_DIR=<path/to/LFRic/code> make
```

It is the responsibility of the user to make sure that the module files
used when compiling the LFRic read-only library are identical to the ones
used when running an LFRic application.

The locations of the ReadOnly and PSyData base classes are specified
using the environment variables ``$LIB_TMPLT_DIR`` and ``$PSYDATA_LIB_DIR``,
respectively. They default to the relative paths to the
[``lib/read_only``](./../) and top-level [``lib``](./../../) directories.

The compilation process will create the wrapper library ``lib_read_only.a``.
The ``Makefile`` will compile the LFRic infrastructure library,
``liblfric.a``, if required, with the previously selected compiler flags.

Similar to compilation of the [examples](
https://psyclone.readthedocs.io/en/latest/examples.html#compilation), the
compiled wrapper library can be removed by running ``make clean``. To also
remove the compiled infrastructure library it is necessary to run
``make allclean`` (this is especially important if changing compilers
or compiler flags).

### Linking the wrapper library

The application needs to provide the parameters to link in this read-only
library, ``_read_only``,  and the LFRic infrastructure library, ``lfric``.
For instance:

```shell
$(F90)  ... -L$(PSYDATA_LIB_DIR)/read_only/lfric -l_read_only \
        -L$(LFRIC_INF_DIR) -llfric $(LFRIC_SPECIFIC_LINKING_PARAMETERS)
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
