# PSyclone Wrapper Library Template

This is a simple example to help writing your own PSyclone [PSyData-API-based](
https://psyclone.readthedocs.io/en/stable/psy_data.html) profile library (see
the ["Profiling"](
https://psyclone.readthedocs.io/en/stable/profiling.html#profiling) section
in the PSyclone [User Guide](https://psyclone.readthedocs.io/en/stable/) for
more information). It only prints out the function called at runtime and does
not do any actual measurements.

## Dependencies

This test library is based on the [PSyData base class](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#psydata-base-class),
which is included in PSyclone as a Jinja template, ``psy_data_base.jinja``.
Full documentation on using this template is provided in the PSyclone
[Developer Guide](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja). The
script [``process.py``](./../../README.md#psydata-base-class) is used to
process this template.

Since the profiling API does not need access to any fields or variables,
only the static subroutines and ``PreStart`` and ``PostEnd`` are implemented;
the ``PreDeclare`` and ``ProvideVariable`` methods are not created at all.

The library uses the ``ProfileData`` type to store the module/region name
(done by the base class).

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use, e.g.

```shell
F90=gfortran F90FLAGS=-g make
```

The compiler flags default to ``gfortran`` and the empty string.

The location of the PSyData base class Jinja template,
``psy_data_base.jinja`` is specified using the environment variable
``$PSYDATA_LIB_DIR``. It defaults to the relative path to the
top-level [``lib``](./../../) directory.

The compilation process will create the wrapper library ``libdummy.a``.

### Linking the wrapper library

In order to link this library with your application, the location of
this library must be provided as an ``include`` path (so that the module
file is found). Also, the library name, ``dummy``, must be specified
at link time:

```shell
$(F90) -c  -I $(PSYDATA_LIB_DIR)/profiling/template some_file.f90
$(F90) some_file.o -L $(PSYDATA_LIB_DIR)/profiling/template -ldummy
```

## Output

The output is written to the command line. A sample output is below:

```
PreStart called for module 'psy_test' region 'invoke_0:r0'
PostEnd called for module 'psy_test' region 'invoke_0:r0'
PreStart called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
PostEnd called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
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
