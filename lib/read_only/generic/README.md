# Read-only Verification Library for Generic Fortran Code

This library implements the [PSyData API](
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html#read-only-verification-library-for-generic-fortran)
to verify that variables declared read-only are not modified (overwritten) in
a kernel.

A full runnable example can be found in [``examples/nemo/eg6/``](
https://github.com/stfc/PSyclone/tree/master/examples/nemo/eg6).

## Dependencies

This library uses the [PSyData API](
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html) to interface
with the application. It needs the ReadOnly (``read_only_base.jinja``) and
PSyData (``psy_data_base.jinja``) base classes, which are included in PSyclone
installation. These Jinja templates are processed to create
the generic read-only verification code for integer and real variables (both
32- and 64-bit), logical and character variables. It will support scalar, and
1- to 4-dimensional arrays. The ``read_only.f90`` file contains no actual
executable code, it inherits from the base classes, and is only used so that
the expected module- and type-names are available for PSyclone.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string.

The locations of the ReadOnly and PSyData base classes are specified
using the environment variables ``$LIB_TMPLT_DIR`` and ``$PSYDATA_LIB_DIR``,
respectively. They default to the relative paths to the
[``lib/read_only``](./../) and top-level [``lib``](./../../) directories.

The compilation process will create the wrapper library ``lib_read_only.a``.

Similar to compilation of the [examples](
https://psyclone.readthedocs.io/en/latest/tutorials_and_examples/examples_intro.html#compilation), the
compiled wrapper library can be removed by running ``make clean``. 

### Linking the wrapper library

The application needs to provide the parameters to link in this read-only
library, ``_read_only``. For instance:

```shell
$(F90)  ... -L$(PSYDATA_LIB_DIR)/read_only/generic -l_read_only
```

<!--
## Licence

-------------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2025, Science and Technology Facilities Council.
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
-->
