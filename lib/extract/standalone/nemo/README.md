# Stand-alone Kernel Extraction Library for NEMO

This wrapper library is used to [write (extract)](
https://psyclone.readthedocs.io/en/stable/psyke.html)
input and output parameters of instrumented code regions to a [binary file](
https://psyclone.readthedocs.io/en/stable/psyke.html#extraction_libraries).
A stand-alone driver can then be used to rerun this specific code region and
verify the results (or compare performance).

A full, stand-alone and runnable example can be found in
[``examples/nemo/eg5/``](
https://github.com/stfc/PSyclone/tree/master/examples/nemo/eg5).

## Dependencies

This library uses the [PSyData API](
https://psyclone.readthedocs.io/en/stable/psy_data.html) to interface with
the application. The following dependencies must be available:

- The ExtractStandalone (``extract_standalone_base.jinja``) and PSyData
  (``psy_data_base.jinja``) base classes, which are included in PSyclone
  installation. These Jinja templates are processed to create the
  code to write 32- and 64-bit ``integer``, 32- and 64-bit ``real`` scalars,
  and 1- to 4-dimensional ``real`` and ``integer`` arrays. The generated
  Fortran modules, ``extract_standalone_base.f90`` and ``psy_data_base.f90``,
  are then used by the supplied kernel-extraction module,
  ``kernel_data_standalone.f90``, to create the wrapper library.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string.

The locations of the ExtractStandalone and PSyData base classes are
specified using the environment variables ``$LIB_TMPLT_DIR`` and
``$PSYDATA_LIB_DIR``, respectively. They default to the relative paths to
the [``lib/extract/standalone``](./../) and top-level [``lib``](./../../../)
directories.

The compilation process will create the wrapper library
``lib_extract.a``.

Similar to compilation of the [examples](
https://psyclone.readthedocs.io/en/latest/examples.html#compilation), the
compiled wrapper library can be removed by running ``make clean``. 

### Linking the wrapper library

At link time, the path to the stand-alone-kernel-extraction library,
``_kernel_data_standalone``, needs to be specified when compiling and linking.
For instance:

```shell
$(F90)  ... -L$(PSYDATA_LIB_DIR)/extract/standalone/dl_esm_inf -l_kernel_data_standalone
```


<!--
## Licence

-------------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
