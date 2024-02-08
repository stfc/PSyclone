# Kernel Extraction Library Using NetCDF for NEMO

This wrapper library is used to [write (extract)](
https://psyclone.readthedocs.io/en/stable/psyke.html)
input and output parameters of instrumented code regions to a [NetCDF file](
https://psyclone.readthedocs.io/en/stable/psyke.html#netcdf-extraction-example)
using the NEMO API.
A stand-alone driver can then be used to rerun this specific code region and
verify the results (or compare performance).

A full, stand-alone and runnable example can be found in
[``examples/nemo/eg5/``](
https://github.com/stfc/PSyclone/tree/master/examples/nemo/eg5).

## Dependencies

This library uses the [PSyData API](
https://psyclone.readthedocs.io/en/stable/psy_data.html) to interface with
the application. The following dependencies must be available:

- This library uses NetCDF to store the data, so NetCDF must
  be available on the system. NetCDF development packages are available via
  the Linux package manager. Otherwise they can be built from source that
  can be downloaded from the [UCAR NetCDF website](
  https://www.unidata.ucar.edu/software/netcdf). For more information please
  refer to the [hands-on practicals documentation](
  https://github.com/stfc/PSyclone/tree/master/tutorial/practicals#netcdf-library-lfric-examples).

- The ExtractNetcdf (``extract_netcdf_base.jinja``) and PSyData
  (``psy_data_base.jinja``) base classes, which are included in PSyclone
  installation. These Jinja templates are processed to create the
  code to write 32- and 64-bit ``integer``, 32- and 64-bit ``real`` scalars,
  and 1- to 4-dimensional ``real`` and ``integer`` arrays. The generated
  Fortran modules, ``extract_netcdf_base.f90`` and ``psy_data_base.f90``,
  are then used by the supplied NetCDF-kernel-extraction module,
  ``kernel_data_netcdf.f90``, to create the wrapper library.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string.

The NetCDF helper program ``nf-config`` is used to get the NetCDF-specific
include paths. 

The locations of the ExtractNetcdf and PSyData base classes are
specified using the environment variables ``$LIB_TMPLT_DIR`` and
``$PSYDATA_LIB_DIR``, respectively. They default to the relative paths to
the [``lib/extract/netcdf``](./../) and top-level [``lib``](./../../../)
directories.

The compilation process will create the wrapper library
``lib_extract.a``.

Similar to compilation of the [examples](
https://psyclone.readthedocs.io/en/latest/examples.html#compilation), the
compiled wrapper library can be removed by running ``make clean``.

### Linking the wrapper library

The application needs to provide the parameters to link in this
NetCDF-kernel-extraction library, ``_kernel_data_netcdf``, and the required
NetCDF parameters when compiling and linking. For instance:

```shell
$(F90)  ... -L$(PSYDATA_LIB_DIR)/extract/netcdf/dl_esm_inf -l_kernel_data_netcdf \
         $(nf-config --flibs)
```

### Note

Certain versions of Fedora have a broken ``nf-config`` script. In
this case the ``Makefile`` has to be modified to provide the required
information (you can try to see if ``nc-config`` can be used,
or you have to explicitly provide the required paths and options).

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
