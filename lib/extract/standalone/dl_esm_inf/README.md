# Stand-alone Kernel Extraction Library for GOcean

This wrapper library is used to [write (extract)](
https://psyclone.readthedocs.io/en/stable/psyke.html)
input and output parameters of instrumented code regions to a [binary file](
https://psyclone.readthedocs.io/en/stable/psyke.html#extraction_libraries)
using the [``dl_esm_inf`` library](https://github.com/stfc/dl_esm_inf).
A stand-alone driver can then be used to rerun this specific code region and
verify the results (or compare performance).

A full, stand-alone and runnable example can be found in
[``examples/gocean/eg5/extract``](
https://github.com/stfc/PSyclone/tree/master/examples/gocean/eg5/extract).

## Dependencies

This library uses the [PSyData API](
https://psyclone.readthedocs.io/en/stable/psy_data.html) to interface with
the application. The following dependencies must be available:

- The [GOcean](https://psyclone.readthedocs.io/en/latest/gocean1p0.html)
  infrastructure library ``dl_esm_inf``. A stable version of this is included
  in the PSyclone repository as a Git submodule (see ["Installation"](
  https://psyclone-dev.readthedocs.io/en/stable/working_practises.html#dev-installation)
  in the PSyclone [Developer Guide](
  https://psyclone-dev.readthedocs.io/en/stable) for details on working with
  submodules). However, it is not included in the PSyclone [installation](
  ./../../../README.md#installation) and has to be cloned separately.

- The ExtractStandalone (``extract_standalone_base.jinja``) and PSyData
  (``psy_data_base.jinja``) base classes, which are included in PSyclone
  installation. These Jinja templates are processed to create the
  code to write ``integer``, 32- and 64-bit ``real`` scalars, and
  2-dimensional ``real`` and ``integer`` arrays. The generated Fortran
  modules, ``extract_standalone_base.f90`` and ``psy_data_base.f90``, are
  then used by the supplied kernel-extraction module,
  ``kernel_data_standalone.f90``, to create the wrapper library.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string.

The library can be compiled with MPI support, which will add the process rank
to each output filename, by setting ``MPI=yes``:

```shell
MPI=yes make
```

The build environment will provide ``NO_MPI`` as a pre-processor definition when
MPI is not enabled (to be compatible with LFRic) so that MPI specific code can
be disabled.

It needs the dl_esm_inf infrastructure library. By default, the version
included in PSyclone repository
(``<PSYCLONEHOME>/external/dl_esm_inf/finite_difference``) is used. This is not
available in the PSyclone [installation](./../../../README.md#installation)
so the exact path **must be specified** using the environment variable
``GOCEAN_INF_DIR``, e.g.

```shell
GOCEAN_INF_DIR=<path/to/dl_esm_inf/finite_difference> make
```

The locations of the ExtractStandalone and PSyData base classes are
specified using the environment variables ``$LIB_TMPLT_DIR`` and
``$PSYDATA_LIB_DIR``, respectively. They default to the relative paths to
the [``lib/extract/standalone``](./../) and top-level [``lib``](./../../../)
directories.

The compilation process will create the wrapper library
``lib_extract.a``. The ``Makefile`` will compile the
``dl_esm_inf`` infrastructure library, ``lib_fd.a``, if required, with the
previously selected compiler flags.

Similar to compilation of the [examples](
https://psyclone.readthedocs.io/en/latest/examples.html#compilation), the
compiled wrapper library can be removed by running ``make clean``. To also
remove the compiled infrastructure library it is necessary to run
``make allclean`` (this is especially important if changing compilers
or compiler flags).

### Linking the wrapper library

At link time, the path to the stand-alone-kernel-extraction library,
``_kernel_data_standalone``, and the ``dl_esm_inf`` infrastructure
library, ``_fd``, need to be specified when compiling and linking.
For instance:

```shell
$(F90)  ... -L$(PSYDATA_LIB_DIR)/extract/standalone/dl_esm_inf -l_kernel_data_standalone \
        -L$(GOCEAN_INF_DIR) -l_fd
```


<!--
## Licence

-------------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
