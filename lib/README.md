# Wrapper Libraries for Use with PSyclone

This directory contains [PSyData-API-based](
https://psyclone.readthedocs.io/en/stable/psy_data.html) wrapper libraries.
They enable PSyclone to provide transformations that will insert callbacks
to an external library at runtime. These callbacks allow third-party libraries
to access data structures at specified locations in the code for different
purposes, such as profiling, verification and extraction of argument values.
The wrapper libraries for the supported use cases are listed
[below](#structure).

## Installation

Wrapper libraries can be accessed from a clone of PSyclone repository
or a PSyclone [installation](
https://psyclone.readthedocs.io/en/stable/getting_going.html). In a
PSyclone installation the libraries may be found in ``share/psyclone/lib``
under your Python (or PSyclone, depending on the ``pip install`` options)
installation, see ["Getting Going"](
https://psyclone.readthedocs.io/en/stable/getting_going.html)
for possible locations.

If working with wrapper libraries from a PSyclone installation, it
is advisable to copy the entire ``lib`` directory to some convenient
location before building and using them. The provided ``Makefile``s
support the options to specify paths to the libraries and their
dependencies, see [below](#compilation) for more information.

## Structure

### PSyData base class

The file ``psy_data_base.jinja`` contains a Jinja template that can be used
by the [PSyData-API-based](
https://psyclone.readthedocs.io/en/stable/psy_data.html) wrapper libraries.
Full documentation on using this template is provided in the PSyclone
[Developer Guide](
https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja). The
script ``process.py`` is used by the derived classes to process this
template. This script is processed with the help of the Shell script
``get_python.sh`` that finds an executable Python command.

There is a simple ``Makefile`` for compilation tests, but any
PSyData-API-based wrapper library (in any of the subdirectories here) will
process this template and compile it in its own directory. This allows each
library to exactly specify which data types are required. No library should
rely on a file compiled in this directory.

### [``extract``](./extract) directory

Contains code for extracting kernel data - i.e. all input and output
parameters of a kernel invocation.

### [``nan_test``](./nan_test) directory

Contains PSyData-API-based libraries for checking that input and output
parameters of kernels are valid numbers (i.e. not ``NaN`` or infinity).

### [``profiling``](./profiling) directory

Contains PSyData-API-based wrapper libraries for various profiling libraries
and stand-alone timing libraries.

### [``read_only``](./read_only) directory

Contains PSyData-API-based libraries for verifying at run time that
parameters declared as read-only in the PSyclone metadata are indeed not
changed in a subroutine.

## Compilation

Every PSyData-API-based wrapper library in the relevant subdirectory can
be compiled individually. Compilation requires a Fortran compiler and
Gnu Make. Individual libraries have other dependencies that are specified
in the relevant ``README.md`` documents.

All ``Makefile``s support the variables ``F90`` and ``F90FLAGS`` to specify
the compiler and compilation flags to use. ``F90`` defaults to the Gnu
Fortran compiler (``gfortran``), i.e. simply running ``make`` will build a
wrapper library with the version of ``gfortran`` available in a user's
environment. The compilation flags vary from one library to another (they
are usually set to debugging). As for the compilation of the [API examples](
https://psyclone.readthedocs.io/en/latest/examples.html#compilation), these
flags can be set to a different compiler. For instance,

```shell
F90=ifort F90FLAGS="-g -check bounds" make
```

Similar to compilation of the [examples](
https://psyclone.readthedocs.io/en/latest/examples.html#compilation), the
compiled library can be removed by running ``make clean``. There is also
the ``allclean`` target that removes the compiled wrapper library as well
as the compiled infrastructure library that the wrapper may
[depend on](#dependencies).

The compilation of wrapper libraries was tested with the Gnu and Intel
Fortran compilers, see [here](
https://psyclone.readthedocs.io/en/latest/examples.html#supported-compilers)
for the full list. Please let the PSyclone developers know if you have
problems using a compiler that has been tested or if you are working
with a different compiler.

### Dependencies

The majority of wrapper libraries use the [PSyData base class](
#psydata-base-class) Jinja templates and Python processing scripts. Their
location is set by the configurable variable ``PSYDATA_LIB_DIR``, which
is by default set to the relative path to the top-level `lib` directory.

**Note**, ``PSYDATA_LIB_DIR`` differs depending on whether the wrapper
libraries are compiled in a clone of PSyclone repository or in a PSyclone
[installation](#installation).

Compilation of ``extract``, ``nan_test``, ``read_only`` and some of the
profiling wrapper libraries depends on infrastructure libraries relevant
to the API they are used for. [GOcean API](
https://psyclone.readthedocs.io/en/stable/gocean1p0.html) uses the
[``dl_esm_inf`` library](https://github.com/stfc/dl_esm_inf) and
[LFRic (Dynamo 0.3) API](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html)
uses the LFRic infrastructure (see the linked documentation on how to
access and use the LFRic code). The locations of the respective
infrastructure libraries can be configured with the variables
``GOCEAN_INF_DIR`` and ``LFRIC_INF_DIR``, respectively (as said above,
to remove the compiled infrastructure libraries it is necessary to
run ``make allclean``). In addition, these wrapper libraries use
specific Jinja templates whose default location is set to the relative
path to the respective library directory but can also be configured with
the variable ``LIB_TMPLT_DIR``.

Some libraries require NetCDF for compilation. Installation of NetCDF is
described in details in the [hands-on practicals documentation](
https://github.com/stfc/PSyclone/tree/master/tutorial/practicals#netcdf-library-lfric-examples).

Profiling wrapper libraries that depend on external tools (for instance,
[dl_timer](./profiling/dl_timer/README.md) have specific variables that
configure paths to where these libraries are located in a user environment.

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
