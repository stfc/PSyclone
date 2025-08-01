# Kernel Extraction Library Using NetCDF

This directory contains files related to [writing (extracting)](
https://psyclone.readthedocs.io/en/latest/user_guide/psyke.html) input and output
parameters of instrumented code regions to a [NetCDF file](
https://psyclone.readthedocs.io/en/latest/user_guide/psyke.html#netcdf-extraction-example).
There is a [PSyData base class](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html#psydata-base-class)
as a Jinja template that can be used to simplify the creation of API-specific
wrapper libraries.

## ExtractNetcdf base class

The file ``extract_netcdf_base.jinja`` contains a Jinja template that is used
by the [GOcean ``dl_esm_inf``-](./dl_esm_inf/README.md) and [LFRic-specific](
./lfric/README.md) wrapper libraries. It implements the required [PSyData API](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html) calls for
Fortran base types (scalar and arrays).
Full documentation to the Jinja implementation of a PSyData base class is
in the PSyclone [Developer Guide](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html#jinja).
The script [``process.py``](./../../README.md#psydata-base-class) is used by
the derived classes to process this template. There is a simple ``Makefile``
contained here for compilation tests, but each API-specific implementation
(in any of the subdirectories here) will process this template and compile
it in their own directory (to allow for the required data types to be
supported). The API-specific implementations do not link with the compiled
version in this directory.

In order to support MPI in extraction (which means each process will write
its own output data by appending its rank to the filename), set the environment
variable ``MPI=yes``.

## [``dl_esm_inf``](./dl_esm_inf) directory

Contains the NetCDF-extract, PSyData-API-based, wrapper library for the
``dl_esm_inf`` [GOcean API](https://psyclone.readthedocs.io/en/latest/user_guide/gocean1p0.html).

## [``lfric``](./lfric) directory

Contains the NetCDF-extract, PSyData-API-based, wrapper library for the
[LFRic API](
https://psyclone.readthedocs.io/en/latest/user_guide/lfric.html).

<!--
## Licence

-------------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
