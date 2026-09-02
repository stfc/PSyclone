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
