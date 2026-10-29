# ``ValueRangeCheck``- Libraries

This directory contains files related to testing all input and output
parameters of a kernel to make sure they are within a user-specified range,
and not [``NaN`` or infinite](
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html#psydata-value-range-check).

There is a [PSyData base class](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html#psydata-base-class)
as a Jinja template that can be used to simplify the creation of API-specific
wrapper libraries.

## ValueRangeCheck base class

The file ``value_range_check_base.jinja`` contains a Jinja template that is used
by the [GOcean ``dl_esm_inf``-](./dl_esm_inf/README.md) and [LFRic-specific](
./lfric/README.md) wrapper libraries. It implements the required [PSyData API](
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html) calls for
Fortran base types (scalar and arrays).
Full documentation to the Jinja implementation of a PSyData base class is
in the PSyclone [Developer Guide](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html#jinja).
The script [``process.py``](./../README.md#psydata-base-class) is used by the
derived classes to process this template. There is a simple ``Makefile``
contained here for compilation tests, but each API-specific implementation (in
any of the subdirectories here) will process this template and compile it in
their own directory (to allow for the required data types to be supported).
The API-specific implementations do not link with the compiled version from
this directory.

There is also a generic implementation, which can be used with any existing
Fortran code.

## [``dl_esm_inf``](./dl_esm_inf) directory

Contains the ``ValueRangeCheck``, PSyData-API-based, wrapper library for the
``dl_esm_inf`` [GOcean API](
https://psyclone.readthedocs.io/en/latest/user_guide/gocean1p0.html).

## [``lfric``](./lfric) directory

Contains the ``ValueRangeCheck``, PSyData-API-based, wrapper library for the
[LFRic API](
https://psyclone.readthedocs.io/en/latest/user_guide/lfric.html).


## [``generic``](./generic) directory
Contains the generic implementation, supporting 1- to 5-dimensional arrays,
and 4 and 8 byte integer and real values.
