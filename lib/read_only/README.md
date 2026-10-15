# Read-Only Verification Libraries

This directory contains files related to [read-only verification](
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html#read-only-verification),
i.e. checks at runtime that a read-only parameter of a subroutine is indeed
not changed in a kernel. There is a
[PSyData read-only verification base class](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html#psydata-read-only-verification-base-class)
as a Jinja template that can be used to simplify the creation of API-specific wrapper libraries.

## ReadOnly base class

The file ``read_only_base.jinja`` contains a Jinja template that is used
by the [GOcean ``dl_esm_inf``-](./dl_esm_inf/README.md) and [LFRic-specific](
./lfric/README.md) wrapper libraries. It implements the required [PSyData API](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html) calls for
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

## [``dl_esm_inf``](./dl_esm_inf) directory

Contains the read-only, PSyData-API-based, wrapper library for the
``dl_esm_inf`` [GOcean API](
https://psyclone.readthedocs.io/en/latest/user_guide/gocean1p0.html).

## [``lfric``](./lfric) directory

Contains the read-only, PSyData-API-based, wrapper library for the
[LFRic API](
https://psyclone.readthedocs.io/en/latest/user_guide/lfric.html).

## [``generic``](./generic) directory

Contains the generic read-only wrapper library.
