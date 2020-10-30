# LFRic Infrastructure Library

This directory contains a heavily modified version of the
LFRic infrastructure library. Some files are used when
running compilation tests with ``pytest``, and the whole library
is used to implement a standalone LFRic example that can
be executed.

## Usage in ``pytest``
The file ``.../tests/lfric_build.py`` contains the list of files
required for compilation testing (see INFRASTRUCTURE_MODULES).
These files will be compiled by each process, and then used
for all compilation tests.

## Usage in LFRic standalone example
The LFRic standalone examples (``.../examples/lfric/``) use
this library and will trigger a compilation if required.
The makefile provides two targets:

```
make standalone
```
will compile a standalone version ``liblfric.a`` with no additional dependencies.
It will use existing unit-tests in the infrastructure modules
to define the required data structures.

```
make netcdf
```
will add support for reading in meshes using NetCDF in a library
``liblfric_netcdf.a``. Access to ``nf-config`` is required to
detect the required settings. When this library is used you
have to link in NetCDF after ``liblfric_netcdf.a``.
