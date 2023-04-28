# LFRic Infrastructure Library

This directory contains a heavily modified version of the
LFRic infrastructure library. Some files are used when
running compilation tests with ``pytest``, and the whole library
is used to implement a standalone LFRic example that can
be executed.

TODO #968: the copyright statements might need to be updated.

## Preprocessing
PSyclone only supports pre-processed files, any preprocessor directive
will cause parsing failures or create incorrect parse trees.
In LFRic, all files will be pre-processed before PSyclone is invoked.
In order to allow tests to work with this infrastructure library
without requiring pre-processing, the pre-processed files are added
in addition to the original files to the repository. This is only done
in order to make sure that our tests work in an environment (e.g. github
actions) where there might be no pre-processor installed. The Makefile
contains a target 'preprocess' which will process all ``.F90``
files and create the corresponding ``.f90`` files. Since the Makefile
relies on time-stamps, you need to provide the ``-B`` flag to ``make``
in order to enforce pre-processing of all files. The environment
variable ``CPP`` can be used to define the preprocessor to use, and
the variable ``FPPFLAGS`` can be used to specify any macros required if
you want to setup any non-default settings, e.g.:

```shell
FPPFLAGS="-DRDEF_PRECISION=32" make -B preprocess
```

## Usage in ``pytest``
The file ``<PSYCLONEHOME>/src/psyclone/tests/lfric_build.py`` contains
the list of files required for compilation testing (see
INFRASTRUCTURE_MODULES). These files will be compiled by each process,
and then used for all compilation tests.

## Usage in LFRic standalone example
The LFRic standalone examples (``<PSYCLONEHOME>/examples/lfric/``) use
this library and will trigger a compilation if required.
The makefile provides two targets:

```shell
make standalone
```
will compile a standalone version ``liblfric.a`` with no additional
dependencies. It will use existing unit-tests in the infrastructure
modules to define the required data structures.

```shell
make netcdf
```
will add support for reading in meshes using NetCDF in a library
``liblfric_netcdf.a``. Access to ``nf-config`` is required to
detect the required settings. When this library is used you
have to link in NetCDF after ``liblfric_netcdf.a``.

## Using the Intel Compiler
In order to use the Intel compiler, use the ``F90`` and ``F90FLAGS``
environmnent variables, e.g.:

```shell
F90=ifort F90FLAGS="-g -check bounds" make standalone
```
