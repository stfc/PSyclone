# Standalone LFRic Kernel Extraction Example

This directory contains a runnable example of kernel extraction
with LFRic. The main code is explained in more details in the
directory ``../full_example``.

It applies the PSyData extraction transformations to the
user-supplied kernels (due to #637 we cannot instrument builtins
at the moment).

## Compilation
A simple makefile is provided to compile the example. It needs:
- the infrastructure library ``liblfric.a`` provided in
  ``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``
- the LFRic PSyData wrapper library ``lib_kernel_data_netcdf`` from
  ``<PSYCLONEHOME>/lib/extract/netcdf/lfric``, and
- NetCDF

The infrastructure and PSyData wrapper libraries will be compiled
if they are not available.

The following environment variables can be set to define the compiler
you want to use:
```shell
export F90=gfortran
export F90FLAGS="-Wall -g -fcheck=bound"
make
```

The location of the PSyData wrapper library can be set with
the environment variable ``EXTRACT_DIR``, and the location
of the LFRic infrastructure libraries is set using
``LFRIC_DIR``.

