# Kernel Extraction Library

This directory contains files related to [writing (extracting)](
https://psyclone.readthedocs.io/en/latest/user_guide/psyke.html) input and output
parameters of instrumented code regions.

For now the parameter extraction is only implemented to write out the
parameters to a [NetCDF file](./netcdf/README.md) (see the
[``netcdf`` directory](./netcdf) for implementation). There is a simple
``Makefile`` contained here for testing purposes, but each API-specific
implementation (in any of the subdirectories here) should compile its
own version of this extract wrapper library.
