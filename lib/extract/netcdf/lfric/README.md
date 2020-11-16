# Kernel Extraction Library Using NetCDF

This library implements the PSyData API. It is used to write
input- and output-parameters of instrumented code regions to a NetCDF
file. A stand-alone driver can then be used to rerun this specific
code region, and verify the results (or compare performance).

## Dependencies

The following dependencies must be available:
- This library uses NetCDF to store the data, so NetCDF must
  be available on the system. It can be downloaded from
  https://www.unidata.ucar.edu/software/netcdf/
- The LFRIc inftrastructure library. It needs a separate
  installation of LFRic at this stage.
It uses the PSyData API to interface with the application.

## Compilation

```sh
make
```
The environment variables ``$F90`` and ``$F90FLAGS`` can be set
to point to the Fortran compiler and flags to use. They default to
``gfortran`` and the empty string. The NetCDF helper program
``nf-config`` is used to get the NetCDF-specific include paths.

.. note::
    Certain versions of Fedora have a broken ``nf-config`` script. In
    this case you have to modify the Makefile to provide the required
    information (you can try to see if ``nc-config`` can be used,
    or you have to explicitly provide the required paths and options).

The application needs to provide the parameters to link in
this netcdf-kernel-extraction library, the infrastructure library
and the required NetCDF parameters when compiling and linking:

```sh
gfortran  ... -L../../../lib/extract/dl_esm_inf/netcdf -l_kernel_data_netcdf \
          $(LFRIC_SPECIFIC_LINKING_PARAMETERS)        \
          $(nf-config --flibs)

```