# Standalone LFRic Kernel Extraction Example

This directory contains a runnable example of kernel extraction
with LFRic. The main code is explained in more details in the
directory ``../full_example``.

It applies the PSyData extraction transformations to the
user-supplied kernels (due to #637 we cannot instrument builtins
at the moment). See the [PSyKE](https://psyclone.readthedocs.io/en/stable/psyke.html)
chapter of the PSyclone documentation for details about this transformation.

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
``LFRIC_DIR`` - both defaulting to the versions included in
PSyclone.

## Running

The binary can be executed using ``extract`` without additional parameters:
```shell
./extract 
 Mesh has           5 layers.
20210318131720.135+1100:INFO : Min/max minmax of field1 =   0.10000000E+01  0.80000000E+01
```
This will produce a NetCDF file called ``main-update.nc``:
```shell
ncdump main-update.nc | less
netcdf main-update {
dimensions:
        field1dim%1 = 539 ;
        field2dim%1 = 539 ;
        map_w0dim%1 = 27 ;
        map_w0dim%2 = 9 ;
        field1_postdim%1 = 539 ;
variables:
        double field1(field1dim%1) ;
        double field2(field2dim%1) ;
        int map_w0(map_w0dim%2, map_w0dim%1) ;
        int ndf_w0 ;
        int nlayers ;
        int undf_w0 ;
        int cell_post ;
        double field1_post(field1_postdim%1) ;
data:

 field1 = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
...
```
