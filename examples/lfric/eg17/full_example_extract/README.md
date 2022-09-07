# Standalone LFRic Kernel Extraction Example

This directory contains a runnable example of kernel extraction
with LFRic. The main code is explained in more details in the
directory ``../full_example``.

This example applies the PSyData extraction transformations to the
two invoke statements, the first initialising two fields, the second
doing some computations.
See the [PSyKE](https://psyclone.readthedocs.io/en/stable/psyke.html)
chapter of the PSyclone documentation for details about this transformation.

## Compilation

A simple makefile is provided to compile the example. It can use one of
two extraction libraries: the stand-alone one, which only uses Fortran IO
to create binary files with the kernel parameters, or the NetCDF version, which
creates NetCDF files. By default, the stand-alone version will be used,
but you can set the ``TYPE`` environment variable to ``netcdf`` when building to
use the NetCDF library::

    $ TYPE=netcdf make compile

 To compile the example, the following dependencies are needed:
- the infrastructure library ``liblfric.a`` provided in
  ``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``
- one of the LFRic PSyData wrapper libraries, either:
    - ``lib_kernel_data_netcdf`` from
      ``<PSYCLONEHOME>/lib/extract/netcdf/lfric`` and NetCDF, or
    - ``lib_kernel_data_standalone`` from
      ``<PSYCLONEHOME>/lib/extract/standalone/lfric``

The infrastructure and PSyData wrapper libraries will be compiled
if they are not available.

The following environment variables can be set to define the compiler
you want to use:
```shell
export F90=gfortran
export F90FLAGS="-Wall -g -fcheck=bound"
```

The location of the PSyData wrapper library can be set with
the environment variable ``EXTRACT_DIR`` specifying the location of the
extraction library. The location of the LFRic infrastructure files is set
using ``LFRIC_DIR`` - both defaulting to the versions included in
PSyclone.

## Running

The binary can be executed using ``extract.standalone`` (or ``extract.netcdf``)
 without additional parameters:
```shell
./extract.standalone
 Mesh has           5 layers.
20210318131720.135+1100:INFO : Min/max minmax of field1 =   0.10000000E+01  0.80000000E+01
```
This will produce two binary files ``main-update.binary`` and ``main-init.binary``.
If you are using the NetCDF-based extraction library, instead two NetCDF files
called ``main-update.nc`` and ``main-init.nc`` will be created, which can be
analysed using ``ncdump``:

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
Note that due to #1392 the driver cannot be created yet.
