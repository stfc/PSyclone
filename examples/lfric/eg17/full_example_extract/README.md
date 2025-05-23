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
  ``<PSYCLONEHOME>/src/psyclone/tests/test_files/lfric/infrastructure``
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

This example can also be used to showcase the extraction if MPI is enabled.
Note that the code is *not* setup to run in parallel with MPI, but it can
be compiled with MPI and run as a single process job.
Extraction in this case means that the single process will write
its output data by appending its rank to the outpout filename. To enable
this, set the environment variable ``MPI=yes``.


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

This will produce two data dumps, by default these are the binary files ``main-update.binary``
and ``main-init.binary``, or if you are using the NetCDF-based extraction library, the NetCDF
files ``main-update.nc`` and ``main-init.nc``, which can be
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

The driver files can be compiled with `make driver-main-init` and `make driver-main-update`,
which will generate executables with the same name. Finally, once the drivers are compiled
and the data dumps generated, the kernel can be executed and compared with the original
results:
