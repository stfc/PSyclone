# Software environment for the practical examples

* Linux OS or virtual machine (e.g. [Oracle VM Virtual Box](
  https://www.virtualbox.org/)).

* Fortran compiler supporting [Fortran 2003 standard.](
  https://gcc.gnu.org/wiki/GFortranStandards#Fortran_2003)
  The `Makefile`s in the examples are based on the freely available [GNU
  Compiler Collection (GCC)](https://gcc.gnu.org/). LFRic code is routinely
  built with GCC 6.1.0, Intel Fortran Compiler 17.0.x and Cray Fortran
  Compilers 8.4.3 and 8.7.0. It has been proven to work with Intel 19.0.x
  and the latest GCC 10.2.0.

* [Python](https://www.python.org/) environment. Note that for this
  tutorial either [Python 2](https://www.python.org/download/releases/2.0/)
  or [Python 3](https://www.python.org/download/releases/3.0/) environments
  are fine (plotting scripts require `numpy` and `matplotlib` packages in
  either case).

* Latest PSyclone release (currently 1.9.0). For installation please refer to
  the [PSyclone user guide.](
  https://psyclone.readthedocs.io/en/stable/getting_going.html)
  The simplest way is to install PSyclone into the Python environment
  that will be used in building LFRic.

* [NetCDF library](https://www.unidata.ucar.edu/software/netcdf/),
  together with its Fortran and C++ bindings, is required for reading in
  LFRic mesh files. The latest `netcdf-c-4.7.3`, NetCDF Fortran binding
  `netcdf-fortran-4.5.2` and and C++ binding `netcdf-cxx4-4.3.1` are
  proven to work for the LFRic code and are advisable versions for this
  tutorial. If you are using `gfortran` then the simplest way of
  installing these is probably to use your package manager, e.g. on Ubuntu:
  ```shell
  $ sudo apt install libnetcdf-dev libnetcdff-dev
  ```
  (see e.g. [this page](
  https://cloud-gc.readthedocs.io/en/latest/chapter04_developer-guide/install-basic.html#install-netcdf-with-package-manager)
  for an overview). If you are not using `gfortran` then you will probably
  have to build NetCDF yourself. This process is described [here](
  https://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html).

  *Note*, if you need to build NetCDF from source, the following libraries
  are also required: [HDF5](https://www.hdfgroup.org/solutions/hdf5), `zlib`
  and `curl` (the latter two can be installed via the package manager).
