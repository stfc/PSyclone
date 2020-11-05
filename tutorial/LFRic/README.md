# Tutorial on building LFRic code

This tutorial gives an overview of building LFRic code, with PSyclone
as an integral part of this process. This repository is structured
into four subdirectories, one with the
[introduction to LFRic](background) (including the overview of LFRic
code structure and how PSyclone is used in LFRic) and three examples.

## Examples

* [**Example 1**](example1) shows how to build and use simple LFRic kernels;

* [**Example 2**](example2) shows the use of PSyclone built-ins;

* **Example 3** shows a time-evolution of a field on a planar mesh (more
  elaborate example of LFRic code).

In order to mimic the LFRic code structure, an example algorithm and
main program (driver) are provided in all Examples. In LFRic,
[kernels](background/LFRic_kernel.md) are *invoke*d from
[algorithms](background/LFRic_intro.md), which are in turn called
from the top-level driver code (see the [*Separation of concerns* section]
(background/LFRic_intro.md#separation-of-concerns) in the [LFRic intro](
background/LFRic_intro.md)).

The kernel and algorithm code in the top-level `example` directories
is incomplete. The completed solutions are provided in the `solutions`
subdirectories of each Example, together with `Makefile`s to build
the code.

## LFRic code support

All the examples here use a pared-down version of the LFRic infrastructure
stored in `src/psyclone/tests/test_files/dynamo0p3/infrastructure`
directory of PSyclone repository. For more information on LFRic code
structure and functionality see [*LFRic code structure* section](
background/LFRic_intro.md#lfric-code-structure) of
[introduction to LFRic](background/LFRic_intro.md).

### Implementation of PSyclone in examples

The pared-down LFRic infrastructure used in these examples does not
have the support for distributed memory (done via the [`YAXT` library](
https://www.dkrz.de/redmine/projects/yaxt) in LFRic). Also, none of
the [PSyclone transformations](
https://psyclone.readthedocs.io/en/stable/transformations.html) are
applied here so the code is generated and run in serial.

## Software environment

* Linux OS or virtual machine (e.g. [Oracle VM Virtual Box](
  https://www.virtualbox.org/)).

* Fortran compiler supporting [Fortran 2003 standard.](
  https://gcc.gnu.org/wiki/GFortranStandards#Fortran_2003)
  The `Makefile`s in the Examples are based on the freely available [GNU
  Compiler Collection (GCC)](https://gcc.gnu.org/). LFRic code is routinely
  built with GCC 6.1.0, Intel Fortran Compiler 17.0.x and Cray Fortran
  Compilers 8.4.3 and 8.7.0. It has been proven to work with Intel 19.0.x
  and the latest GCC 10.2.0.

* [Python](https://www.python.org/) environment. Note that for these
  tutorials either [Python 2](https://www.python.org/download/releases/2.0/)
  or [Python 3](https://www.python.org/download/releases/3.0/) environments
  are fine. Running the entire LFRic code, however, requires Python 3 as
  described in the [*Languages and software requirements* section](
  bacgkround/LFRic_intro.md#languages-and-software-requirements).

* Latest PSyclone release (currently 1.9.0). For installation please refer to
  the [PSyclone user guide.](
  https://psyclone.readthedocs.io/en/stable/getting_going.html)
  The simplest way is to install PSyclone into the Python 3 environment
  that will be used in building LFRic. Alternatively, it can be installed in
  `user` mode or to another location. Note, the latter requires explicit
  setting of environment variables so it is not generally recommended.

* [NetCDF library](https://www.unidata.ucar.edu/software/netcdf/),
  together with its Fortran and C++ bindings, is required for reading in LFRic
  mesh files written in
  [UGRID format](https://ugrid-conventions.github.io/ugrid-conventions/).
  LFRic build system currently uses `netcdf-c-4.6.2`, NetCDF Fortran
  binding `netcdf-fortran-4.4.4` and C++ binding `netcdf-cxx4-4.3.0`.
  However, `netcdf-c-4.7.3`, `netcdf-fortran-4.5.2` and `netcdf-cxx4-4.3.1`
  are also proven to work. Getting and building NetCDF, including the list
  of requirements, is described [here.](
  https://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html)
  The above-listed recent ersions of NetCDF packages were built with
  [HDF5](https://www.hdfgroup.org/solutions/hdf5) 1.10.6 as one of the
  requirements. Other required libraries, `zlib` and `curl`, can be
  installed by a usual method for a particular flavour of Linux OS.
