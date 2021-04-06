# Libraries for use with PSyclone

This directory contains a base class as a Jinja template that can be used to simplify
the creation of [PSyData-based](
https://psyclone.readthedocs.io/en/latest/psy_data.html) wrapper libraries.

## Installation

TBD....

## Structure

### PSyData base class

The file ``psy_data_base.jinja`` contains a Jinja template that can be used
by PSyData-based wrapper libraries. Full documentation on using this template
is provided in the
[developer's guide](https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#jinja).
The script ``process.py`` is used by the derived classes to process this
template. This script is processed with the help of the Shell script
``get_python.sh`` that finds an executable Python command.

There is a simple ``Makefile`` for compilation tests, but any
PSyData wrapper library (in any of the subdirectories here) will process
this template and compile it in its own directory. This allows each library
to exactly specify which data types are required. No library should
rely on a file compiled in this directory.

### [``extract``](extract) directory

Contains code for extracting kernel data - i.e. all input and output parameters
of a kernel invocation.

### [``nan_test``](nan_test) directory

Contains PSyData libraries for checking that input and output parameters of
kernels are valid numbers (i.e. not ``NaN`` or infinity).

### [``profiling``](profiling) directory

Contains PSyData wrapper libraries for various profiling libraries and
stand-alone timing libraries.

### [``read_only``](read_only) directory

Contains PSyData wrapper libraries for verifying at run time that parameters
declared as read-only in the PSyclone metadata are indeed not changed in a
subroutine.

## Compilation

As said above, every PSyData wrapper library in the relevant subdirectories
can be compiled individually.

All ``Makefile``s support the variables ``F90`` and ``F90FLAGS`` to specify
the compiler and compilation flags to use. ``F90`` defaults to the Gnu
Fortran compiler (``gfortran``), i.e. simply running ``make`` will build a
wrapper library with the version of ``gfortran`` available in a user's
environment. The compilation flags vary from one library to another (they
are usually set to debugging). As for the compilation of the [API examples](
https://psyclone.readthedocs.io/en/latest/examples.html#compilation), these
flags can be set to a different compiler. For instance,

```shell
make F90=ifort F90FLAGS="-g -check bounds"
```

Similar to the [examples](
https://psyclone.readthedocs.io/en/latest/examples.html#compilation), the
compiled code can be removed by running ``make clean``. There is also the
``allclean`` target that removes the compiled wrapper library as well
as the compiled infrastructure library that the wrapper may
[depend on](#dependencies).

### Dependencies

The majority of wrapper libraries use the [PSyData base class](
#psydata-base-class) Jinja template and Python processing scripts. Their
location is set by the configurable variable ``ROOT_LIB_DIR``, which is
by default set to the relative path to the top-level `lib` directory.

Compilation of ``extract``, ``nan_test``, ``read_only`` and some of the
profiling wrapper libraries depends on infrastructure libraries relevant
to the API they are used for. [GOcean API](
https://psyclone.readthedocs.io/en/latest/gocean1p0.html) uses the
[```dl_esm_inf`` library](https://github.com/stfc/dl_esm_inf) and
[LFRic (Dynamo 0.3) API](
https://psyclone.readthedocs.io/en/latest/dynamo0p3.html)
uses the LFRic infrastructure (see the linked documentation on how to
access and use the LFRic code). The location of the respective
infrastructure libraries can be configured with the variable ``INF_DIR``
(as said above, to remove the compiled infrastructure library it is
necessary to run ``make allclean``). In addition, these wrapper libraries
use specific Jinja templates whose default location is set to the relative
path to the respective library directory but can also be configured with
the variable ``JINJA_TMPLT_DIR``.

Profiling wrapper libraries that depend on external tools (for instance,
[``dl_timer``](profiling/dl_timer/README.md) have specific variables that
configure paths to where these libraries are located in a user environment.
