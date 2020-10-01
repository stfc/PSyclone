# PSyclone Wrapper Library Template

This is a simple example to help writing your own
PSyclone PSyData-based profile library. It only prints
out the function called at runtime and does not do any
actual measurements.

## Dependencies
This test library is based on the PSyData base class,
which is included in PSyclone as a Jinja template (see the
[developer's guide](https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#psydata-base-class)
). Since the profiling API does not need access to any fields or variables,
only the static subroutines and ``PreStart`` and ``PostEnd`` are implemented,
the ``PreDeclare`` and ``ProvideVariable`` methods are not created at all.

It uses the ProfileData type to store the module/region name (done by the base
class).


## Compilation
A makefile is provided for compilation. The environment variables
``$F90`` and ``$F90FLAGS`` can be set to point to the Fortran compiler
and flags to use. They default to ``gfortran`` and the empty string.

```sh
F90=gfortran F90FLAGS=-g make
```

In order to link this library with your application, you must provide
its location as an include path (so that the module file is found),
and specify the library at link time:

```sh
gfortran -c  -I PATH-TO-PSYCLONE/lib/profiling/template some_file.f90
gfortran some_file.o -L PATH-TO-PSYCLONE/lib/profiling/template -ldummy
```

Sample output:
```
PreStart called for module 'psy_test' region 'invoke_0:r0'
PostEnd called for module 'psy_test' region 'invoke_0:r0'
PreStart called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
PostEnd called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
```
