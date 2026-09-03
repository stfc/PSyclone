# PSyclone Wrapper Library Template

This is a simple example to help writing your own PSyclone [PSyData-API-based](
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html) profile library (see
the [User Guide Profiling](
https://psyclone.readthedocs.io/en/latest/user_guide/profiling.html#profiling) section
for more information). It only prints out the function called at runtime and does
not do any actual measurements.

## Dependencies

This test library is based on the [PSyData base class](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html#psydata-base-class),
which is included in PSyclone as a Jinja template, ``psy_data_base.jinja``.
Full documentation on using this template is provided in the PSyclone
[Developer Guide](
https://psyclone.readthedocs.io/en/latest/developer_guide/psy_data.html#jinja). The
script [``process.py``](./../../README.md#psydata-base-class) is used to
process this template.

Since the profiling API does not need access to any fields or variables,
only the static subroutines and ``PreStart`` and ``PostEnd`` are implemented;
the ``PreDeclare`` and ``ProvideVariable`` methods are not created at all.

The library uses the ``ProfileData`` type to store the module/region name
(done by the base class).

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use, e.g.

```shell
F90=gfortran F90FLAGS=-g make
```

The compiler flags default to ``gfortran`` and the empty string.

The location of the PSyData base class Jinja template,
``psy_data_base.jinja`` is specified using the environment variable
``$PSYDATA_LIB_DIR``. It defaults to the relative path to the
top-level [``lib``](./../../) directory.

The compilation process will create the wrapper library ``libdummy.a``.

### Linking the wrapper library

In order to link this library with your application, the location of
this library must be provided as an ``include`` path (so that the module
file is found). Also, the library name, ``dummy``, must be specified
at link time:

```shell
$(F90) -c  -I $(PSYDATA_LIB_DIR)/profiling/template some_file.f90
$(F90) some_file.o -L $(PSYDATA_LIB_DIR)/profiling/template -ldummy
```

## Output

The output is written to the command line. A sample output is below:

```
PreStart called for module 'psy_test' region 'invoke_0:r0'
PostEnd called for module 'psy_test' region 'invoke_0:r0'
PreStart called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
PostEnd called for module 'psy_test' region 'invoke_1_update_field:update_field_code:r0'
```
