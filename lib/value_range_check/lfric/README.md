# ``ValueRangeCheck`` Library for LFRic

This library implements the [PSyData API](
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html#psydata-value-range-check)
to verify that input and output parameters of an LFRic kernel are within
a user-specified range, and not ``NaN`` or infinite, using the LFRic
infrastructure library.

## Dependencies

This library uses the [PSyData API](
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html) to interface with
the application. The following dependencies must be available:

- The LFRic infrastructure library. A pared-down version of LFRic
  infrastructure is located in the PSyclone repository (see e.g.
  [LFRic Example 17](
  https://github.com/stfc/PSyclone/tree/master/examples/lfric/eg17), however
  it is not included in the PSyclone [installation](
  ./../../README.md#installation). See the [LFRic API](
  https://psyclone.readthedocs.io/en/latest/user_guide/lfric.html) documentation
  for information on how to obtain access to the LFRic code.

- The ValueRangeCheck (``value_range_check_base.jinja``) and PSyData
  (``psy_data_base.jinja``) base classes, which are included in PSyclone
  installation. These Jinja templates are processed to create
  the ``ValueRangeCheck`` verification code for ``integer``, 32- and 64-bit ``real``
  scalars, and 1, 2, 3, and 4-dimensional ``real`` and ``integer`` arrays. The
  generated Fortran modules, ``value_range_check_base.f90`` and ``psy_data_base.f90``,
  are then used by the supplied ``value_range_check.f90`` module to create the wrapper
  library.

## Compilation

The library is compiled with ``make`` using the provided ``Makefile``. The
environment variables ``$F90`` and ``$F90FLAGS`` can be set to point to the
[Fortran compiler](./../../README.md#compilation) and flags to use. They
default to ``gfortran`` and the empty string.

The location of the LFRic infrastructure library is specified using the
environment variable ``LFRIC_PATH``. It defaults to the relative path
to location of LFRic infrastructure located in a clone of
PSyclone repository,
``<PSYCLONEHOME>/external/lfric_infrastructure/src``.
This is not available in the PSyclone [installation](
./../../README.md#installation) so the exact path
**must be specified** during the compilation process, e.g.

```shell
F90=ifort F90FLAGS="-g -check bounds" LFRIC_PATH=<path/to/LFRic/code> make
```

It is the responsibility of the user to make sure that the module files
used when compiling the LFRic ``ValueRangeCheck`` library are identical to the
ones used when running an LFRic application.

The locations of the ValueRangeCheck and PSyData base classes are specified
using the environment variables ``$LIB_TMPLT_DIR`` and ``$PSYDATA_LIB_DIR``,
respectively. They default to the relative paths to the
[``lib/value_range_check``](./../) and top-level [``lib``](./../../) directories.

The compilation process will create the wrapper library ``lib_value_range_check.a``.
The ``Makefile`` will compile the LFRic infrastructure library,
``liblfric.a``, if required, with the previously selected compiler flags.

Similar to compilation of the [examples](
https://psyclone.readthedocs.io/en/latest/tutorials_and_examples/examples_intro.html#compilation), the
compiled wrapper library can be removed by running ``make clean``. To also
remove the compiled infrastructure library it is necessary to run
``make allclean`` (this is especially important if changing compilers
or compiler flags).

### Linking the wrapper library

The application needs to provide the parameters to link in this
``ValueRangeCheck`` library, ``_value_range_check``, and the LFRic infrastructure library,
``lfric``. For instance:

```shell
$(F90)  ... -L$(PSYDATA_LIB_DIR)/value_range_check/lfric -l_value_range_check \
        -L$(LFRIC_PATH) -llfric $(LFRIC_SPECIFIC_LINKING_PARAMETERS)
```
