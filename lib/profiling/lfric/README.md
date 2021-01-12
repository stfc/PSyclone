# PSyclone Wrapper Library for LFRic

This is a wrapper library that maps PSyclone's PSyData profiling API
to the timer functionality provided in LFRic.

## Dependencies

While this wrapper library uses LFRic infrastructure code for the timer,
the library itself can be used independent of LFRic for any code. The
``Makefile`` will create two different versions of the library:
- ``libpsy_lfric_timer.a``

  This version needs to be linked with the
  LFRic infrastructure library, which provides the actual timer
  code (and dependencies for the timer). This version is meant to be
  used for LFRic, since the corresponding objects files are already
  included in the LFRic build. In order to compile this library using
  already compiled LFRic code, you can use the following command:
  ```
  LFRIC_DIR=$SOME_PATH/lfric/trunk/miniapps/gravity_wave/working/utilities/ make libpsy_lfric_timer.a

  ```
  This will pick up the module file for the LFRic timer from the
  specified directory.

- ``libpsy_lfric_timer_standalone.a``

  This version of the library contains all dependencies to use the
  LFRic timer and can be used with any application. It uses the files
  of the simplified LFRic infrastructure library contained in
  ``../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure``.


## Compilation

Compilation is done with ``make``. You can adjust the variables
``F90`` and ``F90FLAGS`` to point to your Fortran compiler and the
compiler flags you want to use. The output is written to the file
``timer.txt``, which will be overwritten if it already exists.

Example output:

```
$ less timer.txt
||=           Routine            =||=   min time(s)     =||=   mean time(s)    =||=   max time(s)     =||=     No. calls     =||=       %time       =||= time per call(s)  =||
||            psy_test:invoke_0:r0||                 0.00||                 0.00||                 0.00||                    1||               100.00||                 0.00||
||psy_test:invoke_1_update_field:u||                 0.00||                 0.00||                 0.00||                    1||                37.11||                 0.00||

```
