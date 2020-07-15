# PSyclone Wrapper Library for LFRic

This is a wrapper library that maps PSyclone's PSyData profiling API
to the timer functionality provided in LFRic. Note that the module
is following the LFRic naming convention, meaning the library
is called libprofile_psy_data_mod.a.

## Dependencies

This directory contains some slightly modified files from the LFRic
infrastructure library to allow compilation without any dependencies.
If there should be changes to LFRic, you have to modify the Makefile
to use the files from your LFRic sources.

The file ``timer_mod.F90`` uses the pre-processor macro
``STANDALONE_COMPILATION`` to remove dependencies to LFRic
modules (except ``constants_mod`` and ``log_mod``, which are stand-alone
and are included here as well). 

## Compilation

Compilation is done with ``make``. You can adjust the variables
``F90`` and ``F90FLAGS`` to point to your Fortran compiler and the
compiler flags you want to use.


Example output:

```
||=           Routine            =||=   min time(s)     =||=   mean time(s)    =||=   max time(s)     =||=     No. calls     =||=       %time       =||= time per call(s)  =||
||            psy_test:invoke_0:r0||                 0.00||                 0.00||                 0.00||                    1||               100.00||                 0.00||
||psy_test:invoke_1_update_field:u||                 0.00||                 0.00||                 0.00||                    1||                37.11||                 0.00||

```
