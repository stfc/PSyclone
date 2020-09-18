# Read-only Verification Library for LFRic

This library implements the PSyData API to verify that variables
declared read-only are not modified (overwritten) in a kernel call
for an application using LFRic.


## Dependencies

This library uses the PSyData API to interface with the application.
The following dependencies must be available:
- The LFRIc infrastructure library. This library is not included
  in PSyclone, and must already be compiled.
- The ReadOnly and PSyData base classes, which are included in
  PSyclone. These Jinja templates are processed to create
  the read-only verification code for integer, 32- and 64-bit
  reals, and 1, 2, 3, and 4-dimensional arrays.

## Compilation
A makefile is provided for compilation. The environment variables
``$F90`` and ``$F90FLAGS`` can be set to point to the Fortran compiler
and flags to use. They default to ``gfortran`` and the empty string.
The location of the LFRic infrastructure library is specified 
using the environment variable ``$LFRIC_DIR``. It defaults to
``../../../../lfric/trunk//miniapps/gravity_wave/working/field``,
which is the gravity wave miniapp included in LFRic if LFRic is
installed 'next' to PSyclone. But any other LFRic miniapp
or the full gungho project can be used as well.
