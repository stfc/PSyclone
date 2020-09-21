# Read-only Verification Library for dl_esm_inf

This library implements the PSyData API to verify that variables
declared read-only are not modified (overwritten) in a kernel call
for an application using the dl_esm_inf library.


## Dependencies

This library uses the PSyData API to interface with the application.
The following dependencies must be available:
- The GOcean infrastructure library ``dl_esm_inf``. A stable
  version of this is included in PSyclone.
- The ReadOnly and PSyData base classes, which are included in
  PSyclone. These Jinja templates are processed to create
  the read-only verification code for integer, 32- and 64-bit
  reals, and 2-dimensional arrays.

## Compilation
A makefile is provided for compilation. The environment variables
``$F90`` and ``$F90FLAGS`` can be set to point to the Fortran compiler
and flags to use. They default to ``gfortran`` and the empty string.
The dl_esm_inf library is also required. It defaults to the version included
in PSyclone (see `../../../external/dl_esm_inf/finite_difference`),
but you can overwrite this by setting ``$INF_DIR`` to point to a different
location. The makefile will compile the library if required.

A full runnable example can be found in
``examples/gocean/eg7``.
