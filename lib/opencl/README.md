# Introduction #

This directory contains Fortran source code that wraps the OpenCL
functionality required by PSyclone when generating code that
targets an OpenCL device. It uses the "clFortran" interface code
available from https://github.com/cass-support/clfortran.

# Compiling #

The Makefile picks-up the compiler, flags etc. from environment
variables. You will need to set F90 (and optionally, F90FLAGS).
