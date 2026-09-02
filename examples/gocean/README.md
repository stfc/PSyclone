# PSyclone GOCean Examples

The sub-directories present in the directory containing this README provide
examples of the use of PSyclone with the GOcean 1.0 API.

## Example 1

Contains a version of the Shallow benchmark with a subset of the kernels
called from within invoke()'s. Contains example scripts showing the use
of PSyclone for adding OpenMP or OpenACC and for performing loop fusion.

## Example 2

A single-kernel example demonstrating the use of PSyclone in generating
a compilable and executable OpenACC code. Note that compiling this
example requires that the dl_esm_inf library ([github.com/stfc/dl_esm_inf](https://github.com/stfc/dl_esm_inf))
be installed first.

## Example 3

Illustrates the use of PSyclone to generate an OpenCL driver layer for
a four-kernel invoke and matching OpenCL kernels. Some useful optimisations
for OpenCL are applied to each kernel.

## Example 4

Examples of the application of kernel transforms to kernels that access
data and/or routines from other Fortran modules. Note that this is not
yet fully supported and is the subject of Issue #342.

## Example 5a (profile)

Illustrates the use of the profiling support in PSyclone. The resulting
code may be compiled and executed.

## Example 5b (extract)

Illustrates the use of the kernel-data extraction support in PSyclone. The
resulting code may be compiled and executed (requires a netcdf installation).

## Example 5c (readonly)

Illustrates the use of the read-only verification in PSyclone. The
resulting code may be compiled and executed to show warnings printed
by the read-only verification.

## Example 5d (value_range_check)

Illustrates the use of the value range check in PSyclone. The
resulting code may be compiled and executed to show warnings printed.
Note that certain environment variables need to be defined to enable
the value range check, see the README.md in that directory for details.

## Example 6

Informs the development of the code generation of PSy-layer code using the
PSyIR language backends.

## Example 7

Demonstrates the use of a script that can transform the algorithm layer.
