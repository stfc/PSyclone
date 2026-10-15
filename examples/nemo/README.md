# PSyclone NEMO Examples

This directory contains various examples of the use of PSyclone
to transform source code from the NEMO ocean model. See the READMEs
in the individual example directories for further details.

## Code

Contains:

1. the Tracer advection benchmark routine (tra_adv), as provided by
   Silvia Mocavero of CMCC and
2. an unmodified NEMO subroutine computing the horizontal component of
   the lateral tracer mixing trend (traldf_iso).

## Scripts

Contains a collection of example scripts and the instructions to process the NEMO code. These
are testend in our integration test against NEMOv4.0.2 and NEMOv5.0.

## Example 1

OpenMP parallelisation (for CPU and GPU) of tra_adv over levels.

## Example 2

OpenMP parallelisation of traldf_iso over levels.

## Example 3

OpenACC parallelisation of tra_adv. Contains a local transformation
script that adds both 'data' and 'kernels' directives to the
code. Also demonstrates the use of the `kernels_trans.py` script from
the `scripts` directory which adds 'kernels' and 'loop' directives as
well as profiling instrumentation. This script is designed for use
with NVIDIA's managed memory technology and therefore does not insert
data regions.

## Example 4

SIR generation and transformation to CUDA using Dawn with simple
examples and a cut down version of the tracer advection (tra_adv)
benchmark.

## Example 5

A simple stand-alone example that shows how data can be extracted for
each loop nest using PSyclone's kernel extraction feature PSyKE. Note
that creation of a driver program (which reads the data files,
execute the original loop and then compares the results) is not yet
supported for generic transformations.

## Example 6

A simple stand-alone example that shows verification that read-only data
is not modified, e.g. by out-of-bounds accesses to other variables.
This uses the PSyData interface to instrument generic Fortran code.

## Example 7

OpenMP parallelisation (for CPU and GPU) of `tra_adv` over levels, using
`nowait` and minimisation of introduced barriers.

## Example 8

A simple profiling example that shows OpenMP offloading transformations
with profiling hooks enabled.

## Example 9

A simple stand-alone example that shows PSyclone's value range transformation.
A user can specify valid ranges for variables, which will be verified at
runtime.
