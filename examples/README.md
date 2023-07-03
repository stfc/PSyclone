# PSyclone Examples

**Author:** J. Henrichs, Bureau of Meteorology
**Modifications:** A. R. Porter, STFC Daresbury Laboratory

# Introduction

This directory contains examples for the various APIs that PSyclone
supports, and data structures. In detail:

## [gocean](./gocean)
Various examples related to the GOCean API.

## [nemo](./nemo)
Various examples related to the NEMO API.

## [lfric](./lfric)
Various examples related to the LFRic API.

## [line_length](./line_length)
This example demonstrates the use of the line length option.

## [psyir](./psyir)
This directory contains examples on how to use PSyIR to create
Fortran and C code as output.

## [stub_generation](./stub_generation)
This directory shows the usage of the stub-generation functionality
of the psyclone-kern utility. This helps with generating a Kernel's
argument list and associated datatypes from the Kernel metadata.

## transformations
This directory shows how to use the inline transformation.

# Compiler Settings
By default, all examples are using ``gfortran`` as compiler.
If you want to change the compiler, use the environment
variables ``F90`` and ``F90FLAGS``, e.g.:

```shell
    F90=ifort F90FLAGS="-g -check bounds" make
```
