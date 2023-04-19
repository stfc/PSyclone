# PSyclone GOcean Example 6

**Author:** S. Siso, STFC Daresbury Lab

## Introduction

This example informs the development of the code generation of PSy-layer
code using the PSyIR language backends.


## Running

The code generation is performed using the `make` command which will call
the necessary PSyclone command. The standard PSyclone code generation is
ignored with the flags `-oalg /dev/null -opsy /dev/null` and for now the
PSyclone transformation script itself is responsible for generating the
code and writing it to stdout.

The code generation is incomplete, so no options to compile nor run the code
have been added to the Makefile.
