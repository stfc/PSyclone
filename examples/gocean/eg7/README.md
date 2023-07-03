# PSyclone GOcean Example 7

**Authors:** R. W. Ford and S. Siso, STFC Daresbury Lab

## Introduction

This example demonstrates the use of the trans_alg() script function
to access to algorithm layer.

## Running

The code generation is performed using the `make` command which will
call the necessary PSyclone command. The standard PSyclone code
generation is ignored with the flags `-oalg /dev/null -opsy /dev/null`
as we are interested in the algorithm information output from the
script.
