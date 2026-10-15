# PSyclone PSyIR Examples

This directory contains examples of how to create and/or modify
instances of PSyIR and how to use backends to transform them into
code.

All of these examples require PSyclone to be installed.

## Example 1:

Create an instance of PSyIR using many of the generic PSyIR nodes and
output the resultant tree as Fortran and C. Currently the C
backend does not support all of the node types so it only outputs a
subset of the tree. This example may be run by doing:

```sh
> python create.py
```

## Example 2:

Demonstrates how to create and manipulate structure types (a.k.a.
derived types in Fortran) within the PSyIR.
To run this example:

```sh
> python create_structure_types.py
```

## Example 3:

Demonstrates how to manipulate an existing PSyIR tree. This example
imports the PSyIR created in Example 1, applies some modifications
to it and then outputs the modified PSyIR as Fortran code. This example may
be run by doing:

```sh
> python modify.py
```
