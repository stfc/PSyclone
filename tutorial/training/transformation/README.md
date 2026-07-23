# PSyclone for Generic Fortran Code
Besides its abilities in a DSL, PSyclone can also be used
to convert existing, generic Fortran code.

## 3.2-openmp
This section shows how to add OpenMP CPU parallelisation to
existing Fortran code.

## 3.4-tiling
PSyclone's loop tiling transformation is investigated in this
section.

## 3.6-sympy
This section introduces the usage of the symbolic maths system
SymPy. It shows how you can convert a Fortran expression to
SymPy, manipulate it, and then convert the expression back
to a Fortran expression that can be used in your code.

## 3.8-code-creation
This example shows how you can create new code from scratch
with PSyclone. It creates two versions of a loop, depending
on loop length. This allows to use different optimisations to
be applied depending on loop length.
