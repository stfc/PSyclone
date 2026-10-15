# PSyclone NEMO Example 2

This directory contains a python script demonstrating the use of
PSyclone to add OpenMP parallelism to the `traldf_iso.F90` code.
It also contains Makefile rules to generate parallel versions of the
file with the ../scripts/ OpenMP scripts.

Once you have installed PSyclone, these scripts may be run by doing:

```sh
psyclone -s ./omp_levels_trans.py traldf_iso.F90
```
or
```sh
psyclone -s ../scripts/omp_[cpu|gpu]_trans.py ../code/traldf_iso.f90
```

`traldf_iso.F90`, is an unmodified NEMO ocean model routine. This code
can be found in the `../code` directory. The PSyclone command will output
the generated Fortran code with the OpenMP directives added.
