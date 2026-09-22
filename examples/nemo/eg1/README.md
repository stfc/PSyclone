# PSyclone NEMO Example 1

This directory contains the PSyclone example scripts:
`openmp_cpu_levels_trans.py` and `openmp_gpu_levels_trans.py`,
that demonstrates the use of PSyclone to parallelise all loops over
levels for the `tra_adv.F90` code, using OpenMP. It also contains
Makefile rules to generate parallel versions of the file with the
../scripts/ OpenMP scripts.

Once you have installed PSyclone, these scripts may be run by doing:

```sh
psyclone -s ./openmp_[cpu|gpu]_levels_trans.py ../code/traadv.f90
```
or
```sh
psyclone -s ../scripts/omp_[cpu|gpu]_trans.py ../code/traadv.f90
```

This will output the generated Fortran code with the OpenMP directives added.

`tra_adv.F90` - is a stand-alone version of one of the tracer-advection
routines from the NEMO ocean model. It was originally extracted by
Silvia Mocavero of CMCC. The code can be found in the `../code`
directory.
