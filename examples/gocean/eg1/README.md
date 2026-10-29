# PSyclone GOcean Example 1

These scripts and this version of PSyclone work with version 1.0 of GOcean.

In order to use PSyclone you must first install it, ideally with pip.
See `../../../README.md` for more details.

PSyclone can be run in the directory containing this file by 
executing, e.g.

```sh
psyclone --psykal-dsl gocean -s <script> shallow_alg.f90
``` 


## OpenMP tasking transformation script

The OpenMP tasking transformation is provided in the form of a PSyclone
transformation script (`openmp_taskloop_trans.py`). This can be run
using the PSyclone command:

```sh
psyclone -nodm -s ./openmp_taskloop_trans.py -api gocean shallow_alg.f90
```

## OpenCL PSyclone script

The OpenCL transformation is provided with a PSyclone transformation script
(`opencl_transformation.py`). This can be run using the PSyclone command:

```sh
psyclone -s ./opencl_transformation.py -api gocean shallow_alg.f90
```
