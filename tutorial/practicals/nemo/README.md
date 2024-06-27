# PSyclone for transforming existing code
 
This tutorial consists of four parts:

1. [Learn about the `psyclone` command and the PSyclone Intermediate Representation](1_nemo_psyir/README.md);
2. [Use PSyclone to add profiling calipers, both automatically and through user-supplied transformations](2_nemo_profiling/README.md);
3. [Use PSyclone to add loop-based OpenMP parallelisation](3_nemo_openmp/README.md);
4. [Use PSyclone to add OpenACC parallelisation](4_nemo_openacc/README.md).

It is best to tackle them in the order listed above since each builds
upon what has gone before. For parts 1-3 it is assumed you have a
working Fortran compiler as you are encouraged to build and run the
code that PSyclone generates.  For part 4 this is optional since it
requires a compiler that supports OpenACC and access to GPU hardware.

Currently this tutorial does not cover the use of PSyclone to transform
code to make use of OpenMP offload. If you are interested in this, please
see the first 'nemo' [example](../../../examples/nemo/eg1/README.md).
