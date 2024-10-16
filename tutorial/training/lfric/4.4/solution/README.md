# Simple LFRic Kernel and Scripts

In this session you will add openmp parallelisation for an LFRic kernel
that updates shared values, which will require colouring.

![FEM mesh](fem-grid.png "Simplified FEM mesh")

In this example a kernel adds a field on the W0
space (vertices) to another field on the W0 space, i.e. it is adding the blue dots
in the diagram above for two fields. The difference to the kernel used previously
is that the vertices (blue dots) are shared between neighbouring columns, which is
not the case for a field on the W3 space. Therefore, the loop over all columns
cannot be parallelised, since the shared vertices could be read and written by
different threads at the same time.

Running PSyclone with the OpenMP transformation script:

    psyclone -api lfric -s ./omp_transformation.py -dm -l output \
         -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90
 
(or run `make transform`) will raise an exception:

    Transformation Error: Error in DynamoOMPParallelLoopTrans transformation. \
    The kernel has an argument with INC access. Colouring is required.


## Hands-on
First of all, the kernel needs to be completed. All the meta-data, subroutine declaration
and even loops are already there, but you still need to add the computation. Once this is
done, you can run `make compile` to compile and link your example and execute it. The
output should be:

    Mesh has           5 layers.
    20240917233109.668+1000:INFO : Min/max minmax of field_3 =   0.80000000E+01  0.80000000E+01

Now finish `openmp.py` to add OpenMP parallelisation. 
