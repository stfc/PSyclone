# OpenMP Offloading with PSyclone

PSyclone offers the ability to apply GPU offloading with OpenMP
to your code using transformations. It therefore requires
you to write a script to actually apply the transformations,
but you can create a very general script that will be able
to be used for different code bases. In this exercise though
we will focus on a custom script for the parallelisation of
Game of Life.

The currently available support for OpenMP offloading in PSyclone
needs the availability of managed memory. I.e. the compiler itself
takes on the task of ensuring that data is copied to/from the GPU
when required. Note that this approach can struggle with Fortran
code containing derived types however.

As such, there are only two main steps when using OpenMP for
offloading: parallelisation of the loops, and marking up
any subroutines called so that they are compiled for GPU.

## Parallelising Loops

Similar to CPU OpenMP parallelisation, OMP offloading uses
two transformations to parallelise loops: the
``teams distribute parallel do`` directive executes a loop in parallel,
and ``target`` marks a region to be executed in parallel. Technically,
these two directives can be combined as "composite" constructs, but
this is not yet supported by PSylcone, so we need two different
transformations. Also be aware that the ``omp target`` directive only
takes one ``omp teams`` directive. So, while OpenMP for CPUs allows
one parallel region to have multiple parallel loops, with GPU offloading
you need a separate ``target`` statement for each loop.

The ``OMPLoopTrans`` transformation needs to be created as
follows, so that it creates a ``teams distribute parallel do``
directive:

    loop_offloading = OMPLoopTrans(
        omp_directive="teamsdistributeparalleldo",
        omp_schedule="none")


TODO1: Create the ``loop_offloading`` transformation.


TODO2: Apply ``target trans`` to the directive you have just
added. There is no direct way of getting the added note as part
of applying a transformation, so you need to use the loop node,
and use ``parent`` (or ``ancestor(Directive)``, looking for the
next directive above the loop, which would be the node you just added.

In the end, the following code should be created:

      !$omp target
      !$omp teams distribute parallel do default(shared), private(i,j)
      DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
        DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
          CALL count_neighbours_code(i, j, neighbours%data, current%data)
          CALL compute_born_code(i, j, born%data, current%data, neighbours%data)
          CALL compute_die_code(i, j, die%data, current%data, neighbours%data)
        END DO
      END DO
      !$omp end teams distribute parallel do
      !$omp end target
      !$omp target
      !$omp teams distribute parallel do default(shared), private(i,j)
      DO j = current%internal%ystart, current%internal%ystop, 1
        DO i = current%internal%xstart, current%internal%xstop, 1
          CALL combine_code(i, j, current%data, die%data, born%data)
        END DO
      END DO
      !$omp end teams distribute parallel do
      !$omp end target

Note that there are two separate ``target`` areas, each of which with its own
``teams distribute parallel do`` section.


## Mark up Subroutines for GPU Execution
As opposed to OpenMP for CPU, each routine to be executed on the
GPU needs the ``!$omp declare target`` directive added to instruct the compiler
to create GPU-specific code for it. We are using module inlining to modify the
kernels after copying them into the psy-layer module.

The following code snippet will add the required directive to each of the kernels:

    from psyclone.transformations import OMPDeclareTargetTrans

    declare_target = OMPDeclareTargetTrans()
    # Put a ``!omp declare target `` directive inside each kernel
    for kern in psyir.walk(GOKern):
        declare_target.apply(kern)

The modified, module-inlined kernels are now:

     SUBROUTINE compute_born_code(i, j, born, current, neighbours)
     ...
     INTEGER, intent(in) :: j

      !$omp declare target
      born(i,j) = 0.0
      ...


After compilation, you can execute the program (ideally with the configuration in
``../gol-lib/config.glider-large``, which is a 1000x1000 grid and uses 2000 time steps).
