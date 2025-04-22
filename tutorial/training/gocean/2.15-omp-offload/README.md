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
two transformations to parallelise loops: ``OMPLoopTrans``
is responsible for marking loops to be parallelised, and
``OMPTargetTrans`` marks a region to be executed in parallel.

In order to use offloading to parallelise a loop, the
``OMPLoopTrans`` transformation needs to be created as
follows, so that it creates a ``teams distribute parallel do``
directive:

    loop_offloading_trans = OMPLoopTrans(
        omp_directive="teamsdistributeparalleldo",
        omp_schedule="none")


TODO1: Apply the ``ACCLoopTrans`` directive to all loops in the schedule
Note that the solution script adds an additional parameter to the
transformation to collapse the two nested loops, which will be more
efficient. These kind of optional parameters will be explained in more
details later.

TODO2: Apply ``ACCParallelTrans`` to the whole schedule.

## Upload Required Data to GPU
When using offload (and no automatic memory management is used),
we need to instruct the CPU to upload the required data to the GPU.
In PSyclone this is done with the ``ACCEnterDataTrans`` transformation.
It does not need any parameter, it will analyse the called kernels
and the required parameters.

TODO3: apply ``ACCEnterDataTrans`` 

Study the created PSy-layer file ``time_step_alg_mod_psy.f90``.
It contains a lot of additional, internal code for managing the GPU
device, and the following directives. 

      !$acc enter data copyin(born,born%data,current,current%data,die,die%data,neighbours,neighbours%data)
      !$acc parallel default(present)
      !$acc loop independent
      DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
        DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
          CALL count_neighbours_code(i, j, neighbours%data, current%data)
          CALL compute_born_code(i, j, born%data, current%data, neighbours%data)
          CALL compute_die_code(i, j, die%data, current%data, neighbours%data)
        END DO
      END DO
      !$acc loop independent
      DO j = current%internal%ystart, current%internal%ystop, 1
        DO i = current%internal%xstart, current%internal%xstop, 1
          CALL combine_code(i, j, current%data, die%data, born%data)
        END DO
      END DO
      !$acc end parallel

Note that the data is not copied back by default. The only
output variable required in Game of Life is ``current``
when outputting the results. The function ``output_field``
contains a directive to update the CPU data with the
data on the GPU:

    !$acc update self(field%data)

Where ``field`` is ``current``. This means the data is only
copied back to CPU, which is a somewhat slow operation, when
really required.


## Mark up Subroutines for GPU Execution
As opposed to OpenMP, each routine to be executed on the
GPU needs the `!$acc routine` directive added to instruct the compiler
to create GPU-specific code for it. You can automate this using
PSyclone with the following code snippet:

    from psyclone.transformations import ACCRoutineTrans

    ktrans = ACCRoutineTrans()
    # Put an 'acc routine' directive inside each kernel
    for kern in schedule.coded_kernels():
        ktrans.apply(kern)

The modified, module-inlined kernels are now:

     SUBROUTINE compute_born_code(i, j, born, current, neighbours)
     ...
     INTEGER, intent(in) :: j

      !$acc routine
      born(i,j) = 0.0
      ...
