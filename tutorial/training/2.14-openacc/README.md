# OpenACC Parallelisation with PSyclone

PSyclone offers the ability to apply GPU offloading with OpenACC
to your code using transformations. It therefore requires
you to write a script to actually apply the transformations,
but you can create a very general script that will be able
to be used for different code bases. In this exercise though
we will focus on a custom script for the parallelisation of
Game of Life.

## OpenACC Transformations of Loops
Three different transformations need to be applied. We
first need to make sure that the required data is copied
to the GPU if required. Then, similar to OpenMP, we need
to add a directive to each loop to be parallelised, and
finally surround all parallel loop with a `acc parallel`
directive.


    from psyclone.transformations import (ACCParallelTrans, ACCEnterDataTrans,
                                          ACCLoopTrans)

    ptrans = ACCParallelTrans()
    ltrans = ACCLoopTrans()
    dtrans = ACCEnterDataTrans()

    invoke = psy.invokes.get("invoke_compute")
    schedule = invoke.schedule

    # Apply the OpenACC Loop transformation to *every* loop
    # nest in the schedule
    for child in schedule.children:
        if isinstance(child, Loop):
            ltrans.apply(child, {"collapse": 2})

    # Put all of the loops in a single parallel region
    ptrans.apply(schedule)

    # Add an enter-data directive
    dtrans.apply(schedule)

This creates a lot of additional, internal code for managing the GPU
device, and the following directives. Notice that the transformation
detected automatically which data needs to be transferred to the
GPU:

      !$acc enter data copyin(born,born%data,current,current%data,die,die%data,neighbours,neighbours%data)
      !$acc parallel default(present)
      !$acc loop independent collapse(2)
      DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
        DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
          CALL count_neighbours_code(i, j, neighbours%data, current%data)
          CALL compute_born_code(i, j, born%data, current%data, neighbours%data)
          CALL compute_die_code(i, j, die%data, current%data, neighbours%data)
        END DO
      END DO
      !$acc loop independent collapse(2)
      DO j = current%internal%ystart, current%internal%ystop, 1
        DO i = current%internal%xstart, current%internal%xstop, 1
          CALL combine_code(i, j, current%data, die%data, born%data)
        END DO
      END DO
      !$acc end parallel



## Additional Directives Required in Kernels
As opposed to OpenMP, each routine to be executed on the
GPU needs the `!$acc routine` directive so the compiler knows
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


## Copying GPU data back to CPU
This is required with GPU computing if the data is currently
on the GPU, but it is needed on the CPU (e.g. to output it).
The following directive is required in the algorithm code:

    do time = 1, time_steps, 1
      call invoke_compute(neighbours, current, born, die)
      if (time_steps <= 20) then
!$acc update self(current%data)
        call output_field(current)
      end if
    enddo

Without the update clause the output will write the unmodified
current status field.

TODO: ATM you need to manually modify the created alg layer :(
Adding it to the .x90 files doesn't work, since the directive is lsot.
ADding it to output_field does not work:
FATAL ERROR: data in update host clause was not found on device 1: name=field%data(:,:)
Even if I renamed 'field' in 'current' in output_field, I get
the same message :(

Additional issue, nvfortran does not support iargc(), so gol.f90 needs to
be modified as well :(