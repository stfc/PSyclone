# Code Optimisations with PSyclone

The measurement at the end of the previous training module showed
that there can be a significant performance loss with the initial
version of the PSyclone Game of Life. In this module we will
apply some transformations to improve the performance of the
code.

## Loop Fusion of Outer Loops
The original design of splitting the code into several
small kernels is typically not ideal from a performance
point of view, since hardly any data is being reused and
data needs to be loaded from memory, instead of from cache.
But from a software engineering point of view splitting
the code into smaller kernels offer better testing
opportunities: in the Game of Life example you can
independently test counting the neighbours, determining
dying and newly born cells, and combining the results.
In case of test failures less manual work is required
to find the reason.

With PSyclone it is possible to design your code in smaller
kernels, but then use transformation scripts to optimise
the code to give better performance.

At the moment, the code created looks like this:

    do j
       do i
           call count_neighbours(i, j, neighbours, current)
       enddo
    enddo
    do j
       do i
           call compute_born(i, j, born,current, neighbours)

Counting the neighbours will mean that the values of `current`
and `neighbours` will be in cache, but (for larger fields)
the cache will not be big enough to store the whole arrays, and
by the time `compute_born` is executed, the data needs to be
reloaded from memory. The performance could be improved
by combining the loops:

    do j
       do i
           call count_neighbours(i, j, neighbours, current)
       enddo
       do i
           call compute_born(i, j, born,current, neighbours)

or even:

    do j
       do i
           call count_neighbours(i, j, neighbours, current)
           call compute_born(i, j, born,current, neighbours)

Which one is better might depend on the actual code used,
size of the fields, compiler optimisations applied, etc.
The first version will have to
store one row of `neighbours` and `current` in cache in
order to improve performance, while the second version
will immediately reuse individual values (and will
have slightly less loop overhead).

> Note that for training purpose we individually apply
> the loop fusion in this training material. For a real
> application you would have one script that would apply
> all required inlining automatically. 

The first version can be created by using the `LoopFuse`
transformation once. Looking at the schedule, the original
loops look like this (several lines of additional details
have been removed):

    GOInvokeSchedule[invoke='invoke_compute']
        0: Loop[type='outer', field_space='go_ct', it_space='go_internal_pts']
            Schedule[]
                0: Loop[type='inner', field_space='go_ct', it_space='go_internal_pts']
                    Schedule[]
                        0: CodedKern count_neighbours_code(neighbours,current) [module_inline=True]
        1: Loop[type='outer', field_space='go_ct', it_space='go_internal_pts']
            Schedule[]
                0: Loop[type='inner', field_space='go_ct', it_space='go_internal_pts']
                    Schedule[]
                        0: CodedKern compute_born_code(born,current,neighbours) [module_inline=True]

The outer schedule (GOInvokeSchedule) has two children for the
first two loops. So you need to provide these two children
to the fuse transformation to combine the loops:

    fuse = GOceanLoopFuseTrans()

    invoke = psy.invokes.get("invoke_compute")
    schedule = invoke.schedule

    # To see the schedule before it is transformed:
    # print(schedule.view())

    # Now merge the first two loops
    fuse.apply(schedule[0], schedule[1])

    # print(schedule.view())

This results in the following schedule:

    0: Loop[type='outer', field_space='go_ct', it_space='go_internal_pts']
        Schedule[]
            0: Loop[type='inner', field_space='go_ct', it_space='go_internal_pts']
                Schedule[]
                    0: CodedKern count_neighbours_code(neighbours,current) [module_inline=True]
            1: Loop[type='inner', field_space='go_ct', it_space='go_internal_pts']
                Schedule[]
                    0: CodedKern compute_born_code(born,current,neighbours) [module_inline=True]

The resulting Fortran code looks like this:

    DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
       DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
         CALL count_neighbours_code(i, j, neighbours%data, current%data)
       END DO
       DO i = born%internal%xstart, born%internal%xstop, 1
         CALL compute_born_code(i, j, born%data, current%data, neighbours%data)
       END DO
    END DO

Extend this script to merge the first three outer loops together,
and measure the performance. Notice that the provided script
`fuse_loops.py` already contains code to enable inlining of each
kernel.

> Note that PSyclone will check the iteration spaces of the loops
> to be merged, i.e. that the loop boundaries are identical among
> the fused loops.

## Loop Fusion of Inner Loops
The performance can be improved further by next inlining all the
inner loops as well. Looking at the fused schedule reveals:

    GOInvokeSchedule[invoke='invoke_compute']
        0: Loop[type='outer', field_space='go_ct', it_space='go_internal_pts']
            Schedule[]
                0: Loop[type='inner', field_space='go_ct', it_space='go_internal_pts']
                    Schedule[]
                        0: CodedKern count_neighbours_code(neighbours,current) [module_inline=True]
                1: Loop[type='inner', field_space='go_ct', it_space='go_internal_pts']
                    Schedule[]
                        0: CodedKern compute_born_code(born,current,neighbours) [module_inline=True]

In order to fuse the inner loops. you need to access the schedule
of the outer loop. This schedule can be accessed using the
`loop_body` attribute, e.g.:

    fuse.apply(schedule[0].loop_body[0], schedule[0].loop_body[1])

Adding this loop fusion to the script will generate the following output
(again shortened):

    0: Loop[type='outer', field_space='go_ct', it_space='go_internal_pts']
        Schedule[]
            0: Loop[type='inner', field_space='go_ct', it_space='go_internal_pts']
                Schedule[]
                    0: CodedKern count_neighbours_code(neighbours,current) [module_inline=True]
                    1: CodedKern compute_born_code(born,current,neighbours) [module_inline=True]

and the corresponding Fortran code:

    DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
       DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
         CALL count_neighbours_code(i, j, neighbours%data, current%data)
         CALL compute_born_code(i, j, born%data, current%data, neighbours%data)
       END DO
    END DO

Now extend this script to completely fuse all three nested loops.

## Why are not All 4 Loops Fused?
If you look at the first and last loop, you will notice that
the last loop writes the field `current`, and the first loop
has a stencil access to `current`. This means the code would
be incorrect if all four loops are fused. The first loop would
read `current(i-1,j)` and `current(i+1,j)` - the first of which
would have the updated value written in the previous loop iteration
in the fourth loop, but the latter one has not been updated.

> Note that at the moment PSyclone does detect that loop fusion
> would create incorrect code, but this test is too conservative (i.e)
> it will reject code that could be fused. This is work in progress.
> Also, the error message is by far not detailed enough.

## Optional Fusing Task 1
Instead of fusing the first three loops, it is also possible to
fuse the last three loops. Modify the `loop_fuse` script to
fuse the last three loops, and measure the performance.


## Full Inlining
Full-inlining, i.e. replacing the function call with the code
of the called function can be used by PSyclone as well.
It is out of scope of this tutorial, but the
solution directory contains a more sophisticated script. It does:

1. Fully inline all kernels.
2. Analyse the algorithm layer to identify and store fields that
   are not used outside of the algorithm layer. These fields are
   therefore not required to store any results, and are considered
   to be temporary variables.
3. It fully fuses the last three loops.
4. Using the list of temporary variables collected in step 2,
   it will identify which of these arrays can be replaced with
   scalar variables.

The output when running the script is:

    potential replacement born
    potential replacement die
    potential replacement neighbours
    REPLACE born r2d_field: DataTypeSymbol Argument(Access.READWRITE) UnresolvedType
    REPLACE die r2d_field: DataTypeSymbol Argument(Access.READWRITE) UnresolvedType
    IN DIFFERENT LOOPS neighbours

It identifies the three fields `born`, `die`, and `neighbours` as potential
temporary variables (the other field, `current`, is used outside of the
algorithm layer ... for printing out the results). Then it replaces `born`
and `die` with scalars. The field `neighbour` is in two different loops
(see [Why are not All 4 Loops Fused?](#why-are-not-all-4-loops-fused)),
and it can therefore not be replaced with a scalar.

Just run `make full_inline` in the solution directory (or `make test`,
which will run the fully inlined version last, so the created files will
be around to be inspected).

The main loops now look like this:

    do j = born%internal%ystart, born%internal%ystop, 1
      do i = born%internal%xstart, born%internal%xstop, 1
        born_scalar = 0.0
        if (current%data(i,j) == 0.0 .AND. neighbours%data(i,j) == 3.00000000000000) then
          born_scalar = 1.0
        end if
        die_scalar = 0.0
        if (current%data(i,j) > 0.0 .AND. (neighbours%data(i,j) > 3.0 .OR. neighbours%data(i,j) < 2.0)) then
          die_scalar = 1.0
        end if
        current%data(i,j) = current%data(i,j) - die_scalar + born_scalar
      enddo
    enddo

Notice how no writes to arrays are done anymore for `born` and `die`, which
have been replaced with new scalar variables. This can trigger
compiler warnings about unused variables (since the input parameters
`die` and `born` are indeed not used anymore).
