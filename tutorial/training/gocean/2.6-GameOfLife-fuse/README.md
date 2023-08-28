# Code Optimisations with PSyclone

The measurement at the end of the previous training module showed
that there can be a significant performance loss with the initial
version of the PSyclone Game of Life. In this module we will
apply some transformations to improve the performance of the
code.

## Inlining
As is obvious from the code being able to inline the kernel calls
is essentials for good performance. If a call needs to be executed
for the computations for each cell, the compiler cannot use
vectorisation, and the call overhead itself is likely higher than
the time required for the actual computations.

The Intel compiler is able (given the right compiler options) to
inline code that is contained in a different program unit. It is
able to inline the kernels contained in the various kernel files
into the psy-layer file, and can then vectorise the loop.

The GNU Fortran compiler on the other hand is not able to inline
code from a different file, but it can inline successfully if the
code is contained in the same file.

At this stage PSyclone does not yet have full inlining capabilities
(as in putting the whole kernel code inside the nested loops
in the psy-layer). But it can move the code from the kernel files
into the psy-layer files, which in turn will enable GNU Fortran
to inline the kernel calls.

In order to do this, the PSyclone `inline` transformation has to
be applied to each kernel that should be inlined. This
requires a script which PSyclone will call after the psy-layer
has been created, but before the Fortran code is produced.

The following example script shows a script that will inline the
first kernel. It imports the inline transformation
`KernelModuleInlineTrans` to be used on the kernels that should
be inlined. It then gets the psy-layer representation of the
`invoke_compute` invoke statement. Have a look at `time_step_alg_mod.x90`
to see the name 'compute' used in the one invoke statement.
It then takes the first kernel in this schedule (`schedule[0]`) and
applies the inline tranformation.


    from psyclone.transformations import KernelModuleInlineTrans
    from psyclone.gocean1p0 import GOKern

    def trans(psy):
        '''
        Take the supplied psy object, and fuse the first two loops
    
        :param psy: the PSy layer to transform.
        :type psy: :py:class:`psyclone.psyGen.PSy`
    
        :returns: the transformed PSy object.
        :rtype: :py:class:`psyclone.psyGen.PSy`
    
        '''
        inline = KernelModuleInlineTrans()
    
        invoke = psy.invokes.get("invoke_compute")
        schedule = invoke.schedule
        # Take the first kernel
        kern = schedule[0]    
        inline.apply(kern)
    
        return psy

If you now invoke PSyclone with the additional parameter
`-s inline.py`, the script will be invoked by PSyclone,
and inlining for the first kernel will be enabled.

> Note that you have to explicitly specify the path to
> the script (or modify your PYTHONPATH). E.g. you
> need to specify `./inline.py`, not just `-s inline.py`.

Look at the psy-layer output file. While the nested loops
and kernel call inside these loops have not changed, the
difference is that the kernel called is now contained in
the same file. This will help the compilers, especially
the GNU Fortran compiler, with inlining, which can make
a significant performance difference.

Measure the performance.

## Inlining All
Obviously, just inlining one kernel is not enough, all
kernels should be inlined. Modify the inline script to
find all invokes and all kernels in each invoke, and
apply the inline transformation to
them. As shown in the presentation, you can use
`psy.invokes.invoke_list` to get a list of all `invoke`
objects, and then use `invoke.schedule.walk(GOKern)` to
get an iterator over all kernels in a schedule.

What is performance with the GNU Fortran compiler after
inlining all kernels?

## Loop Fusion of Outer Loops
The original design of splitting the code into several
small kernels is typically not ideal from a performance
point of view, since hardly any data is being reused and
data needs to be loaded from memory, instead from cache.
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
           call count_neighbours(neighbours, current)
       enddo
    enddo
    do j
       do i
           call compute_born(born,current, neighbours)

Counting the neighbours will mean that the values of `current`
and `neighbours` will be in cache, but (for larger fields)
the cache will not be big enough to store the whole arrays, and
by the time `compute_born` is executed, the data needs to be
reloaded from memory. The performance could be improved
by combining the loops:

    do j
       do i
           call count_neighbours(neighbours, current)
       enddo
       do i
           call compute_born(born,current, neighbours)

or even:

    do j
       do i
           call count_neighbours(neighbours, current)
           call compute_born(born,current, neighbours)

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

> Note that at this stage PSyclone does not automatically detect
> that loop fusion creates incorrect code. This is work in progress,
> and we expect that PSyclone will be able to raise an exception
> in case of invalid loop fusions in the near future. 

## Optional Fusing Task 1
Instead of fusing the first three loops, it is also possible to
fuse the last three loops. Modify the `loop_fuse` script to
fuse the last three loops, and measure the performance.
