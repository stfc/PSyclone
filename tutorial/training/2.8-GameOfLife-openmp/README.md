# OpenMP Parallelisation with PSyclone

PSyclone offers the ability to apply OpenMP parallelisation
to your code using transformations. It therefore requires
you to write a script to actually apply the transformations,
but you can create a very general script that will be able
to be used for different code bases. In this exercise though
we will focus on a custom script for the parallelisation of
Game of Life.

## Simple Parallel Version
As a first start we use a script to add a `omp parallel do`
directive. The transformation script follows the script
for loop fusion:

    from psyclone.transformations import OMPParallelLoopTrans
    omp_parallel = OMPParallelLoopTrans()

    invoke = psy.invokes.get("invoke_compute")
    schedule = invoke.schedule

    omp_parallel.apply(schedule[0])

You can see the inserted OMP parallel directive in the schedule:

    GOInvokeSchedule[invoke='invoke_compute']
    0: OMPParallelDoDirective[]
        Schedule[]
            0: Loop[type='outer', field_space='go_ct', it_space='go_internal_pts']
                Schedule[]
    ...

This will generate the following Fortran code:

    !$omp parallel do default(shared), private(i,j), schedule(static)
    DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
       DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
         CALL count_neighbours_code(i, j, neighbours%data, current%data)
       END DO
    END DO
    !$omp end parallel do

Now extend the script to apply parallel loop directive to all kernels.

You can run the parallel version using:

    OMP_NUM_THREADS=4 ./gol ./config.glider-large

Measure the performance.

## A More General Script to Apply Loop Parallelisation
Each loop object in the GOcean API has a `loop_type`, which is either
`inner` or `outer`. Using this, the script can be simplified by
using a `walk` over all `GOLoops`, and applying the OMP loop parallelisation
to any loop that is an outer loop. Rewrite the script to use this approach.

## Using Parallel Do for Better Performance
The performance of the parallel code is not very good. The reason for this
is that there is a runtime cost associated with starting the threads, and
each `OMP parallel do` statement will first create the threads, and then
destroy them. And with the kernels being very small, there just is too much
overhead to make the parallelisation efficient.

This can be improved by using separate `OMP do` statement for each loop,
and only one `OMP parallel` which spans all loops to be parallelised.
PSyclone offers the separate transformation to apply the `OMP parallel`
and `OMP do`. The `OMPParallelTrans` also accepts a list of nodes.
It doesn't matter in which order you apply the transformations, but
applying the `OMPParallelTrans` will change the tree - it inserts a
`OMPParallel` node at the top of the schedule, so if you want to access
the loops, they are now children of this `OMPParallelNode`:

    omp_parallel = OMPParallelTrans()
    omp_do = OMPLoopTrans()
    omp_parallel.apply(schedule[0:2])   # Apply to loops 0 and 1

Which results in:

    GOInvokeSchedule[invoke='invoke_compute']
        0: OMPParallelDirective[]
            Schedule[]
                0: Loop[type='outer', field_space='go_ct', it_space='go_internal_pts']
                1: Loop[type='outer', field_space='go_ct', it_space='go_internal_pts']
        1: Loop[type='outer', field_space='go_ct', it_space='go_internal_pts']

You can see that accessing the first two loops, which are now
surrounded by an `OMP Parallel`, are deeper in the tree.
So it might be easier to first apply the `OMPLoopTrans`, which adds
an `OMP do` node around each outer loop, and then applying the
outer `OMP parallel`.

Use the `OMPParallelTrans` and `OMPLoopTrans` transformations
to optimise the parallelisation of the first three loops by
adding a single outer `OMP parallel` section, followed by 
individual `OMP do` directives for each outer loop.

## Using Dynamic Schedule
The constructor for any OMP loop takes an optional argument
that specifies the schedule to be used. It defaults to a static
schedule. There are two ways to create a different schedule.
First you can specify the schedule when creating the instance
of the loop transformation:

    omp_do_dynamic = OMPLoopTrans(omp_schedule="dynamic")

By using this instance when applying the OMP loop transformation,
a `schedule(dynamic)` will be created. Alternatively, you can
also assign the schedule to the `omp_schedule` attribute:

    omp_do.omp_schedule = "dynamic"

Valid options for the schedule are: `runtime`, `static`,
`dynamic`, `guided`, and `auto`.

Use a different schedule in your parallelisation script.


## More Optimisations - Combine Loop Fusion and OpenMP
Obviously, we can expect an even better performance if we
have less loop, with kernels that do more work (since there
is less loop- and thread-synchronisation overhead). So we
want to combine the loop fusion code from the previous
loop fusion exercise with the OpenMP parallelisation code.

You could either copy the loop fusion code into `openmp.py`,
or you could import the transformation, and apply it.
Importing it has the advantage that you have less duplicated
code. But on the other hand you risk that changes to
`fuse_loops.py` might affect your `openmp.py` script
unexpectedly - while in many cases that is good (e.g.
improvements to `fuse_loops.py` will flow into other
scripts), but they could also break your `openmp.py` script
unexpectedly.

The important part of importing is to make sure to rename
the the transformation when importing, since otherwise all
transformation scripts will have the function `trans`.

    from fuse_loops import trans as fuse_trans

Then you can just call `fuse_trans(psy)` in your
parallelisation script. Be aware that after applying
the full fusion of the loops, there will be only
one loop left to be put into an OMP parallel region
(in which case there is no advantage of using two
separate transformations, a single one to apply `OMP parallel do`
will be sufficient).

## Task Parallelisation
OpenMP 3.0 introduced task parallelisation, which is a more
flexible implementation of the previous `OMP sections` directive.

As an optional task, we show how OMP task parallelism can be
used with PSyclone as well. The example code has limited task
parallelism, so it will not deliver a significant speed-up.

OMP task directives are typically nested in the following
directive structure:

Hmm - we only have taskloop???? Should we even bother???
