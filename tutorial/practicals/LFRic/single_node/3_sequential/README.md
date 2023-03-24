# PSyclone and LFRic single node tutorial: sequential optimisations #

In this section of the tutorial you will see some examples of how to
modify the psy-layer and kernel code in order to (potentially) improve
the sequential performance of the code.

## Loop fusion ##

Loop fusion can increase the performance of code by improving the
memory access patterns.

The example we have been using so far can not make use of loop fusion
so we will use a different (made up) example called
`example_alg.f90`. It only contains builtins so there is no need for
kernel files.

Take a look at the `loop_fuse.py` script. This script starts at the
first entry in the schedule and tries to loop fuse this with the next
entry in the schedule as many times as possible. This has the effect
of loop fusing as many loops as possible that follow the first
one. This script is clearly specialised for this particular example.

Lets try running it:

```bash
    $ psyclone -s ./loop_fuse.py example_alg.x90 -oalg /dev/null -opsy psy.f90
```

You should see that the loops have not been fused and there should be
an error output by the transformation. This is because the builtins
can operate on fields on any function space and fields on different
function spaces can have different numbers of dofs. Therefore, as
PSyclone does not know whether the loop bounds of the loops are the
same it refuses to fuse them.

Let's assume that we know that the fields are all on the same function
space even though PSyclone does not. In this case we can tell the
PSyclone transformation that it is safe to fuse. Add `same_space=True`
as an argument when the transformation is created:

```python
    ftrans = DynamoLoopFuseTrans(same_space=True)
```

Rerun PSyclone. You should now see that all the loops have been fused
together so that the resulting schedule contains just one loop.

## Kernel constants ##

We're now going back to the usual helmholtz example. There are various
sizes that are passed to kernels as they are configurable at
run-time. For example, the number of levels. If the compiler is given
these sizes at compile time then it could potentially generate better
code. Of course this means that the kernels are specialised for these
particular values but codes are often run many times with the same
configuration (same resolution, etc).

Run psyclone with the kernel_constants.py script:

```bash
    $ psyclone -s ./kernel_constants.py ../code/helmholtz_solver_alg_mod.x90 -oalg /dev/null -opsy psy.f90
```

On the screen you should see a summary of kernel arguments that have
been changed.

The transformation is able to provide constant values for the number
of layers, the order of the problem (which determines how many dofs
per cell there are) and whether the number of quadrature points should
be constant or not.

The resultant modified kernel code is output in the same directory as
this file by default e.g. `apply_variable_hx_kernel_0_mod.f90`.

If you take a look at the modified code you should see that
e.g. `nlayers` is now an integer parameter set to `20`.

Feel free to play with the values in the `kernel_constants.py` script
and check whether the values change in the modified kernel code as
expected. Remember that the modified kernel code will not overwrite
any existing kernel code by default and will be written to a file with
a different index value.

## Fortran intrinsics ##

In certain cases it might be useful to replace Fortran intrinsic
functions in kernels with inline code. In this case we are going to
replace a matrix-multiply call with inline matrix-vector code.

First take a look at the file
`../code/scaled_matrix_vector_kernel_mod.F90`. You should see a
matrix-multiply contained within it.

```fortran
    lhs_e = matmul(matrix(:,:,ik),x_e)
```

Run psyclone with the `matvec_opt.py` script

```bash
    $ psyclone -s ./matvec_opt.py ../code/helmholtz_solver_alg_mod.x90 -oalg /dev/null -opsy psy.f90
```

Now take a look at the transformed
`scaled_matrix_vector_kernel_[NUM]_mod.f90` file within this directory,
where the relevant file should be the one with the largest value of
`[NUM]` (as that is the latest transformed file). You should see that
the `matmul` has been replaced within this file.

If you noticed that the new loop indices `i` and `j` are not
explicitly declared, well done. This is a bug in the transformation.

## Mixing kernel transformations ##

As a last exercise You might like to join the two kernel
transformations together e.g. replace the MATMUL and supply a
constant value for `nlayers`.

## Finished ##

Well done, you have finished the LFRic sequential optimisations part
of the tutorial.
