In this example you will add a builtin that performs a reduction to
the example and see what PSyclone generates when using distributed
memory.

## 1: The example

We will again use the same helmholtz code that we used in the previous
two three, but in this case we will add a new builtin that performs a
reduction to the algorithm layer.

## 2: builtins performing reductions

As you have already learnt in a previous tutorial, PSyclone provides a
number of builtin kernels. Some of these perform reductions. The
builtins are specified in the PSyclone user documentation here
https://psyclone.readthedocs.io/en/latest/dynamo0p3.html#built-ins

If you prefer, a pdf of the user guide is also available in
<psyclone_home> called psyclone.pdf

Choose one of the builtins that performs a reduction. These are either
of the two inner products or the sum of elements builtins.

These reductions take one or two fields as arguments and return a scalar value.

You now need to add this builtin to the existing code. For convenience
a copy of the algorithm file has been provided in this directory.

Edit this file and add in your chosen builtin. You can use the
existing fields as arguments but will need to create a new scalar
return value. If you are unsure how to do this then take a look at the
declaration of the timestep_term argument as an example.

## 3: run and inspect

Running of the code is the same as usual except that PSyclone will not be able to find the kernel files. Try running this and see what happens:

> psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py helmholtz_solver_alg_mod.x90

You can specify a directory in which to look for kernel files on the command line. Try the following which makes use of the -d option:

> psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py -d ../code helmholtz_solver_alg_mod.x90

In the PSyIR you should see a global sum node appearing after the loop containing the builtin that you added.

Take a look at the generated code. You should see the loop summing
into a scalar then that scalar being added to a global_sum object and
that global_sum object calling its own get_sum() function and
returning the global sum. The global sum implementation (in MPI) is
hidden in the get_sum() call and is provided by the LFRic
infrastruture.

## Key points

* PSyclone supports distributed memory global sums.

* The science code (algorithm and kernel code) does not change,
  therefore science developers do not need to be concerned with
  parallelism issues.

* The resultant code is guaranteed to be correct (if the rules being
  followed are correct!). This helps the HPC expert.

## Congratulations

You have finished this section of the tutorial.
