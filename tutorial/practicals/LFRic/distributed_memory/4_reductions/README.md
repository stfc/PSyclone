# PSyclone and LFRic distributed-memory tutorial: reductions #

In this section of the tutorial you will see how PSyclone supports
reductions when generating distributed memory code.

## The example ##

We will again use the same helmholtz code that we used in the previous
tutorial sections, but in this case we will add a new builtin that performs a
reduction to the algorithm layer.

## Builtins performing reductions ##

As you have already learnt in a previous section of the tutorial,
PSyclone provides a number of builtin kernels. Some of these kernels
perform reductions. The builtins are specified in the PSyclone user
documentation
[here](https://psyclone.readthedocs.io/en/latest/dynamo0p3.html#built-ins). If
you prefer, a pdf of the user guide is also available in
`<psyclone_home>` called `psyclone.pdf`.

Choose one of the builtins that performs a reduction. These are either
of the two inner products or the sum of elements builtins.

These reductions take one or two fields as arguments and return a
scalar value.

You now need to add the chosen builtin to the existing code. For
convenience a copy of the algorithm file has been provided in the same
directory as this `README.md` file.

Edit this copy and add in your chosen builtin. You can use the
existing fields as arguments but will need to create a new scalar
return value. If you are unsure how to do this then take a look at the
declaration of the timestep_term argument as an example.

```fortran
    ...
    real(kind=r_def)             :: scalar
    ...
    call invoke( setval_c(grad_p, 0.0_r_def),                                &
                 scaled_matrix_vector_kernel_type(grad_p, p, div_star,       &
                                                  hb_inv),                   &
                 enforce_bc_kernel_type( grad_p ),                           &
                 apply_variable_hx_kernel_type(                              &
                       Hp, grad_p, mt_lumped_inv, p,                         &
                       compound_div, p3theta, ptheta2, m3_exner_star,        &
                       tau_t, timestep_term),                                &
		 x_innerproduct_x(scalar, p))
```

## Run and inspect

Running the code is the same as usual except that PSyclone will not
be able to find the kernel files. Try running this and see what
happens:

```bash
    $ psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py helmholtz_solver_alg_mod.x90
```

You can specify a directory in which to look for kernel files on the
command line. Try the following which makes use of the `-d` option:

```bash
    $ psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py -d ../code helmholtz_solver_alg_mod.x90
```

In the PSyIR you should see a global sum node appearing after the loop
containing the builtin that you added.

Take a look at the generated code. You should see the loop summing
into a scalar and then that scalar being added to a global_sum
object. The global_sum object then calls its own `get_sum()` function
and returns the global sum. The global sum implementation (in MPI) is
hidden in the `get_sum()` function and is provided by the LFRic
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
