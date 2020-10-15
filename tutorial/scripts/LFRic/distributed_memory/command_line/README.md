[Presentation: Parallel strategy for LFRic -> domain decomposition,
iterates_over cells/dofs, discontinuous/continuous, stencils, halos,
halo exchanges or global sums]

In this section you will see how to how to create sequential or
distributed memory code for the LFRic API. You can then take a look at
what code is generated and understand why it is generated this way.

## 1: The algorithm example

Take a look at the algorithm code example to familiarise yourself with
the kernels that are called within the invoke.

../code/xxx.x90

One of the most computationally costly parts of the LFRic model.

## 2: Creating a sequential code PSy-layer

Run ...
> psyclone -nodm -oalg /dev/null -opsy psy.f90 -s ./schedule.py ../code/helmholtz_solver_alg_mod.x90

The default is to create parallel code so we add the -nodm flag to
request sequential code. We are not interested in looking at the
algorithm code so we just send that to /dev/null. We store the
generated psy layer in psy.f90 and use a script to take a look at the
PSyIR intermediate representation, which is output to the terminal.

The sequential implementation of the psy-layer creates loops of the
appropriate size and calls the kernels within those loops.

## 3: Creating a distributed memory parallel PSy-layer

Run ...
> psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py helmholtz_solver_alg_mod.x90

This is just the same command as before but without the -nodm flag

a) explicit and guarded halo exchanges
b) run-time flags
c) reductions
d) interface to lfric infrastructure



## Key points

* Distributed memory code is generated - no manual code writing is required.
* Distributed memory code generation can be switched on or off using a flag.
* The science code does not change, therefore science developers do not need to be concerned with parallelism issues.