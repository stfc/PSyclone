# PSyclone and LFRic distributed-memory tutorial: parallel code #

In this example you will see how to how to switch from generating
sequential code to parallel distributed-memory code using the LFRic
API. You can then take a look at the generated code and understand why
it is generated this way.

## Assumptions ##

This part of the tutorial assumes you have already gone through the
previous tutorials, or that you are already familiar with the PSyKAl
separation of concerns, namely the algorithm, PSy and kernel layers
and their associated rules and structure, that you are aware of
PSyclone and what it does, that you have been introduced to builtins,
function spaces and iterating over dofs and cells and that you are
aware of the LFRic infrastructure and how it is called by users and by
PSyclone.

It is also assumed that you have followed the [installation
instructions](../../../README.md).

## The example ##

Use your favourite editor to take a look at the algorithm code example
to familiarise yourself with the kernels that are called from the
algorithm layer using an invoke. The path below is relative to the
directory in which this `README.md` file resides as are all subsequent
paths in this document.

```bash
    $ my_fav_editor ../code/helmholtz_solver_alg_mod.x90
```

This example is extracted from the LFRic model and is one of the most
computationally costly sections of the LFRic dynamical core. Note
that all of the executable code has been removed apart from the invoke
call that we are interested in.

## Creating a sequential PSy-layer ##

From this directory run:

```bash
    $ psyclone -nodm -oalg /dev/null -opsy psy.f90 -s ./schedule.py ../code/helmholtz_solver_alg_mod.x90
```

Notice the schedule.py script requires a path (`./` in this case). The
script will not be found by PSyclone if a path is not supplied.

The default behaviour of PSyclone is to create parallel code so we
need to add the `-nodm` flag to request sequential code. We are not
interested in looking at the algorithm code so we just send that to
`/dev/null`. We store the generated psy layer in a file called
`psy.f90` and use a script `schedule.py` to take a look at the PSyIR
(PSyclone Internal Representation), which the script outputs to the
terminal.

If the terminal output is not coloured you might like to install
termcolor (`pip install termcolor` if using a virtualenv or take a
look at the [installation instructions](../../../README.md#Requirements))
and re-run.

From inspection of the PSyIR you will see that the kernels are called
in the same order as specified in the invoke call. A loop has also
been created for each of the kernel calls and kernel calls that are
builtins are distinguished from kernel calls that are coded
kernels. The first of these loops is for a builtin and iterates over
`dofs`, with an upper loop bound of `ndofs`. The next three loops
contain coded kernels and they iterate over `cells` (an empty `type`
in the description means `cells`!) with an upper loop bound of
`ncells`.

```bash
InvokeSchedule[invoke='invoke_0', dm=False]
     0: Loop[type='dofs', field_space='any_space_1', it_space='dof', upper_bound='ndofs']
         Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
         Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
         Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
         Schedule[]
             0: BuiltIn setval_c(grad_p,0.0_r_def)
     1: Loop[type='', field_space='any_space_1', it_space='cell_column', upper_bound='ncells']
         Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
         Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
         Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
         Schedule[]
             0: CodedKern scaled_matrix_vector_code(grad_p,p,div_star,hb_inv) [module_inline=False]
     ...
```

## View the generated sequential PSy-layer code ##

Take a look at the generated psy-layer Fortran code:

```bash
    $ my_fav_editor psy.f90
```

As you will see, there is quite a bit of lookup code generated which
extracts the appropriate values from the infrastructure and the data
objects passed from the algorithm layer, however, the code performing
the looping (after the `Call our kernels` comment) is relatively short
and concise.

You should see that the upper bound for the builtin kernel loop is the
total number of dofs for the `grad_p` field that is being assigned to
(find out where `undf_aspc1_grad_p` is declared in order to see the
lookup using `grad_p_proxy`).

```fortran
      ...
      undf_aspc1_grad_p = grad_p_proxy%vspace%get_undf()
      ...
      DO df=1,undf_aspc1_grad_p
        grad_p_proxy%data(df) = 0.0_r_def
      END DO
      ...
```

You should also see that the upper bound for the subsequent 3 loops
uses one of the fields accessed in the loop to lookup the number of
cells to iterate over. In this case the value returned is the same
irrespective of the field, so a single lookup would also work and the
PSyclone code generation could be changed to do this at some point.

```fortran
      ...
      DO cell=1,grad_p_proxy%vspace%get_ncell()
      !
          CALL scaled_matrix_vector_code(nlayers, grad_p_proxy%data, p_proxy%data, div_star_proxy%data, hb_inv_proxy%data, ndf_aspc1_grad_p, undf_aspc1_grad_p, map_aspc1_grad_p(:,cell), ndf_aspc2_p, undf_aspc2_p, map_aspc2_p(:,cell), ndf_w3, undf_w3, map_w3(:,cell))
      END DO
      ...
```

## Creating a distributed-memory parallel PSy-layer ##

Run the same command as before but with the `-nodm` option removed.

```bash
    $ psyclone -oalg /dev/null -opsy psy.f90 -s ./schedule.py ../code/helmholtz_solver_alg_mod.x90
```

The schedule should look the same as before except that there are now
halo exchange calls and the upper bounds of two of the loops have
changed (to `cell_halo(1)`).

Also notice that one of the halo exchange calls has `check_dirty=False`
whilst the rest have `check_dirty=True`.

```bash
InvokeSchedule[invoke='invoke_0', dm=True]
    0: Loop[type='dofs', field_space='any_space_1', it_space='dof', upper_bound='ndofs']
        Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
        Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
        Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
        Schedule[]
            0: BuiltIn setval_c(grad_p,0.0_r_def)
    1: HaloExchange[field='grad_p', type='region', depth=1, check_dirty=False]
    2: HaloExchange[field='p', type='region', depth=1, check_dirty=True]
    3: HaloExchange[field='div_star', type='region', depth=1, check_dirty=True]
    4: HaloExchange[field='hb_inv', type='region', depth=1, check_dirty=True]
    5: Loop[type='', field_space='any_space_1', it_space='cell_column', upper_bound='cell_halo(1)']
        Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
        Literal[value:'NOT_INITIALISED', Scalar<INTEGER, UNDEFINED>]
        Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
        Schedule[]
            0: CodedKern scaled_matrix_vector_code(grad_p,p,div_star,hb_inv) [module_inline=False]
    ...
```

## View the generated distributed memory PSy-layer code ##

Take a look at the generated psy-layer Fortran code:

```bash
    $ my_fav_editor psy.f90
```

As before, the code performing the looping (after the `Call kernels
and communication routines` comment) is relatively short and
concise. You should see the halo exchange calls placed at the
locations shown in the PSyIR of the psy-layer. Notice that the halo
exchanges which had `check_dirty=True` are enclosed within an if
condition whereas the halo exchange which had `check_dirty=False` does
not. Also notice the `set_dirty()` calls that appear after some of the
loops.

```fortran
      ! Call kernels and communication routines
      !
      DO df=1,grad_p_proxy%vspace%get_last_dof_owned()
        grad_p_proxy%data(df) = 0.0_r_def
      END DO
      !
      ! Set halos dirty/clean for fields modified in the above loop
      !
      CALL grad_p_proxy%set_dirty()
      !
      CALL grad_p_proxy%halo_exchange(depth=1)
      !
      IF (p_proxy%is_dirty(depth=1)) THEN
        CALL p_proxy%halo_exchange(depth=1)
      END IF
      ...
```

The distributed memory model, placement of halo exchanges and
`set_dirty()` calls will be introduced next.

## Key points ##

* The sequential or distributed-memory parallel code is generated - no
  manual code writing is required.
  
* Switching between sequential and distributed-memory parallel code
  generation is done with a single flag.

* The science code (algorithm and kernel code) does not change,
  therefore science developers do not need to be concerned with
  issues related to parallelism.

## Congratulations ##

You have finished this part of the tutorial.
