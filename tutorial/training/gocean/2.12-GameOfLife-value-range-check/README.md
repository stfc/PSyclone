# Using PSyclone's PSyData Plugin Structure - Value Range Check

This example shows PSyclone's value range check functionality.
It utilises the PSyData interface.

## Value Range Checking
PSyclone comes with a PSyData library that can automatically check
that kernel parameters are within a user-specified range. For example,
variables (including fields) that represent a fraction, can be tested
to be between 0 and 1. In our "Game of Life" example, we can verify
that the number of neighbours should be between 0 and 8, and the fields
`die` and `born` should be between 0 and 1. Additionally, this transform
will also verify that the values are not infinity or NAN.

You need to apply the `ValueRangeCheck` transformation from
`psyclone.psyir.transformations`. It is important to apply this transformation
to the outer loop and not to the kernel itself - if you would apply it to the kernel
itself, any variable (including arrays) will get checked each time the kernel is called,
not just once before it is called. So instead of looping over all kernels
in a invoke, you can loop over all Loops and select loops that are an
outer loop:

    ...
    for loop in schedule.walk(GOLoop):
        # Only apply to the outer loop, PSyData will
        # get full arrays provided to check for NANs
        if loop.loop_type == "outer":
            # apply transformation to `loop`


A value range is specified using the environment variable PSY_VALUE_RANGE.
It contains a list of `;` separated `key=range` pairs. starting with
Valid key for a variable can be specified in three ways:

    psy_time_step_alg_mod.invoke_compute_r0.neighbours%data=0:8
    psy_time_step_alg_mod.neighbours%data=0:8
    neighbours%data=0:8

The first one will check the variables `neighbours` in the module
`psy_time_step_alg_mod` when calling the kernel `invoke_compute_r0`.
The second specification above will check the variable for any kernel call
in the module `psy_time_step_alg_mod`. The third way
finally will check the variable in any kernel (that is instrumented by the
value range check transformation). Besides upper and lower limit
as shown above, you can also use `0:` for a value of 0 and larger, and
`:8` for a value less than or equal 8.

Note that PSyclones adds unique region numbers (`-r0`) to the names to avoid
potential name clashes. If required, you can look up the names in the psy-layer
file `time_step_alg_mod_psy.f90` - the first call (to `PreStart`) contains
the module and region name:

    CALL value_range_check_psy_data % PreStart("psy_time_step_alg_mod", "invoke_compute-r0", 5, 3)

With the setting above, no messages will be printed, but you can
see the effect of the value checking by setting e.g. an upper limit
of 4: `PSYVERIFY__neighbours=0:4`.
Running the example will then produce the following output (in each
timestep):

    PSyData: Variable 'neighbours' has the value 5.0000000000000000 at index/indices 7 8 in module 'psy_time_step_alg_mod' region 'invoke_compute-r0', which is not between '0.0000000000000000' and '4.0000000000000000'.
    PSyData: Variable 'neighbours' has the value 5.0000000000000000 at index/indices 7 8 in module 'psy_time_step_alg_mod' region 'invoke_compute-r1', which is not between '0.0000000000000000' and '4.0000000000000000'.
    PSyData: Variable 'neighbours' has the value 5.0000000000000000 at index/indices 7 8 in module 'psy_time_step_alg_mod' region 'invoke_compute-r2', which is not between '0.0000000000000000' and '4.0000000000000000'.

In order to use the NAN testing that is part of the value range check, you need
to modify the source code to produce an infinity or NAN result. This directory
contains a modified version of `compute_born`, which will use:

   born(i, j) = 1.0 / (i-j)

This will trigger a division by 0 for all elements on the diagonal. It will only
be used by `Makefile.value_range_check`!

Running this version will now produce the following errors at runtime:

    PSyData: Variable 'current' has the invalid value 'Inf' at index/indices 4 4 in module 'psy_time_step_alg_mod' region 'invoke_compute-r0'.
    PSyData: Variable 'neighbours' has the invalid value 'Inf' at index/indices 3 3 in module 'psy_time_step_alg_mod' region 'invoke_compute-r0'.


It indicates which invoke and kernel caused the error, what the
invalid value was (`Inf`), the indices at which the error
occurred (`4 4` etc). 


You will notice that there are now a lot more calls inserted by
the value range check transformation compared with the profile transformation.
This is because the API specifies a declaration of all variables, then
provide the values of variables before and after the
actual kernel call. You can find additional details in the
'PSyclone Developer Guide', which is especially useful if you
want to implement your own runtime library.
