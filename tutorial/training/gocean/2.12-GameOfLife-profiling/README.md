# Using PSyclone's PSyData Plugin Structure

In this session we will test some of the plugin infrastructure
that PSyclone provides. There are two components that play together:
first there is a transformation that needs to be applied. Then the
application must be linked with the runtime library corresponding
to the applied transformation. We will use three different runtime
libraries as examples.

Each runtime library requires to call a initialisation and shutdown
function, which needs to be added manually to the main program. Here
the modifications that have been applied to `gol.f90`:

    USE profile_psy_data_mod, only: profile_PSyDataInit, &
                                    profile_PSyDataShutdown

    call profile_PSyDataInit()
    ! Read in the initial condition into the field 'current',
    ! and initialise dl_esm_inf.
    call read_config(grid, initial, time_steps) 

    call time_step(grid, initial, time_steps)
    call profile_PSyDataShutdown()


## Profiling Kernels
Profiling is a very frequent task, and as such offers a command
line option to simplify its application. You can use
`--profile kernels` to add a profiling region around each individual
kernel call (even if several kernels are contained in one invoke).
Using `--profile invokes` will add a profiling region around
each invoke only (which might contain several kernel calls).

> Note that applying custom transformation scripts might break the
> command line options, since it could result that the profiling
> regions are applied to an invalid location, e.g. inside a
> `omp parallel do` directive.

Modify the Makefile to apply each one of the two command line
options one after another. See the differences in the code being
added to the psy-layer `time_step_alg_mod_psy.f90`
Look at the calls added in each case.

Then run the executable with the larger glider configuration.
The output when profiling kernels looks like this:

     ===========================================
    module::region                                                        count       sum                 min                 average             max
    psy_time_step_alg_mod::invoke_compute:count_neighbours_code:r0        2000        14.2656250          0.00000000          7.13281240E-03      3.12500000E-02
    psy_time_step_alg_mod::invoke_compute:compute_born_code:r1            2000        11.9218750          0.00000000          5.96093759E-03      3.12500000E-02
    psy_time_step_alg_mod::invoke_compute:compute_die_code:r2             2000        10.0937500          0.00000000          5.04687522E-03      3.12500000E-02
    psy_time_step_alg_mod::invoke_combine:combine_code:r3                 2000        12.0937500          0.00000000          6.04687491E-03      3.12500000E-02
     ===========================================

Profiling invokes in this case give less useful information:

     ===========================================
     module::region                                               count    sum                 min                 average             max
     psy_time_step_alg_mod::invoke_compute:r0                     2000     36.8906250          1.56250000E-02      1.84453130E-02      4.68750000E-02
     psy_time_step_alg_mod::invoke_combine:combine_code:r1        2000     11.2187500          0.00000000          5.60937496E-03      3.12500000E-02
     ===========================================

The current setup uses a small, stand-alone timer library included
with PSyclone, which does not require an external profiling library
to be added. PSyclone provides interfaces to `DrHook`, the 
NVIDIA Tools Extension library (NVTX),
the LFRic-timer (the timer functionality included in LFRic), the
Tuning and Analysis Utilities `Tau` (https://www.cs.uoregon.edu/research/tau/home.php),
and `dl_timer` (https://bitbucket.org/apeg/dl_timer). If you have any
of these libraries available, you only need to modify the compiler
options to point to the right PSyclone wrapper library, and in the
linking step add the wrapper library and your profiling library.
Additional interfaces to other profiling tools can easily be
implemented.

> Many profiling tools offer the option to instrument your source
> code directly. The disadvantage of this feature is that with the
> way kernels are implemented (as functions that operate on a single
> element), profiling overhead would be added to each individual
> element, which can prevent vectorisation, and certainly adds
> significant overhead, which can impact the precision of the
> results.

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


A value range is specified using environment variables starting with
PSYVERIFY. Valid ranges for a variable can be specified in three ways:


    PSYVERIFY__psy_time_step_alg_mod__invoke_compute_r0__neighbours=0:8
    PSYVERIFY__psy_time_step_alg_mod__neighbours=0:8
    PSYVERIFY__neighbours=0:8

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

When setting the environment variables, any `-` must be replaced with
an underscore (`_`).

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

## Kernel Data Extraction
Kernel data extraction describes a very useful and new ability
of PSyclone. Applying the kernel extraction transformation will
insert PSyData calls around kernels or invokes. The runtime library
uses this to create a NetCDF file at runtime, which stores all
input- and output-parameters of the included kernel calls. This
transformation can also create a stand-alone driver - a program
which will read the written NetCDF file, executes the kernel(s),
and then compares the result. This allows you to automatically
develop unit tests, or to optimise the implementation of a single
kernel, without having to rerun the full application every time.

The transformation `GOceanExtractTrans` is used to add the data
extraction code to a region of code and to create the driver.
It takes two optional parameters that can be specified in the
options argument - if a driver should be created (default is
False), and a tuple containing a module and region name. While
PSyclone will create names for you, they are often not intuitive,
so you have an option to specify a module and region name to use.

The following line enables the creation of a driver program, and also
defines the module- and region-name:

    ... apply(..., options={"create_driver": True,
                            "region_name": ("timestep", "myinvoke")})
                            })

Typically you might want to extract more than one kernel at a time
(I often use it for any kernel in an application, e.g. for LFRic's
gungho application over 1500 kernels are created, though only around
700 are executed when running the example configuration), so naming
all kernels becomes impossible. PSyclone will add region and invoke
numbers if required to make sure unique names are created. Initially,
do not specify a name to get used of the automatic PSyclone naming
scheme. 

In the PSyclone script `extraction.py`, add the required parameter
in the options dictionary when calling `apply`. The code can be
compiled by using `Makefile.extraction` (`make -f Makefile.extraction`).
After compilation, run the binary `gol.extraction ../gol-lib/config.glider`.

You should now see that a new file `psy_time_step_alg_mod-invoke_compute-r0.nc`
has been created. The added `r0` is a region number added automatically by
PSyclone (in case that you instrument more than one kernel).

You can use 
`ncdump psy_time_step_alg_mod-invoke_compute-r0.nc | less`
to look at the content of this file. You can see that the dimensions
of each field are specified, as well as the data of each field.
Fields that input- and output-parameters will actually have
two entries in that file. An example is the field `current`: The
first entry is called `current`, the other one `current_post`. This field
is an input-parameter, because it contains the initial state of the 
grid, and it is an output-parameter, since its status is being updated
at the end. Some parameters are (maybe unexpected) output parameters:
`die` and `born` for example. While technically they are only temporary
work arrays, they are written first (so they are not input parameters),
and any written variable is considered an output, and so will be stored
in the file. As they do not need to have input values, only the
output values are stored in the fields `die_post` and `born_post`.

The output values of any variable (i.e. the ones with a `post` attached
to the names) will be used to verify the computation of the kernel.

The driver program is called `driver-psy_time_step_alg_mod-invoke_compute-r0.F90`
(reflecting the name of the created NetCDF file)
and you can have a look at the code created by PSyclone. After
a sequence of reading in the data from the file, you will see:

    ...
    do j = current_internal_ystart, current_internal_ystop, 1
      do i = current_internal_xstart, current_internal_xstop, 1
        call combine_code(i, j, current, die, born)
      enddo
    enddo
    call compare_init(6)
    call compare('born', born, born_post)
    call compare('current', current, current_post)
    call compare('die', die, die_post)
    call compare('i', i, i_post)
    call compare('j', j, j_post)
    call compare('neighbours', neighbours, neighbours_post)
    call compare_summary()


The nested loops are the psy-layer code that calls the kernel. Notice
that the driver is not using any field data types, it is using plain
Fortran arrays and non-structured scalar types, so it can be used
stand-alone without dependencies to a infrastructure library. The kernel
call is followed by a sequence of correctness tests as shown above. For
now all results must be bitwise identical.

You can compile and link the driver (which needs the original kernel
object files) using `make -f Makefile.extraction driver`. If you
execute the binary, it should print:

    current correct
    i correct
    j correct

While it might be surprising that it also checks the loop variables
`i` and `j`, from PSyclones point of view these are variables
that are modified by the code (which includes the loops), and as such
need to be tested.

Note that the extraction transformation can be applied to a list of
kernels at the same time (as long as they are consecutive), and the
driver will call all kernels. Result checking though will only happen
after all kernels have been called, not after each individual kernel.

There are two additional restrictions:
1) If a kernel is called multiple times, the NetCDF file will be
   overwritten each time the kernel is called, only the data of the
   last run will be preserved. We are looking at supporting to only
   store the data for a specific invocation (e.g. say only store the
   file after the 10th call).

2) Distributed memory (MPI) is not yet supported. Each process will
   write to the same file. We will be supporting distributed memory
   to some degree: each file will get a process rank attached, meaning
   that you can test a kernel for each rank.
