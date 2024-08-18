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
the LFRic-timer (the timer functionality included in LFRic), and
`dl_timer` (https://bitbucket.org/apeg/dl_timer). If you have any
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

## NaN Checking
PSyclone comes with a PSyData library that automatically checks
all input- and output-parameters of a kernel to make sure they
are valid numbers, i.e. not infinity, or NAN.

In order to have a real use case, you need to modify for example
the `compute_born` function to compute an invalid result. You can use
the following:

    born(i, j) = 1.0 / (i-j)

This will trigger a division by 0 for all elements on the diagonal.

In order to use the NAN testing, you need to apply the
`NanTestTrans` transformation, which comes from
`psyclone.psyir.transformations`, to all kernels. You can use the
script developed for inlining in section 2.6 as a template for
this script.

Executing this code after compilation and linking with the PSyclone
NAN-test library, will result in the following output:

    PSyData: Variable born has the invalid value
                  Infinity  at index/indices            5           5
    psy_time_step_alg_modinvoke_compute:compute_born_code:r1
    ...

It indicates which invoke and kernel caused the error, what the
invalid value was (`Infinity`), the indices at which the error
occurred (`5 5`). Note that PSyclones adds unique region numbers (`:r1`)
to the names to avoid potential name clashes. If required, you
can look up the names in the psy-layer file
`time_step_alg_mod_psy.f90` - the first call (to `PreStart` contains
the module and region name:

    CALL nan_test_psy_data_1%PreStart("psy_time_step_alg_mod", "invoke_compute:compute_born_code:r1", 2, 1)

You will notice that there are now a lot more calls inserted by
the Nan-test transformation compared with the profile transformation.
This is because the API specifies a declaration of all variables
phase, then provide the values of variables before and after the
actual kernel call. You can find additional details in the
'PSyclone Developer Guide', which is especially useful if you
want to implement your own runtime library.

## Kernel Data Extraction
Kernel data extraction describes a very useful and new ability
of PSyclone. Applying the kernel extraction transformation will
insert PSyData calls around kernels. The runtime library
uses this to create a NetCDF file at runtime, which stores all
input- and output-parameters of the included kernel calls. This
transformation can also create a stand-alone driver - a program
which will read the written NetCDF file, executes the kernel,
and then compares the result. This allows you to automatically
develop unit tests, or to optimise the implementation of a single
kernel, without having to rerun the full application every time.

> Note that at this time LFRic does not yet fully support the
> creation of a driver.

The transformation `GOceanExtractTrans` is used to add the data
extraction code to a region of code and to create the driver.
It takes two optional parameters that can be specified in the
options argument - if a driver should be created (default is
False), and a tuple containing a module and region name. While
PSyclone will create names for you, they are often not intuitive,
so it is recommended to explicitly specify names (assuming of
course that transformation is not applied to a large number of
kernels at the same time). The options for the `apply` method
can be specified as follows:

    options={"create_driver": True,
             "region_name": ("timestep_mod", "combine")})

Indicating that this is part of the timestep module, and
is for the `combine` kernel.

Create an instance of the `GOceanExtractTrans` transformation
and apply it to the combine kernel in a PSyclone transformation
script `extraction.py`. The code can be compiled by using
`Makefile.extraction` (`make -f Makefile.extraction`).
After compilation, run the binary `gol.extraction config.glider`.

You should now see that a new file `timestep-combine.nc` has
been created. You can use `ncdump timestep-combine.nc | less`
to look at the content of this file. You can see that the dimensions
of each field are specified, as well as the data of each field.
For `current` there are actually two entries in the file: one
called `current`, the other one `current_post`. This field
is an input- and output-parameter of the combine function, and
as such both the input value are stored (to provide the input
parameter to the kernel), as well as the output value after the
kernel call (`current_post`), which is used to verify the results.

The driver program is called `driver-timestep-combine.f90`,
and you can have a look at the code created by PSyclone. After
a sequence of reading in the data from the file, you will see:

    do j = current_internal_ystart, current_internal_ystop, 1
      do i = current_internal_xstart, current_internal_xstop, 1
        call combine_code(i, j, current, die, born)
      enddo
    enddo
    if (ALL(current - current_post == 0.0)) then
      PRINT *, "current correct"
    else
      PRINT *, "current incorrect. Values are:"
      PRINT *, current
      PRINT *, "current values should be:"
      PRINT *, current_post
    end if

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
after each kernels have ben called, not after each individual kernel.

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
