# Using PSyclone's PSyData Plugin Structure - Profiling

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
