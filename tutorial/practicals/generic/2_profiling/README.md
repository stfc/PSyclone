# Using Profiling with PSyclone - Tutorial 2 #

This tutorial follows on from [Tutorial 1](../1_intro_psyir/README.md) and
assumes that you are comfortable with the topics covered there. It uses
the same tracer-advection mini-app, although for this tutorial it has
been refactored so that the mini-app itself is called from a separate
driver program. The reason for this will become clear as you work
through the tutorial.

You can find information on the various transformations supported by
PSyclone in the [User Guide](https://psyclone.readthedocs.io/en/latest/user_guide/transformations.html).
There is a separate [section](https://psyclone.readthedocs.io/en/latest/user_guide/profiling.html) on
PSyclone's support for profiling.

## Prerequisites ##

This example includes a Makefile to simplify the compilation process. It
assumes you are using Gnu Make. If you are using a different version of
Make then you may need to edit the Makefile and replace the occurrences of
`?=` with `=`.

## 1. Automatic Profiling ##

To begin, we will make use of PSyclone's support for the
[automatic addition of profiling instrumentation](https://psyclone.readthedocs.io/en/latest/user_guide/profiling.html). For
demonstration purposes we will be using the 'simple-timing' library
distributed with PSyclone since that has no dependencies. (PSyclone
currently provides wrapper libraries for profiling tools such as
[dl_timer](https://bitbucket.org/apeg/dl_timer/src/master/), DrHook
(from ECMWF), [tau](https://www.cs.uoregon.edu/research/tau) and NVIDIA's
nvtx. You may wish to investigate these if you have time at the end of
this session.)

1. Use `psyclone` to generate a version of the mini-app with profiling
   calipers inserted at the beginning and end of each routine:
   ```bash
   psyclone tra_adv_mod.F90 -o output_1.f90 --profile routines
   ```
   When examining the generated Fortran code (in `output_1.f90`), you
   should see that PSyclone has added `USE profile_psy_data_mod, ONLY:
   profile_PSyDataType` as well as calls to
   `profile_psy_data0%PreStart` and `profile_psy_data0%PostEnd`.
   Since the code now depends upon the PSyData API, the location of a
   suitable wrapper library must be provided when compiling the
   mini-app. The supplied Makefile will build the 'simple_timing'
   implementation of this library and link our mini-app against it:
   ```bash
   make allclean
   make tra_adv.exe
   ```
   At this point, the compiled application can be run (either ensure you have
   the necessary environment variables set first - see
   [Tutorial 1](../1_intro_psyir/README.md) - or include them inline
   as indicated):
   ```bash
   JPK=30 JPI=100 JPJ=100 IT=10 ./tra_adv.exe
   ```
   but no timing information is output!

2. No timing information is output because the timing library is not
   being shutdown. This is because PSyclone does not have any
   knowledge of the overall structure of an application and therefore
   it is up to the user to insert startup/shutdown calls in the
   appropriate locations (e.g. before/after MPI is
   initialised/finalised).  The 'simple_timing' library only requires
   a shutdown call so you will need to edit the driver program
   (`runner.f90`), add a `USE profile_psy_data_mod, only:
   profile_psydatashutdown` at the beginning and then add a call,
   `CALL profile_psydatashutdown()` as the final statement before the
   `END PROGRAM`. (This is the reason that the mini-app has been
   restructured slightly for this tutorial - the profiling would work
   fine without it but this way the manual modifications are made to
   a separate file.)

   Rebuild the application (`make tra_adv.exe`) and run it. You should now
   see timing information printed to the terminal, e.g.:

       ===========================================
       module::region           count	sum	   min		average		max
       tra_adv_mod::tra_adv-r0 1   0.718750000    0.718750000   0.718750000    0.718750000
       ===========================================

   Timings are only reported for a single region because our mini-app consists
   of a single subroutine.

## 2. User-specified Profiling ##

Profiling is a good way to get used to using PSyclone transformation scripts
so we will now use a script to achieve the same result as the first step
in this tutorial.

1. Alter the Makefile so that the `psyclone` command that creates `output.f90`
   uses the provided `profile_trans.py` script instead of the `--profile`
   option. If you look at the script, you will see that it encloses all
   children nodes of the Routine within a single profiling region.

   Compiling and executing the generated code should then produce the
   same timing output as we obtained in step 1:

       ===========================================
       module::region   count	sum	   min		average		max
       tra_adv_mod::tra_adv-r0 1   0.718750000    0.718750000   0.718750000    0.718750000
       ===========================================

   If you examine the PSyIR that is displayed when running PSyclone with
   the `profile_trans.py` script, you will see that the whole Schedule
   now has a `Profile` node at its root:

   ```bash
    FileContainer[]
        Container[tra_adv_mod]
            Routine[name:'tra_adv']
                0: Profile[]
                    Schedule[]
                        0: Call[name='get_environment_variable']
                            Reference[name:'get_environment_variable']
                            Literal[value:'JPI', Scalar<CHARACTER, UNDEFINED>]
                            Reference[name:'env']
   ```

## 3. Improving the Profiling ##

So far, we have only used the optimisation script to add profiling
around the whole of the mini-app. We shall now look at using the
transformation script to perform finer-grained profiling.

1. Modify the provided transformation script (`profile_trans.py`) so that
   it uses `walk` to find all Loop nodes:
   ```python
   loops = psyir.walk(Loop)
   ```
   Note, you will need to import the definition of the `Loop` class into
   the script:
   ```python
   from psyclone.psyir.nodes import Loop
   ```
   Next, identify those loops that are over vertical `levels`. These are
   loops that use the 'jk' loop variable as required in the NEMO Code
   Conventions. One way to easily select this loop, is to set the following
   loop_type inference rule:
   ```python
    Loop.set_loop_type_inference_rules({"levels": {"variable": "jk"}})
   ```
   With this, we can use the `loop_type` property and then enclose each of
   them within a profiling region:
   ```python
   for loop in loops:
       if loop.loop_type == "levels":
           p_trans.apply(loop)
   ```
   Examine the PSyIR after you have applied the transformations and check
   that it now has multiple Profile nodes, e.g.:

       12: Profile[]
           Schedule[]
               0: Loop[variable='jk', loop_type='levels']
                   Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
                   BinaryOperation[operator:'SUB']
                       Reference[name:'jpk']
                       Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
                   Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
                   Schedule[]
                       0: Assignment[]
                           ArrayReference[name:'zwx']


   Recompile and run the mini-app. You should now see that there are 14
   regions (r0-r13) reported by the timing library:

       ===========================================
       module::region                 count       sum         min     average         max
       tra_adv_mod::tra_adv-r0            1      2.24609375E-02      2.24609375E-02      2.24609375E-02      2.24609375E-02
       tra_adv_mod::tra_adv-r1            1      0.00000000          0.00000000          0.00000000          0.00000000
       tra_adv_mod::tra_adv-r2           10      5.66406250E-02      3.90625000E-03      5.66406269E-03      1.07421875E-02
       tra_adv_mod::tra_adv-r3           10      5.17578125E-02      2.92968750E-03      5.17578144E-03      1.17187500E-02
       ...
       tra_adv_mod::tra_adv-r12          10      2.73437500E-02      9.76562500E-04      2.73437495E-03      3.90625000E-03
       tra_adv_mod::tra_adv-r13           1     0.449218750         0.449218750         0.449218750         0.449218750
       ===========================================



2. Many PSyclone transformations allow additional options to be supplied
   via a dictionary argument to the `apply()` method. The
   profiling transformation, for instance, allows the user to supply a
   specific name for the region that is being created. Try using the
   "region_name" option to configure the names given to the regions,
   e.g.:

   ```python
   p_trans.apply(loop, {"region_name": ("NAME", "HERE")})
   ```

   For an example, see the `solutions/named_profile_trans.py` script.

3. If you have time, you may want to try repeating this exercise using
   a different PSyData wrapper library. For CPU, the next step up from
   the 'simple_timing' library we have used so far is 'dl_timer' which
   is available from
   [bitbucket](https://bitbucket.org/apeg/dl_timer/src/master/). You
   will need to obtain the source for this library:
   ```
   git clone https://bitbucket.org/apeg/dl_timer.git
   cd dl_timer
   . compiler_setup/gnu.sh
   make sm_lib
   ```
   and then update the
   various `PROFILE_*` variables in the Makefile in this tutorial directory
   to point to its location.

   **NOTE:** dl_timer requires an *initialisation* call to be added to
   `runner.f90` (`call profile_psydatainit()`).

## 4. Conclusion

Congratulations, you have now completed this part of the tutorial. We
have used a PSyclone transformation to add profiling
instrumentation to the tracer-advection mini-app. In the
[next tutorial](../3_openmp/README.md) we will look at using PSyclone
transformations to parallelise the code on CPU.
