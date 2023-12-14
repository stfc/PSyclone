# Using Profiling with PSyclone - Tutorial 2 #

This tutorial follows on from Tutorial 1 (../1_nemo_psyir/README.md) and
assumes that you are comfortable with the topics covered there. It uses
the same tracer-advection mini-app, although for this tutorial it has
been refactored so that the mini-app itself is called from a separate
driver program. The reason for this will become clear as you work
through the tutorial.

You can find information on the various transformations supported by
PSyclone in the [User Guide](https://psyclone.readthedocs.io/en/stable/transformations.html).
There is a separate [section](https://psyclone.readthedocs.io/en/stable/profiling.html) on
PSyclone's support for profiling.

## Prerequisites ##

This example includes a Makefile to simplify the compilation process. It
assumes you are using Gnu Make. If you are using a different version of
Make then you may need to edit the Makefile and replace the occurances of
`?=` with `=`.

## 1. Automatic Profiling ##

To begin, we will make use of PSyclone's support for the automatic
addition of profiling instrumentation
(https://psyclone.readthedocs.io/en/stable/profiling.html). For
demonstration purposes we will be using the 'simple-timing' library
distributed with PSyclone since that has no dependencies. (PSyclone
currently provides wrapper libraries for profiling tools such as
[dl_timer](https://bitbucket.org/apeg/dl_timer/src/master/), DrHook
(from ECMWF), [tau](https://www.cs.uoregon.edu/research/tau) and NVIDIA's
nvtx. You may wish to investigate these if you have time at the end of
this session.)

1. Use the supplied Makefile to generate a version of the mini-app with
   profiling calipers inserted at the beginning and end of each routine:

       $ make transform

   When examining the generated Fortran code (in `psy_1.f90`), you
   should see that PSyclone has added `USE profile_psy_data_mod, ONLY:
   profile_PSyDataType` as well as calls to
   `profile_psy_data0%PreStart` and `profile_psy_data0%PostEnd`.
   Since the code now depends upon the PSyData API, the location of a
   suitable wrapper library must be provided when compiling the
   mini-app.  The supplied Makefile will build the 'simple_timing'
   implementation of this library and link our mini-app against it:

       $ make allclean
       $ make tra_adv.exe

   At this point, the compiled application can be run (ensure you have
   the necessary environment variables set first - see
   ../1_nemo_psyir/README.md):

       $ ./tra_adv.exe

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
       module::region   count	sum	   min		average		max
       tra_adv::r0        1   0.718750000    0.718750000   0.718750000    0.718750000
       ===========================================

   Timings are only reported for a single region because our mini-app consists
   of a single subroutine.

   If you examine the Makefile, you will see that PSyclone has been run with
   the `--profile routines` option and it is this that causes the subroutine
   to be instrumented for profiling:

   ```make
   psy.f90: tra_adv_mod.F90
   	$(PSYCLONE) --profile routines -api nemo \
                     -opsy psy.f90 -l output tra_adv_mod.F90
   ```

## 2. User-specified Profiling ##

Profiling is a good way to get used to using PSyclone transformation scripts
so we will now use a script to achieve the same result as the first step
in this tutorial.

1. Alter the Makefile so that the `psyclone` command that creates `psy.f90`
   uses the provided `profile_trans.py` script instead of the `--profile`
   option (or run psyclone separately on the command line). If you look at the
   script, you will see that it encloses all child nodes of the
   Schedule within a single profiling region.

   Compiling and executing the generated code should then produce the
   same timing output as we obtained in step 1:

       ===========================================
       module::region   count	sum	   min		average		max
       tra_adv::r0        1   0.718750000    0.718750000   0.718750000    0.718750000
       ===========================================

   If you examine the PSyIR that is displayed when running PSyclone with
   the `profile_trans.py` script, you will see that the whole Schedule
   now has a `Profile` node at its root:

   ```bash
    NemoInvokeSchedule[invoke='tra_adv']
        0: Profile[]
            Schedule[]
                0: Call[name='get_environment_variable']
                    Literal[value:'JPI', Scalar<CHARACTER, UNDEFINED>]
                    Reference[name:'env']
                1: CodeBlock[[<class 'fparser.two.Fortran2003.Read_Stmt'>]]
                ...
   ```

## 3. Improving the Profiling ##

So far, we have only used the optimisation script to add profiling
around the whole of the mini-app. We shall now look at using the
transformation script to perform finer-grained profiling.

1. Modify the provided transformation script (`profile_trans.py`) so that
   it uses `walk` to find all Loop nodes:
   ```python
   loops = sched.walk(Loop)
   ```
   Next, identify those loops that are over `levels` using the `loop_type`
   property and then enclose each of them within a profiling region:
   ```python
   for loop in loops:
       if loop.loop_type == "levels":
           p_trans.apply(loop)
   ```
   Examine the PSyIR after you have applied the transformations and check
   that it now has multiple Profile nodes, e.g.:

       12: Profile[]
           Schedule[]
               0: Loop[type='levels', field_space='None', it_space='None']
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
       module::region   count	       sum	     min		average	          max
       tra_adv::r0        1 	  3.12500000E-02    3.12500000E-02   3.12500000E-02    3.12500000E-02
       tra_adv::r1        1 	  0.00000000        0.00000000       0.00000000        0.00000000
       tra_adv::r2       10 	  3.12500000E-02    0.00000000       3.12500005E-03    3.12500000E-02
       ...
       tra_adv::r13       1 	  0.187500000       0.187500000      0.187500000       0.187500000
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
   will need to obtain the source for this library and then update the
   three `PROFILE_*` variables in the Makefile in this directory.

## 4. Conclusion

Congratulations, you have now completed this part of the tutorial. We
have used a PSyclone transformation to add profiling
instrumentation to the tracer-advection mini-app. In subsequent
tutorials we will look at using PSyclone transformations to
parallelise the code.