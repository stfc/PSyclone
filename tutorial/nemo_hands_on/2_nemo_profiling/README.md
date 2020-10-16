# Using Profiling with PSyclone and NEMO - Tutorial 2 #

This tutorial follows on from Tutorial 1 (../1_nemo_psyir/README.md) and
assumes that you are comfortable with the topics covered there. It uses
the same tracer-advection mini-app although for this tutorial it has
been refactored so that the mini-app itself is called from a separate
driver program. The reason for this will become clear as you work
through the tutorial.

You can find information on the various transformations supported by
PSyclone in the User Guide
(https://psyclone.readthedocs.io/en/stable/transformations.html).

## Prerequisites ##

This example includes a Makefile to simplify the compilation process. It
assumes you are using Gnu Make. If you are using a different version of
Make then you may need to edit the Makefile and replace the occurances of
`?=` with `=`.

## Basic Profiling ##

1. Use PSyclone with the provided `profiling_trans.py` script in order
   to add profiling around the entire body of the mini-app in the
   `tra_adv_mod.F90` file. When examining the generated Fortran code,
   you should see that PSyclone has added ``USE profile_psy_data_mod,
   ONLY: profile_PSyDataType`` as well as calls to
   ``profile_psy_data0%PreStart()`` and ``profile_psy_data0%PostEnd``.

2. Compile the generated code. The code now depends upon the PSyData
   API and therefore the location of a suitable wrapper library must
   be provided when compiling the mini-app. PSyclone is distributed
   with a simple, self-contained timing library 'simple_timing' which
   we will use here. The supplied Makefile will build this library and
   then build the tracer-advection mini-app:

    $ make tra_adv.exe

   At this point, the compiled application can be run:

    $ ./tra_adv.exe

   but no timing information is output!

3. No timing information is output because the timing library is not
   being shutdown. This is because PSyclone does not have any
   knowledge of the overall structure of an application and therefore it is
   up to the user to insert startup/shutdown calls in the appropriate
   locations.  The 'simple_timing' library only requires a shutdown
   call so you will need to edit the driver program (`runner.f90`), add a
   `USE profile_psy_data_mod, only: profile_psydatashutdown` at the beginning
   and then add a call, `CALL profile_psydatashutdown()` as the
   final statement before the `END PROGRAM`.

   The compiled application should now output timing information when
   run, e.g.:

```
    ===========================================
    module::region   count	sum	   min		average		max
    tra_adv::r0        1   0.718750000    0.718750000   0.718750000    0.718750000    
    ===========================================
```

## Improving the Profiling ##

So far, we have only added profiling around the whole of the mini-app. We
shall now look at using the transformation script to perform finer-grained
profiling.

4. Modify the provided transformation script (`profile_trans.py`) so that
   it finds all loops over `levels` and encloses them within profiling
   regions. Hint: you will probably want to use the `walk` method and then
   the `loop_type` property of the `Loop`-node class. Examine the PSyIR
   after you have applied the transformations and check that it now has
   multiple Profile nodes, e.g.:

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
   regions (`r0`-`r13`) reported by the timing library:

```
 ===========================================
 module::region   count	       sum	     min		average	          max
 tra_adv::r0        1 	  3.12500000E-02    3.12500000E-02   3.12500000E-02    3.12500000E-02
 tra_adv::r1        1 	  0.00000000        0.00000000       0.00000000        0.00000000    
 tra_adv::r2       10 	  3.12500000E-02    0.00000000       3.12500005E-03    3.12500000E-02
 ...
 tra_adv::r13       1 	  0.187500000       0.187500000      0.187500000       0.187500000    
 ===========================================
```

5. Naming profile regions. The `apply()` method of `ProfileTrans` takes
   an optional dictionary as argument allowing additional options to
   be specified. Try using the "region_name" option to configure the
   names given to the regions, e.g.:

       p_trans.apply(loop, {"region_name": ("name","here")})

We have now used a PSyclone transformation to add profiling instrumentation
to the tracer-advection mini-app. In subsequent tutorials we will look
at using PSyclone transformations to parallelise the code.