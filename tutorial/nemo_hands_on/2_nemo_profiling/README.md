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

```bash
    $ make tra_adv.exe
```

   At this point, the compiled application can be run:

```bash
    $ ./tra_adv.exe
```

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

