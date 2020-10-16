# Using PSyclone to add OpenMP - Tutorial 3 #

This tutorial follows on from Tutorials 1 and 2
(../1_nemo_psyir/README.md and ../2_nemo_profiling/README.md) and
assumes that you are comfortable with the topics covered there. It
uses PSyclone to parallelise the tracer-advection mini-app by adding
OpenMP directives.

You can find information on the various transformations supported by
PSyclone in the User Guide
(https://psyclone.readthedocs.io/en/stable/transformations.html).

## Prerequisites ##

This example includes a Makefile to simplify the compilation process. It
assumes you are using Gnu Make. If you are using a different version of
Make then you may need to edit the Makefile and replace the occurances of
`?=` with `=`.

The flags to enable OpenMP will depend upon which Fortran compiler you
are using. By default the Makefile is configured to use gfortran. If you
are using some other compiler then you must either edit the Makefile
or set the F90 and F90FLAGS environment variables. (Since OpenMP directives
are just comments, the compiler will ignore them unless the appropriate
flag is set.)

## Adding basic OpenMP parallelism ##

The supplied `Makefile` processes the mini-app with PSyclone using the
supplied `omp_trans.py` transformation script.

    for loop in sched.loops():
        kernels = loop.walk(NemoKern)
        if kernels and loop.loop_type == "levels":
            sched, _ = OMP_TRANS.apply(loop)


