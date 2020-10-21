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

Ideally you will be familiar with the use of OpenMP to parallelise code
although this is not absolutely essential.

## Validation ##

When parallelising any code, it is always important to validate the
results on a regular basis to check that no bugs have been introduced.
Therefore, before doing anything else, we need some 'known good answer'
with which we can compare. If you have done either of the first two
parts of the NEMO tutorial then you will have run the mini-app on a
single core and it will have produced an `output.dat` file. Copy that
file to `output.dat.serial` or similar so that we have something to
compare with. (Note that this 'known good answer' is problem-size
specific so if you change JPI, JPJ, JPK or IT then you will need to
re-generate it.)

## Adding basic OpenMP parallelism ##

The supplied `Makefile` processes the mini-app with PSyclone using the
supplied `omp_trans.py` transformation script. Before we do anything
else, let's take a look at the script. There are two key differences
compared to the script we used to introduce profiling in previous
tutorial:

 1. the script is written specifically to work on the 'tra_adv' routine:

        sched = psy.invokes.get('tra_adv').schedule

    this is just a choice. Normally a script will be written to be as
    general as possible but occasionally something tailored to a particular
    routine may be required.

 2. it blindly applies a transformation to each loop over levels that is
    an immediate child of the Schedule:
```python
    for child in sched.children:
        if isinstance(child, Loop) and child.loop_type == "levels":
            sched, _ = OMP_TRANS.apply(child)
```

Hopefully that looks similar to what you may have ended up with at the
end of the profiling part of this tutorial although we are applying a
different transformation here.

The next step is to attempt to use this script to transform the mini-app.
You can use the Makefile or just run PSyclone directly:

    $ psyclone -s ./omp_trans.py -api nemo -opsy psy.f90 -l output tra_adv_mod.F90

This will produce an error message. Can you see what causes this?

The problem is that not all of the loops in the mini-app are parallelisable.
The last loop nest contains:

```fortran
   DO jk = 1, jpk-1
      DO jj = 2, jpj-1
         DO ji = 2, jpi-1
            write(4,*) mydomain(ji,jj,jk)
         END DO
      END DO
   END DO
```

and the `write` statement is represented as a CodeBlock in the PSyIR:

    13: Loop[type='levels', field_space='None', it_space='None']
        ...
        Schedule[]
            0: Loop[type='lat', field_space='None', it_space='None']
               ...
               Schedule[]
                   0: Loop[type='lon', field_space='None', it_space='None']
                      ...
                      Schedule[]
                          0: CodeBlock[[<class 'fparser.two.Fortran2003.Write_Stmt'>]]

Since, by definition, a CodeBlock contains code that PSyclone does not
understand, it will refuse to parallelise any region that contains
one.

We therefore need to edit the transformation script and make it a bit
smarter. The easiest thing to do is to continue with our rather brute-force
approach and simply get the script to ignore any errors and carry on. In
Python, this is achieved by enclosing the application of the transformation
within a `try`:

```python
  try:
      sched, _ = OMP_TRANS.apply(child)
  except TransformationError:
      pass
```

Edit the `omp_trans.py` script to use this approach and then build the
code. Verify that PSyclone now successfully transforms the code and
examine the PSyIR to see where the OpenMP directives have been
inserted.

We are now ready to do our first parallel run. A detailed explanation
of OpenMP is beyond the scope of this course but, in short, OpenMP
enables work to be shared over the cores of a multi-core CPU (or CPUs
in a multi-socket machine). It does this by spawning threads, each of
which has full access to all of the memory of the parent process. The
number of threads to use is set via the OMP_NUM_THREADS environment
variable at run time:

    $ OMP_NUM_THREADS=4 ./tra_adv.exe




