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
are using. By default, the Makefile is configured to use gfortran. If you
are using some other compiler then you must either edit the Makefile
or set the F90 and F90FLAGS environment variables. (Since OpenMP directives
are just comments, the compiler will ignore them unless the appropriate
flag is set.)

Ideally you will be familiar with the use of OpenMP to parallelise code
although this is not absolutely essential. A detailed explanation
of OpenMP is beyond the scope of this course but, for the purposes of
this tutorial, OpenMP enables work to be shared over the cores of a
multi-core CPU (or CPUs in a multi-socket machine). It does this by
spawning threads, each of which has full access to all of the memory
of the parent process.

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

## 1. Adding basic OpenMP parallelism ##

The supplied `Makefile` processes the mini-app with PSyclone using the
supplied `omp_trans.py` transformation script. Before we do anything
else, let's take a look at the script. There are three key differences
compared to the script we used to introduce profiling in the previous
tutorial:

 * the script uses the `OMPParallelLoopTrans` transformation which
   decorates the target loop with an `OMP PARALLEL DO` directive.

 * the script is written specifically to work on the 'tra_adv' routine:
   ```python
        sched = psy.invokes.get('tra_adv').schedule
   ```
   this is just a choice. Normally a script will be written to be as
   general as possible but occasionally, something tailored to a
   particular routine may be required.

 * it blindly applies a transformation to each loop over vertical levels
   that is an immediate child of the Schedule:
   ```python
   for child in sched.children:
       if isinstance(child, Loop) and child.loop_type == "levels":
           OMP_TRANS.apply(child)
   ```

Hopefully that looks similar to what you may have ended up with at the
end of the profiling part of this tutorial although we are applying a
different transformation here.

Note that in this tutorial we will only be applying OpenMP
parallelisation to the loops over vertical levels. This is because, in
the full NEMO code, the horizontal domain is already decomposed over
MPI processes and there is no attempt to exploit the parallelism
available in the vertical.  This parallelism is available throughout
the majority of the code. (Of course, it would also be possible to use
OpenMP to parallelise the horizontal domain in NEMO so as to reduce
the number of MPI processes and resulting inter-process communication.)

1. Now that we have examined the script, the next stage is to use it to
   transform the mini-app. You can use the Makefile or just run
   PSyclone directly:
   ```bash
   $ psyclone -s ./omp_trans.py -api nemo -opsy psy.f90 -l output tra_adv_mod.F90
   ```
   This will produce an error message. Can you see what causes this?

2. The problem is that not all of the loops in the mini-app are
   parallelisable. The last loop nest contains:

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
   ```
    20: Loop[type='levels', field_space='None', it_space='None']
        ...
        Schedule[]
            0: Loop[type='lat', field_space='None', it_space='None']
               ...
               Schedule[]
                   0: Loop[type='lon', field_space='None', it_space='None']
                      ...
                      Schedule[]
                          0: CodeBlock[[<class 'fparser.two.Fortran2003.Write_Stmt'>]]
   ```
   Since, by definition, a CodeBlock contains code that PSyclone does not
   understand, it will refuse to parallelise any region that contains
   one.

3. We therefore need to edit the transformation script and make it a bit
   smarter. The easiest thing to do is to continue with our rather
   brute-force approach and simply get the script to ignore any
   transformation errors and carry on. In Python, this is achieved by
   enclosing the application of the transformation within a `try`:

   ```python
     try:
         OMP_TRANS.apply(child)
     except TransformationError:
         pass
   ```

   Edit the `omp_trans.py` script to use this approach and then build the
   code (`make tra_adv.exe`). Verify that PSyclone now successfully
   transforms the code and examine the PSyIR to see where the OpenMP
   directives have been inserted. You should see that there are now
   `Directive` nodes in the PSyIR, e.g.:

       14: OMPParallelDoDirective[omp_schedule=auto]
           Schedule[]
               0: Loop[type='levels', field_space='None', it_space='None']
                   Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
                   Reference[name:'jpk']
                   Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
                   Schedule[]
                       0: Loop[type='lat', field_space='None', it_space='None']
                           Literal[value:'1', Scalar<INTEGER, UNDEFINED>]

   and the corresponding Fortran looks like:

   ```fortran
       !$OMP parallel do default(shared), private(ji,jj,jk), schedule(auto)
       DO jk = 1, jpk
         DO jj = 1, jpj
           DO ji = 1, jpi
             umask(ji, jj, jk) = ji * jj * jk / r
   ```

   Note that PSyclone has automatically identified all scalars
   accessed within the loop and has declared them as thread
   private. All other variables are declared to be shared between
   threads.

4. We are now ready to do our first parallel run. The number of threads
   to use is set via the OMP_NUM_THREADS environment variable at run
   time, e.g. in bash:
   ```bash
    $ OMP_NUM_THREADS=4 ./tra_adv.exe
   ```
   At this point, the first thing to do is to check that we haven't
   broken anything. Assuming you've followed the steps in the
   [Validation](#validation) section then doing:
   ```bash
   $ diff output.dat output.dat.serial
   ```
   should show no differences.

## 2. Add Profiling ##

Obviously, the point of parallelising code is to make it go faster. We
therefore need some way of assessing the performance of the code and
to do this we will use PSyclone to add profiling instrumentation. (As
in tutorial 2, we will use the 'simple_timing' library for this but
other options are available.)

1. The quickest way to add profiling instrumentation is to edit the Makefile
   and add `--profile routines` to the PSyclone command line. You will also
   need to edit `runner.f90` and uncomment the call to
   `profile_psydatashutdown`. Having done this, `make clean` followed by
   `make tra_adv.exe` will rebuild the mini-app, now instrumented using the
   simple timing library.
   Running the mini-app should now produce timing information:
   ```bash
   $ OMP_NUM_THREADS=4 ./tra_adv.exe
    Tracer-advection Mini-app:
    Domain is  100x 100 grid points
    Performing   10 iterations
    Mini-app finished.

    ===========================================
    module::region  count     sum          min       average      max
    tra_adv::r0       1    0.593750000 0.593750000 0.593750000 0.593750000    
    ===========================================
   ```

At this point, you can play with running the mini-app on different
numbers of threads but you will see very little variation in
performance. The reason for this is that our simple script has
actually parallelised very little of the mini-app. If you investigate
the transformed PSyIR (or generated Fortran), you will see that only
the initialisation loops have been parallelised. Since each of these
loop nests are before the main iteration loop, their effect on the
overall runtime is negligible.

## 3. Improving Coverage ##

Clearly, the optimisation script needs to be improved so that it finds
all of the loops over vertical levels, rather than just those that are
immediate children of the root Schedule.

1. Edit the optimisation script so that it uses `sched.walk()` to do this
   (in the same way as was done for profiling in tutorial 2). Check that
   the generated PSyIR looks as you would expect. (You can use a second
   `walk`, after the transformation is complete, to count the number of
   `Directive` nodes that have been inserted in the Schedule - there
   should be 13.)

2. Now that we've parallelised a reasonable percentage of the mini-app,
   you should see a speed-up as you increase OMP_NUM_THREADS. For
   instance, for the default problem size (100x100x30) on a quad-core
   Intel I7 with hyperthreading:

   | Number of threads | Time (s) | Speed-up |
   | ----------------- | -------- | -------- |
   | 1                 | 0.56250  | 1.0      |
   | 2                 | 0.40625  | 1.4      |
   | 4                 | 0.31250  | 1.8      |
   | 8                 | 0.28125  | 2.0      |

Hopefully you too will be able to see a speedup when running the code
on your machine although you will probably need to increase the number
of iterations (`IT`) that the code does to get reliable timings. Note
that there are many things to consider when
looking at performance including (but not limited to); the compiler
and compiler flags, the number of physical cores your particular CPU
has, binding threads to cores, whether or not you're running inside a
Virtual Machine, ensuring repeatable timings, and any other, competing
activity on your machine. This is all well beyond the scope of this
tutorial.

(Note also that the 'simple_timing' library has been found to have very
poor granularity on MACOS.)

## 4. Improving Performance ##

The next section is optional and so, depending on how much time you
have, you may want to move on to the OpenACC part of this tutorial
(../4_nemo_openacc). If you are interested but don't have much time
then example solutions are provided in the
`parallel_region_omp_trans.py` and
`general_parallel_region_omp_trans.py` scripts in the `solutions`
directory.

If time allows then it is possible to improve upon the parallelisation
achieved in the previous section by creating parallel regions containing
multiple loop nests (this reduces any possible overhead associated with the
creation and destruction of the OpenMP threads). For instance, if
you examine the `psy.f90` that has been created, you will see:

```fortran
      !$OMP parallel do default(shared), private(ji,jj,jk), schedule(static)
      DO jk = 1, jpk - 1
        DO jj = 2, jpj
          DO ji = 2, jpi
            zslpx(ji, jj, jk) = (zwx(ji, jj, jk) + zwx(ji - 1, jj, jk)) * (0.25D0 + SIGN(0.25D0, zwx(ji, jj, jk) * zwx(ji - 1, jj, jk)))
            ...
          END DO
        END DO
      END DO
      !$OMP end parallel do
      !$OMP parallel do default(shared), private(ji,jj,jk), schedule(static)
      DO jk = 1, jpk - 1
        DO jj = 2, jpj
          DO ji = 2, jpi
            zslpx(ji, jj, jk) = SIGN(1.D0, zslpx(ji, jj, jk)) * MIN(ABS(zslpx(ji, jj, jk)), 2.D0 * ABS(zwx(ji - 1, jj, jk)), 2.D0 * ABS(zwx(ji, jj, jk)))
            ...
          END DO
         END DO
      END DO
      !$OMP end parallel do
      !$OMP parallel do default(shared), private(ji,jj,jk,z0u,z0v,zalpha,zdt,zu,zv,zzwx,zzwy), schedule(static)
      DO jk = 1, jpk - 1
        zdt = 1
        DO jj = 2, jpj - 1
          DO ji = 2, jpi - 1
            z0u = SIGN(0.5D0, pun(ji, jj, jk))
            ...
```

In fact, in the PSyIR, the loops corresponding to children 6-9 of the
Schedule of the outer loop over iterations are *all* parallel with no
intervening statements. These may then be enclosed in a single
parallel region. Although it is possible to write a general-purpose
transformation script to identify such opportunities, for this
tutorial we will modify our script to create a parallel region around
children 6-9.

To do this we will use PSyclone's `OMPParallelTrans` to create the
parallel region and `OMPLoopTrans` to decorate each loop inside this
region.

Beginning with the first, working optimisation script, the suggested
steps to achieve this are:

1. Parallelise all suitable loops over levels that are immediate
   children of the root Schedule;

2. Find the outer, iteration loop (PSyclone identifies this as being a
   "tracers" loop);

3. Decorate each of children 6-9 (inclusive) of the body of this loop
   with an OMP Loop Directive. The body of a loop node in the PSyIR may
   be obtained using the `loop_body` property so, using Python's slice
   notation, these children are `iter_loop.loop_body[6:10]` where
   `iter_loop` is the PSyIR loop node representing the iteration loop.

4. Enclose children 6-9 within a single OMP Parallel region. Note that
   after step 3, these children are now OMP Parallel Directive nodes;

5. Parallelise all remaining children of the iteration loop that are
   loops over levels;

## 5. Conclusion ##

Congratulations, you have now completed part 3 of the NEMO
tutorial. We have looked in more detail at the process of creating an
optimisation script and have used PSyclone to add OpenMP
parallelisation to the tracer-advection mini-app. With the caveats
noted earlier, you should have been able to measure a performance
improvement.


