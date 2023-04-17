# Using PSyclone to add OpenACC directives - Tutorial 4 #

This tutorial follows on from Tutorials 1-3 and assumes that you are
comfortable with the topics covered there. It constructs an
optimisation script that adds OpenACC directives to the tracer-advection
mini-app. When built with a suitable compiler this then enables the
code to be run on a GPU (but this is not required for this tutorial).

You may find it helpful to read the section on
[OpenACC](https://psyclone.readthedocs.io/en/stable/transformations.html?highlight=accdatatrans#openacc)
in the
[Transformations](https://psyclone.readthedocs.io/en/stable/transformations.html?highlight=accdatatrans#transformations)
section of the PSyclone User Guide.

The OpenACC specification may be found at
https://www.openacc.org/sites/default/files/inline-files/OpenACC.2.6.final.pdf

## Prerequisites ##

Are the same as those for the previous code-transformation tutorials.

## Optional ##

In this tutorial it is not necessary to be able to compile the
generated OpenACC code but if you wish to then you will also need a
Fortran compiler with OpenACC support. Versions 8 and higher of
gfortran have OpenACC support (but you will need to ensure that the
offloading support is installed, e.g. sudo apt install
gcc-offload-nvptx) or you can use the NVIDIA HPC SDK
(https://developer.nvidia.com/hpc-sdk).  Obviously, to actually
execute the code you will need access to a machine with a GPU but that
too is optional. Note that if you are doing this we will assume you
are familiar with executing code on a GPU in your local environment.

Note that the Makefile for this part of the tutorial does *not* compile
the generated code by default. If you have a suitable compiler and
want to actually perform the compilation then use the `tra_adv.exe`
target, i.e. `make tra_adv.exe`.

## Parallelisation using KERNELS ##

The simplest way to add OpenACC directives to a code is often to use
the KERNELS directive - this instructs the compiler to automatically
parallelise any loop nests within the marked-up region. In PSyclone
this is achieved by applying the [`ACCKernelsTrans`](https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1transformations_1_1ACCKernelsTrans.html)
transformation to suitable regions of the code. The advantage of this
approach is that it minimises the number of directives that must be
inserted and makes use of the compiler's own dependency analysis to
ensure that loops may be safely parallelised. This means that,
for the tracer advection mini-app, we could simply enclose each
top-level loop (that does not contain a CodeBlock) within a KERNELS
region:

```python
    for node in sched.children:
        if isinstance(node, Loop):
	    try:
                ACC_KERNELS_TRANS.apply(node)
            except TransformationError:
                pass
```

In doing this we will enclose the outer, 'iteration' loop within a
KERNELS region. If we look at the original Fortran (in
`tra_adv_mod.F90`) we can see that this loop cannot be parallelised
because it both reads and writes the `mydomain` array so that each
iteration depends upon the results of the previous one. We would
therefore be relying upon the OpenACC compiler to "do the right thing"
and parallelise the loops *within* the iteration loop.

However, since the HPC expert is free to use their domain-specific
knowledge when applying PSyclone,  we can be more prescriptive
about how we want the compiler to parallelise the code (in order to
achieve better computational performance). There are also things that
cannot be put inside KERNELS regions: these include calls to other
subroutines and certain types of statement that are known to trigger
compiler bugs (e.g. `MIN(my_array(:,:,:), dim=2)` caused problems with
NVIDIA (PGI) < 20.7).

When working with a large code such as NEMO, it is therefore necessary
to exercise fine-grained control when inserting KERNELS regions. In
this tutorial we will walk through some of the steps and apply them to
the tracer-advection mini-app. (If you already have OpenACC experience
you will see that some of these steps are not going to result in
performant code, however, they are included here to illustrate the
various ways in which PSyclone can be used.)

### 1. Enclose loops over vertical levels ###

1. Modify the supplied `kernels_trans.py` optimisation script to apply the
   `ACCKernelsTrans` transformation to every loop over vertical levels
   within the outer, iteration loop of the mini-app. The script already
   locates that loop:

   ```python
    # Find the outer, 'iteration' loop
    tloop = None
    for node in sched.children:
        if isinstance(node, Loop) and node.loop_type == "tracers":
            tloop = node
            break
    ```

   Similar to what has been done in previous tutorials, you will need to
   loop over the children of that loop, identify those that are loops of
   the correct "levels" type, and transform them:
   ```python
    for node in tloop.loop_body.children:
        if isinstance(node, Loop) and node.loop_type == "levels":
            ACC_KERNELS_TRANS.apply(node)
   ```

2. Use the supplied Makefile to run PSyclone and generate the transformed
   code (just type `make tra_adv.exe`). If you examine the generated Fortran
   in `psy_openacc.f90` you should see that ACC Kernels Directive nodes have
   been added to the Schedule, e.g.:
   ```fortran
    DO jt = 1, it
      !$ACC KERNELS
      DO jk = 1, jpk
        DO jj = 1, jpj
          DO ji = 1, jpi
            IF (tsn(ji, jj, jk) <= ztfreez(ji, jj) + 0.1D0) THEN
              zice = 1.D0
            ELSE
              zice = 0.D0
            END IF
            zind(ji, jj, jk) = MAX(rnfmsk(ji, jj) * rnfmsk_z(jk), &
	                       upsmsk(ji, jj), zice) * tmask(ji, jj, jk)
            zind(ji, jj, jk) = 1 - zind(ji, jj, jk)
          END DO
        END DO
      END DO
      !$ACC END KERNELS
      zwx(:, :, jpk) = 0.E0
      zwy(:, :, jpk) = 0.E0
      !$ACC KERNELS
      DO jk = 1, jpk - 1
        ...
   ```

Although this code will compile, run and produce the correct answers,
its performance will be very poor. This is because we have only
enclosed the loops over vertical levels within KERNELS regions and
each of these regions acts as an implicit data region. This means that
data will be copied from the host to the GPU before each region and
then back again afterwards.

If we are to get any sort of reasonabe performance then these memory
copies must be eliminated by keeping data on the GPU for as long as
possible. In order to do that we must first ensure that all computation
is performed on the GPU.

From the Fortran code fragment above, we can see that we have some
array initialisation specified using Fortran array syntax. Since these
are also loops and are writing to arrays, these too can be enclosed
with KERNELS regions in order to perform them on the GPU.

### 2. Enclose array assignments ###

1. We will continue working with the `kernels_trans.py` script. It must
   now be extended to identify those children of the iteration loop
   that are array assignments. If this is the case they will be instances
   of the `Assignment` class and will be marked as applying to an
   array range:
   ```python
        # Enclose array assignments (implicit loops)
        if isinstance(node, Assignment) and node.is_array_assignment:
            ACC_KERNELS_TRANS.apply(node)
   ```

2. Once you have this script working, the fragment of Fortran reproduced
   above should now look like:
   ```fortran
   DO jt = 1, it
      !$ACC KERNELS
      DO jk = 1, jpk
        DO jj = 1, jpj
          DO ji = 1, jpi
            IF (tsn(ji, jj, jk) <= ztfreez(ji, jj) + 0.1D0) THEN
              zice = 1.D0
            ELSE
              zice = 0.D0
            END IF
            zind(ji, jj, jk) = MAX(rnfmsk(ji, jj) * rnfmsk_z(jk), upsmsk(ji, jj), zice) * tmask(ji, jj, jk)
            zind(ji, jj, jk) = 1 - zind(ji, jj, jk)
          END DO
        END DO
      END DO
      !$ACC END KERNELS
      !$ACC KERNELS
      zwx(:, :, jpk) = 0.E0
      !$ACC END KERNELS
      !$ACC KERNELS
      zwy(:, :, jpk) = 0.E0
      !$ACC END KERNELS
      !$ACC KERNELS
      DO jk = 1, jpk - 1
        ...
   ```

All of the compute within the 'iteration' loop is now being performed on
the GPU. However, data will still be moved back and forth as one KERNELS
region ends and another begins.

## Controlling Data Movement ##

A vital part of achieving good GPU performance is minimising the
amount of data that is moved between the memory of the host CPU and
the memory of the GPU. Even with hardware technology such as NVLink,
the bandwidth available between GPU and CPU is still only of the order
of that between the CPU and main memory. Therefore, frequent data
movement on and off the GPU will destroy performance.

The OpenACC specification allows for both implicit (compiler generated)
and explicit data movement. NVIDIA also supports 'managed'/'unified' memory
where page faults on either the CPU or GPU cause the necessary memory
to be moved automatically to the correct location.

Explicit data movement can be controlled using OpenACC Data Regions and
PSyclone can create these using the [`ACCDataTrans`](https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1transformations_1_1ACCDataTrans.html)
transformation. A data region can be used to keep data on the GPU
between various kernel invocations.

### 3. Add a static DATA region ###

1. We can remove much of the data movement by adding a DATA region. The
   `kernels_trans.py` script must be further extended so that, as a final
   step, the entire body of the 'iteration' loop is enclosed within a
   DATA region. To do this, we must apply an `ACCDataTrans` transformation
   to the body of the 'iteration' loop:
   ```python
    # Finally, enclose all of the children of the 'iteration' loop within
    # a data region
    ACC_DATA_TRANS.apply(tloop.loop_body)
   ```

2. With that addition, running `make` should re-generate `psy.f90`. If we
   examine that file then we see that a DATA region is now created at the
   start of the 'iteration' loop:
   ```fortran
    DO jt = 1, it
      !$ACC DATA COPYIN(pun,pvn,pwn,rnfmsk,rnfmsk_z,tmask,tsn,umask,upsmsk,vmask,ztfreez)&
      !$ACC COPYOUT(zind,zslpx,zslpy,zwx,zwy) COPY(mydomain)
      !$ACC KERNELS
      DO jk = 1, jpk
        DO jj = 1, jpj
          DO ji = 1, jpi
            IF (tsn(ji, jj, jk) <= ztfreez(ji, jj) + 0.1D0) THEN
              ...
      !$ACC END KERNELS
      !$ACC END DATA
    END DO
   ```

   In creating the DATA region, PSyclone has analysed the arrays used
   within it and used that information to add clauses about whether
   arrays are read-only (`COPYIN`), write-only (`COPYOUT`) or
   read-write (`COPY`). This helps to reduce unnecessary memory
   copying (e.g. if a field is not modified within a data region then
   it is clearly not necessary to copy it back to the CPU).

(Currently PSyclone's analysis is limited to the contents of the DATA
region.  In our example, we can see that although `zind` for instance
is written to within the data region, it is in fact not used again
outside the data region and therefore does not actually need to be
copied back from the GPU.)

3. Although we have massively reduced the amount of data movement, we will
   still be copying data to and from the GPU upon every trip of the
   'iteration' loop as we enter and leave the DATA region. This can be
   fixed by tweaking our script so as to enclose the whole loop within
   the region rather than just its body:
   ```python
    # Finally, enclose the whole 'iteration' loop within a data region
    ACC_DATA_TRANS.apply(tloop)
   ```
   With this change, the generated code should now look like:
   ```fortran
    !$ACC DATA COPYIN(pun,pvn,pwn,rnfmsk,rnfmsk_z,tmask,tsn,umask,upsmsk,vmask,ztfreez) &
    !$ACC COPYOUT(zind,zslpx,zslpy,zwx,zwy) COPY(mydomain)
    DO jt = 1, it
      !$ACC KERNELS
      DO jk = 1, jpk
        ...
   ```

At this point we have an implementation that should give reasonable
performance on a GPU; all of the compute has been moved to the GPU and
data is only copied to the device at the start and copied back at the
end.

## 4. Collapsing Loop Nests ##

Although we now have a basic GPU implementation of the mini-app, it is
possible to make further use of our knowledge of the domain to improve
performance. For instance, we can expose more parallelism to the
compiler by instructing it to 'collapse' (merge into a single
iteration space) tightly-nested inner loops. We do this in PSyclone by
applying the `ACCLoopTrans` transformation in order to decorate a loop
(or loops) with an `ACC LOOP` directive.

We'll start by using this transformation without any additional options
so that it will just create `ACC LOOP INDEPENDENT` directives. We'll then
add the necessary option to add the `COLLAPSE` clause.

1. Create a brand-new transformation script and, for demonstration purposes,
   apply the `ACCLoopTrans` without any options to every 'latitude' loop:
   ```python
    from psyclone.transformations import ACCLoopTrans, TransformationError
    ACC_LOOP_TRANS = ACCLoopTrans()
    ...
    loops = sched.walk(Loop)
    for loop in loops:
        if loop.loop_type == "lat":
            try:
                ACC_LOOP_TRANS.apply(loop)
            except TransformationError:
                pass
   ```
   (Note that for simplicity, we are relying on the fact that all latitude
   loops in the mini-app correspond to tight, doubly-nested loops. In
   practice extra checking will be required to ensure this is the case.)

   Running PSyclone with this transformation script will fail at
   code-generation time because none of the added `ACC LOOP` directives
   are within an OpenACC parallel region:
   ```
   Generation Error: ACCLoopDirective must have an ACCParallelDirective or ACCKernelsDirective as an ancestor in the Schedule
   ```

2. We must therefore extend the optimisation script to add a kernels
   region. Using parts of the script developed earlier, put the body
   of the 'iteration' loop within a KERNELS region:
   ```python
    # Find the outer, 'iteration' loop
    tloop = None
    for node in sched.children:
        if isinstance(node, Loop) and node.loop_type == "tracers":
            tloop = node
            break
    ACC_KERNELS_TRANS.apply(tloop.loop_body)
   ```

3. We can then refine our first attempt so that we only collapse latitude
   loops that are *within* this region (i.e. children of the 'iteration'
   loop):
   ```python
    loops = tloop.walk(Loop)
    for loop in loops if loop.loop_type == "lat":
        ACC_LOOP_TRANS.apply(loop)
   ```
   At this point, PSyclone should successfully generate valid Fortran
   with OpenACC directives. However, if we examine the Fortran produced
   we will see that the LOOP directives currently only specify
   `INDEPENDENT`, e.g.:
   ```fortran
      DO jk = 1, jpk - 1
        !$ACC LOOP INDEPENDENT
        DO jj = 2, jpj - 1
          DO ji = 2, jpi - 1
   ```

4. How do we add the `COLLAPSE` clause? If we look at the documentation
   for `ACCLoopTrans` in the
   [Transformations](https://psyclone.readthedocs.io/en/stable/transformations.html?highlight=accdatatrans#transformations)
   section of the User Guide, we see that it takes an `options`
   dictionary argument. We can therefore specify that we want
   `COLLAPSE(2)` by doing:
   ```python
   ACC_LOOP_TRANS.apply(loop, options={"collapse": 2})
   ```

   The resulting Fortran should now look like:
   ```fortran
      DO jk = 1, jpk - 1
        !$ACC LOOP INDEPENDENT COLLAPSE(2)
        DO jj = 2, jpj - 1
          DO ji = 2, jpi - 1
   ```
   This option has been found to improve the performance of the NEMO model
   on GPU by a few percent.

If time allows, you might wish to try modifying the mini-app so that
at least one of the latitude loops does *not* correspond to a
tightly-nested loop, e.g.:

```fortran
    DO jk = 1, jpk - 1
      DO jj = 2, jpj - 1
        DO ji = 2, jpi - 1
	  ...
	END DO
	DO ji = 2, jpi - 1
	  some-new-statements-here
	END DO
      END DO
    END DO
```

You will then need to generalise your script so that it only applies
the `ACCLoopTrans` to valid loop nests. (Hint: you will need to examine
the nodes in the `loop_body` of the candidate latitude loop.)

## 5. Managed Memory ##

In practice, the work being done to extend PSyclone to process the
whole of the NEMO code is currently using NVIDIA's 'managed memory'
support. No explicit data regions are added to the code. Instead, the
run-time system moves data to/from the GPU automatically when page
faults occur. This was originally intended as being a quick way to get
something working on the GPU but it has actually proved to work well
in general.


## 6. Conclusion ##

Congratulations, you have now completed the OpenACC part of the
tutorial.  You should now understand the basic OpenACC transformations
provided by PSyclone and how these can be used to accelerate
NEMO-style code on a GPU.
