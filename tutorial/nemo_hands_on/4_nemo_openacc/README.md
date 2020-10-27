# Using PSyclone to add OpenACC to NEMO - Tutorial 4 #

This tutorial builds on what has been covered in parts 1-3 in order
to construct an optimisation script that adds OpenACC directives to
the tra_adv mini-app. When built with a suitable compiler this then
enables the code to be run on a GPU.

You may find it helpful to read the section on
[OpenACC](https://psyclone.readthedocs.io/en/stable/transformations.html?highlight=accdatatrans#openacc)
in the
[Transformations](https://psyclone.readthedocs.io/en/stable/transformations.html?highlight=accdatatrans#transformations)
section of the PSyclone User Guide.

The OpenACC specification may be found at
https://www.openacc.org/sites/default/files/inline-files/OpenACC.2.6.final.pdf

## Prerequisites ##

Are the same as those for the first tutorial
(../1_nemo_psyir/README.md).

## Optional ##

It is not necessary to be able to compile the generated OpenACC code
but if you wish to then you will also need a Fortran compiler with
OpenACC support. Versions 8 and higher of gfortran have OpenACC
support (but you will need to ensure that the offloading support is
installed, e.g. sudo apt install gcc-offload-nvptx) or you can use the
NVIDIA HPC SDK (https://developer.nvidia.com/hpc-sdk).  Obviously, to
actually execute the code you will need access to a machine with a GPU
but that too is optional. Note that if you are doing this we will
assume you are familiar with executing code on a GPU in your local
environment.

## Parallelisation using KERNELS ##

The simplest way to add OpenACC directives to a code is often to use
the KERNELS directive - this instructs the compiler to automatically
parallelise any loop nests within the marked-up region. In PSyclone
this is achieved by applying the [`ACCKernelsTrans`][ref_kernelstrans]
transformation to suitable regions of the code. The advantage of this
approach is that it minimises the number of directives that must be
inserted and makes use of the compiler's own dependency analysis to
ensure that loops may be safely parallelised. (We have found that for
NEMO, this approach achieves relatively good performance for the
majority of the code base.)

### 1. Enclose loops over vertical levels ###

Modify the supplied `kernels_trans.py` optimisation script to apply the
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
    for child in tloop.loop_body.children:
        if isinstance(node, Loop) and node.loop_type == "levels":
	    ACC_KERNELS_TRANS.apply(node)
```

Use the supplied Makefile to run PSyclone and generate the transformed
code. If you examine the generated Fortran in `psy.f90` you should see
that ACC Kernels Directive nodes have been added to the Schedule,
e.g.:

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

We will continue working with the `kernels_trans.py` script. It must
now be extended to identify those children of the iteration loop
that are array assignments. If this is the case they will be instances
of the `Assignment` class and will be marked as applying to an
array range:

```python
        # Enclose array assignments (implicit loops)
        if isinstance(node, Assignment) and node.is_array_range:
	    ACC_KERNELS_TRANS.apply(node)
```

Once you have this script working, the fragment of Fortran reproduced
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
and explicit data movement. NVIDIA also supports 'managed memory'
where page faults on either the CPU or GPU cause the necessary memory
to be moved automatically to the correct location.

Explicit data movement can be controlled using OpenACC Data Regions and
PSyclone can create these using the [`ACCDataTrans`][ref_datatrans]
transformation. A data region can be used to keep data on the GPU
between various kernel invocations.

### 3. Add a static DATA region ###

We can remove much of the data movement by adding a DATA region. The
`kernels_trans.py` script must be further extended so that, as a final
step, the entire body of the 'iteration' loop is enclosed within a
DATA region. To do this, we must apply an `ACCDataTrans` transformation
to the body of the 'iteration' loop:

```python
    # Finally, enclose all of the children of the 'iteration' loop within
    # a data region
    ACC_DATA_TRANS.apply(tloop.loop_body)
```

With that addition, running `make` should re-generate `psy.f90`. If we
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
arrays are read-only, write-only or read-write. This helps to reduce
unnecessary memory copying (e.g. if a field is not modified within a
data region then it is clearly not necessary to copy it back to the
CPU). 

(Currently PSyclone's analysis is limited to the contents of the DATA
region.  In our example, we can see that although `zind` for instance
is written to within the data region, it is in fact not used again
outside the data region and therefore does not actually need to be
copied back from the GPU.)

Although we have massively reduced the amount of data movement, we will
still be copying data to and from the GPU upon every trip of the
'iteration' loop as we enter and leave the DATA region. This can be
fixed by tweaking our script so as to enclose the whole loop within
the region rather than just its body:

```python
    # Finally, enclose all of the children of the 'iteration' loop within
    # a data region
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

## Collapsing Loop Nests ##

Although we now have a basic GPU implementation of the mini-app, it is possible to
make further use of our knowledge of the domain to improve performance. For instance,
we can expose more parallelism to the compiler by instructing it to 'collapse' (merge
into a single iteration space) tightly-nested inner loops. We do this in PSyclone by
applying the `ACCLoopTrans` transformation in order to decorate a loop (or loops)
with an ACC LOOP directive.

Create a brand-new transformation script and, for demonstration purposes,
apply the `ACCLoopTrans` to every 'latitude' loop:

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

(At code-generation time, such a directive must be
within an OpenACC parallel region such as that defined by a KERNELS directive.)

### 2. Using `validate()`??? ###

### DATA Regions with Dynamic Scope ###
## Using KERNELS in Practice??? ##

Point out that we could have just whacked 'KERNELS' around every loop and it would
work, in this simple case:

Note that the script has enclosed the outer, 'iteration' loop within a
KERNELS region. If we look at the generated code we can see that this
loop cannot be parallelised because it both reads and writes the
`mydomain` array so that each iteration depends upon the results of
the previous one. We are therefore relying upon the OpenACC compiler
to "do the right thing" and parallelise the loops *within* the iteration loop.

In general, there are things that cannot be put inside KERNELS
regions: calls to other subroutines, certain statements that are know
to cause the compiler to fail (e.g. `MIN(my_array(:,:,:), dim=2)`).

[ref_kernelstrans]: https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1transformations_1_1ACCKernelsTrans.html "ACCKernelsTrans"

[ref_datatrans]: https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1transformations_1_1ACCDataTrans.html "ACCDataTrans"