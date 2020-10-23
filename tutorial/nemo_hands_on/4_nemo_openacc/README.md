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

The OpenACC specification may be found at
https://www.openacc.org/sites/default/files/inline-files/OpenACC.2.6.final.pdf

1. Apply KERNELS to each loop over levels separately
   - Point out data copies
2. Enclose whole body of iteration loop in a KERNELS region
   - Fewer data copies but still enough to hurt
2. Use a DATA region to keep data on the GPU for the duration
   of the iteration loop
   
3. Use ENTER DATA to keep data on the GPU between kernel calls
4. Use COLLAPSE on loop nests

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

Modify the supplied `data_trans.py` optimisation script to apply the
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
code. If you examine the transformed PSyIR you should see that ACC Kernels
Directive nodes have been added to the Schedule, e.g.:

    ...
    10: Directive[ACC Kernels]
        Schedule[]
            0: Loop[type='levels', field_space='None', it_space='None']
                ...
                Schedule[]
                    0: InlinedKern[]
                        Schedule[]
                            0: Assignment[]
                                ArrayReference[name:'rnfmsk_z']
                                    Reference[name:'jk']
                                BinaryOperation[operator:'DIV']
                                    Reference[name:'jk']
                                    Reference[name:'jpk']
    11: Directive[ACC Kernels]
        Schedule[]
            0: Loop[type='tracers', field_space='None', it_space='None']
                ...
                Schedule[]
                    0: Loop[type='levels', field_space='None', it_space='None']
                       ...

Note that the script has enclosed the outer, 'iteration' loop within a
KERNELS region. If we look at the generated code we can see that this
loop cannot be parallelised because it both reads and writes the
`mydomain` array so that each iteration depends upon the results of
the previous one. We are therefore relying upon the OpenACC compiler
to "do the right thing" and parallelise the loops *within* the iteration loop.

### 2. Using `validate()`??? ###

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

Try modifying the supplied `data_trans.py` optimisation script to
apply the `ACCDataTrans` transformation to the outer, iteration loop
of the mini-app. The script already locates that loop:

```python
    # Find the outer, 'iteration' loop
    tloop = None
    for node in sched.children:
        if isinstance(node, Loop) and node.loop_type == "tracers":
            tloop = node
            break
```

You will need to create an `ACCDataTrans` object and then call the `apply`
method and supply it with the `tloop`.


### Explicit DATA Regions ###

## Collapsing Loop Nests ##


[ref_kernelstrans]: https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1transformations_1_1ACCKernelsTrans.html "ACCKernelsTrans"

[ref_datatrans]: https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1transformations_1_1ACCDataTrans.html "ACCDataTrans"