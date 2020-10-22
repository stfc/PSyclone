# Using PSyclone to add OpenACC to NEMO - Tutorial 4 #

This tutorial builds on what has been covered in parts 1-3 in order
to construct an optimisation script that adds OpenACC directives to
the tra_adv mini-app. When built with a suitable compiler this then
enables the code to be run on a GPU.

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

## Parallelisation using KERNELS ##

The simplest way to add OpenACC directives to a code is often to use
the KERNELS directive - this instructs the compiler to automatically
parallelise any loop nests within the marked-up region. In PSyclone
this is achieved by applying the [`ACCKernelsTrans`][kernelstrans_def]
transformation to suitable regions of the code. The advantage of this
approach is that it minimises the number of directives that must be
inserted and makes use of the compiler's own dependency analysis to
ensure that loops may be safely parallelised. (We have found that for
NEMO, this approach achieves relatively good performance for the
majority of the code base.)

The supplied script, `kernels_trans.py`, does this in as simple a way
as possible. It attempts to enclose *every* Loop that is an immediate
child of the root Schedule within a KERNELS region. (i.e. it does not
consider the type of the loop.)

### 1. Generate and Examine the Basic Code ###

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

### 2. Using `validate()`??? ###

## Controlling Data Movement ##

## Collapsing Loop Nests ##


[kernelstrans_def]: https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1transformations_1_1ACCKernelsTrans.html "ACCKernelsTrans"
