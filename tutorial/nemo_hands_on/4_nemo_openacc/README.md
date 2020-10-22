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

## Basic Parallelisation ##

The simplest way to add OpenACC directives to a code is often to use
the KERNELS directive - this instructs the compiler to automatically
parallelise any loop nests within the marked-up region. In PSyclone
this is achieved by applying the `ACCKernelsTrans` transformation to
suitable regions of the code. The advantage of this approach is that
it minimises the number of directives that must be inserted and makes
use of the compiler's own dependency analysis to ensure that loops may
be safely parallelised.

1. Use the `ACCKernelsTrans` transformation
(https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1transformations_1_1ACCKernelsTrans.html)
to enclose suitable regions of code within a KERNELS region.

## Controlling Data Movement ##

## Collapsing Loop Nests ##

