# Using PSyclone to add OpenACC to NEMO - Tutorial 4 #

This tutorial builds on what has been covered in parts 1-3 in order
to construct an optimisation script that adds OpenACC directives to
the tra_adv mini-app.

## Prerequisites ##

Are the same as those for the first tutorial
(../1_nemo_psyir/README.md).

## Optional ##

It is not necessary to be able to compile
the generated OpenACC code but if you wish to then you will also need
a Fortran compiler with OpenACC support. Versions 8 and higher of
gfortran have OpenACC support (but you will need to ensure that the
offloading support is installed, e.g. sudo apt install
gcc-offload-nvptx) or you can use the NVIDIA HPC SDK
(https://developer.nvidia.com/hpc-sdk).  Obviously, to actually
execute the code you will need access to a machine with a GPU but that
too is optional.

The OpenACC specification may be found at
https://www.openacc.org/sites/default/files/inline-files/OpenACC.2.6.final.pdf

## Basic Parallelisation ##

The simplest way to add OpenACC directives to a code is often to use
the KERNELS directive - this instructs the compiler to automatically
parallelise any loop nests within the marked-up region.

Use the ACCKernelsTrans transformation (https://psyclone-ref.readthedocs.io/en/latest/_static/html/classpsyclone_1_1transformations_1_1ACCKernelsTrans.html) to enclose suitable regions of code within a KERNELS region.

## Collapsing Loop Nests ##

## Controlling Data Movement ##

