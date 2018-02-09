.. Modified I. Kavcic Met Office
.. _introduction:

Introduction
============

PSyclone, the PSy code generator, is being developed for use in finite
element, finite volume and finite difference codes. PSyclone is being
developed to support the emerging API in the GungHo project for a
finite element dynamical core.

The `GungHo project
<https://www.metoffice.gov.uk/binaries/content/assets/mohippo/pdf/g/p/mosac_16.10.pdf>`_
is designing and building the heart of the Met Office's next generation
software (known as the dynamical core) using algorithms that will
scale to millions of cores. The project is a collaboration between the
Met Office, NERC (via NERC funded academics) and STFC, and the
resultant software is expected to be operational in 2022.

The associated GungHo software infrastructure is being developed to
support multiple meshes and element types thus allowing for future
model development. GungHo is also proposing a novel separation of
concerns for the software implementation of the dynamical core. This
approach distinguishes between three layers: the Algorithm layer, the
Kernel layer and the Parallelisation System (PSy) layer. Together this
separation is termed PSyKAl.

The Algorithm layer specifies the algorithm that the scientist would
like to run (in terms of calls to kernel routines and built-in operations)
and logically operates on full fields.

The Kernel layer provides the implementation of the code kernels as
subroutines. These subroutines operate on local fields (a set of
elements, a vertical column, or a set of vertical columns, depending
on the kernel).

The PSy layer sits in-between the algorithm and kernel layers and its
primary role is to provide node-based parallel performance for the target
architecture. The PSy layer can be optimised for a particular hardware
architecture, such as multi-core, many-core, GPGPUs, or some
combination thereof with no change to the algorithm or kernel layer
code. This approach therefore offers the potential for portable
performance.

Rather than writing the PSy layer manually, the GungHo project is
developing the PSyclone code generation system which can help a user to
optimise the code for a particular architecture (by providing
optimisations such as blocking, loop merging, inlining etc), or
alternatively, generate the PSy layer automatically.

PSyclone is also being extended to support an API being developed in
the GOcean project for two finite difference ocean model benchmarks,
one of which is based on the NEMO ocean model.
