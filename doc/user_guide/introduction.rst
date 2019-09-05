.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018-2019, Science and Technology Facilities Council.
.. All rights reserved.
..
.. Redistribution and use in source and binary forms, with or without
.. modification, are permitted provided that the following conditions are met:
..
.. * Redistributions of source code must retain the above copyright notice, this
..   list of conditions and the following disclaimer.
..
.. * Redistributions in binary form must reproduce the above copyright notice,
..   this list of conditions and the following disclaimer in the documentation
..   and/or other materials provided with the distribution.
..
.. * Neither the name of the copyright holder nor the names of its
..   contributors may be used to endorse or promote products derived from
..   this software without specific prior written permission.
..
.. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
.. "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
.. LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
.. FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
.. COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
.. INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
.. BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
.. LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
.. CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.. LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
.. ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
.. POSSIBILITY OF SUCH DAMAGE.
.. -----------------------------------------------------------------------------
.. Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab
.. Modified I. Kavcic, Met Office
.. Modified B. P. Kinoshita, NIWA, New Zealand. 

.. _introduction:

Introduction
============

PSyclone, the PSy code generator, is being developed for use in finite
element, finite volume and finite difference codes. PSyclone development
started with the aim to support the emerging API in the GungHo project
for a finite element dynamical core.

The `GungHo project
<https://www.metoffice.gov.uk/research/foundation/dynamics/next-generation>`_
was initiated in 2011 to address challenges of weather and climate
prediction on the next generation of supercomputers. The project ran for
5 years as a collaboration between the Met Office, NERC (via NERC funded
academics) and STFC. It laid a foundation for redesign of the heart of
the Met Office's Unified Model, known as the dynamical core, from the
choices of numerical methods and model grids to the implementation of
parallel algorithms that will scale to millions of cores.

The software infrastructure based on the GungHo project recommendations
is now being developed in the
`LFRic project <https://www.metoffice.gov.uk/research/modelling-systems/lfric>`_
and is expected to be operational in 2022. Its development is led by the
requirements to support multiple meshes and element types, thus allowing
for future model development.

GungHo also proposed a novel separation of concerns for the software
implementation of the dynamical core. This approach distinguishes between
three layers: the Algorithm layer, the Kernel layer and the Parallelisation
System (PSy) layer. Together this separation is termed PSyKAl.

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

Rather than writing the PSy layer manually, the PSyclone code generation
system can help a user to optimise the code for a particular architecture
(by providing optimisations such as blocking, loop merging, inlining etc),
or alternatively, generate the PSy layer automatically.

PSyclone is also being extended to support an API being developed in
the `GOcean project <https://puma.nerc.ac.uk/trac/GOcean>`_ for two finite
difference ocean model benchmarks, one of which is based on the
`NEMO <https://www.nemo-ocean.eu/>`_ ocean model.
