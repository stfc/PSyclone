.. -----------------------------------------------------------------------------
   BSD 3-Clause License

   Copyright (c) 2017-2025, Science and Technology Facilities Council.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

   * Neither the name of the copyright holder nor the names of its
     contributors may be used to endorse or promote products derived from
     this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
   -----------------------------------------------------------------------------
   Written by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
   Modified by I. Kavcic, Met Office

:html_theme.sidebar_secondary.remove: true

PSyclone Documentation 
======================

PSyclone is a source-to-source Fortran compiler designed to programmatically
optimise, parallelise and instrument HPC applications via user-provided
transformation scripts.
By encapsulating the performance-portability aspects (e.g. whether to
parallelise with OpenMP or OpenACC), these scripts enable a separation of
concerns between the scientific implementation and the optimisation choices.
This allows each aspect to be explored and developed largely independently.
Additionally, PSyclone supports the development of kernel-based, Fortran-embedded
DSLs following the PSyKAl model developed in the
`GungHo project <https://www.metoffice.gov.uk/research/foundation/dynamics/next-generation>`_.

PSyclone is currently used to support the
`LFRic <https://www.metoffice.gov.uk/research/modelling-systems/lfric/>`_
mixed finite-element PSyKAl DSL for the UK MetOffice's next generation
modelling system and the
`GOcean <https://gtr.ukri.org/projects?ref=NE%2FL01209X%2F1>`_
finite-difference PSyKAl DSL for a prototype 2D ocean modelling system.
It is also used to insert GPU offloading directives into existing
directly-addressed MPI applications such as the
`NEMO ocean model <https://www.nemo-ocean.eu/>`_.


.. toctree::
    :hidden:
    :caption: Table of Contents

.. toctree::
    :maxdepth: 1
    :hidden:
    :caption: User Guide

    user_guide/index

.. toctree::
    :maxdepth: 1
    :hidden:
    :caption: Tutorials and Examples

    tutorials_and_examples/index

.. toctree::
    :maxdepth: 1
    :hidden:
    :caption: Developer Guide

    developer_guide/index

.. only:: html

    .. toctree::
        :maxdepth: 1
        :hidden:
        :caption: Reference Guide

        reference_guide/index.rst
