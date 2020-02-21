.. -----------------------------------------------------------------------------
   BSD 3-Clause License

   Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
   Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab

.. only:: html

    .. image:: ../logo/psyclone_v1.0.png
        :width: 75%
        :align: center
        :alt: PSyclone

PSyclone User Guide
=====================


.. only:: html

    PSyclone is a code generation system developed to support domain-specific
    languages (DSLs) for finite element, finite volume and finite difference
    codes. Notably, it is used in the
    `LFRic Project <https://www.metoffice.gov.uk/research/modelling-systems/
    lfric/>`_, and it also supports the `GOcean <https://puma.nerc.ac.uk/trac
    /GOcean>`_ (2D, finite difference) DSL. In addition it is being extended
    to support the processing of existing finite difference codes such as the 
    `NEMO <https://www.nemo-ocean.eu/>`_ ocean model. 
    Find more information in the
    `Developer's Guide <https://psyclone-dev.readthedocs.io/>`_
    and the
    `Reference Guide <https://psyclone-ref.readthedocs.io/>`_.

.. toctree::
    :hidden:
    :caption: Table of Contents

.. toctree::
    :maxdepth: 2
    :caption: Introduction

    introduction
    getting_going
    examples
    system_specific_setup

.. toctree::
    :maxdepth: 2
    :caption: PSyKAl

    kernel_layer
    algorithm_layer
    psy_layer
    built_ins

.. toctree::
    :maxdepth: 2
    :caption: Domain APIs

    dynamo0p3
    gocean1p0
    nemo

.. toctree::
    :maxdepth: 2
    :caption: PSyclone Transformations

    psyclone_script
    psyir
    transformations
    distributed_memory


.. toctree::
    :maxdepth: 2
    :caption: Utilities and Conventions

    stub_gen
    line_length
    fortran_naming_conventions
    api
    psy_data
    profiling
    psyke
    configuration

.. toctree::
    :maxdepth: 2
    :caption: Bibliography

    zz_bibliography

..   tutorial
..   FAQS

.. when generating latex the index and module index are generated
   automatically and the entries below are rendered as plain text.
.. only:: html
	  
    Indices and tables
    ==================

    * :ref:`genindex`
    * :ref:`modindex`
    * :ref:`search`
