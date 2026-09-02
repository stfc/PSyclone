.. -----------------------------------------------------------------------------
.. SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
..                         Facilities Council
.. SPDX-License-Identifier: BSD-3-Clause
.. See the full LICENSE file in the project root for details.
.. -----------------------------------------------------------------------------

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
`LFRic <https://www.metoffice.gov.uk/research/approach/modelling-systems/lfric/>`_
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
