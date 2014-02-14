.. psyclone documentation master file, created by
   sphinx-quickstart on Mon Jan 27 12:50:29 2014.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Introduction
============

PSyclone, the PSy code generator is being developed for use in the
`GungHo <http://www.metoffice.gov.uk/media/pdf/g/p/MOSAC_16.10.pdf>`_
project.

The GungHo project is designing and building the heart of the Met
Office's next generation software (known as the dynamical core) using
algorithms that will scale to millions of cores. The project is a
collaboration between the Met Office, NERC (via NERC funded academics)
and STFC, and the resultant software is expected to be operational in
2020.

In order to be able to support different types of threading within
each mpi process (specifically OpenMP for multiple cores and/or
OpenACC for GPU's) a layered architecture is being suggested for the
GungHo dynamical core in which the threading and communications
support is sandwiched between the algorithm specification and the
scientific kernels. It is proposed that the kernel code should not
include any threading code, directives, or communication
routines. This separation should allow the threading and
communications layer (termed the Parallelisation System, or PSy for
short) to be tailored to the particular machine in question without
affecting the kernel code. The machine specific PSy layer should help
with performance portability as any required changes to achieve
performance are isolated within a single layer.

The role of PSyclone is to help generate the PSy layer to avoid it having
to be written completely manually.

.. The potential benefits of code generation ...

.. potential to be used more widely ...

Contents:

.. toctree::
   :maxdepth: 2

   getting_going
   kernel_layer
   infrastructure_calls
   tutorial
   examples
   developers
   FAQS
   api

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
