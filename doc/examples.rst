.. _examples:

Examples
========

Various examples of the use of PSyclone are provided under the ``examples``
directory. All of these examples require that PSyclone be installed on the
host system, see :ref:`getting-going`. This document is intended to provide
an overview of the various examples so that a user can find one that is
appropriate to them. For details of how to run each example please see the
README files in the associated directories.

GOcean
------

Example 1
^^^^^^^^^

Examples of applying various transformations (loop fusion, OpenMP,
OpenACC) to the semi-PSyKAl'd version of the Shallow
benchmark. ("semi" because not all kernels are called from within
invoke()'s.) Also includes an example of generating a DAG from a Schedule.

Example 2
^^^^^^^^^

This is a simple but complete example of using PSyclone to enable an
application to run on a GPU by adding OpenACC directives. A Makefile
is included which will use PSyclone to generate the PSy code and
transformed kernels and then compile the application. This compilation
requires that the dl_esm_inf library (github.com/stfc/dl_esm_inf) be
installed.

Dynamo
------

Examples 1 and 2 are for the (deprecated) Dynamo 0.1 API. The remaining
examples are all for the Dynamo 0.3 API.

Example 1
^^^^^^^^^

Basic operation of PSyclone with invoke()'s containing just one kernel
for the Dynamo 0.1 API. Also includes an example of transforming for
OpenMP.

Example 2
^^^^^^^^^

A more complex example for the Dynamo 0.1 API containing multi-kernel
invokes. Provides examples of OpenMP and loop fusion transformations.

Example 3
^^^^^^^^^

Shows the use of colouring and OpenMP for the Dynamo 0.3 API. Includes
multi-kernel, named invokes with both user-supplied and built-in
kernels.

Example 4
^^^^^^^^^

Demonstrates the use of the special ``enforce_bc_kernel`` which
PSyclone recognises as a boundary-condition kernel.

Example 5
^^^^^^^^^

Example of kernels which require stencil information.

Example 6
^^^^^^^^^

Example of applying OpenMP to a Schedule containing kernels
that perform reduction operations. Two scripts are provided, one of
which demonstrates how to request that PSyclone generate code for a
reproducible OpenMP reduction. (The default OpenMP reduction is not
guaranteed to be reproducible from one run to the next on the same
number of threads.)

Example 7
^^^^^^^^^

Example of kernels requiring Column-Matrix Assembly operators.

Example 8
^^^^^^^^^

Example of the use of the redundant-computation and move
transformations to eliminate and re-order halo exchanges.

Example 9
^^^^^^^^^
Demonstrates the behaviour of PSyclone for kernels that read and write
quantities on horizontally-discontinuous function spaces.

Example 10
^^^^^^^^^^

Demonstrates the user of "inter-grid" kernels that prolong or restrict
fields (map between grids of different resolutions).

