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
quantities on horizontally-discontinuous function spaces. In addition,
this example demonstrates how to write a PSyclone transformation script
that only colours loops over continuous spaces.

Example 10
^^^^^^^^^^

Demonstrates the use of "inter-grid" kernels that prolong or restrict
fields (map between grids of different resolutions).

Example 11
^^^^^^^^^^

Example of the use of transformations to introduce redundant computation,
split synchronous halo exchanges into asynchronous exchanges (start and
stop) and move the starts of those exchanges in order to overlap them
with computation.

Example 12
^^^^^^^^^^

Example of applying code extraction to Nodes in an Invoke Schedule:

.. code-block:: bash

  > psyclone -nodm -s ./extract_nodes.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

or to a Kernel in an Invoke after applying transformations:

.. code-block:: bash

  > psyclone -nodm -s ./extract_kernel_with_transformations.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

For now it only inserts comments in appropriate locations while the
the full support for code extraction is being developed.

This example also contains a Python helper script ``find_kernel.py``
which displays the names and Schedules of Invokes containing call(s)
to the specified Kernel:

.. code-block:: bash

  > python find_kernel.py
