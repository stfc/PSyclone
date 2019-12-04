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

Various examples of the use of PSyclone are provided under the
``examples`` directory in the git repository. If you have installed
PSyclone using pip then the examples may be found in
``share/psyclone/examples`` under your Python installation
(e.g. ``~/.local`` for a user-local installation).

Running any of these examples requires that PSyclone be installed on
the host system, see :ref:`getting-going`. This section is intended
to provide an overview of the various examples so that a user can find
one that is appropriate to them. For details of how to run each
example please see the ``README.md`` files in the associated directories.

GOcean
------

Example 1: Loop transformations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Examples of applying various transformations (loop fusion, OpenMP,
OpenACC) to the semi-PSyKAl'd version of the Shallow
benchmark. ("semi" because not all kernels are called from within
invoke()'s.) Also includes an example of generating a DAG from an
InvokeSchedule.

Example 2: OpenACC
^^^^^^^^^^^^^^^^^^

This is a simple but complete example of using PSyclone to enable an
application to run on a GPU by adding OpenACC directives. A Makefile
is included which will use PSyclone to generate the PSy code and
transformed kernels and then compile the application. This compilation
requires that the dl_esm_inf library (github.com/stfc/dl_esm_inf) be
installed/available - it is provided as a git submodule of the PSyclone
project (see :ref:`dev_guide:dev-installation` in the Developers' Guide
for details on working with submodules).

The supplied Makefile also provides a second, "profile" target which
performs the same OpenACC transformations but then encloses the whole
of the resulting PSy layer in a profiling region. By linking this with
the PSyclone NVTX profiling wrapper (and the NVTX library itself), the
resulting application can be profiled using NVIDIA's `nvprof` or
`nvvp` tools.

Example 3: OpenCL
^^^^^^^^^^^^^^^^^

Example of the use of PSyclone to generate an OpenCL driver version of
the PSy layer and OpenCL kernels.

Example 4: Kernels containing use statements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Transforming kernels for use with either OpenACC or OpenCL requires
that we handle those that access data and/or routines via module
``use`` statements. This example shows the various forms for which
support is planned (Issues #323 and #342).

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

Example of applying OpenMP to an InvokeSchedule containing kernels
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
fields (map between grids of different resolutions), as well as the
use of ``ANY_DISCONTINUOUS_SPACE`` metadata.

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

Example 13 : Kernel transformation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Demonstrates how an LFRic kernel can be transformed. The example
transformation makes Kernel values constant where appropriate. For
example, the number of levels is usually passed into a kernel by
argument but the transformation allows a particular value to be
specified which the transformation then sets as a parameter in the
kernel. Hard-coding values in a kernel helps the compiler to do a
better job when optimising the code.

Example 14: OpenACC
^^^^^^^^^^^^^^^^^^^

Example of adding OpenACC directives in the dynamo0.3 API. This is a
work in progress so the generated code may not work as
expected. However it is never-the-less useful as a starting
point. Three scripts are provided.

The first script (``acc_kernels.py``) shows how to add OpenACC Kernels
directives to the PSy-layer. This example only works with distributed
memory switched off as the OpenACC Kernels transformation does not yet
support halo exchanges within an OpenACC Kernels region.

The second script (``acc_parallel.py``)shows how to add OpenACC Loop,
Parallel and Enter Data directives to the PSy-layer. Again this
example only works with distributed memory switched off as the OpenACC
Parallel transformation does not support halo exchanges within an
OpenACC Parallel region.

The third script (``acc_parallel_dm.py``) is the same as the second
except that it does support distributed memory being switched on by
placing an OpenACC Parallel directive around each OpenACC Loop
directive, rather than having one for the whole invoke. This approach
avoids having halo exchanges within an OpenACC Parallel region.

The generated code has a number of problems including 1) it does not
modify the kernels to include the OpenACC Routine directive, 2) a
loop's upper bound is computed via a derived type (this should be
computed beforehand) 3) set_dirty and set_clean calls are placed
within an OpenACC Parallel directive and 4) there are no checks on
whether loops are parallel or not, it is just assumed they are -
i.e. support for colouring or locking is not yet implemented.


NEMO
----

These examples may all be found in the ``examples/nemo`` directory.

Example 1: OpenMP parallelisation of tra_adv
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Demonstrates the use of PSyclone to parallelise the loops over vertical levels
in a NEMO tracer-advection benchmark using OpenMP.

Example 2: OpenMP parallelisation of traldf_iso
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Demonstrates the use of PSyclone to parallelise the loops over vertical levels
in some NEMO tracer-diffusion code using OpenMP.

Example 3: OpenACC parallelisation of tra_adv
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Demonstrates the introduction of simple OpenACC parallelisation (using the
``data`` and ``kernels`` directives) for a NEMO tracer-advection benchmark.

.. _nemo-eg4-sir:

Example 4: Transforming Fortran code to the SIR
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Demonstrates that simple Fortran code examples which conform to the
NEMO API can be transformed to the Stencil Intermediate Representation
(SIR). The SIR is the front-end language to DAWN
(https://github.com/MeteoSwiss-APN/dawn), a tool which generates
optimised cuda, or gridtools code. Thus these simple Fortran examples
can be transformed to optimised cuda and/or gridtools code by using
PSyclone and then DAWN.
