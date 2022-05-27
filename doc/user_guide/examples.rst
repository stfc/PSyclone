.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018-2021, Science and Technology Facilities Council.
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
.. Written by: R. W. Ford and A. R. Porter, STFC Daresbury Lab
.. Modified by I. Kavcic, Met Office
.. Modified by J. Henrichs, Bureau of Meteorology

.. _examples:

Examples
========

Various examples of the use of PSyclone are provided under the
``examples`` directory in the Git repository. If you have installed
PSyclone using ``pip`` then the examples may be found in
``share/psyclone/examples`` under your Python installation
(see :ref:`here <getting-going-env-loc>` for possible locations).

Running any of these examples requires that PSyclone be installed on
the host system, see Section :ref:`Getting Going <getting-going>`.
This section is intended to provide an overview of the various examples
so that a user can find one that is appropriate to them. For details of
what each example does and how to run each example please see the
``README.md`` files in the associated directories.

Alternatively, some of the examples have associated Jupyter notebooks
that may be launched with Binder on `MyBinder <https://mybinder.org/>`_.
This is most easily done by following the links from the top-level
`README <https://github.com/stfc/PSyclone#user-content-try-it-on-binder>`_.

For the purposes of correctness checking, the whole suite of examples
may be executed using Gnu ``make`` (this functionality is used by GitHub
Actions alongside the test suite). The default target is ``transform`` which
just performs the PSyclone code transformation steps for each
example. For those examples that support it, the ``compile`` target
also requests that the generated code be compiled. The ``notebook``
target checks the various Jupyter notebooks using ``nbconvert``.

.. note:: As outlined in the :ref:`Run <getting-going-run>` section, if
          working with the examples from a PSyclone installation, it is
          advisable to copy the whole ``examples`` directory to some
          convenient location before running them. If you have copied the
          ``examples`` directory but still wish to use ``make`` then you
          will also have to set the ``PSYCLONE_CONFIG`` environment variable
          to the full path to the PSyclone configuration file, e.g.
          ``$ PSYCLONE_CONFIG=/some/path/psyclone.cfg make``.

.. _examples-compilation:

Compilation
-----------

Some of the examples support compilation (and some even execution of
a compiled binary). Please consult the ``README.md`` to check which ones
can be compiled and executed.

As mentioned above, by default each example will execute the
``transform`` target, which performs the PSyclone code transformation
steps. In order to compile the sources, use the target ``compile``:

.. code-block:: bash

    make compile

which will first perform the transformation steps before compiling
any created Fortan source files. If the example also supports running
a compiled and linked binary, use the target:

.. code-block:: bash

    make run

This will first trigger compilation using the ``compile`` target, and
then execute the program with any parameters that might be required
(check the corresponding ``README.md`` document for details).

All ``Makefile``\s support the variables ``F90`` and ``F90FLAGS`` to specify
the compiler and compilation flags to use. By default, the Gnu Fortran
compiler (``gfortran``) is used, and the compilation flags will be set
to debugging. If you want to change the compiler or flags, just define
these as environment variables:

.. code-block:: bash

    F90=ifort F90FLAGS="-g -check bounds" make compile

To clean all compiled files (and potential output files from a run),
use:

.. code-block:: bash

    make clean

This will clean up in the ``examples`` directory. If you want to change compilers
or compiler flags, you should run ``make allclean``, see the section
about :ref:`examples_dependencies` for details.

.. _supported-compilers:

Supported Compilers
^^^^^^^^^^^^^^^^^^^

All examples have been tested with the following compilers.
Please let the developers know if you have problems using a compiler
that has been tested or if you are working with a different compiler
so it can be recorded in this table.

.. tabularcolumns:: |l|L|

======================= =======================================================
Compiler                Version
======================= =======================================================
Gnu Fortran compiler    9.3
Intel Fortran compiler  17, 21
======================= =======================================================

.. _examples_dependencies:

Dependencies
^^^^^^^^^^^^

Any required library that is included in PSyclone (typically
the infrastructure libraries for the APIs, or :ref:`PSyData wrapper
libraries <libraries>`) will automatically be compiled with the same
compiler and compilation flags as the examples.

.. note:: Once a dependent library is compiled, changing the
          compilation flags will not trigger a recompilation
          of this library. For example, if an example is first compiled
          with debug options, and later the same or a different
          example is compiled with optimisations, the dependent library
          will not automatically be recompiled!

All ``Makefile``\s support an ``allclean`` target, which will not only
clean the current directory, but also all libraries the current
example depends on.

.. important:: Using ``make allclean`` is especially important if
               the compiler is changed. Typically, one compiler cannot
               read module information from a different compiler, and
               then compilation will fail.

NetCDF
~~~~~~

Some examples require NetCDF for compilation. Installation of NetCDF
is described in detail in
`the hands-on practicals documentation
<https://github.com/stfc/PSyclone/tree/master/tutorial/practicals#user-content-netcdf-library-lfric-examples>`_.

GOcean
------

Example 1: Loop transformations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Examples of applying various transformations (loop fusion, OpenMP,
OpenMP Taskloop, OpenACC, OpenCL) to the semi-PSyKAl'd version of
the Shallow benchmark. ("semi" because not all kernels are called
from within invoke()'s.) Also includes an example of generating a
DAG from an InvokeSchedule.

Example 2: OpenACC
^^^^^^^^^^^^^^^^^^

This is a simple but complete example of using PSyclone to enable an
application to run on a GPU by adding OpenACC directives. A ``Makefile``
is included which will use PSyclone to generate the PSy code and
transformed kernels and then compile the application. This compilation
requires that the `dl_esm_inf library <https://github.com/stfc/dl_esm_inf>`_
be installed/available - it is provided as a Git submodule of the PSyclone
project (see :ref:`dev_guide:dev-installation` in the Developers' Guide
for details on working with submodules).

The supplied ``Makefile`` also provides a second, ``profile`` target which
performs the same OpenACC transformations but then encloses the whole
of the resulting PSy layer in a profiling region. By linking this with
the PSyclone NVTX profiling wrapper (and the NVTX library itself), the
resulting application can be profiled using NVIDIA's `nvprof` or
`nvvp` tools.

Example 3: OpenCL
^^^^^^^^^^^^^^^^^

Example of the use of PSyclone to generate an OpenCL driver version of
the PSy layer and OpenCL kernels. The ``Makefile`` in this example provides
a target (`make compile-ocl`) to compile the generated OpenCL code. This
requires an OpenCL implementation installed in the system. Read the README
provided in the example folder for more details about how to compile and
execute the generated OpenCL code.

Example 4: Kernels containing use statements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Transforming kernels for use with either OpenACC or OpenCL requires
that we handle those that access data and/or routines via module
``use`` statements. This example shows the various forms for which
support is being implemented. Although there is support for converting
global-data accesses into kernel arguments, PSyclone does not yet support
nested ``use`` of modules (i.e. data accessed via a module that in turn
imports that symbol from another module) and kernels that call other
kernels (Issue #342).

.. _gocean_example_psydata:

Example 5: PSyData
^^^^^^^^^^^^^^^^^^
This directory contains all examples that use the
:ref:`PSyData API<psy_Data>`. At this stage there are three
runnable examples:

Example 5.1: Kernel data extraction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example shows the use of kernel data extraction in PSyclone.
It instruments each of the two invokes in the example program
with the PSyData-based kernel extraction code.
It uses the dl_esm_inf-specific extraction library ``netcdf``
(``lib/extract/netcdf/dl_esm_inf``), and needs NetCDF to be
available (including ``nf-config`` to detect installation-specific
paths). You need to compile the NetCDF extraction library
(see :ref:`psyke_netcdf`).
The ``Makefile`` in this example will link with the compiled NetCDF
extraction library and NetCDF. You can execute the created
binary and it will create two output NetCDF files, one for
each of the two invokes.

It will also create two stand-alone driver programs (one for
each invoke), that will read the corresponding NetCDF file,
and then executes the original code.

.. note:: At this stage the driver program will not compile
    (see issue #644).

Example 5.2: Profiling
~~~~~~~~~~~~~~~~~~~~~~
This example shows how to use the profiling support in PSyclone.
It instruments two invoke statements and can link in with any
of the following profiling wrapper libraries: template,
simple_timer, dl_timer, and DrHook (see
:ref:`profiling_third_party_tools`). The ``README.md``
file contains detailed instructions on how to build the
different executables. By default (i.e. just using ``make``
without additional parameters) it links in with the
template profiling library included in PSyclone. This library just
prints out the name of the module and region before and after each
invoke is executed. This example can actually be executed to
test the behaviour of the various profiling wrappers, and is
also useful if you want to develop your own wrapper libraries.

.. _gocean_example_readonly:

Example 5.3: Read-only-verification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example shows the use of read-only-verification with PSyclone.
It instruments each of the two invokes in the example program
with the PSyData-based read-only-verification code.
It uses the dl_esm_inf-specific read-only-verification library
(``lib/read_only/dl_esm_inf/``).

.. note:: The ``update_field_mod`` subroutine contains some very
    buggy and non-standard code to change the value of some
    read-only variables and fields, even though the variables
    are all declared with
    ``intent(in)``. It uses the addresses of variables and
    then out-of-bound writes to a writeable array to
    actually overwrite the read-only variables. Using
    array bounds checking at runtime will be triggered by these
    out-of-bound writes.

The ``Makefile`` in this example will link with the compiled
read-only-verification library. You can execute the created
binary and it will print two warnings about modified
read-only variables:

.. code-block:: none

    --------------------------------------
    Double precision field b_fld has been modified in main : update
    Original checksum:   4611686018427387904
    New checksum:        4638355772470722560
    --------------------------------------
    --------------------------------------
    Double precision variable z has been modified in main : update
    Original value:    1.0000000000000000     
    New value:         123.00000000000000     
    --------------------------------------

.. _gocean_example_nan:

Example 5.4: Valid Number Verification (NaN Test)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example shows the use of valid number verification with PSyclone.
It instruments each of the two invokes in the example program
with the PSyData-based NaN-verification code.
It uses the dl_esm_inf-specific nan_test library
(``lib/nan_test/dl_esm_inf/``).

.. note:: The ``update_field_mod`` subroutine contains code
    that will trigger a division by 0 to create NaNs. If
    the compiler should add floating point exception handling
    code, this will take effect before the NaN testing is done
    by the PSyData-based verification code.

The ``Makefile`` in this example will link with the compiled
nan_test library. You can execute the created
binary and it will print five warnings about invalid numbers
at the indices  1 1, ..., 5 5:

.. code-block:: none

    PSyData: Variable a_fld has the invalid value
                     Infinity  at index/indices            1           1
    mainupdate
    ...


Example 6: PSy-layer Code Creation using PSyIR
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This example informs the development of the code generation of PSy-layer
code using the PSyIR language backends.


.. _examples_lfric:

LFRic
------

These examples illustrate the functionality of PSyclone for the LFRic
domain.

Example 1: Basic Operation
^^^^^^^^^^^^^^^^^^^^^^^^^^

Basic operation of PSyclone with an ``invoke()`` containing two
kernels, one :ref:`user-supplied <dynamo0.3-kernel>`, the other a
:ref:`Built-in <lfric-built-ins>`. Code is generated both with and
without distributed-memory support. Also demonstrates the use of the
``-d`` flag to specify where to search for user-supplied kernel code
(see :ref:`psyclone_command` section for more details).

Example 2: Applying Transformations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A more complex example showing the use of PSyclone
:ref:`transformations <dynamo0.3-api-transformations>` to
change the generated PSy-layer code. Provides examples of
kernel-inlining and loop-fusion transformations.

Example 3: Distributed and Shared Memory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Shows the use of colouring and OpenMP for the Dynamo 0.3 API. Includes
multi-kernel, named invokes with both user-supplied and built-in
kernels. Also shows the use of ``Wchi`` function space metadata for
coordinate fields in LFRic.

Example 4: Multiple Built-ins, Named Invokes and Boundary Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Demonstrates the use of the special ``enforce_bc_kernel`` which
PSyclone recognises as a boundary-condition kernel.

Example 5: Stencils
^^^^^^^^^^^^^^^^^^^

Example of kernels which require stencil information.

Example 6: Reductions
^^^^^^^^^^^^^^^^^^^^^

Example of applying OpenMP to an InvokeSchedule containing kernels
that perform reduction operations. Two scripts are provided, one of
which demonstrates how to request that PSyclone generate code for a
reproducible OpenMP reduction. (The default OpenMP reduction is not
guaranteed to be reproducible from one run to the next on the same
number of threads.)

Example 7: Column-Matrix Assembly Operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Example of kernels requiring Column-Matrix Assembly operators.

Example 8: Redundant Computation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Example of the use of the redundant-computation and move
transformations to eliminate and re-order halo exchanges.

Example 9: Writing to Discontinuous Fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Demonstrates the behaviour of PSyclone for kernels that read and write
quantities on horizontally-discontinuous function spaces. In addition,
this example demonstrates how to write a PSyclone transformation script
that only colours loops over continuous spaces.

Example 10: Inter-grid Kernels
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Demonstrates the use of "inter-grid" kernels that prolong or restrict
fields (map between grids of different resolutions), as well as the
use of ``ANY_DISCONTINUOUS_SPACE`` function space metadata.

Example 11: Asynchronous Halo Exchanges
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Example of the use of transformations to introduce redundant computation,
split synchronous halo exchanges into asynchronous exchanges (start and
stop) and move the starts of those exchanges in order to overlap them
with computation.

Example 12: Code Extraction
^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

Example 13 : Kernel Transformation
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

Example 15: CPU Optimisation of Matvec
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Example of optimising the LFRic matvec kernel for CPUs. This is work
in progress with the idea being that PSyclone transformations will be
able to reproduce hand-optimised code.

There is one script which, when run:

.. code-block:: bash

   > psyclone ./matvec_opt.py ../code/gw_mixed_schur_preconditioner_alg_mod.x90

will print out the modified matvec kernel code. At the moment no
transformations are included (as they are work-in-progress) so the
code that is output is the same as the original (but looks different
as it has been translated to PSyIR and then output by the PSyIR
Fortran back-end).

Example 16: Generating LFRic Code Using LFRic-specific PSyIR
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This example shows how LFRic-specific PSyIR can be used to create
LFRic kernel code. There is one Python script provided which when run:

.. code-block:: bash

   > python create.py

will print out generated LFRic kernel code. The script makes use of
LFRic-specific data symbols to simplify code generation.

Example 17: Runnable Simplified Examples
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This directory contains three simplified LFRic examples that can be
compiled and executed - of course, a suitable Fortran compiler is
required. The examples are using a subset of the LFRic infrastructure
library, which is contained in PSyclone and which has been slightly
modified to make it easier to create stand-alone, non-MPI LFRic codes.

Example 17.1: A Simple Runnable Example
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The subdirectory ``full_example`` contains a very simple example code
that uses PSyclone to process two invokes. It uses unit-testing
code from various classes to create the required data structures like
initial grid etc. The code can be compiled with ``make compile``, and
the binary executed with either ``make run`` or ``./example``.

Example 17.2: A Simple Runnable Example With NetCDF
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The subdirectory ``full_example_netcdf`` contains code very similar
to the previous example, but uses NetCDF to read the initial grid
from the NetCDF file ``mesh_BiP128x16-400x100.nc``.
Installation of NetCDF is described in
`the hands-on practicals documentation
<https://github.com/stfc/PSyclone/tree/master/tutorial/practicals#user-content-netcdf-library-lfric-examples>`_.
The code can be compiled with ``make compile``, and
the binary executed with either ``make run`` or ``./example``.

Example 17.3: Kernel Data Extraction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The example in the subdirectory ``full_example_extract`` shows the
use of :ref:`kernel extraction <psyke>`. It requires the
installation of a NetCDF development environment (see
`here
<https://github.com/stfc/PSyclone/tree/master/tutorial/practicals#user-content-netcdf-library-lfric-examples>`_
for installing NetCDF).
The code can be compiled with ``make compile``, and
the binary executed with either ``make run`` or ``./extract``
Running the compiled binary will create one NetCDF file ``main-update.nc``
containing the input and output parameters for the ``testkern_w0``
kernel call. For example:

.. code-block:: bash

    cd full_example_extraction
    make compile
    ./extract
    ncdump ./main-update.nc | less

Example 18: Special Accesses of Continuous Fields - Incrementing After Reading and Writing Before (Potentially) Reading
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Example containing one kernel with a ``GH_READINC`` access and one
with a ``GH_WRITE`` access, both for continuous fields. A kernel with
``GH_READINC`` access first reads the field data and then increments
the field data. This contrasts with a ``GH_INC`` access which simply
increments the field data. As an increment is effectively a read
followed by a write, it may not be clear why we need to distinguish
between these cases. The reason for distinguishing is that the
``GH_INC`` access is able to remove a halo exchange (or at least
reduce its depth by one) in certain circumstances, whereas a
``GH_READINC`` is not able to take advantage of this optimisation.

A kernel with a ``GH_WRITE`` access for a continuous field must guarantee to
write the same value to a given shared DoF, independent of which cell
is being updated. As :ref:`described <dev_guide:iterators_continuous>`
in the Developer Guide, this means that annexed DoFs are computed
correctly without the need to iterate into the L1 halo and thus can
remove the need for halo exchanges on those fields that are read.

NEMO
----

These examples may all be found in the ``examples/nemo`` directory.

Example 1: OpenMP parallelisation of tra_adv
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Demonstrates the use of PSyclone to parallelise the loops over vertical levels
in a NEMO tracer-advection benchmark using OpenMP for CPUs and for GPUs.

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

Scripts
^^^^^^^

This contains examples of two different scripts that aid the use of PSyclone
with the full NEMO model. The first, `process_nemo.py` is a simple wrapper
script that allows a user to control which source files are transformed, which
only have profiling instrumentation added and which are ignored altogether.
The second, `kernels_trans.py` is a PSyclone transformation script which
adds the largest possible OpenACC Kernels regions to the code being processed.

For more details see the ``examples/nemo/README.md`` file.

Note that these scripts are here to support the ongoing development of the
NEMO API in PSyclone. They are *not* intended as 'turn-key' solutions but
as a starting point.

PSyIR
-----

Examples may all be found in the ``examples/psyir`` directory. Read the
``README.md`` file in this directory for full details.

Example 1: Constructing PSyIR and Generating Code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``create.py`` is a Python script that demonstrates the use of the various
``create`` methods to build a PSyIR tree from scratch.

Example 2: Creating PSyIR for Structure Types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``create_structure_types.py`` demonstrates the representation of
structure types (i.e. Fortran derived types or C structs) in the PSyIR.
