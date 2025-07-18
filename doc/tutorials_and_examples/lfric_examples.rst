.. _examples_lfric:

LFRic Examples
==============

These examples illustrate the functionality of PSyclone for the LFRic
domain.

Example 1: Basic Operation
--------------------------

Basic operation of PSyclone with an ``invoke()`` containing two
kernels, one :ref:`user-supplied <lfric-kernel>`, the other a
:ref:`Built-in <lfric-built-ins>`. Code is generated both with and
without distributed-memory support. Also demonstrates the use of the
``-d`` flag to specify where to search for user-supplied kernel code
(see :ref:`psyclone_command` section for more details).

Example 2: Applying Transformations
-----------------------------------

A more complex example showing the use of PSyclone
:ref:`transformations <lfric-api-transformations>` to
change the generated PSy-layer code. Provides examples of
kernel-inlining and loop-fusion transformations.

Example 3: Distributed and Shared Memory
----------------------------------------

Shows the use of colouring and OpenMP for the LFRic domain. Includes
multi-kernel, named invokes with both user-supplied and built-in
kernels. Also shows the use of ``Wchi`` function space metadata for
coordinate fields in LFRic.

Example 4: Multiple Built-ins, Named Invokes and Boundary Conditions
--------------------------------------------------------------------

Demonstrates the use of the special ``enforce_bc_kernel`` which
PSyclone recognises as a boundary-condition kernel.

Example 5: Stencils
-------------------

Example of kernels which require stencil information.

Example 6: Reductions
---------------------

Example of applying OpenMP to an InvokeSchedule containing kernels
that perform reduction operations. Two scripts are provided, one of
which demonstrates how to request that PSyclone generate code for a
reproducible OpenMP reduction. (The default OpenMP reduction is not
guaranteed to be reproducible from one run to the next on the same
number of threads.)

Example 7: Column-Matrix Assembly Operators
-------------------------------------------

Example of kernels requiring Column-Matrix Assembly operators.

Example 8: Redundant Computation
--------------------------------

Example of the use of the redundant-computation and move
transformations to eliminate and re-order halo exchanges.

Example 9: Writing to Discontinuous Fields
------------------------------------------

Demonstrates the behaviour of PSyclone for kernels that read and write
quantities on horizontally-discontinuous function spaces. In addition,
this example demonstrates how to write a PSyclone transformation script
that only colours loops over continuous spaces.

Example 10: Inter-grid Kernels
------------------------------

Demonstrates the use of "inter-grid" kernels that prolong or restrict
fields (map between grids of different resolutions), as well as the
use of ``ANY_DISCONTINUOUS_SPACE`` function space metadata.

Example 11: Asynchronous Halo Exchanges
---------------------------------------

Example of the use of transformations to introduce redundant computation,
split synchronous halo exchanges into asynchronous exchanges (start and
stop) and move the starts of those exchanges in order to overlap them
with computation.

Example 12: Code Extraction
---------------------------

Example of applying code extraction to Nodes in an Invoke Schedule:

.. code-block:: bash

  > psyclone -nodm -s ./extract_nodes.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

or to a Kernel in an Invoke after applying transformations:

.. code-block:: bash

  > psyclone -nodm -s ./extract_kernel_with_transformations.py \
      gw_mixed_schur_preconditioner_alg_mod.x90

This example also contains a Python helper script ``find_kernel.py``
which displays the names and Schedules of Invokes containing call(s)
to the specified Kernel:

.. code-block:: bash

  > python find_kernel.py

Example 13 : Kernel Transformation
----------------------------------

Demonstrates how an LFRic kernel can be transformed. The example
transformation makes Kernel values constant where appropriate. For
example, the number of levels is usually passed into a kernel by
argument but the transformation allows a particular value to be
specified which the transformation then sets as a parameter in the
kernel. Hard-coding values in a kernel helps the compiler to do a
better job when optimising the code.

Example 14: OpenACC
-------------------

Example of adding OpenACC directives in the LFRic API.
A single transformation script (``acc_parallel.py``) is provided
which demonstrates how to add OpenACC Kernels and Enter Data
directives to the PSy-layer. It supports distributed memory being
switched on by placing an OpenACC Kernels directive around each
(parallelisable) loop, rather than having one for the whole invoke.
This approach avoids having halo exchanges within an OpenACC Parallel
region. The script also uses :ref:`ACCRoutineTrans <available_kernel_trans>`
to transform the one user-supplied kernel through
the addition of an ``!$acc routine`` directive. This ensures that the
compiler builds a version suitable for execution on the accelerator (GPU).

This script is used by the supplied Makefile. The invocation of PSyclone
within that Makefile also specifies the ``--profile invokes`` option so that
each ``invoke`` is enclosed within profiling calipers (by default the
'template' profiling library supplied with PSyclone is used at the link
stage). Compilation of the example using the NVIDIA compiler may be performed
by e.g.:

.. code-block:: bash
		
   > F90=nvfortran F90FLAGS="-acc -Minfo=all" make compile

Launching the resulting binary with ``NV_ACC_NOTIFY`` set will show details
of the kernel launches and data transfers:

.. code-block:: bash

   > NV_ACC_NOTIFY=3 ./example_openacc
   ...
     Step             5 : chksm =    2.1098315506694516E-004
     PreStart called for module 'main_psy' region 'invoke_2-setval_c-r2'
    upload CUDA data  file=PSyclone/examples/lfric/eg14/main_psy.f90 function=invoke_2 line=183 device=0 threadid=1 variable=.attach. bytes=144
    upload CUDA data  file=PSyclone/examples/lfric/eg14/main_psy.f90 function=invoke_2 line=183 device=0 threadid=1 variable=.attach. bytes=144
    launch CUDA kernel  file=PSyclone/examples/lfric/eg14/main_psy.f90 function=invoke_2 line=186 device=0 threadid=1 num_gangs=5 num_workers=1 vector_length=128 grid=5 block=128
     PostEnd called for module 'main_psy' region 'invoke_2-setval_c-r2'
    download CUDA data  file=PSyclone/src/psyclone/tests/test_files/lfric/infrastructure//field/field_r64_mod.f90 function=log_minmax line=756 device=0 threadid=1 variable=self%data(:) bytes=4312
    20230807214504.374+0100:INFO : Min/max minmax of field1 =   0.30084014E+00  0.17067212E+01
   ...

However, performance will be very poor as, with the limited
optimisations and directives currently applied, the NVIDIA compiler
refuses to run the user-supplied kernel in parallel.

Example 15: CPU Optimisation of Matvec
--------------------------------------

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
------------------------------------------------------------

This example shows how LFRic-specific PSyIR can be used to create
LFRic kernel code. There is one Python script provided which when run:

.. code-block:: bash

   > python create.py

will print out generated LFRic kernel code. The script makes use of
LFRic-specific data symbols to simplify code generation.

Example 17: Runnable Simplified Examples
----------------------------------------

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
use of :ref:`kernel extraction <psyke>`. The code can be compiled with
``make compile``, and the binary executed with either ``make run`` or
``./extract.binary``. By default, it will be using
a stand-alone extraction library using a Fortran binary format
(see :ref:`extraction_libraries`).
If you want to use the NetCDF version, set the environment variable
``TYPE`` to be ``netcdf``:

.. code-block:: bash

    TYPE=netcdf make compile

This requires the installation of a NetCDF development environment
(see `here
<https://github.com/stfc/PSyclone/tree/master/tutorial/practicals#user-content-netcdf-library-lfric-examples>`_
for installing NetCDF). The binary will be called ``extract.netcdf``,
and the output files will have the ``.nc`` extension.

Similarly, you can use ``TYPE==ascii`` to use an ASCII output format.

Running the compiled binary will create two Fortran binary files (or
two NetCDF files if the NetCDF library was used, or ASCII files if
ASCII output was used). They contain
the input and output parameters for the two invokes in this example:

.. code-block:: bash

    cd full_example_extraction
    TYPE=netcdf make compile
    ./extract.netcdf
    ncdump ./main-update.nc | less


Example 18: Special Accesses of Continuous Fields - Incrementing After Reading and Writing Before (Potentially) Reading
-----------------------------------------------------------------------------------------------------------------------

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
is being updated. As :ref:`described <iterators_continuous>`
in the Developer Guide, this means that annexed DoFs are computed
correctly without the need to iterate into the L1 halo and thus can
remove the need for halo exchanges on those fields that are read.

Example 19: Mixed Precision
---------------------------

This example shows the use of the LFRic :ref:`mixed-precision support
<lfric-mixed-precision>` to call a kernel with :ref:`scalars
<lfric-mixed-precision-scalars>`, :ref:`fields <lfric-mixed-precision-fields>`
and :ref:`operators <lfric-mixed-precision-lma-operators>` of different
precision.

.. _lfric_alg_gen_example:

Example 20: Algorithm Generation
--------------------------------

Illustration of the use of the ``psyclone-kern`` tool to create an
algorithm layer for a kernel. A makefile is provide that also
runs ``psyclone`` to create an executable program from the generated
algorithm layer and original kernel code. To see the generated
algorithm layer run:

.. code-block:: bash

    cd eg20/
    psyclone-kern -gen alg ../code/testkern_mod.F90
