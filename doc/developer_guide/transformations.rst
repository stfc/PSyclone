.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
.. Written by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

.. testsetup::

    # Define SOURCE_FILE to point to an existing gocean 1.0 file.
    SOURCE_FILE = ("../../src/psyclone/tests/test_files/"
        "gocean1p0/test11_different_iterates_over_one_invoke.f90")


Transformations
###############

Kernel Transformations
======================

PSyclone is able to perform kernel transformations by obtaining the PSyIR
representation of the kernel with:

.. automethod:: psyclone.psyGen.CodedKern.get_kernel_schedule

The result of `psyclone.psyGen.Kern.get_kernel_schedule` is a
`psyclone.psyir.nodes.KernelSchedule` which is a specialisation of the
`Routine` class with the `is_program` and `return_type` properties set to
`False` and `None`, respectively.

In addition to modifying the kernel PSyIR with the desired transformations,
the `modified` flag of the `CodedKern` node has to be set. This will let
PSyclone know which kernel files it may have to rename and rewrite
during the code generation.

Raising Transformations
=======================

Whenever the PSyIR is created from existing source code using one of
the frontends, the result is language-level PSyIR. That is, it
contains only nodes that can be mapped directly into a language such
as C or Fortran by one of the PSyIR backends. In order to utilise
domain-specific knowledge, this language level PSyIR must be 'raised'
to a domain-specific PSyIR. The resulting PSyIR will then contain
nodes representing higher-level concepts such as kernels or halo
exchanges. This raising is performed by means of the transformations
listed in the sub-sections below.

Raising Transformations for the NEMO API
----------------------------------------

The top-level raising transformation creates NEMO PSy layer PSyIR:

.. autoclass:: psyclone.domain.nemo.transformations.CreateNemoPSyTrans

This transformation is itself implemented using three separate transformations:

.. autoclass:: psyclone.domain.nemo.transformations.CreateNemoKernelTrans
.. autoclass:: psyclone.domain.nemo.transformations.CreateNemoLoopTrans
.. autoclass:: psyclone.domain.nemo.transformations.CreateNemoInvokeScheduleTrans

Raising Transformations for the LFRic API
-----------------------------------------

.. autoclass:: psyclone.domain.lfric.transformations.LFRicAlgTrans

.. autoclass:: psyclone.domain.lfric.transformations.LFRicRaiseCall2InvokeTrans

Algorithm Transformations
=========================

In order to generate the transformed version of the algorithm with normal
subroutine calls to PSy-layer routines, PSyclone provides a transformation that
converts an individual ``AlgorithmInvokeCall`` into a ``Call`` to an
appropriate subroutine:

.. autoclass:: psyclone.domain.common.transformations.AlgInvoke2PSyCallTrans

Algorithm Transformations for the LFRic API
-------------------------------------------

Since the LFRic API has the concept of Builtin kernels, there is more work
to do when transforming an invoke into a call to a PSy layer routine and
therefore there is a specialised class for this:

.. autoclass:: psyclone.domain.lfric.transformations.LFRicAlgInvoke2PSyCallTrans


OpenACC
=======

PSyclone is able to generate code for execution on a GPU through the
use of OpenACC. Support for generating OpenACC code is implemented via
:ref:`transformations`. The specification of parallel regions and
loops is very similar to that in OpenMP and does not require any
special treatment.  However, a key feature of GPUs is the fact that
they have their own, on-board memory which is separate from that of
the host. Managing (i.e. minimising) data movement between host and
GPU is then a very important part of obtaining good performance.

Since PSyclone operates at the level of Invokes, it has no information
about when an application starts and thus no single place in which to
initiate data transfers to a GPU. (We assume that the host is
responsible for model I/O and therefore for populating fields with
initial values.) Fortunately, OpenACC provides support for this kind of
situation with the ``enter data`` directive. This may be used to
"define scalars, arrays and subarrays to be allocated in the current
device memory for the remaining duration of the program"
:cite:`openacc_enterdata`. The ``ACCEnterDataTrans`` transformation adds
an ``enter data`` directive to an Invoke:

.. autoclass:: psyclone.transformations.ACCEnterDataTrans
   :noindex:

The resulting generated code will then contain an ``enter data``
directive.

Of course, a given field may already be on the device (and have been
updated) due to a previous Invoke. In this case, the fact that the
OpenACC run-time does not copy over the now out-dated host version of
the field is essential for correctness.

In order to support the incremental porting and/or debugging of an
application, PSyclone also supports the OpenACC ``data`` directive
that creates a statically-scoped data region. See the
description of the ``ACCDataTrans`` transformation in the
:ref:`sec_transformations_available` section for more details.

.. _opencl_dev:

OpenCL
======

PSyclone is able to generate an OpenCL :cite:`opencl` version of
PSy-layer code for the GOcean 1.0 API and its associated kernels.
Such code may then be executed on devices such as GPUs and FPGAs
(Field-Programmable Gate Arrays).

The PSyKAl model of calling kernels for pre-determined iteration
spaces is a natural fit to OpenCL's concept of an
``NDRangeKernel``. However, the kernels themselves must be created or
loaded at runtime, their arguments explicitly set and any arrays
copied to the compute device. All of this 'boilerplate' code is
generated by PSyclone. In order to minimise the changes required, the
generated code is still Fortran and makes use of the FortCL library
(https://github.com/stfc/FortCL) to access OpenCL functionality. We
could of course generate the PSy layer in C instead but this would
require further extension of PSyclone.

Consider the following invoke:

.. code-block:: fortran

    call invoke( compute_cu(CU_fld, p_fld, u_fld) )

When creating the OpenCL PSy layer for this invoke, PSyclone creates
three subroutines instead of the usual one. The first, ``psy_init``
is responsible for ensuring that a valid kernel object is created
for each kernel called by the invoke, e.g.:

.. code-block:: fortran

    use fortcl, only: ocl_env_init, add_kernels
    ...
    ! Initialise the OpenCL environment/device
    CALL ocl_env_init
    ! The kernels this PSy layer module requires
    kernel_names(1) = "compute_cu_code"
    ! Create the OpenCL kernel objects. Expects to find all of the
    ! compiled kernels in PSYCLONE_KERNELS_FILE.
    CALL add_kernels(1, kernel_names)

As indicated in the comment, the ``FortCL::add_kernels`` routine
expects to find all kernels in a pre-compiled file pointed to by the
PSYCLONE_KERNELS_FILE environment variable. (A pre-compiled file is
used instead of run-time kernel compilation in order to support
execution on FPGAs.)

The second routine created by PSyclone sets the kernel arguments, e.g.:

.. code-block:: fortran

    SUBROUTINE compute_cu_code_set_args(kernel_obj, nx, cu_fld, p_fld, u_fld)
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: c_sizeof, c_loc, c_intptr_t
      ...
      INTEGER(KIND=c_intptr_t), target :: cu_fld, p_fld, u_fld
      INTEGER(KIND=c_intptr_t), target :: kernel_obj
      INTEGER, target :: nx
      ! Set the arguments for the compute_cu_code OpenCL Kernel
      ierr = clSetKernelArg(kernel_obj, 0, C_SIZEOF(nx), C_LOC(nx))
      ierr = clSetKernelArg(kernel_obj, 1, C_SIZEOF(cu_fld), C_LOC(cu_fld))
      ...
    END SUBROUTINE compute_cu_code_set_args

The third routine generated is the ususal psy-layer routine that is
responsible for calling all of the kernels. However, it must now also
call ``psy_init``, create buffers on the compute device (if they are
not already present) and copy data over:

.. code-block:: fortran

    SUBROUTINE invoke_compute_cu(...)
      ...
      IF (first_time) THEN
        first_time = .false.
        CALL psy_init
        num_cmd_queues = get_num_cmd_queues()
        cmd_queues => get_cmd_queues()
        kernel_compute_cu_code = get_kernel_by_name("compute_cu_code")
      END IF 
      globalsize = (/p_fld%grid%nx, p_fld%grid%ny/)
      ! Ensure field data is on device
      IF (.NOT. cu_fld%data_on_device) THEN
        size_in_bytes = int(p_fld%grid%nx*p_fld%grid%ny, 8)* &
                        c_sizeof(cu_fld%data(1,1))
        ! Create buffer on device
        cu_fld%device_ptr = create_rw_buffer(size_in_bytes)
        ierr = clEnqueueWriteBuffer(cmd_queues(1), cu_fld%device_ptr,  &
                                CL_TRUE, 0, size_in_bytes,       &
                                C_LOC(cu_fld%data), 0, C_NULL_PTR, &
                                C_LOC(write_event))
        cu_fld%data_on_device = .true.
      END IF
      ...
    END SUBROUTINE

Note that we use the ``data_on_device`` member of the field derived
type (implemented in github.com/stfc/dl_esm_inf) to keep track of
whether a given field has been copied to the compute device.  Once all
of this setup is done, the kernel itself is launched by calling
``clEnqueueNDRangeKernel``:

.. code-block:: fortran

    ierr = clEnqueueNDRangeKernel(cmd_queues(1), kernel_compute_cu_code, &
                                  2, C_NULL_PTR, C_LOC(globalsize),      &
                                  C_NULL_PTR, 0, C_NULL_PTR, C_NULL_PTR)

Limitations
-----------

The current implementation only supports the conversion of a single whole
Invoke to use OpenCL. In the future we may refine this functionality
so that it may be applied to just a subset of kernels within an
Invoke and/or to multiple invokes.

Since PSyclone knows nothing about the I/O performed by a model, the
task of ensuring that the correct data is written out by a model
(including when doing halo exchanges for distributed memory) is left
to the dl_esm_inf library since that has the information on whether
field data is local or on a remote compute device. How the data is sent or
retrieved from the OpenCL device is provided by the dl_esm_inf
``read_from_device_*`` and ``write_to_device_*`` function pointers.
In the current implementation it does a just-when-is-needed synchronous data
transfer using a single command queue which can bottleneck the OpenCL
performance if there are many I/O operations.


ArrayRange2LoopTrans
====================

The ArrayRange2LoopTrans transformation has the following known
issues:

1) code in the NEMO API remains unchanged after this transformation is
   applied. This is the case for all transformations that manipulate
   the PSyIR as the NEMO API currently manipulates and outputs the
   underlying fparser2 tree. In the future the NEMO API will output
   code from the PSyIR representation using the back-ends provided.

2) if the indices of the ranges in different array accesses that are
   going to be modified to use a loop index are not the same then the
   transformation raises an exception. For example ``a(1:2) =
   b(2:3)``. Issue #814 captures removing this limitation.

3) at the moment, to test whether two loop ranges are the same, we
   first check whether they both access the full bounds of the
   array. If so we assume that they are the same (otherwise the code
   will not run). If this is not the case, then PSyclone uses :ref:`SymPy`
   for comparing ranges, which will consider the two ranges
   ``range(1:n+1:1)`` and ``range(1:1+n:1)`` to be equal.

4) there is a test for non-elementwise operations on the rhs of an
   assignment as it is not possible to turn this into an explicit
   loop. At the moment, the type of data that a PSyIR Expression Node
   returns can not be determined, so it is not possible to check
   directly for a non-elementwise operation. Fixing this issue is the
   subject of #685. For the moment the test just checks for MATMUL as
   that is currently the only non-elementwise operation in the PSyiR.

OpenMP Tasking
==============
OpenMP tasking is supported in PSyclone, currently by the combination
of the `OMPTaskloopTrans` and the `OMPTaskwaitTrans`.
Dependency analysis and handling is done by the `OMPTaskwaitTrans`,
which uses its own `get_forward_dependence` function to compute them.

get_forward_dependence
------------------------
This function searches through the current section of the PSyIR tree for
the given taskloop's next forward dependency, using the dependency analysis
tools provided in `psyclone.psyir.tools.dependency_tools`. It searches
through the tree for all `Loop`, `OMPDoDirective`, `OMPTaskloopDirective`,
and `OMPTaskwaitDirective`. It then iterates forward through these until it
finds:

1) A `Loop`, `OMPDoDirective`, or `OMPTaskloopDirective` which contains a 
   Read-after-Write (RaW) or Write-after-Read (WaR) dependency, in which
   case that node is returned as the next dependence if it is contained
   within the same `OMPSerialDirective`. If it is not contained within
   the same `OMPSerialDirective`, the taskloop's parent
   `OMPSingleDirective` is returned instead, provided it has no
   `nowait` clause associated with it.

2) An `OMPTaskloopDirective` within the same `OMPSingleDirective`
   provided the single region has no `nowait` clause associated with it.
   If this criteria is satisfied the taskloop directive is returned.

The forward dependency will never be a child node of the provided taskloop,
and the dependency's `abs_position` will always be great than 
`taskloop.abs_position`.

The RaW and WaR dependencies are computed by gathering all of the variable
accesses contained inside the relevant directive's subtrees (other than 
loop variables which are ignored), and checking for collisions between the lists.
If those collisions are not both read-only, then we know there must be a RaW or
WaR dependency.

If no dependency is found, then `None` is returned.

If a taskloop has no `nogroup` clause associated, it will be skipped over
during the `OMPTaskwaitTransformation.apply` call, as any solvable dependencies
will be satisfied by the implicit taskgroup.

These structures are the only way to satisfy dependencies between taskloops,
and any other structures of dependendent taskloops will be caught by the
`OMPTaskwaitTransformation.validate` call, which will raise an Error explaining
why the dependencies cannot be resolved.
