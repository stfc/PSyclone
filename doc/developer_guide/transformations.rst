.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
.. Written by R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab

.. testsetup::

    # Define GOCEAN_SOURCE_FILE to point to an existing gocean 1.0 file.
    GOCEAN_SOURCE_FILE = ("../../src/psyclone/tests/test_files/"
        "gocean1p0/test11_different_iterates_over_one_invoke.f90")
    # Define NEMO_SOURCE_FILE to point to an existing nemo file.
    NEMO_SOURCE_FILE = ("../../examples/nemo/code/tra_adv.F90")


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

Raising Transformations for the LFRic API
-----------------------------------------

.. autoclass:: psyclone.domain.lfric.transformations.LFRicAlgTrans

.. autoclass:: psyclone.domain.lfric.transformations.RaisePSyIR2LFRicAlgTrans

.. autoclass:: psyclone.domain.lfric.transformations.RaisePSyIR2LFRicKernTrans

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

Kernel Transformations for the GOCean and LFRic APIs
----------------------------------------------------

The LFRic RaisePSyIR2LFRicKernTrans and GOcean
RaisePSyIR2GOceanKernTrans translate generic PSyIR to LFRic-specific
Kernel PSyIR. At the moment these transformations are limited to
creating Python classes for LFRic or GOcean kernel metadata,
respectively. These classes allow easy reading, modification, creation
and writing back of generic Kernel PSyIR.

OpenACC
=======

PSyclone is able to generate code for execution on a GPU through the
use of OpenACC. Support for generating OpenACC code is implemented via
:ref:`transformations`. The specification of parallel regions and
loops is very similar to that in OpenMP and does not require any
special treatment.  However, a key feature of GPUs is that, typically,
they have their own, on-board memory which is separate from that of
the host. Managing (i.e. minimising) data movement between host and
GPU is then a very important part of obtaining good performance.

Since PSyclone operates at the level of Invokes for the LFRic (Dynamo0.3) and
GOcean1.0 APIs and of single routines for the NEMO API, it has no information
about where an application starts and thus no single place in which to initiate
data transfers to a GPU. (We assume that the host is responsible for model I/O
and therefore for populating fields with initial values.) Fortunately, OpenACC
provides support for this kind of situation with the ``enter data`` directive.
This may be used to "define scalars, arrays and subarrays to be allocated in
the current device memory for the remaining duration of the program"
:cite:`openacc_enterdata`. The ``ACCEnterDataTrans`` transformation adds
an ``enter data`` directive to an Invoke or a routine:

.. autoclass:: psyclone.transformations.ACCEnterDataTrans
   :noindex:

The resulting generated code will then contain an ``enter data`` directive. The
directive is placed in the body of the Invoke or the routine just before the
first of its statements containing an OpenACC parallel or kernels construct.
All the data that is accessed on the device, i.e. on at least one of all the
OpenACC parallel and kernels constructs in the Invoke or the routine, is copied
to the device's memory. For derived types, if a member is accessed on one of
these constructs, in addition to that member, its parent is also copied in
beforehand. This guarantees that, if the member is an allocatable or pointer,
we levarage the implicit pointer attach behaviour of OpenACC.

Of course, a given field may already be on the device (and have been
updated) due to a previous Invoke or routine. In this case, the fact that the
OpenACC runtime does not copy over the now outdated host version of the field
is essential for correctness.

On the other hand, if a section of the code must be executed on the host, it is
paramount it accesses an up to date version of the data and that, at the end,
any written data is returned to the device. To enable this workflow, the NEMO
API supports the OpenACC ``update`` directive with either the ``self``/``host``
or the ``device`` clause to update each target before and after a host code
section in a routine:

.. autoclass:: psyclone.psyir.transformations.ACCUpdateTrans
   :noindex:

The ``update`` directives will not necessarily be placed immediately next to
the host code section. In fact, this could lead to poor performance whenever
those sections happen to be inside a loop statement. Instead, the algorithm
tries to push the directives up the routine's body as far as legally possible
as determined by the data dependencies of parallel and kernels constructs in
the routine and potential dependencies in called routines. In addition,
whenever the scheme would place an ``update`` directive immediately next to a
previously placed ``update`` directive with the same target, these are instead
combined together.

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

Inlining
========

PSyclone supports two different inlining transformations:
``KernelModuleInlineTrans`` and ``InlineTrans``. The former is relatively
simple and creates a copy of the Kernel routine within the same Container
as the routine from which it is called. The latter is far more intrusive
and replaces a call to a routine with the actual body of that routine.
This can be complex due to the fact that Fortran allows the bounds of
arrays within a routine to differ from those at the call site, e.g.:

.. code-block:: fortran

  integer :: my_array(10)
  ...
  call my_sub(my_array)

  subroutine my_sub(x)
    integer, intent(inout), :: x(2:11)
    ...

As a consequence of this, ensuring that any array index expressions are
correctly handled when inlining the routine body will often mean that
full type information is required for every dummy argument. However, there are
exceptions that arise when the dimensions of an array are not actually
declared within the subroutine, e.g.:

.. code-block:: fortran

   type(my_type) :: var
   ...
   call my_sub(var)

   subroutine my_sub(x)
     type(my_var), intent(inout) :: x
     x%data(3:5) = ...

In this case, the definition of the array being accessed is contained
within `my_var` and is therefore common to both the call site and
the subroutine. We therefore do not require full type information
for the dummy argument `x` in order to safely inline the routine.
However, as soon as the dummy argument is in the form of an array
then we will require full type information for the corresponding
argument at both the call site and within the routine, e.g.:

.. code-block:: fortran

   type(my_type) :: var
   ...
   call my_sub(var%data)

   subroutine my_sub(x)
     real, dimension(2:5, 6) :: x
     x(2, 4) = ...

In this case, the correct index expressions for the inlined code will
depend on the bounds with which the `data` member of `my_type` is
declared and therefore this information must be available.

OpenMP Tasking with Taskloops
=============================
OpenMP taskloops are supported in PSyclone, currently by the combination
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
and any other structures of dependent taskloops will be caught by the
`OMPTaskwaitTransformation.validate` call, which will raise an Error explaining
why the dependencies cannot be resolved.

OpenMP Tasking
==============

OpenMP explicit tasking is also supported by PSyclone, by the `OMPTaskTrans`
transformation. Dependencies between tasks are handled by the task directives,
and validated by the containing `OMPSerialDirective`. `OMPTaskwaitDirective`
nodes are added to handle any dependencies not covered by the OpenMP standard.

The `OMPTaskTrans` is a parallel loop trans, so explicit tasking is only supported
on loops in PSyclone at this time.

Restrictions
------------
When using explicit tasking, PSyclone has a variety of restrictions. These
restrictions are either due to the OpenMP standard, or difficulty in
computing the dependencies at compile-time.

1) Array indices cannot be `ArrayReference` or `ArrayMember` nodes inside a
   task region.

2) Array indices that are `BinaryOperation` nodes must have one `Reference`
   and one `Literal` child, and must be either `ADD` or `SUB` operations.

3) Array indices cannot be shared variables.

4) `StructureReference` nodes cannot contain multiple `ArrayMember` or
   `ArrayOfStructureMember` children when accessed inside a task.

5) `ArrayOfStructureReference` nodes cannot contain any `ArrayMember` or
   `ArrayOfStructureMember` children when accessed inside a task.

6) The left hand side of any `Assignment` node must be a `Reference` or
   sub-class inside a task region.

7) `Loop` nodes inside task regions must not have a `shared` loop variable.

8) `Loop` nodes inside task regions must not have `ArrayReference` nodes for
   the start, stop or step expressions.

Dependency computation details
------------------------------
OpenMP task nodes in PSyclone will have 5 `Clause` children, 3 data-sharing
attribute clauses (`OMPPrivateClause`, `OMPFirstprivateClause`, `OMPSharedClause`)
and 2 dependency clauses (`OMPDependClause`, one with `IN` `DependClauseType` and
the other with `OUT` `DependClauseType`).

These are computed at code-generation time. The data-sharing clauses are based upon
the information from the ancestor `OMPParallelDirective`. All variables declared as
`private` or `firstprivate` by the ancestor parallel region will be either
`private` or `firstprivate` for the task (but not necessarily the same as for the
ancestor), while all variables declared `shared` by the ancestor parallel region
will be explicitly declared either `shared` or `firstprivate` for the task.

To help improve dependencies between tasks, PSyclone introduces the concepts of 
"parent loop variable" and "proxy loop variable".

A parent loop variable is the
loop variable of any ancestor `Loop` node that is contained in the subtree of
the ancestor `OMPParallelDirective` of the task.

A proxy loop variable is the variable of a child `Loop` node, where the loop's
start node is based upon a parent loop variable, e.g. if `i` is a parent loop
variable, then loops such as `do j=i, i+32` or `do k=i+1, i+33` would satisfy
the criteria, and both `j` and `k` would be marked as proxy loop variables.

When computing dependencies on indices that are proxy loop variables, the
dependency system will ensure that the relevant parent loop variable is
`firstprivate`, and then use the parent loop variable in the dependency
clause instead of the proxy loop variable (i.e. in the previous code segments,
dependencies based upon `j` or `k` would instead be based upon `i`). This enables
the dependencies to exist upon smaller sections of arrays, which we expect to
improve parallelism both between loop iterations and between loops.

In addition, rather than creating dependencies to array sections, PSyclone will
aim to create dependencies to individual indices of arrays, so a dependency on
`array(i:i+32)` is instead created on `array(i)`. If the array access uses a
`BinaryOperation` for indexing, the array will appear multiple times in the
`depend` clause, with each index being based upon the step size of the containing
loop where possible. If PSyclone cannot reduce the dependency to a single element,
then the full array will be indexed in the depend clause instead.

All array dependencies are validated by the `OMPSerialDirective`. If there are
dependencies between tasks that will not be covered by the computed `OMPDependClause`
nodes, then additional `OMPTaskwaitDirectives` will be added to ensure code
correctness.

If an OpenMP task region contains an `LBOUND`, `UBOUND` or `SIZE` intrinsic inside
an if condition or a loop condition, and that intrinsic contains an array section
then PSyclone may generate extra dependencies, which may hurt code performance. If
this causes issues, please open an issue.
