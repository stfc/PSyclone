.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019, Science and Technology Facilities Council.
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

Transformations
###############

Kernel Transformations
======================

PSyclone is able to perform kernel transformations. Currently it has
two ways to apply transformations: by directly manipulating the
language AST or by translating the language AST to PSyIR, applying the
transformation in the PSyIR and using one of the back-ends to generate
the resulting code.

For now, both methods only support the fparser2 AST for kernel code.
This AST is obtained by converting the fparser1 AST (stored
when the kernel code was originally parsed to process the meta-data)
back into a Fortran string and then parsing that with fparser2.
(Note that in future we intend to adopt fparser2 throughout PSyclone so that
this translation between ASTs will be unnecessary.)
The `ast` property of the `psyclone.psyGen.Kern` class is responsible
for performing this translation the first time it is called. It also
stores the resulting AST in `Kern._fp2_ast` for return by future calls.

See `psyclone.transformations.ACCRoutineTrans` for an example of directly
manipulating the fparser2 AST.

Alternatively, one can call the `psyclone.psyGen.CodedKern.get_kernel_schedule()`
to generate the PSyIR representation of the kernel code. 

.. automethod:: psyclone.psyGen.CodedKern.get_kernel_schedule

The language AST to PSyIR transformation is done using a PSyIR front-end.
This are found in the `psyclone.psyir.frontend` module. 
The only currently available front-end is `Fparser2Reader` but this can
be specialized for by the application APIs (e.g. Nemo has `NemoFparser2Reader`
sub-class).
The naming convention used for the PSyIR front-ends is
<API><languageAST>Reader.

.. autoclass:: psyclone.psyir.frontend.fparser2.Fparser2Reader
    :members:

The results of `psyclone.psyGen.Kern.get_kernel_schedule` is a
`psyclone.psyGen.KernelSchedule` which has the same functionality as
a PSyIR Schedule but with the addition of a Symbol Table
(see :ref:`kernel_schedule-label`).

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
Such code may then be executed
on devices such as GPUs and FPGAs (Field-Programmable Gate
Arrays). Since OpenCL code is very different to that which PSyclone
normally generates, its creation is handled by ``gen_ocl`` methods
instead of the normal ``gen_code``. Which of these to use is
determined by the value of the ``InvokeSchedule.opencl`` flag.  In turn,
this is set at a user level by the ``transformations.OCLTrans``
transformation.

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

Consider the following invoke::

    call invoke( compute_cu(CU_fld, p_fld, u_fld) )

When creating the OpenCL PSy layer for this invoke, PSyclone creates
three subroutines instead of the usual one. The first, ``psy_init``
is responsible for ensuring that a valid kernel object is created
for each kernel called by the invoke, e.g.::

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

The second routine created by PSyclone sets the kernel arguments, e.g.::

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
not already present) and copy data over::

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
                                    CL_TRUE, 0_8, size_in_bytes,       &
      			            C_LOC(cu_fld%data), 0, C_NULL_PTR, &
      			            C_LOC(write_event))
        cu_fld%data_on_device = .true.
      END IF 
      ...

Note that we use the ``data_on_device`` member of the field derived
type (implemented in github.com/stfc/dl_esm_inf) to keep track of
whether a given field has been copied to the compute device.  Once all
of this setup is done, the kernel itself is launched by calling
``clEnqueueNDRangeKernel``::

    ierr = clEnqueueNDRangeKernel(cmd_queues(1), kernel_compute_cu_code, &
                                  2, C_NULL_PTR, C_LOC(globalsize),      &
				  C_NULL_PTR, 0, C_NULL_PTR, C_NULL_PTR)

Limitations
-----------

In OpenCL, all tasks to be performed (whether copying data or kernel
execution) are associated with a command queue. Tasks submitted to
different command queues may then be executed concurrently,
potentially giving greater performance. The OpenCL PSy code currently
generated by PSyclone makes use of just one command queue but again,
this could be extended in the future.

The current implementation only supports the conversion of a whole
Invoke to use OpenCL. In the future we may refine this functionality
so that it may be applied to just a subset of kernels within an
Invoke.

Since PSyclone knows nothing about the I/O performed by a model, the
task of ensuring that the correct data is written out by a model
(including when doing halo exchanges for distributed memory) is left
to the dl_esm_inf library since that has the information on whether
field data is local or on a remote compute device.

