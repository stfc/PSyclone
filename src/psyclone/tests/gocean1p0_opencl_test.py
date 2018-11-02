# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ----------------------------------------------------------------------------
# Author A. R. Porter, STFC Daresbury Lab

'''Tests for OpenCL PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

from __future__ import print_function, absolute_import
import pytest
from psyclone.transformations import OCLTrans
from psyclone_test_utils import get_invoke

API = "gocean1.0"


def test_use_stmts():
    ''' Test that generating code for OpenCL results in the correct
    module use statements. '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen).lower()
    print(generated_code)
    expected = '''\
    subroutine invoke_0_compute_cu(cu_fld, p_fld, u_fld)
      use fortcl, only: create_rw_buffer
      use fortcl, only: get_num_cmd_queues, get_cmd_queues, get_kernel_by_name
      use clfortran
      use iso_c_binding'''
    assert expected in generated_code
    assert "if (first_time) then" in generated_code


def test_psy_init():
    ''' Check that we create a psy_init() routine that sets-up the
    OpenCL environment. '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    print(generated_code)
    expected = (
        "    SUBROUTINE psy_init()\n"
        "      USE fortcl, ONLY: ocl_env_init, add_kernels\n"
        "      CHARACTER(LEN=30) kernel_names(1)\n"
        "      LOGICAL, save :: initialised=.False.\n"
        "      ! Check to make sure we only execute this routine once\n"
        "      IF (.not. initialised) THEN\n"
        "        initialised = .True.\n"
        "        ! Initialise the OpenCL environment/device\n"
        "        CALL ocl_env_init\n"
        "        ! The kernels this PSy layer module requires\n"
        "        kernel_names(1) = \"compute_cu_code\"\n"
        "        ! Create the OpenCL kernel objects. Expects to find all of "
        "the compiled\n"
        "        ! kernels in PSYCLONE_KERNELS_FILE.\n"
        "        CALL add_kernels(1, kernel_names)\n"
        "      END IF \n"
        "    END SUBROUTINE psy_init\n")

    assert expected in generated_code


def test_set_kern_args():
    ''' Check that we generate the necessary code to set kernel arguments. '''
    psy, _ = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    # Check we've only generated one set-args routine
    assert generated_code.count("SUBROUTINE compute_cu_code_set_args("
                                "kernel_obj, nx, cu_fld, p_fld, u_fld)") == 1
    # Declarations
    expected = '''\
    SUBROUTINE compute_cu_code_set_args(kernel_obj, nx, cu_fld, p_fld, u_fld)
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: c_sizeof, c_loc, c_intptr_t
      USE ocl_utils_mod, ONLY: check_status
      INTEGER ierr
      INTEGER(KIND=c_intptr_t), target :: cu_fld, p_fld, u_fld
      INTEGER(KIND=c_intptr_t), target :: kernel_obj'''
    assert expected in generated_code
    expected = '''\
      ! Set the arguments for the compute_cu_code OpenCL Kernel
      ierr = clSetKernelArg(kernel_obj, 0, C_SIZEOF(nx), C_LOC(nx))
      ierr = clSetKernelArg(kernel_obj, 1, C_SIZEOF(cu_fld), C_LOC(cu_fld))
      CALL check_status('clSetKernelArg: arg 1 of compute_cu_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 2, C_SIZEOF(p_fld), C_LOC(p_fld))
      CALL check_status('clSetKernelArg: arg 2 of compute_cu_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 3, C_SIZEOF(u_fld), C_LOC(u_fld))
      CALL check_status('clSetKernelArg: arg 3 of compute_cu_code', ierr)
    END SUBROUTINE compute_cu_code_set_args'''
    assert expected in generated_code
    assert generated_code.count("SUBROUTINE time_smooth_code_set_args("
                                "kernel_obj, nx, u_fld, "
                                "unew_fld, uold_fld)") == 1
    assert ("CALL compute_cu_code_set_args(kernel_compute_cu_code, "
            "p_fld%grid%nx, cu_fld%device_ptr, p_fld%device_ptr, "
            "u_fld%device_ptr)" in generated_code)


def test_set_kern_float_arg():
    ''' Check that we generate correct code to set a real, scalar kernel
    argument. '''
    psy, _ = get_invoke("single_invoke_scalar_float_arg.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    print(generated_code)
    expected = '''\
    SUBROUTINE bc_ssh_code_set_args(kernel_obj, nx, a_scalar, ssh_fld, tmask)
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: c_sizeof, c_loc, c_intptr_t
      USE ocl_utils_mod, ONLY: check_status
      REAL(KIND=wp), intent(in), target :: a_scalar
      INTEGER ierr
      INTEGER(KIND=c_intptr_t), target :: ssh_fld, tmask
      INTEGER(KIND=c_intptr_t), target :: kernel_obj
      INTEGER, target :: nx
'''
    assert expected in generated_code
    expected = '''\
      ! Set the arguments for the bc_ssh_code OpenCL Kernel
      ierr = clSetKernelArg(kernel_obj, 0, C_SIZEOF(nx), C_LOC(nx))
      ierr = clSetKernelArg(kernel_obj, 1, C_SIZEOF(a_scalar), C_LOC(a_scalar))
      CALL check_status('clSetKernelArg: arg 1 of bc_ssh_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 2, C_SIZEOF(ssh_fld), C_LOC(ssh_fld))
      CALL check_status('clSetKernelArg: arg 2 of bc_ssh_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 3, C_SIZEOF(tmask), C_LOC(tmask))
      CALL check_status('clSetKernelArg: arg 3 of bc_ssh_code', ierr)
    END SUBROUTINE bc_ssh_code_set_args'''
    assert expected in generated_code


def test_set_arg_const_scalar():  # pylint:disable=invalid-name
    ''' Check that an invoke that passes a scalar kernel argument by
    value is rejected. (We haven't yet implemented the necessary code for
    setting the value of such an argument in OpenCL.) '''
    psy, _ = get_invoke("test00.1_invoke_kernel_using_const_scalar.f90",
                        API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    with pytest.raises(NotImplementedError) as err:
        otrans.apply(sched)
    assert ("Cannot generate OpenCL for Invokes that contain kernels with "
            "arguments passed by value" in str(err))
