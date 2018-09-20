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
from __future__ import print_function, absolute_import

'''Tests for OpenCL PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

import os
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
from psyclone.generator import GenerationError, ParseError
from psyclone.transformations import OCLTrans
from psyclone_test_utils import get_invoke

API = "gocean1.0"


def test_use_stmts():
    ''' Test that generating code for OpenCL results in the correct
    module use statements '''
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


@pytest.mark.xfail(reason="DeclGen does not support character")
def test_psy_init():
    ''' Check that we create a psy_init() routine that sets-up the
    OpenCL environment '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    print(generated_code)
    expected = '''\
    SUBROUTINE psy_init()
      USE ocl_env_mod, ONLY: ocl_env_init, add_kernels
      CHARACTER, LEN(40) :: kernel_names(1)
      ! Initialise the OpenCL environment/device
      CALL ocl_env_init
      ! The kernels this PSy layer module requires
      kernel_names(1) = "compute_cu_code"
      ! Create the OpenCL kernel objects. Expects to find all of the compiled 
      ! kernels in PSYCLONE_KERNELS_FILE.
      CALL add_kernels(1, kernel_names)
    END SUBROUTINE psy_init
'''
    assert expected in generated_code


def test_set_kern_args():
    ''' Check that we generate the necessary code to set kernel arguments '''
    psy, _ = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    from psyclone.transformations import OCLTrans
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


@pytest.mark.xfail(reason="Fail to handle kernel arguments passed by value")
def test_set_arg_const_scalar():  # pylint:disable=invalid-name
    ''' Check that using a const scalar as parameter works when setting
    kernel arguments. '''
    psy, _ = get_invoke("test00.1_invoke_kernel_using_const_scalar.f90",
                        API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    otrans.apply(sched)
    generated = str(psy.gen)
    print(generated)
    assert "clSetKernelArg(kernel_obj, 1, C_SIZEOF(0), C_LOC(0))" not in \
        generated
