
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council.
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
# Authors A. R. Porter and S. Siso, STFC Daresbury Lab

'''Tests for OpenCL PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

from __future__ import print_function, absolute_import
import pytest
from gocean1p0_build import GOcean1p0OpenCLBuild
from psyclone.transformations import OCLTrans
from psyclone.gocean1p0 import GOKernelSchedule
from psyclone.psyGen import GenerationError
from psyclone_test_utils import Compile, get_invoke

API = "gocean1.0"


# ----------------------------------------------------------------------------
@Compile.COMPILE_OPENCL
def test_opencl_compiler_works(tmpdir):
    ''' Check that the specified compiler works for a hello-world
    opencl example. This is done in this file to alert the user
    that all compiles tests are skipped if only the '--compile'
    command line option is used (instead of --compileopencl)
    '''
    example_ocl_code = '''
program hello
  USE fortcl
  write (*,*) "Hello"
end program hello
'''
    old_pwd = tmpdir.chdir()
    try:
        with open("hello_world_opencl.f90", "w") as ffile:
            ffile.write(example_ocl_code)
        GOcean1p0OpenCLBuild(tmpdir).\
            compile_file("hello_world_opencl.f90",
                         link=True)
    finally:
        old_pwd.chdir()


# ----------------------------------------------------------------------------
def test_use_stmts(tmpdir):
    ''' Test that generating code for OpenCL results in the correct
    module use statements. '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen).lower()
    expected = '''\
    subroutine invoke_0_compute_cu(cu_fld, p_fld, u_fld)
      use fortcl, only: create_rw_buffer
      use fortcl, only: get_num_cmd_queues, get_cmd_queues, get_kernel_by_name
      use clfortran
      use iso_c_binding'''
    assert expected in generated_code
    assert "if (first_time) then" in generated_code
    assert GOcean1p0OpenCLBuild(tmpdir).code_compiles(psy)


def test_psy_init(tmpdir):
    ''' Check that we create a psy_init() routine that sets-up the
    OpenCL environment. '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
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
    assert GOcean1p0OpenCLBuild(tmpdir).code_compiles(psy)


def test_set_kern_args(tmpdir):
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
    assert GOcean1p0OpenCLBuild(tmpdir).code_compiles(psy)


def test_set_kern_float_arg(tmpdir):
    ''' Check that we generate correct code to set a real, scalar kernel
    argument. '''
    psy, _ = get_invoke("single_invoke_scalar_float_arg.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    expected = '''\
    SUBROUTINE bc_ssh_code_set_args(kernel_obj, nx, a_scalar, ssh_fld, tmask)
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: c_sizeof, c_loc, c_intptr_t
      USE ocl_utils_mod, ONLY: check_status
      REAL(KIND=go_wp), intent(in), target :: a_scalar
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
    assert GOcean1p0OpenCLBuild(tmpdir).code_compiles(psy)


def test_set_arg_const_scalar():
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


def test_opencl_kernel_code_generation():
    ''' Tests that gen_ocl method of the GOcean Kernel Schedule generates
    the expected OpenCL code.
    '''

    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    kernel = sched.children[0].children[0].children[0]  # compute_cu kernel
    kschedule = kernel.get_kernel_schedule()

    # TODO: At the moment, due to fparser/171, the body of compute_cu
    # is not generated, so I provisionally create a simple assignment statement
    # for testing that the body of the kernel is properly generated.
    from psyclone.psyGen import Assignment, Reference, Literal
    assignment = Assignment(parent=kschedule)
    kschedule.addchild(assignment)
    ref = Reference("i", assignment)
    lit = Literal("1", assignment)
    assignment.addchild(ref)
    assignment.addchild(lit)

    expected_code = (
        "__kernel void compute_cu_code(\n"
        "    __global double * restrict cu,\n"
        "    __global double * restrict p,\n"
        "    __global double * restrict u\n"
        "    ){\n"
        "    int cuLEN1 = get_global_size(0);\n"
        "    int cuLEN2 = get_global_size(1);\n"
        "    int pLEN1 = get_global_size(0);\n"
        "    int pLEN2 = get_global_size(1);\n"
        "    int uLEN1 = get_global_size(0);\n"
        "    int uLEN2 = get_global_size(1);\n"
        "    int i = get_global_id(0);\n"
        "    int j = get_global_id(1);\n"
        "    i = 1;\n"
        "}\n"
        )

    assert expected_code in kschedule.gen_ocl()


def test_opencl_kernel_variables_definitions():
    ''' Tests that gen_ocl method of the GOcean Kernel Schedule generates
    the expected OpenCL argument/variable declarations.
    '''
    kschedule = GOKernelSchedule('test')
    kschedule.symbol_table.declare("i", "integer", [])
    kschedule.symbol_table.declare("j", "integer", [])
    kschedule.symbol_table.declare("intarg", "integer", [])
    kschedule.symbol_table.declare("realarg", "real", [])
    kschedule.symbol_table.declare("chararg", "character", [])
    kschedule.symbol_table.declare("arrayarg", "real", [3, 4, 5, 3])
    kschedule.symbol_table.declare("intvar", "integer", [])
    kschedule.symbol_table.declare("realvar", "real", [])
    kschedule.symbol_table.declare("charvar", "character", [])
    kschedule.symbol_table.declare("arrayvar", "real", [3, 4, 5, 3])
    kschedule.symbol_table.specify_argument_list(["i", "j", "intarg",
                                                  "realarg", "chararg",
                                                  "arrayarg"])
    opencl = kschedule.gen_ocl()

    # Check Arguments are part (or not) of the generated opencl
    assert "int i," not in opencl
    assert "int j," not in opencl
    assert "int intarg" in opencl
    assert "double realarg" in opencl
    assert "char chararg" in opencl
    assert "__global double * restrict arrayarg" in opencl

    # Check local variables are declared
    assert "int intvar;" in opencl
    assert "double realvar;" in opencl
    assert "char charvar;" in opencl
    assert "double realvar;" in opencl
    assert "double * restrict arrayvar;" in opencl
    assert "int arrayargLEN1 = get_global_size(0);" in opencl
    assert "int arrayargLEN2 = get_global_size(1);" in opencl
    assert "int arrayargLEN3 = get_global_size(2);" in opencl
    assert "int arrayargLEN4 = get_global_size(3);" in opencl
    assert "int i = get_global_id(0);" in opencl
    assert "int j = get_global_id(1);" in opencl


def test_opencl_kernel_gen_wrong_kernel():
    ''' Tests that gen_ocl method raises the proper error when the
    GOKernelSchedule does not represent a proper GOcean kernel.
    '''

    kschedule = GOKernelSchedule('test')

    # Test gen_ocl without any kernel argument
    with pytest.raises(GenerationError) as err:
        kschedule.gen_ocl()
    assert ("GOcean 1.0 API kernels should always have at least two "
            "arguments representing the iteration indices but the Symbol "
            "Table for kernel 'test' has only 0 argument(s).") in str(err)

    # Test gen_ocl with 1 kernel argument
    kschedule.symbol_table.declare("arg1", "integer", [], "global_argument",
                                   True, False)
    kschedule.symbol_table.specify_argument_list(["arg1"])
    with pytest.raises(GenerationError) as err:
        kschedule.gen_ocl()
    assert ("GOcean 1.0 API kernels should always have at least two "
            "arguments representing the iteration indices but the Symbol "
            "Table for kernel 'test' has only 1 argument(s).") in str(err)

    # Test gen_ocl with 2 kernel argument
    kschedule.symbol_table.declare("arg2", "integer", [], "global_argument",
                                   True, False)
    kschedule.symbol_table.specify_argument_list(["arg1", "arg2"])
    kschedule.gen_ocl()

    # Test gen_ocl with wrong iteration indices types and shapes.
    kschedule.symbol_table.lookup("arg1")._datatype = "real"
    with pytest.raises(GenerationError) as err:
        kschedule.gen_ocl()
    assert ("GOcean 1.0 API kernels first argument should be a scalar integer"
            " but got a scalar of type 'real' for kernel 'test'.")\
        in str(err)
    kschedule.symbol_table.lookup("arg1")._datatype = "integer"  # restore

    kschedule.symbol_table.lookup("arg2")._shape = [None]
    with pytest.raises(GenerationError) as err:
        kschedule.gen_ocl()
    assert ("GOcean 1.0 API kernels second argument should be a scalar integer"
            " but got an array of type 'integer' for kernel 'test'.")\
        in str(err)
    kschedule.symbol_table.lookup("arg2")._shape = []  # restore

    # Test gen_ocl with clashing variable names for array lengths.
    kschedule.symbol_table.declare("array", "integer", [None],
                                   "global_argument", True, True)
    kschedule.symbol_table.declare("arrayLEN1", "integer", [])
    kschedule.symbol_table.specify_argument_list(["arg1", "arg2", "array"])
    with pytest.raises(GenerationError) as err:
        kschedule.gen_ocl()
    assert ("Unable to declare the variable 'arrayLEN1' to store the length"
            " of 'array' because the kernel 'test' already contains a "
            "symbol with the same name.") in str(err)
