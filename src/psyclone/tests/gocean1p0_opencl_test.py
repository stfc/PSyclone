# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2021, Science and Technology Facilities Council.
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
# Modified by R. W. Ford, STFC Daresbury Lab

'''Tests for OpenCL PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

from __future__ import print_function, absolute_import
import os
import pytest

from psyclone.configuration import Config
from psyclone.transformations import OCLTrans, TransformationError
from psyclone.gocean1p0 import GOKernelSchedule
from psyclone.errors import GenerationError
from psyclone.psyir.symbols import DataSymbol, ArgumentInterface, \
    ScalarType, ArrayType, INTEGER_TYPE, REAL_TYPE
from psyclone.tests.utilities import Compile, get_invoke
from psyclone.psyir.backend.opencl import OpenCLWriter
from psyclone.tests.gocean1p0_build import GOcean1p0OpenCLBuild
from psyclone.domain.gocean.transformations import \
    GOMoveIterationBoundariesInsideKernelTrans


API = "gocean1.0"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use gocean1.0 as API.'''
    Config.get().api = "gocean1.0"
    yield()
    Config._instance = None


# ----------------------------------------------------------------------------
def test_opencl_compiler_works(kernel_outputdir):
    ''' Check that the specified compiler works for a hello-world
    opencl example. This is done in this file to alert the user
    that all compiles tests are skipped if only the '--compile'
    command line option is used (instead of --compileopencl)
    '''
    Compile.skip_if_opencl_compilation_disabled()
    example_ocl_code = '''
program hello
  USE fortcl
  write (*,*) "Hello"
end program hello
'''
    old_pwd = kernel_outputdir.chdir()
    try:
        with open("hello_world_opencl.f90", "w") as ffile:
            ffile.write(example_ocl_code)
        GOcean1p0OpenCLBuild(kernel_outputdir).\
            compile_file("hello_world_opencl.f90",
                         link=True)
    finally:
        old_pwd.chdir()


@pytest.mark.parametrize("debug_mode", [True, False])
def test_invoke_use_stmts(kernel_outputdir, monkeypatch, debug_mode):
    ''' Test that generating code for OpenCL results in the correct
    module use statements. '''
    api_config = Config.get().api_conf("gocean1.0")
    monkeypatch.setattr(api_config, "_debug_mode", debug_mode)
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule

    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen).lower()
    expected = '''\
    subroutine invoke_0_compute_cu(cu_fld, p_fld, u_fld)
      use fortcl, only: create_rw_buffer\n'''

    # When in debug mode, import also the check_status function
    if debug_mode:
        expected += "      use ocl_utils_mod, only: check_status\n"

    expected += '''\
      use fortcl, only: get_num_cmd_queues, get_cmd_queues, get_kernel_by_name
      use clfortran
      use iso_c_binding'''
    assert expected in generated_code
    assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)


def test_invoke_opencl_initialisation(kernel_outputdir):
    ''' Test that generating code for OpenCL results in the correct
    OpenCL first time initialisation code '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen).lower()

    # Test that the necessary variables are declared at the beginning
    # of the invoke
    expected = '''\
      integer(kind=c_size_t), target :: localsize(2)
      integer(kind=c_size_t), target :: globalsize(2)
      integer(kind=c_intptr_t), target :: write_event
      integer(kind=c_size_t) size_in_bytes
      integer(kind=c_intptr_t), target, save :: kernel_compute_cu_code
      logical, save :: first_time=.true.
      integer ierr
      integer(kind=c_intptr_t), pointer, save :: cmd_queues(:)
      integer, save :: num_cmd_queues'''
    assert expected in generated_code

    # Test that a conditional 'first_time' code is generated with the
    # expected initialisation statements
    expected = '''\
      if (first_time) then
        first_time = .false.
        ! ensure opencl run-time is initialised for this psy-layer module
        call psy_init
        num_cmd_queues = get_num_cmd_queues()
        cmd_queues => get_cmd_queues()
        kernel_compute_cu_code = get_kernel_by_name("compute_cu_code")
      end if'''
    assert expected in generated_code
    assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)


@pytest.mark.parametrize("debug_mode", [True, False])
def test_invoke_opencl_kernel_call(kernel_outputdir, monkeypatch, debug_mode):
    ''' Check that the Invoke OpenCL produce the expected kernel enqueue
    statement to launch OpenCL kernels. '''
    api_config = Config.get().api_conf("gocean1.0")
    monkeypatch.setattr(api_config, "_debug_mode", debug_mode)
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)

    # Set up globalsize and localsize values
    expected = '''\
      globalsize = (/p_fld%grid%nx, p_fld%grid%ny/)
      localsize = (/64, 1/)'''

    if debug_mode:
        # Check that the globalsize first dimension is a multiple of
        # the localsize first dimension
        expected += '''
      IF (mod(p_fld%grid%nx, 64) .ne. 0) THEN
        CALL check_status("Global size is not a multiple of local size \
(mandatory in OpenCL < 2.0).", -1)
      END IF'''
    assert expected in generated_code

    # Call the set_args subroutine with the boundaries corrected for the
    # OpenCL 0-indexing
    expected += '''
      CALL compute_cu_code_set_args(kernel_compute_cu_code, \
xstart - 1, xstop - 1, \
ystart - 1, ystop - 1, \
cu_fld%device_ptr, p_fld%device_ptr, u_fld%device_ptr)'''

    expected += '''
      ! Launch the kernel'''

    if debug_mode:
        # Check that there is no pending error in the queue before launching
        # the kernel
        expected += '''
      ierr = clFinish(cmd_queues(1))
      CALL check_status('Errors before compute_cu_code launch', ierr)'''

    expected += '''
      ierr = clEnqueueNDRangeKernel(cmd_queues(1), kernel_compute_cu_code, \
2, C_NULL_PTR, C_LOC(globalsize), C_LOC(localsize), 0, C_NULL_PTR, \
C_NULL_PTR)
      !'''

    if debug_mode:
        # Check that there are no errors during the kernel launch or during
        # the execution of the kernel.
        expected += '''
      CALL check_status('compute_cu_code clEnqueueNDRangeKernel', ierr)
      ierr = clFinish(cmd_queues(1))
      CALL check_status('Errors during compute_cu_code', ierr)'''

    assert expected in generated_code
    assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)


def test_grid_proprty(kernel_outputdir):
    # pylint: disable=unused-argument
    ''' Test that using nx and ny from the gocean property dictionary
    works.'''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen).lower()
    assert "globalsize = (/p_fld%grid%nx, p_fld%grid%ny/)" in generated_code
    expected = "size_in_bytes = int(p_fld%grid%nx*p_fld%grid%ny, 8)*" \
               "c_sizeof(p_fld%data(1,1))"
    assert expected in generated_code


def test_field_arguments(kernel_outputdir):
    # pylint: disable=unused-argument
    ''' Test that with an invoke transformed to OpenCL that has fields,
    all the fields are initialized into OpenCL buffers, the data is copied
    in, and a function to get the data back from the device is also
    generated.'''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen).lower()

    # The array size expression always uses the same field, in this case p_fld
    size_expression = "int(p_fld%grid%nx*p_fld%grid%ny, 8)"

    # For each of the invoke fields, add a conditional block that:
    # - Creates a OpenCL rw buffer.
    # - Writes the field data into the buffer.
    # - Marks data_on_device flag as true.
    # - Points the read_from_device_f attribute to the read_from_device
    # local function.
    # - Blocks OpenCL Queue until the data copy has finished.
    single_invoke_fields = ["cu_fld", "p_fld", "u_fld"]
    for field in single_invoke_fields:
        expected = (
            "      if (.not. {0}%data_on_device) then\n"
            "        size_in_bytes = " + size_expression +
            "*c_sizeof({0}%data(1,1))\n"
            "        ! create buffer on device\n"
            "        {0}%device_ptr = create_rw_buffer(size_in_bytes)\n"
            "        ierr = clenqueuewritebuffer(cmd_queues(1), "
            "{0}%device_ptr, cl_true, 0_8, size_in_bytes, "
            "c_loc({0}%data), 0, c_null_ptr, "
            "c_loc(write_event))\n"
            "        {0}%data_on_device = .true.\n"
            "        {0}%read_from_device_f => read_from_device\n"
            "        ! block until data copies have finished\n"
            "        ierr = clfinish(cmd_queues(1))\n"
            "      end if\n").format(field)
        assert expected in generated_code

    # Check that the read_from_device routine has also been generated.
    expected = (
        "    subroutine read_from_device(from, to, nx, ny, width)\n"
        "      use iso_c_binding, only: c_intptr_t\n"
        "      use fortcl, only: read_buffer\n"
        "      integer(kind=c_intptr_t), intent(in) :: from\n"
        "      real(kind=go_wp), intent(inout), dimension(:,:) :: to\n"
        "      integer, intent(in) :: nx, ny, width\n"
        "      call read_buffer(from, to, int(width*ny, kind=8))\n"
        "    end subroutine read_from_device\n")
    assert expected in generated_code


def test_psy_init(kernel_outputdir, monkeypatch):
    ''' Check that we create a psy_init() routine that sets-up the
    OpenCL environment. '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    expected = (
        "    SUBROUTINE psy_init()\n"
        "      USE fortcl, ONLY: ocl_env_init, add_kernels\n"
        "      CHARACTER(LEN=30) kernel_names(1)\n"
        "      INTEGER :: ocl_device_num=1\n"
        "      LOGICAL, save :: initialised=.False.\n"
        "      ! Check to make sure we only execute this routine once\n"
        "      IF (.not. initialised) THEN\n"
        "        initialised = .True.\n"
        "        ! Initialise the OpenCL environment/device\n"
        "        CALL ocl_env_init(1, ocl_device_num, .False., .False.)\n"
        "        ! The kernels this PSy layer module requires\n"
        "        kernel_names(1) = \"compute_cu_code\"\n"
        "        ! Create the OpenCL kernel objects. Expects to find all of "
        "the compiled\n"
        "        ! kernels in FORTCL_KERNELS_FILE.\n"
        "        CALL add_kernels(1, kernel_names)\n"
        "      END IF\n"
        "    END SUBROUTINE psy_init\n")
    assert expected in generated_code
    assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)

    # Test with a non-default number of OpenCL queues
    sched.coded_kernels()[0].set_opencl_options({'queue_number': 5})
    generated_code = str(psy.gen)
    expected = (
        "    SUBROUTINE psy_init()\n"
        "      USE fortcl, ONLY: ocl_env_init, add_kernels\n"
        "      CHARACTER(LEN=30) kernel_names(1)\n"
        "      INTEGER :: ocl_device_num=1\n"
        "      LOGICAL, save :: initialised=.False.\n"
        "      ! Check to make sure we only execute this routine once\n"
        "      IF (.not. initialised) THEN\n"
        "        initialised = .True.\n"
        "        ! Initialise the OpenCL environment/device\n"
        "        CALL ocl_env_init(5, ocl_device_num, .False., .False.)\n"
        "        ! The kernels this PSy layer module requires\n"
        "        kernel_names(1) = \"compute_cu_code\"\n"
        "        ! Create the OpenCL kernel objects. Expects to find all of "
        "the compiled\n"
        "        ! kernels in FORTCL_KERNELS_FILE.\n"
        "        CALL add_kernels(1, kernel_names)\n"
        "      END IF\n"
        "    END SUBROUTINE psy_init\n")
    assert expected in generated_code
    assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)

    # Test with a different configuration value for OCL_DEVICES_PER_NODE
    # that needs a mod() and a get_rank() expression.
    monkeypatch.setattr(Config.get(), "_ocl_devices_per_node", 2)
    generated_code = str(psy.gen)
    expected = (
        "    SUBROUTINE psy_init()\n"
        "      USE parallel_mod, ONLY: get_rank\n"
        "      USE fortcl, ONLY: ocl_env_init, add_kernels\n"
        "      CHARACTER(LEN=30) kernel_names(1)\n"
        "      INTEGER :: ocl_device_num=1\n"
        "      LOGICAL, save :: initialised=.False.\n"
        "      ! Check to make sure we only execute this routine once\n"
        "      IF (.not. initialised) THEN\n"
        "        initialised = .True.\n"
        "        ! Initialise the OpenCL environment/device\n"
        "        ocl_device_num = mod(get_rank() - 1, 2) + 1\n"
        "        CALL ocl_env_init(5, ocl_device_num, .False., .False.)\n"
        "        ! The kernels this PSy layer module requires\n"
        "        kernel_names(1) = \"compute_cu_code\"\n"
        "        ! Create the OpenCL kernel objects. Expects to find all of "
        "the compiled\n"
        "        ! kernels in FORTCL_KERNELS_FILE.\n"
        "        CALL add_kernels(1, kernel_names)\n"
        "      END IF\n"
        "    END SUBROUTINE psy_init\n")
    assert expected in generated_code
    assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)


def test_psy_init_with_options(kernel_outputdir):
    ''' Check that we create a psy_init() routine that sets-up the
    OpenCL environment with the provided non-default options. '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched, options={"end_barrier": True,
                                 "enable_profiling": True,
                                 "out_of_order": True})
    generated_code = str(psy.gen)
    assert "CALL ocl_env_init(1, ocl_device_num, .True., .True.)\n" \
        in generated_code
    assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)


@pytest.mark.usefixtures("kernel_outputdir")
def test_opencl_options_validation():
    ''' Check that OpenCL options which are not supported provide appropiate
    errors.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()

    # Unsupported options are not accepted
    with pytest.raises(TransformationError) as err:
        otrans.apply(sched, options={'unsupported': 1})
    assert "InvokeSchedule does not support the OpenCL option 'unsupported'." \
        in str(err.value)

    # end_barrier option must be a boolean
    with pytest.raises(TransformationError) as err:
        otrans.apply(sched, options={'end_barrier': 1})
    assert "InvokeSchedule OpenCL option 'end_barrier' should be a boolean." \
        in str(err.value)

    # enable_profiling option must be a boolean
    with pytest.raises(TransformationError) as err:
        otrans.apply(sched, options={'enable_profiling': 1})
    assert ("InvokeSchedule OpenCL option 'enable_profiling' should be a "
            "boolean." in str(err.value))

    # out_of_order option must be a boolean
    with pytest.raises(TransformationError) as err:
        otrans.apply(sched, options={'out_of_order': 1})
    assert "InvokeSchedule OpenCL option 'out_of_order' should be a boolean." \
        in str(err.value)

    # Unsupported kernel options are not accepted
    with pytest.raises(AttributeError) as err:
        sched.coded_kernels()[0].set_opencl_options({'unsupported': 1})
    assert "CodedKern does not support the OpenCL option 'unsupported'." \
        in str(err.value)

    # local_size must be an integer
    with pytest.raises(TypeError) as err:
        sched.coded_kernels()[0].set_opencl_options({'local_size': 'a'})
    assert "CodedKern OpenCL option 'local_size' should be an integer." \
        in str(err.value)

    # queue_number must be an integer
    with pytest.raises(TypeError) as err:
        sched.coded_kernels()[0].set_opencl_options({'queue_number': 'a'})
    assert "CodedKern OpenCL option 'queue_number' should be an integer." \
        in str(err.value)


@pytest.mark.usefixtures("kernel_outputdir")
@pytest.mark.parametrize("option_to_check", ['enable_profiling',
                                             'out_of_order'])
def test_opencl_multi_invoke_options_validation(option_to_check):
    ''' Check that the OpenCL options constrains are enforced when there are
    multiple invokes.
    '''
    psy, _ = get_invoke("test12_two_invokes_two_kernels.f90", API, idx=0)
    invoke1_schedule = psy.invokes.invoke_list[0].schedule
    invoke2_schedule = psy.invokes.invoke_list[1].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in invoke1_schedule.coded_kernels():
        trans.apply(kernel)
    for kernel in invoke2_schedule.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(invoke1_schedule, options={option_to_check: False})
    otrans.apply(invoke2_schedule, options={option_to_check: True})
    with pytest.raises(NotImplementedError) as err:
        _ = str(psy.gen)
    assert ("The current implementation creates a single OpenCL context for "
            "all the invokes which needs certain OpenCL options to match "
            "between invokes. Found '{0}' with unmatching values between "
            "invokes.".format(option_to_check) in str(err.value))


@pytest.mark.usefixtures("kernel_outputdir")
def test_opencl_options_effects():
    ''' Check that the OpenCL options produce the expected changes in the
    PSy layer.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)

    # By default there is 1 queue, with an end barrier and local_size is 64
    assert "localsize = (/64, 1/)" in generated_code
    assert "ierr = clEnqueueNDRangeKernel(cmd_queues(1), " \
        "kernel_compute_cu_code, 2, C_NULL_PTR, C_LOC(globalsize), " \
        "C_LOC(localsize), 0, C_NULL_PTR, C_NULL_PTR)" in generated_code
    assert "! Block until all kernels have finished\n" \
        "      ierr = clFinish(cmd_queues(1))" in generated_code
    assert "ierr = clFinish(cmd_queues(2))" not in generated_code

    # Change kernel local_size to 4
    sched.coded_kernels()[0].set_opencl_options({'local_size': 4})
    generated_code = str(psy.gen)
    assert "localsize = (/4, 1/)" in generated_code

    # Change kernel queue to 2 (the barrier should then also go up to 2)
    sched.coded_kernels()[0].set_opencl_options({'queue_number': 2})
    generated_code = str(psy.gen)
    assert "ierr = clEnqueueNDRangeKernel(cmd_queues(2), " \
        "kernel_compute_cu_code, 2, C_NULL_PTR, C_LOC(globalsize), " \
        "C_LOC(localsize), 0, C_NULL_PTR, C_NULL_PTR)" in generated_code
    assert "! Block until all kernels have finished\n" \
        "      ierr = clFinish(cmd_queues(1))\n" \
        "      ierr = clFinish(cmd_queues(2))\n" in generated_code
    assert "ierr = clFinish(cmd_queues(3))" not in generated_code

    # Remove barrier at the end of the Invoke
    otrans.apply(sched, options={'end_barrier': False})
    generated_code = str(psy.gen)
    assert "! Block until all kernels have finished" not in generated_code
    assert "ierr = clFinish(cmd_queues(2))" not in generated_code


def test_set_kern_args(kernel_outputdir):
    ''' Check that we generate the necessary code to set kernel arguments. '''
    psy, _ = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    # Check we've only generated one set-args routine with arguments:
    # kernel object + boundary values + kernel arguments
    assert generated_code.count("SUBROUTINE compute_cu_code_set_args("
                                "kernel_obj, xstart, xstop, ystart, ystop, "
                                "cu_fld, p_fld, u_fld)") == 1
    # Declarations
    expected = '''\
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: c_sizeof, c_loc, c_intptr_t
      USE ocl_utils_mod, ONLY: check_status
      INTEGER, intent(in), target :: xstart, xstop, ystart, ystop
      INTEGER(KIND=c_intptr_t), intent(in), target :: cu_fld, p_fld, u_fld
      INTEGER ierr
      INTEGER(KIND=c_intptr_t), target :: kernel_obj'''
    assert expected in generated_code
    expected = '''\
      ! Set the arguments for the compute_cu_code OpenCL Kernel
      ierr = clSetKernelArg(kernel_obj, 0, C_SIZEOF(cu_fld), C_LOC(cu_fld))
      CALL check_status('clSetKernelArg: arg 0 of compute_cu_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 1, C_SIZEOF(p_fld), C_LOC(p_fld))
      CALL check_status('clSetKernelArg: arg 1 of compute_cu_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 2, C_SIZEOF(u_fld), C_LOC(u_fld))
      CALL check_status('clSetKernelArg: arg 2 of compute_cu_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 3, C_SIZEOF(xstart), C_LOC(xstart))
      CALL check_status('clSetKernelArg: arg 3 of compute_cu_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 4, C_SIZEOF(xstop), C_LOC(xstop))
      CALL check_status('clSetKernelArg: arg 4 of compute_cu_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 5, C_SIZEOF(ystart), C_LOC(ystart))
      CALL check_status('clSetKernelArg: arg 5 of compute_cu_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 6, C_SIZEOF(ystop), C_LOC(ystop))
      CALL check_status('clSetKernelArg: arg 6 of compute_cu_code', ierr)
    END SUBROUTINE compute_cu_code_set_args'''
    assert expected in generated_code
    assert generated_code.count("SUBROUTINE time_smooth_code_set_args("
                                "kernel_obj, xstart, xstop, ystart, ystop, "
                                "u_fld, unew_fld, uold_fld)") == 1
    assert ("CALL compute_cu_code_set_args(kernel_compute_cu_code, "
            "xstart - 1, xstop - 1, "
            "ystart - 1, ystop - 1, "
            "cu_fld%device_ptr, p_fld%device_ptr, "
            "u_fld%device_ptr)" in generated_code)
    assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)


@pytest.mark.usefixtures("kernel_outputdir")
def test_set_kern_args_real_grid_property():
    ''' Check that we generate correct code to set a real scalar grid
    property. '''
    psy, _ = get_invoke("driver_test.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    expected = '''\
    SUBROUTINE compute_kernel_code_set_args(kernel_obj, xstart, xstop, \
ystart, ystop, out_fld, in_out_fld, in_fld, dx, dx, gphiu)
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: c_sizeof, c_loc, c_intptr_t
      USE ocl_utils_mod, ONLY: check_status
      INTEGER, intent(in), target :: xstart, xstop, ystart, ystop
      INTEGER(KIND=c_intptr_t), intent(in), target :: out_fld, in_out_fld, \
in_fld, dx, gphiu
      REAL(KIND=go_wp), intent(in), target :: dx'''
    assert expected in generated_code
    # This generated code cannot be compiled due to issue #798. Note the
    # duplicated dx symbol name in the argument list. This is not essential
    # for the purpose of this test that just checks that the grid property
    # dx is declared as 'REAL(KIND=go_wp), intent(in), target :: dx'
    # assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)


@pytest.mark.usefixtures("kernel_outputdir")
def test_set_kern_float_arg():
    ''' Check that we generate correct code to set a real, scalar kernel
    argument. '''
    psy, _ = get_invoke("single_invoke_scalar_float_arg.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    expected = '''\
    SUBROUTINE bc_ssh_code_set_args(kernel_obj, xstart, xstop, ystart, ystop, \
a_scalar, ssh_fld, xstop, tmask)
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: c_sizeof, c_loc, c_intptr_t
      USE ocl_utils_mod, ONLY: check_status
      INTEGER, intent(in), target :: xstart, xstop, ystart, ystop
      INTEGER(KIND=c_intptr_t), intent(in), target :: ssh_fld, tmask
      REAL(KIND=go_wp), intent(in), target :: a_scalar
      INTEGER ierr
      INTEGER(KIND=c_intptr_t), target :: kernel_obj
'''
    assert expected in generated_code
    expected = '''\
      ! Set the arguments for the bc_ssh_code OpenCL Kernel
      ierr = clSetKernelArg(kernel_obj, 0, C_SIZEOF(a_scalar), C_LOC(a_scalar))
      CALL check_status('clSetKernelArg: arg 0 of bc_ssh_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 1, C_SIZEOF(ssh_fld), C_LOC(ssh_fld))
      CALL check_status('clSetKernelArg: arg 1 of bc_ssh_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 2, C_SIZEOF(xstop), C_LOC(xstop))
      CALL check_status('clSetKernelArg: arg 2 of bc_ssh_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 3, C_SIZEOF(tmask), C_LOC(tmask))
      CALL check_status('clSetKernelArg: arg 3 of bc_ssh_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 4, C_SIZEOF(xstart), C_LOC(xstart))
      CALL check_status('clSetKernelArg: arg 4 of bc_ssh_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 5, C_SIZEOF(xstop), C_LOC(xstop))
      CALL check_status('clSetKernelArg: arg 5 of bc_ssh_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 6, C_SIZEOF(ystart), C_LOC(ystart))
      CALL check_status('clSetKernelArg: arg 6 of bc_ssh_code', ierr)
      ierr = clSetKernelArg(kernel_obj, 7, C_SIZEOF(ystop), C_LOC(ystop))
      CALL check_status('clSetKernelArg: arg 7 of bc_ssh_code', ierr)
    END SUBROUTINE bc_ssh_code_set_args'''
    assert expected in generated_code
    # The generated code of this test cannot be compiled due the duplication
    # of the xstop symbol in the argument list. This happens because the first
    # instance of the symbol is not declared in the symbol table. Issue #798
    # should fix this problem. This is not essential for the purpose of this
    # test that just checks that a_scalar argument is generated appropriately
    # assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)


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
            "arguments passed by value" in str(err.value))


@pytest.mark.usefixtures("kernel_outputdir")
def test_opencl_kernel_code_generation():
    ''' Tests that gen_ocl method of the GOcean Kernel Schedule generates
    the expected OpenCL code. Note this test doesn't prepare the kernel to
    conform to OpenCL GOcean expected interface, see
    test_opencl_prepared_kernel_code_generation for that.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0, dist_mem=False)
    sched = psy.invokes.invoke_list[0].schedule
    kernel = sched.children[0].loop_body[0].loop_body[0]  # compute_cu kernel
    kschedule = kernel.get_kernel_schedule()

    expected_code = (
        "__kernel void compute_cu_code(\n"
        "  __global double * restrict cu,\n"
        "  __global double * restrict p,\n"
        "  __global double * restrict u\n"
        "  ){\n"
        "  int cuLEN1 = get_global_size(0);\n"
        "  int cuLEN2 = get_global_size(1);\n"
        "  int pLEN1 = get_global_size(0);\n"
        "  int pLEN2 = get_global_size(1);\n"
        "  int uLEN1 = get_global_size(0);\n"
        "  int uLEN2 = get_global_size(1);\n"
        "  int i = get_global_id(0);\n"
        "  int j = get_global_id(1);\n"
        "  cu[j * cuLEN1 + i] = ((0.5e0 * (p[j * pLEN1 + (i + 1)]"
        " + p[j * pLEN1 + i])) * u[j * uLEN1 + i]);\n"
        "}\n\n"
        )

    openclwriter = OpenCLWriter()
    assert expected_code == openclwriter(kschedule)


@pytest.mark.usefixtures("kernel_outputdir")
def test_opencl_prepared_kernel_code_generation():
    ''' Tests that the _prepare_opencl_kernel_schedule method for the GOcean
    API adds the 4 boundary values as kernel arguments and adds a masking
    statement at the beginning of the executable code.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0, dist_mem=False)
    sched = psy.invokes.invoke_list[0].schedule
    kernel = sched.children[0].loop_body[0].loop_body[0]  # compute_cu kernel
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    trans.apply(kernel)
    kschedule = kernel.get_kernel_schedule()

    expected_code = (
        "__kernel void compute_cu_code(\n"
        "  __global double * restrict cu,\n"
        "  __global double * restrict p,\n"
        "  __global double * restrict u,\n"
        "  int xstart,\n"
        "  int xstop,\n"
        "  int ystart,\n"
        "  int ystop\n"
        "  ){\n"
        "  int cuLEN1 = get_global_size(0);\n"
        "  int cuLEN2 = get_global_size(1);\n"
        "  int pLEN1 = get_global_size(0);\n"
        "  int pLEN2 = get_global_size(1);\n"
        "  int uLEN1 = get_global_size(0);\n"
        "  int uLEN2 = get_global_size(1);\n"
        "  int i = get_global_id(0);\n"
        "  int j = get_global_id(1);\n"
        "  if ((((i < xstart) || (i > xstop)) || ((j < ystart) ||"
        " (j > ystop)))) {\n"
        "    return;\n"
        "  }\n"
        "  cu[j * cuLEN1 + i] = ((0.5e0 * (p[j * pLEN1 + (i + 1)]"
        " + p[j * pLEN1 + i])) * u[j * uLEN1 + i]);\n"
        "}\n\n"
        )

    openclwriter = OpenCLWriter()
    assert expected_code == openclwriter(kschedule)


@pytest.mark.usefixtures("kernel_outputdir")
def test_opencl_kernel_missing_boundary_symbol():
    '''Check that an OpenCL file named modulename_kernelname_0 is generated.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule

    # Create dummy boundary symbols for the "name" kernel with one missing
    # symbol
    sched.symbol_table.new_symbol(
        "a", tag="xstart_name", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    sched.symbol_table.new_symbol(
        "c", tag="ystart_name", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    sched.symbol_table.new_symbol(
        "d", tag="ystop_name", symbol_type=DataSymbol, datatype=INTEGER_TYPE)

    otrans = OCLTrans()
    otrans.apply(sched)
    sched.kernels()[0].name = "name"

    with pytest.raises(GenerationError) as err:
        _ = psy.gen  # Generates the OpenCL kernels as a side-effect.
    assert ("Boundary symbol tag 'xstop_name' not found while generating the "
            "OpenCL code for kernel 'name'. Make sure to apply the "
            "GOMoveIterationBoundariesInsideKernelTrans for correct OpenCL "
            "code generation." in str(err.value))


def test_opencl_kernel_output_file(kernel_outputdir):
    '''Check that an OpenCL file named modulename_kernelname_0 is generated.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Create dummy boundary symbols for the "name" kernel
    sched.symbol_table.new_symbol(
        "a", tag="xstart_name", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    sched.symbol_table.new_symbol(
        "b", tag="xstop_name", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    sched.symbol_table.new_symbol(
        "c", tag="ystart_name", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    sched.symbol_table.new_symbol(
        "d", tag="ystop_name", symbol_type=DataSymbol, datatype=INTEGER_TYPE)

    otrans = OCLTrans()
    otrans.apply(sched)
    sched.kernels()[0].name = "name"
    _ = psy.gen  # Generates the OpenCL kernels as a side-effect.

    assert os.path.exists(
        os.path.join(str(kernel_outputdir), "compute_cu_name_0.cl"))


def test_opencl_kernel_output_file_with_suffix(kernel_outputdir):
    '''Check that an OpenCL file named modulename_kernelname_0 is
    generated without the _code suffix in the kernelname
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = OCLTrans()
    otrans.apply(sched)
    _ = psy.gen  # Generates the OpenCL kernels as a side-effect.

    assert os.path.exists(
        os.path.join(str(kernel_outputdir), "compute_cu_compute_cu_0.cl"))


def test_symtab_implementation_for_opencl():
    ''' Tests that the GOcean specialised Symbol Table implements the
    abstract properties needed to generate OpenCL.
    '''
    kschedule = GOKernelSchedule('test')

    # Test symbol table without any kernel argument
    with pytest.raises(GenerationError) as err:
        _ = kschedule.symbol_table.iteration_indices
    assert ("GOcean 1.0 API kernels should always have at least two "
            "arguments representing the iteration indices but the Symbol "
            "Table for kernel 'test' has only 0 argument(s)."
            in str(err.value))

    # Test symbol table with 1 kernel argument
    arg1 = DataSymbol("arg1", INTEGER_TYPE,
                      interface=ArgumentInterface(
                          ArgumentInterface.Access.READ))
    kschedule.symbol_table.add(arg1)
    kschedule.symbol_table.specify_argument_list([arg1])
    with pytest.raises(GenerationError) as err:
        _ = kschedule.symbol_table.iteration_indices
    assert ("GOcean 1.0 API kernels should always have at least two "
            "arguments representing the iteration indices but the Symbol "
            "Table for kernel 'test' has only 1 argument(s)."
            in str(err.value))

    # Test symbol table with 2 kernel argument
    arg2 = DataSymbol("arg2", INTEGER_TYPE,
                      interface=ArgumentInterface(
                          ArgumentInterface.Access.READ))
    kschedule.symbol_table.add(arg2)
    kschedule.symbol_table.specify_argument_list([arg1, arg2])
    iteration_indices = kschedule.symbol_table.iteration_indices
    assert iteration_indices[0] is arg1
    assert iteration_indices[1] is arg2

    # Test symbol table with 3 kernel argument
    array_type = ArrayType(REAL_TYPE, [10, 10])
    arg3 = DataSymbol("buffer1", array_type,
                      interface=ArgumentInterface(
                          ArgumentInterface.Access.READ))
    kschedule.symbol_table.add(arg3)
    kschedule.symbol_table.specify_argument_list([arg1, arg2, arg3])
    iteration_indices = kschedule.symbol_table.iteration_indices
    data_args = kschedule.symbol_table.data_arguments
    assert iteration_indices[0] is arg1
    assert iteration_indices[1] is arg2
    assert data_args[0] is arg3

    # Test gen_ocl with wrong iteration indices types and shapes.
    arg1._datatype._intrinsic = ScalarType.Intrinsic.REAL
    with pytest.raises(GenerationError) as err:
        _ = kschedule.symbol_table.iteration_indices
    assert ("GOcean 1.0 API kernels first argument should be a scalar "
            "integer but got 'Scalar<REAL, UNDEFINED>' for kernel 'test'."
            in str(err.value))

    arg1._datatype._intrinsic = ScalarType.Intrinsic.INTEGER  # restore
    arg2._datatype = ArrayType(INTEGER_TYPE, [10])
    with pytest.raises(GenerationError) as err:
        _ = kschedule.symbol_table.iteration_indices
    assert ("GOcean 1.0 API kernels second argument should be a scalar integer"
            " but got 'Array<Scalar<INTEGER, UNDEFINED>, shape=["
            "Literal[value:'10', Scalar<INTEGER, UNDEFINED>]]>' for "
            "kernel 'test'." in str(err.value))


@pytest.mark.usefixtures("kernel_outputdir")
def test_opencl_kernel_with_use():
    ''' Check that we refuse to transform a Schedule to use OpenCL if any
    of the kernels use module data. '''
    psy, _ = get_invoke("single_invoke_kern_with_use.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = OCLTrans()
    with pytest.raises(TransformationError) as err:
        otrans.apply(sched)
    assert ("'kernel_with_use_code' contains the following symbols with "
            "'global' scope: ['rdt']. An OpenCL kernel cannot call other "
            "kernels and all of the data" in str(err.value))
