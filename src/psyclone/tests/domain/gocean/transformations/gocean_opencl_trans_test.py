# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
# Modified by J. Henrichs, Bureau of Meteorology
# ----------------------------------------------------------------------------

''' Module containing tests for the PSyclone GOcean OpenCL transformation.'''

import os
import pytest

from psyclone.configuration import Config
from psyclone.domain.gocean.transformations import (
    GOMoveIterationBoundariesInsideKernelTrans, GOOpenCLTrans)
from psyclone.errors import GenerationError
from psyclone.gocean1p0 import GOKernelSchedule
from psyclone.psyir.symbols import (DataSymbol, ArgumentInterface,
                                    ScalarType, ArrayType, INTEGER_TYPE,
                                    REAL_TYPE)
from psyclone.tests.gocean_build import GOceanOpenCLBuild
from psyclone.tests.utilities import (Compile, get_base_path, get_invoke)
from psyclone.transformations import (TransformationError,
                                      KernelImportsToArguments)


@pytest.fixture(scope="function", autouse=True)
def setup():
    '''Make sure that all tests here use the GOcean API and include the
    gocean test_files directory (as some modules are imported from there
    in the examples) and that we clean up the config file at the end of
    the tests.'''

    Config._instance = None
    filepath = get_base_path("gocean1.0")
    Config.get().api = "gocean1.0"
    Config.get()._include_paths = [filepath]
    yield
    # At the end of every tests make sure that we wipe the Config object
    # so we get a fresh/default one for any further test (and not a
    # left-over one from a test here).
    Config._instance = None


# PSyclone API under test
API = "gocean1.0"


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
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

    with open("hello_world_opencl.f90", "w", encoding="utf-8") as ffile:
        ffile.write(example_ocl_code)
    GOceanOpenCLBuild(kernel_outputdir).\
        compile_file("hello_world_opencl.f90",
                     link=True)


def test_transformation_name():
    ''' Check that the GOOpenCLTransformation returns the correct name'''
    trans = GOOpenCLTrans()
    assert trans.name == "GOOpenCLTrans"


def test_validate_unsupported_api():
    ''' Check that attempting to apply an OpenCL transformation to a Dynamo
    InvokeSchedule raises the expected error. '''
    # Undo the effect of the 'auto-use' fixture (at the top of this file)
    # that sets-up the Config object for the GOcean API. By setting the
    # instance to None we force it to be re-initialised with the API specified
    # in the call to get_invoke().
    Config._instance = None
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3",
                           name="invoke_0_testkern_type", dist_mem=False)
    sched = invoke.schedule
    trans = GOOpenCLTrans()
    with pytest.raises(TransformationError) as err:
        trans.apply(sched)
    assert ("OpenCL generation is currently only supported for the GOcean "
            "API but got an InvokeSchedule of type:" in str(err.value))


def test_ocl_apply(kernel_outputdir):
    ''' Check that GOOpenCLTrans generates correct code '''
    psy, invoke = get_invoke("test11_different_iterates_over_"
                             "one_invoke.f90", API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in schedule.coded_kernels():
        trans.apply(kernel)
    ocl = GOOpenCLTrans()

    # Check that we raise the correct error if we attempt to apply the
    # transformation to something that is not an InvokeSchedule
    with pytest.raises(TransformationError) as err:
        ocl.apply(schedule.children[0])
    assert "the supplied node must be a (sub-class of) InvokeSchedule " \
        in str(err.value)

    ocl.apply(schedule)

    gen = str(psy.gen)
    assert "USE clfortran" in gen

    # Check that the new kernel file have been generated
    kernel_files = os.listdir(str(kernel_outputdir))
    assert len(kernel_files) == 1
    assert "opencl_kernels_0.cl" in kernel_files
    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


@pytest.mark.parametrize("debug_mode", [True, False])
def test_invoke_use_stmts_and_decls(kernel_outputdir, monkeypatch, debug_mode,
                                    fortran_writer):
    ''' Test that generating code for OpenCL results in the correct
    module use statements and declarations. '''
    api_config = Config.get().api_conf("gocean1.0")
    monkeypatch.setattr(api_config, "_debug_mode", debug_mode)
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule

    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = fortran_writer(sched).lower()

    assert ("use fortcl, only : get_cmd_queues, get_kernel_by_name"
            in generated_code)
    assert "use clfortran" in generated_code
    assert "use iso_c_binding" in generated_code

    # When in debug mode, import also the check_status function
    if debug_mode:
        assert "use ocl_utils_mod, only : check_status" in generated_code
    else:
        assert "use ocl_utils_mod, only : check_status" not in generated_code

    assert "type(r2d_field), intent(inout) :: cu_fld" in generated_code
    assert "type(r2d_field), intent(inout) :: p_fld" in generated_code
    assert "type(r2d_field), intent(inout) :: u_fld" in generated_code
    assert "integer :: j" in generated_code
    assert "integer :: i" in generated_code
    assert "integer :: ystop" in generated_code
    assert "integer :: ystart" in generated_code
    assert "integer :: xstop" in generated_code
    assert "integer :: xstart" in generated_code

    assert "integer(kind=c_size_t), target :: localsize(2)" in generated_code
    assert "integer(kind=c_size_t), target :: globalsize(2)" in generated_code
    assert "integer(kind=c_intptr_t) :: u_fld_cl_mem" in generated_code
    assert "integer(kind=c_intptr_t) :: p_fld_cl_mem" in generated_code
    assert "integer(kind=c_intptr_t) :: cu_fld_cl_mem" in generated_code
    assert ("integer(kind=c_intptr_t), target, save :: kernel_compute_cu_code"
            in generated_code)
    assert "logical, save :: first_time = .true." in generated_code
    assert "integer :: ierr" in generated_code
    assert ("integer(kind=c_intptr_t), pointer, save :: cmd_queues(:)"
            in generated_code)

    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


def test_invoke_opencl_initialisation(kernel_outputdir, fortran_writer):
    ''' Test that generating code for OpenCL results in the correct
    OpenCL first time initialisation code '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = fortran_writer(sched).lower()

    # Test that the necessary variables are declared at the beginning
    # of the invoke
    assert "integer(kind=c_size_t), target :: localsize(2)" in generated_code
    assert "integer(kind=c_size_t), target :: globalsize(2)" in generated_code
    assert "integer(kind=c_intptr_t) :: u_fld_cl_mem" in generated_code
    assert "integer(kind=c_intptr_t) :: p_fld_cl_mem" in generated_code
    assert "integer(kind=c_intptr_t) :: cu_fld_cl_mem" in generated_code
    assert ("integer(kind=c_intptr_t), target, save :: kernel_compute_cu_code"
            in generated_code)
    assert "logical, save :: first_time = .true." in generated_code
    assert "integer :: ierr" in generated_code
    assert ("integer(kind=c_intptr_t), pointer, save :: cmd_queues(:)"
            in generated_code)

    # Test that a conditional 'first_time' code is generated with the
    # expected initialisation statements:
    # - Call psy_init
    # - Set cmd_queues pointers
    # - OpenCL kernel setters
    # - Initialization of all OpenCL field buffers
    # - Call set_arg of the kernels (with necessary boundary and cl_mem
    #   buffers initialisation)
    # - Write data into the OpenCL buffers
    expected = '''\
  ! initialise opencl runtime, kernels and buffers
  if (first_time) then
    call psy_init()
    cmd_queues => get_cmd_queues()
    kernel_compute_cu_code = get_kernel_by_name('compute_cu_code')
    call initialise_device_buffer(cu_fld)
    call initialise_device_buffer(p_fld)
    call initialise_device_buffer(u_fld)
    ! do a set_args now so subsequent writes place the data appropriately
    cu_fld_cl_mem = transfer(cu_fld%device_ptr, cu_fld_cl_mem)
    p_fld_cl_mem = transfer(p_fld%device_ptr, p_fld_cl_mem)
    u_fld_cl_mem = transfer(u_fld%device_ptr, u_fld_cl_mem)
    call compute_cu_code_set_args(kernel_compute_cu_code, cu_fld_cl_mem, \
p_fld_cl_mem, u_fld_cl_mem, xstart - 1, xstop - 1, ystart - 1, ystop - 1)
    ! write data to the device'''
    assert expected in generated_code

    # The write_to_device() can appear in any order in the following 3 lines
    lines = generated_code.split('\n')
    idx = lines.index('    ! write data to the device')
    candidates = '\n'.join(lines[idx+1:idx+4])
    assert "call u_fld%write_to_device()" in candidates
    assert "call cu_fld%write_to_device()" in candidates
    assert "call p_fld%write_to_device()" in candidates

    # Just before the end of the subroutine first_time is set to False
    expected = '''\
  first_time = .false.

end subroutine'''
    assert expected in generated_code
    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


@pytest.mark.usefixtures("kernel_outputdir")
def test_invoke_opencl_initialisation_grid():
    ''' Test that generating OpenCL generation code when there are grid
    property accesses generated the proper grid on device initialisation
    code '''
    psy, _ = get_invoke("driver_test.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen).lower()

    check_properties = ['area_t', 'area_u', 'area_v', 'dx_u', 'dx_v', 'dx_t',
                        'dy_u', 'dy_v', 'dy_t', 'gphiu', 'gphiv']

    # Check that device grid initialisation routine is generated
    expected = '''
    subroutine initialise_grid_device_buffers(field)
      use fortcl, only: create_ronly_buffer
      use iso_c_binding, only: c_size_t
      use field_mod
      type(r2d_field), intent(inout), target :: field
      integer(kind=c_size_t) size_in_bytes

      if (.not.c_associated(field%grid%tmask_device)) then
        size_in_bytes = int(field%grid%nx * field%grid%ny, 8) * \
c_sizeof(field%grid%tmask(1,1))
        field%grid%tmask_device = transfer(create_ronly_buffer(size_in_bytes),\
 field%grid%tmask_device)
        size_in_bytes = int(field%grid%nx * field%grid%ny, 8) * \
c_sizeof(field%grid%'''
    assert expected in generated_code

    for grid_property in check_properties:
        code = (f"field%grid%{grid_property}_device = transfer("
                f"create_ronly_buffer(size_in_bytes), "
                f"field%grid%{grid_property}_device)")
        assert code in generated_code

    # Check that device grid write routine is generated
    expected = '''
    subroutine write_grid_buffers(field)
      use fortcl, only: get_cmd_queues
      use iso_c_binding, only: c_intptr_t, c_size_t, c_sizeof
      use clfortran
      use ocl_utils_mod, only: check_status
      type(r2d_field), intent(inout), target :: field
      integer(kind=c_size_t) size_in_bytes
      integer(kind=c_intptr_t), pointer :: cmd_queues(:)
      integer(kind=c_intptr_t) cl_mem
      integer ierr

      cmd_queues => get_cmd_queues()
      size_in_bytes = int(field%grid%nx * field%grid%ny, 8) * \
c_sizeof(field%grid%tmask(1,1))
      cl_mem = transfer(field%grid%tmask_device, cl_mem)
      ierr = clenqueuewritebuffer(cmd_queues(1),cl_mem,cl_true,0_8,\
size_in_bytes,c_loc(field%grid%tmask),0,c_null_ptr,c_null_ptr)
      call check_status('clenqueuewritebuffer tmask', ierr)
      size_in_bytes = int(field%grid%nx * field%grid%ny, 8) * \
c_sizeof(field%grid%area_t(1,1))'''
    assert expected in generated_code

    for grid_property in check_properties:
        code = (f"      cl_mem = transfer(field%grid%{grid_property}_device, "
                f"cl_mem)\n"
                f"      ierr = clenqueuewritebuffer(cmd_queues(1),cl_mem,"
                f"cl_true,0_8,size_in_bytes,c_loc(field%grid%{grid_property}),"
                f"0,c_null_ptr,c_null_ptr)\n"
                f"      call check_status('clenqueuewritebuffer "
                f"{grid_property}_device', ierr)\n")
        assert code in generated_code

    # Check that during the first time set-up the previous routines are called
    # for a kernel which contains a grid property access.
    expected = '''
      xstart = out_fld%internal%xstart
      xstop = out_fld%internal%xstop
      ystart = out_fld%internal%ystart
      ystop = out_fld%internal%ystop
      ! initialise opencl runtime, kernels and buffers
      if (first_time) then
        call psy_init
        cmd_queues => get_cmd_queues()
        kernel_compute_kernel_code = get_kernel_by_name('compute_kernel_code')
        call initialise_device_buffer(out_fld)
        call initialise_device_buffer(in_out_fld)
        call initialise_device_buffer(in_fld)
        call initialise_device_buffer(dx)
        call initialise_grid_device_buffers(in_fld)
        ! do a set_args now so subsequent writes place the data appropriately
        out_fld_cl_mem = transfer(out_fld%device_ptr, out_fld_cl_mem)
        in_out_fld_cl_mem = transfer(in_out_fld%device_ptr, in_out_fld_cl_mem)
        in_fld_cl_mem = transfer(in_fld%device_ptr, in_fld_cl_mem)
        dx_cl_mem = transfer(dx%device_ptr, dx_cl_mem)
        gphiu_cl_mem = transfer(in_fld%grid%gphiu_device, gphiu_cl_mem)
        call compute_kernel_code_set_args(kernel_compute_kernel_code, \
out_fld_cl_mem, in_out_fld_cl_mem, in_fld_cl_mem, dx_cl_mem, \
in_fld%grid%dx, gphiu_cl_mem, xstart - 1, xstop - 1, ystart - 1, \
ystop - 1)
        ! write data to the device'''
    assert expected in generated_code

    # The write_to_device() can appear in any order in the following 5 lines
    lines = generated_code.split('\n')
    idx = lines.index('        ! write data to the device')
    candidates = '\n'.join(lines[idx+1:idx+6])
    assert "call out_fld%write_to_device" in candidates
    assert "call in_out_fld%write_to_device" in candidates
    assert "call in_fld%write_to_device" in candidates
    assert "call dx%write_to_device" in candidates
    assert "call write_grid_buffers(in_fld)" in candidates

    # TODO 284: Currently this example cannot be compiled because it needs to
    # import a module which won't be found on kernel_outputdir


def test_opencl_routines_initialisation(kernel_outputdir):
    ''' Test that an OpenCL invoke file has the necessary routines
    to initialise, read and write from buffers. '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen).lower()

    # Check that the read_from_device routine has been generated
    expected = '''\
    subroutine read_from_device(from, to, startx, starty, nx, ny, blocking)
      use iso_c_binding, only: c_intptr_t, c_ptr, c_size_t, c_sizeof
      use ocl_utils_mod, only: check_status
      use kind_params_mod, only: go_wp
      use clfortran
      use fortcl, only: get_cmd_queues
      type(c_ptr), intent(in) :: from
      real(kind=go_wp), intent(inout), dimension(:, :), target :: to
      integer, intent(in) :: startx
      integer, intent(in) :: starty
      integer, intent(in) :: nx
      integer, intent(in) :: ny
      logical, intent(in) :: blocking
      integer(kind=c_size_t) size_in_bytes
      integer(kind=c_size_t) offset_in_bytes
      integer(kind=c_intptr_t) cl_mem
      integer(kind=c_intptr_t), pointer :: cmd_queues(:)
      integer ierr
      integer i

      cl_mem = transfer(from, cl_mem)
      cmd_queues => get_cmd_queues()
      if (nx < size(to, 1) / 2) then
        do i = starty, starty + ny, 1
          size_in_bytes = int(nx, 8) * c_sizeof(to(1,1))
          offset_in_bytes = int(size(to, 1) * (i - 1) + \
(startx - 1)) * c_sizeof(to(1,1))
          ierr = clenqueuereadbuffer(cmd_queues(1),cl_mem,cl_false,\
offset_in_bytes,size_in_bytes,c_loc(to(startx,i)),0,c_null_ptr,c_null_ptr)
          call check_status('clenqueuereadbuffer', ierr)
        end do
        if (blocking) then
          call check_status('clfinish on read', clfinish(cmd_queues(1)))
        end if
      else
        size_in_bytes = int(size(to, 1) * ny, 8) * c_sizeof(to(1,1))
        offset_in_bytes = int(size(to, 1) * (starty - 1), 8) * \
c_sizeof(to(1,1))
        ierr = clenqueuereadbuffer(cmd_queues(1),cl_mem,cl_true,\
offset_in_bytes,size_in_bytes,c_loc(to(1,starty)),0,c_null_ptr,c_null_ptr)
        call check_status('clenqueuereadbuffer', ierr)
      end if

    end subroutine read_from_device'''
    assert expected in generated_code

    # Check that the write_to_device routine has been generated
    expected = '''\
    subroutine write_to_device(from, to, startx, starty, nx, ny, blocking)
      use iso_c_binding, only: c_intptr_t, c_ptr, c_size_t, c_sizeof
      use ocl_utils_mod, only: check_status
      use kind_params_mod, only: go_wp
      use clfortran
      use fortcl, only: get_cmd_queues
      real(kind=go_wp), intent(in), dimension(:, :), target :: from
      type(c_ptr), intent(in) :: to
      integer, intent(in) :: startx
      integer, intent(in) :: starty
      integer, intent(in) :: nx
      integer, intent(in) :: ny
      logical, intent(in) :: blocking
      integer(kind=c_intptr_t) cl_mem
      integer(kind=c_size_t) size_in_bytes
      integer(kind=c_size_t) offset_in_bytes
      integer(kind=c_intptr_t), pointer :: cmd_queues(:)
      integer ierr
      integer i

      cl_mem = transfer(to, cl_mem)
      cmd_queues => get_cmd_queues()
      if (nx < size(from, 1) / 2) then
        do i = starty, starty + ny, 1
          size_in_bytes = int(nx, 8) * c_sizeof(from(1,1))
          offset_in_bytes = int(size(from, 1) * (i - 1) + (startx - 1)) * \
c_sizeof(from(1,1))
          ierr = clenqueuewritebuffer(cmd_queues(1),cl_mem,cl_false,\
offset_in_bytes,size_in_bytes,c_loc(from(startx,i)),0,c_null_ptr,c_null_ptr)
          call check_status('clenqueuewritebuffer', ierr)
        end do
        if (blocking) then
          call check_status('clfinish on write', clfinish(cmd_queues(1)))
        end if
      else
        size_in_bytes = int(size(from, 1) * ny, 8) * c_sizeof(from(1,1))
        offset_in_bytes = int(size(from, 1) * (starty - 1)) * \
c_sizeof(from(1,1))
        ierr = clenqueuewritebuffer(cmd_queues(1),cl_mem,cl_true,\
offset_in_bytes,size_in_bytes,c_loc(from(1,starty)),0,c_null_ptr,c_null_ptr)
        call check_status('clenqueuewritebuffer', ierr)
      end if

    end subroutine write_to_device'''
    assert expected in generated_code

    # Check that the device buffer initialisation routine has been generated
    expected = '''\
    subroutine initialise_device_buffer(field)
      use fortcl, only: create_rw_buffer
      use iso_c_binding, only: c_size_t
      use field_mod
      type(r2d_field), intent(inout), target :: field
      integer(kind=c_size_t) size_in_bytes

      if (.not.field%data_on_device) then
        size_in_bytes = int(field%grid%nx * field%grid%ny, 8) * \
c_sizeof(field%data(1,1))
        field%device_ptr = transfer(create_rw_buffer(size_in_bytes), \
field%device_ptr)
        field%data_on_device = .true.
        field%read_from_device_f => read_from_device
        field%write_to_device_f => write_to_device
      end if

    end subroutine initialise_device_buffer'''
    assert expected in generated_code
    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


def test_psy_init_defaults(kernel_outputdir):
    ''' Check that we create a psy_init() routine that sets-up the
    OpenCL environment. '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0, dist_mem=True)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    expected = '''
    subroutine psy_init()
      use fortcl, only: add_kernels, ocl_env_init
      character(len=30) kernel_names(1)
      integer, save :: ocl_device_num = 1
      logical, save :: initialised = .false.

      if (.not.initialised) then
        initialised = .true.
        call ocl_env_init(1, ocl_device_num, .false., .false.)
        kernel_names(1) = 'compute_cu_code'
        call add_kernels(1, kernel_names)
      end if

    end subroutine psy_init'''
    assert expected in generated_code.lower()
    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


def test_psy_init_multiple_kernels(kernel_outputdir):
    ''' Check that we create a psy_init() routine that sets-up the
    kernel_names correctly when there are multiple kernels, some of
    them repeated. '''
    # This example has 2 unique kernels, one of them repeated twice
    psy, _ = get_invoke("single_invoke_three_kernels_with_use.f90",
                        API, idx=0, dist_mem=True)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel and removing
    # kernel imports are prerequisites for this test.
    trans1 = GOMoveIterationBoundariesInsideKernelTrans()
    trans2 = KernelImportsToArguments()
    for kernel in sched.coded_kernels():
        trans1.apply(kernel)
        trans2.apply(kernel)

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)

    # Check that the kernel_names has enough space for all kernels
    assert "CHARACTER(LEN=30) kernel_names(2)" in generated_code

    # The order doesn't matter as far as the two kernels are loaded
    assert ("kernel_names(1) = 'kernel_with_use_code'" in generated_code or
            "kernel_names(2) = 'kernel_with_use_code'" in generated_code)

    assert ("kernel_names(1) = 'kernel_with_use2_code'" in generated_code or
            "kernel_names(2) = 'kernel_with_use2_code'" in generated_code)
    assert "kernel_names(3)" not in generated_code

    # Check that add_kernels is provided with the total number of kernels
    assert "CALL add_kernels(2, kernel_names)" in generated_code

    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(
            psy, dependencies=["model_mod.f90"])


def test_psy_init_multiple_devices_per_node(kernel_outputdir, monkeypatch):
    ''' Test that we create the appropriate subroutine to initialise an
    hybrid MPI-OpenCL environment with multiple devices per node. '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0, dist_mem=True)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    # Test with a different configuration value for OCL_DEVICES_PER_NODE
    # that needs a mod() and a get_rank() expression, and a kernel with
    # a higher queue number.
    monkeypatch.setattr(Config.get(), "_ocl_devices_per_node", 2)

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)

    expected = '''
    subroutine psy_init()
      use parallel_mod, only: get_rank
      use fortcl, only: add_kernels, ocl_env_init
      character(len=30) kernel_names(1)
      integer, save :: ocl_device_num = 1
      logical, save :: initialised = .false.

      if (.not.initialised) then
        initialised = .true.
        ocl_device_num = mod(get_rank() - 1, 2) + 1
        call ocl_env_init(1, ocl_device_num, .false., .false.)
        kernel_names(1) = 'compute_cu_code'
        call add_kernels(1, kernel_names)
      end if

    end subroutine psy_init'''
    assert expected in generated_code.lower()
    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


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

    # Use non-default kernel and transformation options
    sched.coded_kernels()[0].set_opencl_options({'queue_number': 5})
    otrans = GOOpenCLTrans()
    otrans.apply(sched, options={"enable_profiling": True,
                                 "out_of_order": True})
    generated_code = str(psy.gen)
    assert "CALL ocl_env_init(5, ocl_device_num, .true., .true.)\n" \
        in generated_code
    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


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

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)

    # Set up globalsize and localsize values
    expected = '''
      globalsize = (/p_fld%grid%nx, p_fld%grid%ny/)
      localsize = (/64, 1/)'''

    if debug_mode:
        # Check that the globalsize first dimension is a multiple of
        # the localsize first dimension
        expected += '''
      IF (MOD(p_fld%grid%nx, 64) /= 0) THEN
        CALL check_status('Global size is not a multiple of local size \
(mandatory in OpenCL < 2.0).', -1)
      END IF'''

    if debug_mode:
        # Check that there is no pending error in the queue before launching
        # the kernel
        expected += '''
      ierr = clFinish(cmd_queues(1))
      CALL check_status('Errors before compute_cu_code launch', ierr)'''

    # Cast dl_esm_inf pointers to cl_mem handlers
    expected += '''
      cu_fld_cl_mem = TRANSFER(cu_fld%device_ptr, cu_fld_cl_mem)
      p_fld_cl_mem = TRANSFER(p_fld%device_ptr, p_fld_cl_mem)
      u_fld_cl_mem = TRANSFER(u_fld%device_ptr, u_fld_cl_mem)'''

    # Call the set_args subroutine with the boundaries corrected for the
    # OpenCL 0-indexing
    expected += '''
      CALL compute_cu_code_set_args(kernel_compute_cu_code, \
cu_fld_cl_mem, p_fld_cl_mem, u_fld_cl_mem, \
xstart - 1, xstop - 1, \
ystart - 1, ystop - 1)'''

    expected += '''
      ! Launch the kernel
      ierr = clEnqueueNDRangeKernel(cmd_queues(1), kernel_compute_cu_code, \
2, C_NULL_PTR, C_LOC(globalsize), C_LOC(localsize), 0, C_NULL_PTR, \
C_NULL_PTR)'''

    if debug_mode:
        # Check that there are no errors during the kernel launch or during
        # the execution of the kernel.
        expected += '''
      CALL check_status('compute_cu_code clEnqueueNDRangeKernel', ierr)
      ierr = clFinish(cmd_queues(1))
      CALL check_status('Errors during compute_cu_code', ierr)'''

    assert expected in generated_code
    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


@pytest.mark.usefixtures("kernel_outputdir")
def test_opencl_kernel_boundaries_validation():
    ''' Check that the OpenCL transformation can not be applied if the
    kernel loop doesn't iterate the whole grid.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule

    otrans = GOOpenCLTrans()

    # Try to apply the OpenCL transformation without moving the boundaries
    with pytest.raises(TransformationError) as err:
        otrans.apply(sched)
    assert ("The kernel 'compute_cu_code' does not iterate over all grid "
            "points. This is a necessary requirement for generating the "
            "OpenCL code and can be done by applying the GOMoveIteration"
            "BoundariesInsideKernelTrans to each kernel before the "
            "GOOpenCLTrans." in str(err.value))

    # After move the boundaries the OpenCL transformation should pass
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)
    otrans.apply(sched)


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

    otrans = GOOpenCLTrans()

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

    otrans = GOOpenCLTrans()
    otrans.apply(invoke1_schedule, options={option_to_check: False})
    with pytest.raises(TransformationError) as err:
        otrans.apply(invoke2_schedule, options={option_to_check: True})
    assert (f"Can't generate an OpenCL Invoke with {option_to_check}='True' "
            f"since a previous transformation used a different value, and "
            f"their OpenCL environments must match."
            in str(err.value))


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

    otrans = GOOpenCLTrans()
    otrans.apply(sched)

    # By default there is 1 queue, with an end barrier and local_size is 64
    generated_code = str(psy.gen)
    assert "localsize = (/64, 1/)" in generated_code
    assert "ierr = clEnqueueNDRangeKernel(cmd_queues(1), " \
        "kernel_compute_cu_code, 2, C_NULL_PTR, C_LOC(globalsize), " \
        "C_LOC(localsize), 0, C_NULL_PTR, C_NULL_PTR)" in generated_code
    assert "ierr = clFinish(cmd_queues(1))" in generated_code
    assert "ierr = clFinish(cmd_queues(2))" not in generated_code

    # Reparse the example as changes are not possible after a psy.gen
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)
    # Change kernel local_size to 4
    sched.coded_kernels()[0].set_opencl_options({'local_size': 4})
    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    assert "localsize = (/4, 1/)" in generated_code

    # Reparse the example as changes are not possible after a psy.gen
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)
    # Change kernel queue number to 2 (the barrier should then also go up to 2)
    sched.coded_kernels()[0].set_opencl_options({'queue_number': 2})
    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    assert "ierr = clEnqueueNDRangeKernel(cmd_queues(2), " \
        "kernel_compute_cu_code, 2, C_NULL_PTR, C_LOC(globalsize), " \
        "C_LOC(localsize), 0, C_NULL_PTR, C_NULL_PTR)" in generated_code
    assert "      ierr = clFinish(cmd_queues(1))\n" \
           "      ierr = clFinish(cmd_queues(2))\n" in generated_code
    assert "ierr = clFinish(cmd_queues(3))" not in generated_code

    # Reparse the example as changes are not possible after a psy.gen
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)
    otrans = GOOpenCLTrans()

    # Remove barrier at the end of the Invoke
    otrans.apply(sched, options={'end_barrier': False})
    generated_code = str(psy.gen)
    assert "! Block until all kernels have finished" not in generated_code
    assert "ierr = clFinish(cmd_queues(1))" not in generated_code


@pytest.mark.parametrize("dist_mem", [True, False])
@pytest.mark.usefixtures("kernel_outputdir")
def test_multiple_command_queues(dist_mem):
    ''' Check that barriers (with clFinish) are inserted when a kernel (or a
    haloexchange in distributed memory) is dispatched to a different queue than
    its dependency predecessor. '''
    psy, _ = get_invoke("single_invoke_two_identical_kernels.f90", API, idx=0,
                        dist_mem=dist_mem)
    sched = psy.invokes.invoke_list[0].schedule

    # Set the boundaries inside the kernel
    trans = GOMoveIterationBoundariesInsideKernelTrans()

    # Set each kernel to run in a different OpenCL queue (kernel1 will run in
    # queue 2 and kernel2 will run in queue 3. This is also different from the
    # OCL_MANAGEMENT_QUEUE used by the haloexchange data transfer which will
    # use queue 1, therefore barriers will always be needed in this example.
    for idx, kernel in enumerate(sched.coded_kernels()):
        trans.apply(kernel)
        kernel.set_opencl_options({'queue_number': idx+2})

    # Apply OpenCL transformation
    otrans = GOOpenCLTrans()
    otrans.apply(sched)

    generated_code = str(psy.gen)

    kernelbarrier = '''
      ierr = clFinish(cmd_queues(2))
      p_fld_cl_mem = TRANSFER(p_fld%device_ptr, p_fld_cl_mem)'''

    haloexbarrier = '''
      ierr = clFinish(cmd_queues(2))
      CALL cu_fld%halo_exchange(1)'''

    if dist_mem:
        # In distributed memory the command_queue synchronisation happens
        # before the HaloExchange (so it is not necessary before the kernel)
        assert kernelbarrier not in generated_code
        assert haloexbarrier in generated_code
    else:
        # Without distributed memory we need a barrier for the first
        # command queue before launching the second kernel
        assert kernelbarrier in generated_code


def test_set_kern_args(kernel_outputdir):
    ''' Check that we generate the necessary code to set kernel arguments. '''
    psy, _ = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    # Check we've only generated one set-args routine with arguments:
    # kernel object + kernel arguments + boundary values
    assert generated_code.count("SUBROUTINE compute_cu_code_set_args("
                                "kernel_obj, cu_fld, p_fld, u_fld, xstart, "
                                "xstop, ystart, ystop)") == 1
    # Declarations
    expected = '''\
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: C_LOC, C_SIZEOF, c_intptr_t
      USE ocl_utils_mod, ONLY: check_status
      INTEGER(KIND=c_intptr_t), TARGET :: kernel_obj
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: cu_fld
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: p_fld
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: u_fld
      INTEGER, INTENT(IN), TARGET :: xstart
      INTEGER, INTENT(IN), TARGET :: xstop
      INTEGER, INTENT(IN), TARGET :: ystart
      INTEGER, INTENT(IN), TARGET :: ystop
      INTEGER ierr'''
    assert expected in generated_code
    expected = '''\
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

    # The call to the set_args matches the expected kernel signature with
    # the boundary values converted to 0-indexing
    assert ("CALL compute_cu_code_set_args(kernel_compute_cu_code, "
            "cu_fld_cl_mem, p_fld_cl_mem, u_fld_cl_mem, "
            "xstart - 1, xstop - 1, "
            "ystart - 1, ystop - 1)" in generated_code)

    # There is also only one version of the set_args for the second kernel
    assert generated_code.count("SUBROUTINE time_smooth_code_set_args("
                                "kernel_obj, u_fld, unew_fld, uold_fld, "
                                "xstart_1, xstop_1, ystart_1, ystop_1)") == 1
    assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


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

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    expected = '''\
    SUBROUTINE compute_kernel_code_set_args(kernel_obj, out_fld, in_out_fld, \
in_fld, dx, dx_1, gphiu, xstart, xstop, ystart, ystop)
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: C_LOC, C_SIZEOF, c_intptr_t
      USE ocl_utils_mod, ONLY: check_status
      INTEGER(KIND=c_intptr_t), TARGET :: kernel_obj
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: out_fld
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: in_out_fld
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: in_fld
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: dx
      REAL(KIND=go_wp), INTENT(IN), TARGET :: dx_1
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: gphiu
      INTEGER, INTENT(IN), TARGET :: xstart
      INTEGER, INTENT(IN), TARGET :: xstop
      INTEGER, INTENT(IN), TARGET :: ystart
      INTEGER, INTENT(IN), TARGET :: ystop'''
    assert expected in generated_code
    # TODO 284: Currently this example cannot be compiled because it needs to
    # import a module which won't be found on kernel_outputdir


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

    otrans = GOOpenCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    # This set_args has a name clash on xstop (one is a grid property and the
    # other a loop boundary). One of they should appear as 'xstop_1'
    expected = '''\
    SUBROUTINE bc_ssh_code_set_args(kernel_obj, a_scalar, ssh_fld, xstop, \
tmask, xstart, xstop_1, ystart, ystop)
      USE clfortran, ONLY: clSetKernelArg
      USE iso_c_binding, ONLY: C_LOC, C_SIZEOF, c_intptr_t
      USE ocl_utils_mod, ONLY: check_status
      INTEGER(KIND=c_intptr_t), TARGET :: kernel_obj
      REAL(KIND=go_wp), INTENT(IN), TARGET :: a_scalar
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: ssh_fld
      INTEGER, INTENT(IN), TARGET :: xstop
      INTEGER(KIND=c_intptr_t), INTENT(IN), TARGET :: tmask
      INTEGER, INTENT(IN), TARGET :: xstart
      INTEGER, INTENT(IN), TARGET :: xstop_1
      INTEGER, INTENT(IN), TARGET :: ystart
      INTEGER, INTENT(IN), TARGET :: ystop
      INTEGER ierr
'''
    assert expected in generated_code
    expected = '''\
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
      ierr = clSetKernelArg(kernel_obj, 5, C_SIZEOF(xstop_1), C_LOC(xstop_1))
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
    # assert GOceanOpenCLBuild(kernel_outputdir).code_compiles(psy)


def test_set_arg_const_scalar():
    ''' Check that an invoke that passes a scalar kernel argument by
    value is rejected. (We haven't yet implemented the necessary code for
    setting the value of such an argument in OpenCL.) '''
    psy, _ = get_invoke("test00.1_invoke_kernel_using_const_scalar.f90",
                        API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = GOOpenCLTrans()
    with pytest.raises(TransformationError) as err:
        otrans.apply(sched)
    assert ("Cannot generate OpenCL for Invokes that contain kernel arguments"
            " which are a literal, but found the literal '0' used as an "
            "argument in invoke 'invoke_0_bc_ssh'." in str(err.value))


@pytest.mark.usefixtures("kernel_outputdir")
def test_opencl_kernel_missing_boundary_symbol(monkeypatch):
    '''Check that during code generation if a tagged symbol to represent a
    loop boundary doesn't exist the relevant error is raised.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule

    # Create dummy boundary symbols for the "name" kernel with one missing
    # symbol
    sched.symbol_table.new_symbol(
        "a", tag="xstart_compute_cu_code", symbol_type=DataSymbol,
        datatype=INTEGER_TYPE)
    sched.symbol_table.new_symbol(
        "c", tag="ystart_compute_cu_code", symbol_type=DataSymbol,
        datatype=INTEGER_TYPE)
    sched.symbol_table.new_symbol(
        "d", tag="ystop_compute_cu_code", symbol_type=DataSymbol,
        datatype=INTEGER_TYPE)

    otrans = GOOpenCLTrans()
    # We skip validation as in this test we purposefully want to have the issue
    monkeypatch.setattr(otrans, "validate", lambda x, y: None)
    with pytest.raises(GenerationError) as err:
        otrans.apply(sched)
    assert ("Boundary symbol tag 'xstop_compute_cu_code' not found while "
            "generating the OpenCL code for kernel 'compute_cu_code'. Make "
            "sure to apply the GOMoveIterationBoundariesInsideKernelTrans "
            "before attempting the OpenCL code generation." in str(err.value))


def test_opencl_kernel_output_file(kernel_outputdir):
    '''Check that a new OpenCL file named opencl_kernels_{suffix}.cl is
    generated.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)

    # Create a opencl_kernels_0.cl so another name is needed for the new file
    filename = os.path.join(str(kernel_outputdir), "opencl_kernels_0.cl")
    with open(filename, "w", encoding="utf-8") as myfile:
        myfile.write("This file exists!")

    otrans = GOOpenCLTrans()
    otrans.apply(sched)  # Generates the OpenCL kernels as a side-effect.

    assert os.path.exists(
        os.path.join(str(kernel_outputdir), "opencl_kernels_1.cl"))


def test_symtab_implementation_for_opencl():
    ''' Tests that the GOcean specialised Symbol Table implements the
    abstract properties needed to generate OpenCL.
    '''
    kschedule = GOKernelSchedule('test')

    # Test symbol table without any kernel argument
    with pytest.raises(GenerationError) as err:
        _ = kschedule.symbol_table.iteration_indices
    assert ("GOcean API kernels should always have at least two "
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
    assert ("GOcean API kernels should always have at least two "
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
    assert ("GOcean API kernels first argument should be a scalar "
            "integer but got 'Scalar<REAL, UNDEFINED>' for kernel 'test'."
            in str(err.value))

    arg1._datatype._intrinsic = ScalarType.Intrinsic.INTEGER  # restore
    arg2._datatype = ArrayType(INTEGER_TYPE, [10])
    with pytest.raises(GenerationError) as err:
        _ = kschedule.symbol_table.iteration_indices
    assert ("GOcean API kernels second argument should be a scalar integer"
            " but got 'Array<Scalar<INTEGER, UNDEFINED>, shape=[10]>' for "
            "kernel 'test'." in str(err.value))


@pytest.mark.usefixtures("kernel_outputdir")
def test_opencl_kernel_with_use():
    ''' Check that we refuse to transform a Schedule to use OpenCL if any
    of the kernels use module data. '''
    psy, _ = get_invoke("single_invoke_kern_with_use.f90", API, idx=0)
    sched = psy.invokes.invoke_list[0].schedule
    otrans = GOOpenCLTrans()
    with pytest.raises(TransformationError) as err:
        otrans.apply(sched)
    assert ("'kernel_with_use_code' contains the following symbols with "
            "'global' scope: ['rdt']. An OpenCL kernel cannot call other "
            "kernels and all of the data" in str(err.value))
