# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council
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
# -----------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology
# Modified: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Module containing tests for creating drivers that read
previously dumped kernel input- and output-data.
'''

from pathlib import Path

import pytest

from psyclone.domain.gocean import GOceanDriverCreator
from psyclone.domain.gocean.transformations import (GOceanExtractTrans,
                                                    GOConstLoopBoundsTrans)
from psyclone.parse import ModuleManager
from psyclone.psyir.nodes import Routine, Loop
from psyclone.psyir.symbols import ContainerSymbol, SymbolTable
from psyclone.psyir.transformations import PSyDataTrans
from psyclone.tests.utilities import (
    Compile, get_base_path, get_infrastructure_path, get_invoke)

# API names
GOCEAN_API = "gocean"


@pytest.fixture(scope='function', autouse=True)
def init_module_manager():
    ''' The tests in this module all assume that there is no pre-existing
    ModuleManager object, so this fixture ensures that the module manager
    instance is deleted before and after each test function. The latter
    makes sure that any other test executed next will automatically reload
    the default ModuleManager file.
    '''

    test_files_dir = get_base_path(GOCEAN_API)
    infrastructure_path = Path(get_infrastructure_path(GOCEAN_API))
    # Define the path to the ReadKernelData module (which contains functions
    # to read extracted data from a file) relative to the infrastructure path:
    psyclone_root = infrastructure_path.parents[3]
    read_mod_path = (psyclone_root / "lib" / "extract" /
                     "binary")
    # Enforce loading of the default ModuleManager
    ModuleManager._instance = None

    module_manager = ModuleManager.get()
    # Ignore the MPI implementation of parallel utils_mod,
    # which means the parallel_utils_stub_mod will be found and used
    module_manager.add_ignore_file("parallel_utils_mod")
    module_manager.add_search_path(test_files_dir)
    module_manager.add_search_path(str(infrastructure_path))
    module_manager.add_search_path(str(read_mod_path))

    # Now execute all tests
    yield

    # Enforce loading of the default ModuleManager
    ModuleManager._instance = None


@pytest.fixture(scope="function", autouse=True)
def clear_region_name_cache():
    '''All PSyData nodes keep a list of used region names as class variables
    to avoid name clashes. This needs to be cleared, otherwise the indices
    used when creating unique region identifier will change depending on the
    order in which tests are run.
    '''
    PSyDataTrans._used_kernel_names = {}
    yield
    PSyDataTrans._used_kernel_names = {}


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_driver_creation1() -> None:
    '''Test that driver is created correctly for all variable access
    modes (input, input-output, output). Do not specify a region name,
    so test that the driver (including its filename) use the proper
    default name.

    '''
    # Use tmpdir so that the driver is created in tmp
    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    etrans.apply(schedule.children[0], {'create_driver': True})
    # We are only interested in the driver, so ignore results.
    _ = psy.gen

    driver = Path("driver-psy_extract_example_with_various_"
                  "variable_access_patterns-invoke_0_compute_"
                  "kernel-compute_kernel_code-r0.F90")
    assert driver.is_file()

    with driver.open("r", encoding="utf-8") as driver_file:
        driver_code = driver_file.read()

    # This is an excerpt of the code that should get created.
    # It is tested line by line since there is other code in between
    # which is not important, and the order might also change. It also
    # tests if unique variable names are created in the driver: the user
    # program contains a local variable 'dx', which clashes with the grid
    # property dx. The grid property will be renamed to 'dx_1':
    expected = '''
  use read_kernel_data_mod, only : ReadKernelDataType
  use kernel_driver_test_mod, only : compute_kernel_code
  use compare_variables_mod, only : compare, compare_init, compare_summary
  integer :: out_fld_internal_ystart
  integer :: out_fld_internal_ystop
  integer :: out_fld_internal_xstart
  integer :: out_fld_internal_xstop
  integer :: i
  integer :: j
  real*8, allocatable, dimension(:,:) :: out_fld_data
  real*8, allocatable, dimension(:,:) :: in_out_fld_data
  real*8, allocatable, dimension(:,:) :: in_fld_data
  real*8, allocatable, dimension(:,:) :: dx_data
  real*8 :: in_fld_grid_dx
  real*8, allocatable, dimension(:,:) :: in_fld_grid_gphiu
  type(ReadKernelDataType) :: extract_psy_data
  real*8, allocatable, dimension(:,:) :: dx_data_post
  real*8, allocatable, dimension(:,:) :: in_fld_data_post
  real*8 :: in_fld_grid_dx_post
  real*8, allocatable, dimension(:,:) :: in_fld_grid_gphiu_post
  real*8, allocatable, dimension(:,:) :: in_out_fld_data_post
  real*8, allocatable, dimension(:,:) :: out_fld_data_post

  call extract_psy_data%OpenReadModuleRegion('psy_extract_example_with_\
various_variable_access_patterns', &
&'invoke_0_compute_kernel-compute_kernel_code-r0')
  call extract_psy_data%ReadVariable('dx_data', dx_data)
  call extract_psy_data%ReadVariable('in_fld_data', in_fld_data)
  call extract_psy_data%ReadVariable('in_fld_grid_dx', in_fld_grid_dx)
  call extract_psy_data%ReadVariable('in_fld_grid_gphiu', in_fld_grid_gphiu)
  call extract_psy_data%ReadVariable('in_out_fld_data', in_out_fld_data)
  call extract_psy_data%ReadVariable('out_fld_data', out_fld_data)
  call extract_psy_data%ReadVariable('out_fld_internal_xstart', \
out_fld_internal_xstart)
  call extract_psy_data%ReadVariable('out_fld_internal_xstop', \
out_fld_internal_xstop)
  call extract_psy_data%ReadVariable('out_fld_internal_ystart', \
out_fld_internal_ystart)
  call extract_psy_data%ReadVariable('out_fld_internal_ystop', \
out_fld_internal_ystop)
  call extract_psy_data%ReadVariable('dx_data_post', dx_data_post)
  call extract_psy_data%ReadVariable('in_fld_data_post', in_fld_data_post)
  call extract_psy_data%ReadVariable('in_fld_grid_dx_post', \
in_fld_grid_dx_post)
  call extract_psy_data%ReadVariable('in_fld_grid_gphiu_post', \
in_fld_grid_gphiu_post)
  call extract_psy_data%ReadVariable('in_out_fld_data_post', \
in_out_fld_data_post)
  call extract_psy_data%ReadVariable('out_fld_data_post', out_fld_data_post)
  do j = out_fld_internal_ystart, out_fld_internal_ystop, 1
    do i = out_fld_internal_xstart, out_fld_internal_xstop, 1
      call compute_kernel_code(i, j, out_fld_data, in_out_fld_data, \
in_fld_data, dx_data, in_fld_grid_dx, in_fld_grid_gphiu)
    enddo
  enddo
  call compare_init(6)
  call compare('in_out_fld_data', in_out_fld_data, in_out_fld_data_post)
  call compare('out_fld_data', out_fld_data, out_fld_data_post)
  call compare_summary()
'''
    expected_lines = expected.split("\n")
    for line in expected_lines:
        assert line in driver_code, line + "\n -- not in --\n" + driver_code

    # Currently the following fields are also compared, even if DSL info tells
    # they are only read. If we take advantage of this information, these
    # and the associated _post declarations would be gone, and we would have
    # a: call compare_init(4)
    expected = '''
  call compare('dx_data', dx_data, dx_data_post)
  call compare('in_fld_data', in_fld_data, in_fld_data_post)
  call compare('in_fld_grid_dx', in_fld_grid_dx, in_fld_grid_dx_post)
  call compare('in_fld_grid_gphiu', in_fld_grid_gphiu, \
in_fld_grid_gphiu_post)
    '''
    expected_lines = expected.split("\n")
    for line in expected_lines:
        assert line in driver_code, line + "\n -- not in --\n" + driver_code

    build = Compile(".")
    build.compile_file("driver-psy_extract_example_with_various_"
                       "variable_access_patterns-invoke_0_compute_"
                       "kernel-compute_kernel_code-r0.F90")


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_driver_creation2():
    '''Verify that the region names are used when opening the file, and that
    constant loop boundaries work as expected.

    '''
    # Use tmpdir so that the driver is created in tmp
    psy, invoke = get_invoke("driver_test.f90", GOCEAN_API,
                             idx=0, dist_mem=False)

    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(invoke.schedule)
    etrans = GOceanExtractTrans()
    first_loop = invoke.schedule.walk(Loop)[0]
    etrans.apply(first_loop, {'create_driver': True,
                              'region_name':
                              ("module_name", "local_name")})

    _ = psy.gen
    driver = Path("driver-module_name-local_name.F90")
    assert driver.is_file()

    with driver.open("r", encoding="utf-8") as driver_file:
        driver_code = driver_file.read()

    # This is an excerpt of the code that should get created.
    # It is tested line by line since there is other code in between
    # which is not important, and the order might also change. It also
    # tests if unique variable names are created in the driver: the user
    # program contains a local variable 'dx', which clashes with the grid
    # property dx. The grid property will be renamed to 'dx_1':
    expected = '''use read_kernel_data_mod, only : ReadKernelDataType

  integer :: istop
  integer :: jstop
  real*8, allocatable, dimension(:,:) :: out_fld_data
  real*8, allocatable, dimension(:,:) :: in_out_fld_data
  real*8, allocatable, dimension(:,:) :: in_fld_data
  real*8, allocatable, dimension(:,:) :: dx_data
  real*8, allocatable, dimension(:,:) :: in_fld_grid_gphiu
  real*8, allocatable, dimension(:,:) :: out_fld_data_post
  real*8 :: in_fld_grid_dx
  real*8, allocatable, dimension(:,:) :: in_out_fld_data_post
  type(ReadKernelDataType) :: extract_psy_data
  call extract_psy_data%OpenReadModuleRegion('module_name', 'local_name')
  call extract_psy_data%ReadVariable('out_fld_data_post', out_fld_data_post)
  call extract_psy_data%ReadVariable('in_fld_data', in_fld_data)
  call extract_psy_data%ReadVariable('in_out_fld_data_post', \
in_out_fld_data_post)
  call extract_psy_data%ReadVariable('dx_data', dx_data)
  call extract_psy_data%ReadVariable('in_fld_grid_dx', in_fld_grid_dx)
  call extract_psy_data%ReadVariable('in_fld_grid_gphiu', in_fld_grid_gphiu)
  do j = 2, jstop, 1
    do i = 2, istop + 1, 1
      call compute_kernel_code(i, j, out_fld_data, in_out_fld_data, \
in_fld_data, dx_data, in_fld_grid_dx, in_fld_grid_gphiu)
    enddo
  enddo
  call compare_init(6)
  call compare('in_out_fld_data', in_out_fld_data, in_out_fld_data_post)
  call compare('out_fld_data', out_fld_data, out_fld_data_post)
  call compare_summary()'''
    expected_lines = expected.split("\n")
    for line in expected_lines:
        assert line in driver_code, line + "\n -- not in --\n" + driver_code

    # Currently the following fields are also compared, even if DSL info tells
    # they are only read. If we take advantage of this information, these
    # and the associated _post declarations would be gone, and we would have
    # a: call compare_init(4)
    expected = '''
  call compare('dx_data', dx_data, dx_data_post)
  call compare('in_fld_data', in_fld_data, in_fld_data_post)
  call compare('in_fld_grid_dx', in_fld_grid_dx, in_fld_grid_dx_post)
  call compare('in_fld_grid_gphiu', in_fld_grid_gphiu, \
in_fld_grid_gphiu_post)
    '''

    expected_lines = expected.split("\n")
    for line in expected_lines:
        assert line in driver_code, line + "\n -- not in --\n" + driver_code

    build = Compile(".")
    build.compile_file(str(driver))


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_rename_suffix_if_name_clash():
    '''Test that driver is created correctly if there is a clash
    with the variable names, e.g. an output variable 'a', and
    an input variable 'a_post' - writing the output variable 'a'
    would use 'a_post' as name, so the suffix must be changed.
    This is technically done in the transformation that calls
    the driver creation, but the proper integration of the transformation
    with the driver creation needs to be tested.
    Also check that the specified region_name is used for the output
    filename.

    '''
    # Use tmpdir so that the driver is created in tmp
    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=1, dist_mem=False)
    schedule = invoke.schedule
    etrans.apply(schedule.children[0], {'create_driver': True,
                                        'region_name':
                                        ("module_name", "local_name")})
    extract_code = str(psy.gen)

    # This kernel calls compute_kernel(out_fld%data, out_fld_data%data,
    #                                  out_fld_data_post%data, dx%data)
    # Make sure the extraction does not clash the names of
    # flattening out_fld%data to out_fld_data or out_fld_data_post
    expected_lines = [
      'CALL extract_psy_data % PreDeclareVariable("out_fld_data_1", '
      'out_fld_data_1)',
      'CALL extract_psy_data % PreDeclareVariable("out_fld_data_1_post", '
      'out_fld_data_1)',
      'call compute_kernel_code(i, j, out_fld_data_1, out_fld_data_data, '
      'out_fld_data_post_data, dx_data, out_fld_data_post_grid_dx, '
      'out_fld_data_post_grid_gphiu)']
    for line in expected_lines:
        assert line in extract_code, line + "\n -- not in --\n" + extract_code

    # Now we also need to check that the driver uses the new suffix,
    # i.e. both as key for ReadVariable, as well as for the variable
    # names.
    driver = Path("driver-module_name-local_name.F90")
    assert driver.is_file()

    with driver.open("r", encoding="utf-8") as driver_file:
        driver_code = driver_file.read()

    expected = """
  real*8, allocatable, dimension(:,:) :: out_fld_data_1
  real*8, allocatable, dimension(:,:) :: out_fld_data_1_post
  call extract_psy_data%ReadVariable('out_fld_data_1', out_fld_data_1)
  call extract_psy_data%ReadVariable('out_fld_data_1_post', \
out_fld_data_1_post)"""

    for line in expected.split("\n"):
        assert line in driver_code, line + "\n -- not in --\n" + driver_code

    build = Compile(".")
    build.compile_file(str(driver))


# -----------------------------------------------------------------------------
def test_errors_add_call():
    '''Test that an error is raised if the symbol supplied to 'add_call()' is
    not a RoutineSymbol.

    '''
    program = Routine.create("test", is_program=True)
    program_symbol_table = program.symbol_table

    # Add 'psy_data_mod' as a container symbol:
    psy_data_mod = ContainerSymbol("psy_data_mod")
    program_symbol_table.add(psy_data_mod)

    edc = GOceanDriverCreator()

    # Then try to add a call to 'psy_data_mod':
    with pytest.raises(TypeError) as err:
        edc.add_call(program, "psy_data_mod", [])
    assert ("Error creating call to 'psy_data_mod' - existing symbol is "
            "of type 'ContainerSymbol', not a 'RoutineSymbol'."
            in str(err.value))


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_driver_creation_same_symbol():
    '''Make sure that if a symbol appears in more than one invoke no duplicated
    symbol is created.

    '''
    # The fourth invoke calls a kernel twice with identical parameters.
    # This means the same symbols are re-encountered when handling the
    # second kernel call (since each kernel argument has already been
    # declared when the first kernel was done).
    psy, invoke = get_invoke("driver_test.f90", GOCEAN_API,
                             idx=3, dist_mem=False)

    etrans = GOceanExtractTrans()
    etrans.apply(invoke.schedule, {'create_driver': True,
                                   'region_name':
                                   ("module_name", "local_name")})
    code = psy.gen

    # Only one version of the flattened symbol is created and given
    # the value back
    assert """
    out_fld_internal_ystart = out_fld%internal%ystart
    out_fld_internal_ystop = out_fld%internal%ystop
    out_fld_internal_xstart = out_fld%internal%xstart
    out_fld_internal_xstop = out_fld%internal%xstop
    out_fld_data = out_fld%data
    in_out_fld_data = in_out_fld%data
    in_fld_data = in_fld%data
    dx_data = dx%data
    in_fld_grid_dx = in_fld%grid%dx
    in_fld_grid_gphiu = in_fld%grid%gphiu
    CALL extract_psy_data % PreStart("module_name", "local_name", 10, 6)
    """ in code
    assert """
    CALL extract_psy_data % PostEnd
    in_fld%grid%gphiu = in_fld_grid_gphiu
    in_fld%grid%dx = in_fld_grid_dx
    dx%data = dx_data
    in_fld%data = in_fld_data
    in_out_fld%data = in_out_fld_data
    out_fld%data = out_fld_data
    out_fld%internal%xstop = out_fld_internal_xstop
    out_fld%internal%xstart = out_fld_internal_xstart
    out_fld%internal%ystop = out_fld_internal_ystop
    out_fld%internal%ystart = out_fld_internal_ystart""" in code
    driver = Path("driver-module_name-local_name.F90")
    assert driver.is_file()

    with driver.open("r", encoding="utf-8") as driver_file:
        driver_code = driver_file.read()

    # Make sure that all kernels use the same flattened symbols
    correct = """
  do j = out_fld_internal_ystart, out_fld_internal_ystop, 1
    do i = out_fld_internal_xstart, out_fld_internal_xstop, 1
      call compute_kernel_code(i, j, out_fld_data, in_out_fld_data, \
in_fld_data, dx_data, in_fld_grid_dx, in_fld_grid_gphiu)
    enddo
  enddo
  do j = out_fld_internal_ystart, out_fld_internal_ystop, 1
    do i = out_fld_internal_xstart, out_fld_internal_xstop, 1
      call compute_kernel_code(i, j, out_fld_data, in_out_fld_data, \
in_fld_data, dx_data, in_fld_grid_dx, in_fld_grid_gphiu)
    enddo
  enddo
  do j = out_fld_internal_ystart, out_fld_internal_ystop, 1
    do i = out_fld_internal_xstart, out_fld_internal_xstop, 1
      call compute_kernel_code(i, j, out_fld_data, in_out_fld_data, \
in_fld_data, dx_data, in_fld_grid_dx, in_fld_grid_gphiu)
    enddo
  enddo"""
    assert correct in driver_code

    build = Compile(".")
    build.compile_file(str(driver))


# -----------------------------------------------------------------------------
def test_driver_creation_import_modules(fortran_reader):
    '''Test that calls to RoutineSymbols with both imported and local
    interfaces are handled correctly.

    '''
    code = '''program test_prog
              use my_module, only : mod_func
              call mod_func()
              call ext_func()
              call mod_func()
              end program test_prog'''
    psyir = fortran_reader.psyir_from_source(code)
    program = psyir.children[0]   # psyir is a FileContainer, take the program
    edc = GOceanDriverCreator()
    # Delete all symbols in the symbol table so we can check if the right
    # symbols are added:
    program.scope._symbol_table = SymbolTable()
    edc.import_modules(program)
    symbol_table = program.scope.symbol_table
    all_symbols = symbol_table.get_symbols()
    assert len(all_symbols) == 2
    assert str(all_symbols["my_module"]) == \
        "my_module: ContainerSymbol<not linked>"
    mod_func = all_symbols["mod_func"]
    assert str(mod_func) == ("mod_func: RoutineSymbol<UnresolvedType, "
                             "pure=unknown, elemental=unknown>")
