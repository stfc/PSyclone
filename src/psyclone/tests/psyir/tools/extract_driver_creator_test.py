# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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
# -----------------------------------------------------------------------------

''' Module containing tests for creating drivers that read
previously dumped kernel input- and output-data.
'''

from __future__ import absolute_import

import pytest

from psyclone.configuration import Config
from psyclone.domain.gocean.transformations import GOceanExtractTrans
from psyclone.psyir.nodes import Reference
from psyclone.psyir.symbols import SymbolTable
from psyclone.psyir.tools import ExtractDriverCreator
from psyclone.psyir.transformations import PSyDataTrans, TransformationError
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import GOConstLoopBoundsTrans

# API names
GOCEAN_API = "gocean1.0"


@pytest.fixture(scope="function", autouse=True)
def clear_region_name_cache():
    '''All PSyData nodes keep a list of used region names as class variables
    to avoid name clashes. This needs to be cleared, otherwise the indices
    used when creating unique region identifier will change depending on the
    order in which tests are run.
    '''
    PSyDataTrans._used_kernel_names = {}
    yield()
    PSyDataTrans._used_kernel_names = {}


# -----------------------------------------------------------------------------
def test_driver_creation1(tmpdir):
    '''Test that driver is created correctly for all variable access \
    modes (input, input-output, output).

    '''
    # Use tmpdir so that the driver is created in tmp
    tmpdir.chdir()

    etrans = GOceanExtractTrans()
    ctrans = GOConstLoopBoundsTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    # This test expects constant loop bounds
    ctrans.apply(schedule)

    etrans.apply(schedule.children[0], {'create_driver': True})
    # We are only interested in the driver, so ignore results.
    str(psy.gen)

    driver = tmpdir.join("driver-psy_extract_example_with_various_variable_"
                         "access_patterns-invoke_0_compute_kernel:compute_"
                         "kernel_code:r0.f90")
    assert driver.isfile()

    with driver.open("r") as driver_file:
        driver_code = driver_file.read()

    # This is an excerpt of the code that should get created.
    # It is tested line by line since there is other code in between
    # which is not important, and the order might also change. It also
    # tests if unique variable names are created in the driver: the user
    # program contains a local variable 'dx', which clashes with the grid
    # property dx. The grid property will be renamed to 'dx_1':
    expected = '''use extract_psy_data_mod, only : extract_PsyDataType

  real*8, allocatable, dimension(:,:) :: out_fld
  real*8, allocatable, dimension(:,:) :: in_out_fld
  real*8, allocatable, dimension(:,:) :: in_fld
  real*8, allocatable, dimension(:,:) :: dx
  real*8, allocatable, dimension(:,:) :: in_fld_grid_gphiu
  real*8, allocatable, dimension(:,:) :: out_fld_post
  real*8 :: in_fld_grid_dx
  real*8, allocatable, dimension(:,:) :: in_out_fld_post
  type(extract_PsyDataType) :: extract_psy_data
  call extract_psy_data%OpenRead('psy_extract_example_with_various_variable_''' \
  '''access_patterns', 'invoke_0_compute_kernel:compute_kernel_code:r0')
  call extract_psy_data%ReadVariable('out_fld_post', out_fld_post)
  ALLOCATE(out_fld(SIZE(out_fld_post, 1), SIZE(out_fld_post, 2)))
  out_fld = 0
  call extract_psy_data%ReadVariable('in_fld', in_fld)
  call extract_psy_data%ReadVariable('in_out_fld_post', in_out_fld_post)
  call extract_psy_data%ReadVariable('dx', dx)
  call extract_psy_data%ReadVariable('in_fld%grid%dx', in_fld_grid_dx)
  call extract_psy_data%ReadVariable('in_fld%grid%gphiu', in_fld_grid_gphiu)
  do j = 2, jstop, 1
    do i = 2, istop+1, 1
      call compute_kernel_code(i, j, out_fld, in_out_fld, in_fld, dx, ''' \
      '''in_fld_grid_dx, in_fld_grid_gphiu)
    enddo
  enddo
  if (i == i_post) then
    PRINT *, "i correct"
  else
    PRINT *, "i incorrect. Values are:"
    PRINT *, i
    PRINT *, "i values should be:"
    PRINT *, i_post
  end if
  if (ALL(in_out_fld - in_out_fld_post == 0.0)) then
    PRINT *, "in_out_fld correct"
  else
    PRINT *, "in_out_fld incorrect. Values are:"
    PRINT *, in_out_fld
    PRINT *, "in_out_fld values should be:"
    PRINT *, in_out_fld_post
  end if'''
    expected_lines = expected.split("\n")
    for line in expected_lines:
        assert line in driver_code


# -----------------------------------------------------------------------------
def test_driver_creation2(tmpdir):
    '''Test different ways of specifying the nodes, including code creation
    without constant loop boundaries.

    '''
    # Use tmpdir so that the driver is created in tmp
    tmpdir.chdir()

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    etrans.apply([schedule.children[0]], {'create_driver': True})
    # We are only interested in the driver, so ignore results.
    str(psy.gen)

    driver = tmpdir.join("driver-psy_extract_example_with_various_variable_"
                         "access_patterns-invoke_0_compute_kernel:compute_"
                         "kernel_code:r0.f90")
    assert driver.isfile()

    with driver.open("r") as driver_file:
        driver_code = driver_file.read()

    # This is an excerpt of the code that should get created.
    # It is tested line by line since there is other code in between
    # which is not important, and the order might also change. It also
    # tests if unique variable names are created in the driver: the user
    # program contains a local variable 'dx', which clashes with the grid
    # property dx. The grid property will be renamed to 'dx_1':
    expected = '''use extract_psy_data_mod, only : extract_PsyDataType

  real*8, allocatable, dimension(:,:) :: out_fld
  real*8, allocatable, dimension(:,:) :: in_out_fld
  real*8, allocatable, dimension(:,:) :: in_fld
  real*8, allocatable, dimension(:,:) :: dx
  real*8, allocatable, dimension(:,:) :: in_fld_grid_gphiu
  real*8, allocatable, dimension(:,:) :: out_fld_post
  real*8 :: in_fld_grid_dx
  real*8, allocatable, dimension(:,:) :: in_out_fld_post
  type(extract_PsyDataType) :: extract_psy_data
  call extract_psy_data%OpenRead('psy_extract_example_with_various_variable_''' \
  '''access_patterns', 'invoke_0_compute_kernel:compute_kernel_code:r0')
  call extract_psy_data%ReadVariable('out_fld_post', out_fld_post)
  ALLOCATE(out_fld(SIZE(out_fld_post, 1), SIZE(out_fld_post, 2)))
  out_fld = 0
  call extract_psy_data%ReadVariable('in_fld', in_fld)
  call extract_psy_data%ReadVariable('in_out_fld_post', in_out_fld_post)
  call extract_psy_data%ReadVariable('dx', dx)
  call extract_psy_data%ReadVariable('in_fld%grid%dx', in_fld_grid_dx)
  call extract_psy_data%ReadVariable('in_fld%grid%gphiu', in_fld_grid_gphiu)
  do j = out_fld_internal_ystart, out_fld_internal_ystop, 1
    do i = out_fld_internal_xstart, out_fld_internal_xstop, 1
      call compute_kernel_code(i, j, out_fld, in_out_fld, in_fld, dx, ''' \
      '''in_fld_grid_dx, in_fld_grid_gphiu)
    enddo
  enddo
  if (i == i_post) then
    PRINT *, "i correct"
  else
    PRINT *, "i incorrect. Values are:"
    PRINT *, i
    PRINT *, "i values should be:"
    PRINT *, i_post
  end if
  if (ALL(in_out_fld - in_out_fld_post == 0.0)) then
    PRINT *, "in_out_fld correct"
  else
    PRINT *, "in_out_fld incorrect. Values are:"
    PRINT *, in_out_fld
    PRINT *, "in_out_fld values should be:"
    PRINT *, in_out_fld_post
  end if'''
    expected_lines = expected.split("\n")
    for line in expected_lines:
        assert line in driver_code


# -----------------------------------------------------------------------------
def test_rename_suffix_if_name_clash(tmpdir):
    '''Test that driver is created correctly if there is a clash
    with the variable names, e.g. an output variable 'a', and
    an input variable 'a_post' - writing the output variable 'a'
    would use 'a_post' as name, so the suffix must be changed.

    '''
    # Use tmpdir so that the driver is created in tmp
    tmpdir.chdir()

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=1, dist_mem=False)
    schedule = invoke.schedule

    etrans.apply(schedule.children[0], {'create_driver': True})
    extract_code = str(psy.gen)

    # Due to the name clash of "out_fld"+"_post" and "out_fld_post"
    # the _post suffix is changed to _post0. So the file will
    # contain out_fld_post for the input variable out_fld_post,
    # and "out_fld_post0" for the output value of out_fld.
    expected = """
      CALL extract_psy_data%PreDeclareVariable("out_fld_post", out_fld_post)
      CALL extract_psy_data%PreDeclareVariable("in_out_fld_post0", in_out_fld)
      CALL extract_psy_data%PreDeclareVariable("out_fld_post0", out_fld)
      CALL extract_psy_data%ProvideVariable("in_out_fld", in_out_fld)
      CALL extract_psy_data%ProvideVariable("out_fld_post", out_fld_post)
      CALL extract_psy_data%ProvideVariable("in_out_fld_post0", in_out_fld)
      CALL extract_psy_data%ProvideVariable("out_fld_post0", out_fld)"""
    expected_lines = expected.split("\n")
    for line in expected_lines:
        assert line in expected_lines

    # Now we also need to check that the driver uses the new suffix,
    # i.e. both as key for ReadVariable, as well as for the variable
    # names.
    driver = tmpdir.join("driver-psy_extract_example_with_various_variable_"
                         "access_patterns-invoke_1_compute_kernel:compute_"
                         "kernel_code:r0.f90")
    assert driver.isfile()

    with driver.open("r") as driver_file:
        driver_code = driver_file.read()

    expected = """
  real*8, allocatable, dimension(:,:) :: out_fld
  real*8, allocatable, dimension(:,:) :: in_out_fld
  real*8, allocatable, dimension(:,:) :: out_fld_post
  real*8, allocatable, dimension(:,:) :: in_out_fld_post0
  real*8, allocatable, dimension(:,:) :: out_fld_post0
  call extract_psy_data%ReadVariable('in_out_fld', in_out_fld)
  call extract_psy_data%ReadVariable('in_out_fld_post0', in_out_fld_post0)
  call extract_psy_data%ReadVariable('out_fld_post0', out_fld_post0)
  ALLOCATE(out_fld(SIZE(out_fld_post0, 1), SIZE(out_fld_post0, 2)))
  call extract_psy_data%ReadVariable('out_fld_post', out_fld_post)"""

    for line in expected.split("\n"):
        assert line in driver_code

    # Now test that more than one variable clash is handled. The third
    # invoke uses:
    # "out_fld" as output field
    # "out_fld_post" as input field (first clash --> suffix becomes "_post0")
    # "out_fld_post0" as input+output field (next clash --> suffix = "_post1")
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=2, dist_mem=False)
    schedule = invoke.schedule
    # We don't check the driver, we already tested that the
    # driver picks up the adjusted suffix above
    etrans.apply(schedule.children[0])
    extract_code = str(psy.gen)

    # Check that *out_fld* is declared correctly: it is only declared as
    # output value, so must use key out_fld_post1 once, and not be declared
    # as input value:
    assert 'PreDeclareVariable("out_fld_post1", out_fld)' in extract_code
    assert 'PreDeclareVariable("out_fld", out_fld)' not in extract_code

    # Check that *out_fld_post* (input/output) is declared correctly. It must
    # be declared twice: once for the input value using the original variable
    # name, and once as output using the "_post1" suffix"
    assert 'PreDeclareVariable("out_fld_post", out_fld_post)' in extract_code
    assert 'PreDeclareVariable("out_fld_post_post1", out_fld_post)' \
        in extract_code

    # Check that *out_fld_post0* is declared correctly: as input-only
    # variable it must be declared once for using the original variable name.
    assert 'PreDeclareVariable("out_fld_post0", out_fld_post0)' in extract_code
    assert 'PreDeclareVariable("out_fld_post0_post1", out_fld_post0)' \
        not in extract_code


# -----------------------------------------------------------------------------
def test_driver_creation_create_flattened_symbol_errors(monkeypatch):
    '''Test that create_flattened_symbol raises the appropriate errors.

    '''
    _, invoke = get_invoke("driver_test.f90",
                           GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Get the reference to the 6th argument, which is the gocean
    # property.
    ref = schedule.children[0].loop_body.children[0] \
        .loop_body.children[0].arguments.args[5].psyir_expression()
    # Make sure we have the right reference:
    assert isinstance(ref, Reference)
    assert ref.children[0].children[0].name == "gphiu"

    edc = ExtractDriverCreator()

    # Remove the default types to trigger the error of
    # not finding the default type to use:
    edc._default_types = {}
    with pytest.raises(TransformationError) as err:
        # The symbol table can be None, that code is not reached.
        edc.create_flattened_symbol("new_name", ref, None)
    assert "Unknown type 'real' in the reference 'in_fld%grid%gphiu' in the " \
           "GOcean API" in str(err.value)

    # Monkey patch the grid property dictionary to remove the
    # go_grid_lat_u entry, triggering an earlier error:
    api_config = Config.get().api_conf("gocean1.0")
    grid_properties = api_config.grid_properties
    monkeypatch.delitem(grid_properties, "go_grid_lat_u")

    with pytest.raises(TransformationError) as err:
        # The symbol table can be None, that code is not reached.
        edc.create_flattened_symbol("new_name", ref, None)
    assert "Could not find type for reference 'in_fld%grid%gphiu'" \
        in str(err.value)


# -----------------------------------------------------------------------------
def test_driver_creation_add_all_kernel_symbols_errors():
    '''Test the error that can be raised in add_all_kernel_symbols.

    '''
    _, invoke = get_invoke("driver_test.f90",
                           GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Create the copy and lower it before the default_types are
    # changed (which will prevent lowering from working).
    schedule_copy = schedule.copy()
    schedule_copy.lower_to_language_level()

    # First provide a structured type that is not an r2d_field:
    edc = ExtractDriverCreator()
    ref = schedule_copy.children[0].loop_body.children[0] \
        .loop_body.children[0].children[2]
    assert ref.symbol.name == "out_fld"
    assert ref.symbol.datatype.name == "r2d_field"
    ref.symbol.datatype._name = "unknown type"
    symbol_table = SymbolTable()
    with pytest.raises(TransformationError) as err:
        edc.add_all_kernel_symbols(schedule_copy, symbol_table)
    assert "Error when constructing driver for 'invoke_0_compute_kernel': " \
           "Unknown derived type 'unknown type'" in str(err.value)
    ref.symbol.datatype._name = "r2d_field"

    # Remove the default types to trigger the error of
    # not finding the default type to use:
    edc._default_types = {}
    symbol_table = SymbolTable()
    with pytest.raises(TransformationError) as err:
        edc.add_all_kernel_symbols(schedule_copy, symbol_table)
    assert "Error when constructing driver for 'invoke_0_compute_kernel': " \
           "Unknown intrinsic data type 'Intrinsic.INTEGER'" in str(err.value)


# -----------------------------------------------------------------------------
def test_driver_creation_same_symbol(tmpdir):
    '''Make sure that if a symbol appears in more than one invoke no duplicated
    symbol is created.

    '''
    # Use tmpdir so that the driver is created in tmp
    tmpdir.chdir()

    etrans = GOceanExtractTrans()
    # The fourth invoke calls a kernel twice with identical parameters
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=3, dist_mem=False)
    schedule = invoke.schedule

    etrans.apply(schedule.children[0], {'create_driver': True})
    # We are only interested in the driver, so ignore results.
    str(psy.gen)

    driver = tmpdir.join("driver-psy_extract_example_with_various_variable_"
                         "access_patterns-invoke_3:compute_kernel_code:r0.f90")
    assert driver.isfile()

    with driver.open("r") as driver_file:
        driver_code = driver_file.read()
    # Make sure we have both kernel calls in the driver.
    correct = """  do j = out_fld_internal_ystart, out_fld_internal_ystop, 1
    do i = out_fld_internal_xstart, out_fld_internal_xstop, 1
      call compute_kernel_code(i, j, out_fld, in_out_fld, in_fld, dx, """ \
      """in_fld_grid_dx, in_fld_grid_gphiu)
    enddo
  enddo
  do j = out_fld_internal_ystart, out_fld_internal_ystop, 1
    do i = out_fld_internal_xstart, out_fld_internal_xstop, 1
      call compute_kernel_code(i, j, out_fld, in_out_fld, in_fld, dx, """ \
      """in_fld_grid_dx, in_fld_grid_gphiu)
    enddo
  enddo"""
    assert correct in driver_code


# -----------------------------------------------------------------------------
def test_driver_creation_import_modules(fortran_reader):
    '''Tests if global and local interfaces in calls are handled correctly.
    '''

    code = '''program test_prog
              use my_module, only : mod_func
              call mod_func()
              call ext_func()
              call mod_func()
              end program test_prog'''
    psyir = fortran_reader.psyir_from_source(code)
    program = psyir.children[0]   # psyir is a FileContainer, take the program
    edc = ExtractDriverCreator()
    # Delete all symbols in the symbol table so we can check if the right
    # symbols are added:
    program.scope._symbol_table = SymbolTable()
    edc.import_modules(program, psyir.children[0])
    symbol_table = program.scope.symbol_table
    all_symbols = symbol_table.get_symbols()
    assert len(all_symbols) == 2
    assert str(all_symbols["my_module"]) == "my_module: <not linked>"
    mod_func = all_symbols["mod_func"]
    assert str(mod_func) == "mod_func : RoutineSymbol <DeferredType>"
