# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council
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

# TODO #706: Add compilation support

from collections import namedtuple
from pathlib import Path
import os
import re

import pytest

from psyclone.configuration import Config
from psyclone.errors import InternalError
from psyclone.domain.common import ExtractDriverCreator
from psyclone.domain.gocean.transformations import (GOceanExtractTrans,
                                                    GOConstLoopBoundsTrans)
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Reference, Routine
from psyclone.psyir.symbols import ContainerSymbol, SymbolTable
from psyclone.psyir.tools import DependencyTools, ReadWriteInfo
from psyclone.psyir.transformations import PSyDataTrans, TransformationError
from psyclone.tests.utilities import get_base_path, get_invoke

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
    yield
    PSyDataTrans._used_kernel_names = {}


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_driver_creation1():
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
                  "kernel:compute_kernel_code:r0.f90")
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

  real*8, allocatable, dimension(:,:) :: out_fld
  real*8, allocatable, dimension(:,:) :: in_out_fld
  real*8, allocatable, dimension(:,:) :: in_fld
  real*8, allocatable, dimension(:,:) :: dx
  real*8, allocatable, dimension(:,:) :: in_fld_grid_gphiu
  real*8, allocatable, dimension(:,:) :: out_fld_post
  real*8 :: in_fld_grid_dx
  real*8, allocatable, dimension(:,:) :: in_out_fld_post
  type(ReadKernelDataType) :: extract_psy_data
  call extract_psy_data%OpenRead('psy_extract_example_with_various_variable_''' \
  '''access_patterns', 'invoke_0_compute_kernel:compute_kernel_code:r0')
  call extract_psy_data%ReadVariable('out_fld_post', out_fld_post)
  ALLOCATE(out_fld, mold=out_fld_post)
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
@pytest.mark.usefixtures("change_into_tmpdir")
def test_driver_creation2():
    '''Verify that the region names are used when opening the file, and that
    constant loop boundaries work as expected.

    '''
    # Use tmpdir so that the driver is created in tmp

    _, invoke = get_invoke("driver_test.f90", GOCEAN_API,
                           idx=0, dist_mem=False)

    nodes = [invoke.schedule.children[0]]
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(invoke.schedule)

    dep = DependencyTools()
    read_write_info = dep.get_in_out_parameters(nodes)

    edc = ExtractDriverCreator()

    driver_code = edc.get_driver_as_string(nodes, read_write_info,
                                           "extract", "_post",
                                           ("module_name", "local_name"))

    # This is an excerpt of the code that should get created.
    # It is tested line by line since there is other code in between
    # which is not important, and the order might also change. It also
    # tests if unique variable names are created in the driver: the user
    # program contains a local variable 'dx', which clashes with the grid
    # property dx. The grid property will be renamed to 'dx_1':
    expected = '''use read_kernel_data_mod, only : ReadKernelDataType

  integer :: istop
  integer :: jstop
  real*8, allocatable, dimension(:,:) :: out_fld
  real*8, allocatable, dimension(:,:) :: in_out_fld
  real*8, allocatable, dimension(:,:) :: in_fld
  real*8, allocatable, dimension(:,:) :: dx
  real*8, allocatable, dimension(:,:) :: in_fld_grid_gphiu
  real*8, allocatable, dimension(:,:) :: out_fld_post
  real*8 :: in_fld_grid_dx
  real*8, allocatable, dimension(:,:) :: in_out_fld_post
  type(ReadKernelDataType) :: extract_psy_data
  call extract_psy_data%OpenRead('module_name', 'local_name')
  call extract_psy_data%ReadVariable('out_fld_post', out_fld_post)
  ALLOCATE(out_fld, mold=out_fld_post)
  out_fld = 0
  call extract_psy_data%ReadVariable('in_fld', in_fld)
  call extract_psy_data%ReadVariable('in_out_fld_post', in_out_fld_post)
  call extract_psy_data%ReadVariable('dx', dx)
  call extract_psy_data%ReadVariable('in_fld%grid%dx', in_fld_grid_dx)
  call extract_psy_data%ReadVariable('in_fld%grid%gphiu', in_fld_grid_gphiu)
  istop = out_fld_grid_subdomain_internal_xstop
  jstop = out_fld_grid_subdomain_internal_ystop
  do j = 2, jstop, 1
    do i = 2, istop + 1, 1
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
    driver = Path("driver-module_name-local_name.f90")
    assert driver.is_file()

    with driver.open("r", encoding="utf-8") as driver_file:
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
  ALLOCATE(out_fld, mold=out_fld_post0)
  call extract_psy_data%ReadVariable('out_fld_post', out_fld_post)"""

    for line in expected.split("\n"):
        assert line in driver_code

    # Now test that more than one variable clash is handled. The third
    # invoke uses:
    # "out_fld" as output field
    # "out_fld_post" as input field (first clash -->
    # suffix becomes "_post0")
    # "out_fld_post0" as input+output field (next clash -->
    # suffix = "_post1")
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

    # Check that *out_fld_post* (input/output) is declared correctly. It
    # must be declared twice: once for the input value using the original
    # variable name, and once as output using the "_post1" suffix.
    assert ('PreDeclareVariable("out_fld_post", out_fld_post)'
            in extract_code)
    assert ('PreDeclareVariable("out_fld_post_post1", out_fld_post)'
            in extract_code)

    # Check that *out_fld_post0* is declared correctly: as input-only
    # variable it must be declared once for using the original variable
    # name.
    assert ('PreDeclareVariable("out_fld_post0", out_fld_post0)'
            in extract_code)
    assert ('PreDeclareVariable("out_fld_post0_post1", out_fld_post0)'
            not in extract_code)


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
    with pytest.raises(InternalError) as err:
        # The symbol table can be None, that code is not reached.
        edc.create_flattened_symbol("new_name", ref, None)
    # Leave out the config filename in the test
    assert re.search("Type 'real' of the property reference "
                     "'in_fld%grid%gphiu' as defined in the config file "
                     "'.*' is not supported in the GOcean API",
                     str(err.value))

    # Monkey patch the grid property dictionary to remove the
    # go_grid_lat_u entry, triggering an earlier error:
    api_config = Config.get().api_conf("gocean1.0")
    grid_properties = api_config.grid_properties
    monkeypatch.delitem(grid_properties, "go_grid_lat_u")

    with pytest.raises(InternalError) as err:
        # The symbol table can be None, that code is not reached.
        edc.create_flattened_symbol("new_name", ref, None)
    assert re.search("Could not find type for reference 'in_fld%grid%gphiu' "
                     "in the config file '.*'.", str(err.value))

    grid_properties = api_config.grid_properties

    # We have to create an invalid property from scratch, since the
    # GOceanConfig implementation only allows valid types.
    Property = namedtuple("Property", "fortran type intrinsic_type")
    prop = Property("{0}%grid%gphiu", "invalid-type", "real")
    monkeypatch.setitem(grid_properties, "go_grid_lat_u", prop)
    # We need a new instance, since the instance above was modified
    edc = ExtractDriverCreator()
    with pytest.raises(InternalError) as err:
        edc.create_flattened_symbol("new_name", ref, SymbolTable())
    assert re.search("The expression 'in_fld%grid%gphiu' maps to an unknown "
                     "GOcean property type 'invalid-type' in the config "
                     "file '.*'.", str(err.value))


# -----------------------------------------------------------------------------
def test_errors_add_call():
    '''Test that an error is raised if the symbol supplied to 'add_call()' is
    not a RoutineSymbol.

    '''
    program = Routine("test", is_program=True)
    program_symbol_table = program.symbol_table

    # Add 'psy_data_mod' as a container symbol:
    psy_data_mod = ContainerSymbol("psy_data_mod")
    program_symbol_table.add(psy_data_mod)

    edc = ExtractDriverCreator()

    # Then try to add a call to 'psy_data_mod':
    with pytest.raises(TypeError) as err:
        edc.add_call(program, "psy_data_mod", [])
    assert ("Error when adding call: Routine 'psy_data_mod' is a "
            "symbol of type 'ContainerSymbol', not a 'RoutineSymbol'."
            in str(err.value))


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
    with pytest.raises(InternalError) as err:
        edc.add_all_kernel_symbols(schedule_copy, symbol_table)
    assert ("Error when constructing driver for 'invoke_0_compute_kernel': "
            "Unknown derived type 'unknown type' in reference "
            "'out_fld%internal%ystart'." in str(err.value))
    ref.symbol.datatype._name = "r2d_field"

    # Define some new default types to trigger the error of
    # not finding the default type to use:
    edc._default_types = {"a": "a", "b": "b"}
    symbol_table = SymbolTable()
    with pytest.raises(InternalError) as err:
        edc.add_all_kernel_symbols(schedule_copy, symbol_table)
    # With no default types defined at all, the reference to 'i' will be
    # the first reference that triggers the unknown intrinsic
    assert ("Error when constructing driver for 'invoke_0_compute_kernel': "
            "Unknown intrinsic data type 'Intrinsic.INTEGER' in reference "
            "'i'. Valid types are '['a', 'b']'" in str(err.value))


# -----------------------------------------------------------------------------
def test_driver_creation_same_symbol():
    '''Make sure that if a symbol appears in more than one invoke no duplicated
    symbol is created.

    '''
    # The fourth invoke calls a kernel twice with identical parameters.
    # This means the same symbols are re-encountered when handling the
    # second kernel call (since each kernel argument has already been
    # declared when the first kernel was done).
    _, invoke = get_invoke("driver_test.f90", GOCEAN_API,
                           idx=3, dist_mem=False)

    nodes = [invoke.schedule.children[0]]
    dep = DependencyTools()
    read_write_info = dep.get_in_out_parameters(nodes)

    edc = ExtractDriverCreator()
    driver_code = edc.get_driver_as_string(nodes, read_write_info,
                                           "extract", "_post",
                                           ("module_name", "local_name"))
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
    edc = ExtractDriverCreator()
    # Delete all symbols in the symbol table so we can check if the right
    # symbols are added:
    program.scope._symbol_table = SymbolTable()
    edc.import_modules(program, psyir.children[0])
    symbol_table = program.scope.symbol_table
    all_symbols = symbol_table.get_symbols()
    assert len(all_symbols) == 2
    assert str(all_symbols["my_module"]) == \
        "my_module: ContainerSymbol<not linked>"
    mod_func = all_symbols["mod_func"]
    assert str(mod_func) == ("mod_func: RoutineSymbol<UnresolvedType, "
                             "pure=unknown, elemental=unknown>")


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_driver_node_verification():
    '''Test that the create() method verifies the node list it receives
    and only accept the valid parameters.

    '''
    # Use tmpdir in case that the call below does not raise an
    # exception, which would result in the driver being created
    # in the current directory.

    api = "gocean1.0"
    _, info = parse(os.path.join(get_base_path(api), "driver_test.f90"),
                    api=api)
    psy = PSyFactory(api, distributed_memory=False).create(info)
    invokes = psy.invokes.invoke_list

    edc = ExtractDriverCreator()

    # Provide the nodes in the wrong order.
    # Invoke #3 has all in all three kernels:
    schedule = invokes[3].schedule
    read_write_info = ReadWriteInfo()
    with pytest.raises(TransformationError) as err:
        edc.create(nodes=[schedule.children[1],
                          schedule.children[2],
                          schedule.children[0]],
                   read_write_info=read_write_info, prefix="extract",
                   postfix="post", region_name=("file", "region"))
    assert ("Children are not consecutive children of one parent"
            in str(err.value))
    assert ("has position 0, but previous child had position 2."
            in str(err.value))

    # Provide nodes from different invokes:
    with pytest.raises(TransformationError) as err:
        edc.create(nodes=[invokes[3].schedule.children[1],
                          invokes[2].schedule.children[0]],
                   read_write_info=read_write_info, prefix="extract",
                   postfix="post", region_name=("file", "region"))
    assert ("supplied nodes are not children of the same parent."
            in str(err.value))
