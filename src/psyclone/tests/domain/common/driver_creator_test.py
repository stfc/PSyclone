# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2026, Science and Technology Facilities Council.
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
# Modified: S. Siso, STFC Daresbury Lab,
#           I. Kavcic, Met Office

''' This module tests the driver creation for extracted kernels.'''

import re

import pytest

from psyclone.domain.common import DriverCreator
from psyclone.domain.lfric import LFRicDriverCreator
from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.psyir.nodes import (Assignment, Literal, Routine,
                                  StructureReference)
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, RoutineSymbol
from psyclone.tests.utilities import Compile, get_invoke


def test_basic_driver_add_call(fortran_writer):
    '''Tests that adding a call detects errors and adds calls
    with and without parameters as expected.
    '''
    program = Routine.create("routine", is_program=True)
    program.symbol_table.find_or_create_tag("test")
    with pytest.raises(TypeError) as err:
        DriverCreator.add_call(program, "test", [])
    assert ("Error creating call to 'test' - existing symbol is of type "
            "'Symbol', not a 'RoutineSymbol'" in str(err.value))

    # Clean up previous invalid test symbol
    del program.symbol_table.symbols_dict['test']
    del program.symbol_table.tags_dict['test']

    DriverCreator.add_call(program, "my_sub", [])
    DriverCreator.add_call(program, "my_sub_2",
                           [Literal("1", INTEGER_TYPE)])
    out = fortran_writer(program)
    assert "call my_sub()" in out
    assert "call my_sub_2(1)" in out


def test_lfric_driver_add_result_tests(fortran_writer):
    '''Tests adding tests that compare results.
    '''
    program = Routine.create("routine", is_program=True)
    program.symbol_table.find_or_create_tag("test", symbol_type=RoutineSymbol)
    a1 = program.symbol_table.find_or_create(
        "a1", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    a1_orig = program.symbol_table.find_or_create(
        "a1_orig", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    # This will add one test for the variable a1 with the
    # correct values a1_orig.
    DriverCreator.add_result_tests(program, [(a1, a1_orig)])
    out = fortran_writer(program)
    expected = """  call compare_init(1)
  call compare('a1', a1, a1_orig)
  call compare_summary()"""
    assert expected in out


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager_lfric")
def test_lfric_driver_simple_test():
    '''Test the full pipeline: Add kernel extraction to a kernel and
    request driver creation. Read in the written driver, and make sure
    any variable that is provided in the kernel call is also read
    in the driver. '''

    psy, invoke = get_invoke("26.6_mixed_precision_solver_vector.f90",
                             "lfric", dist_mem=False, idx=0)

    extract = LFRicExtractTrans()

    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    _ = psy.gen

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    driver = driver.lower()
    for line in [
        "if (ALLOCATED(psydata_filename)) then",
        "call extract_psy_data%OpenReadFileName(psydata_filename)",
        "else",
        "call extract_psy_data%OpenReadModuleRegion('field', 'test')",
        "end if",
        "call extract_psy_data%ReadVariable('a', a)",
        "call extract_psy_data%ReadVariable('m1_data', m1_data)",
        "call extract_psy_data%ReadVariable('m2_data', m2_data)",
        "call extract_psy_data%ReadVariable('map_w1', map_w1)",
        "call extract_psy_data%ReadVariable('map_w2', map_w2)",
        "call extract_psy_data%ReadVariable('map_w3', map_w3)",
        "call extract_psy_data%ReadVariable('ndf_w1', ndf_w1)",
        "call extract_psy_data%ReadVariable('ndf_w2', ndf_w2)",
        "call extract_psy_data%ReadVariable('ndf_w3', ndf_w3)",
        "call extract_psy_data%ReadVariable('nlayers_x_ptr_vector', "
        "nlayers_x_ptr_vector)",
        "call extract_psy_data%ReadVariable('"
        "self_vec_type_vector_data', self_vec_type_vector_data)",
        "call extract_psy_data%ReadVariable('undf_w1', undf_w1)",
        "call extract_psy_data%ReadVariable('undf_w2', undf_w2)",
        "call extract_psy_data%ReadVariable('undf_w3', undf_w3)",
        "call extract_psy_data%ReadVariable('x_ptr_vector_data', "
        "x_ptr_vector_data)",
    ]:
        assert line.lower() in driver, line

    # Loop variables should be removed. Test that the loop variable
    # is indeed called `cell`:
    assert re.search("do *cell *=", driver) is not None
    # Then make sure that the output variable is not stored:
    assert re.search("readvariable.*cell_post", driver) is None

    # A read-write/inc variable should not be allocated (since it will
    # be allocated as part of reading in its value):
    assert "ALLOCATE(x_ptr_vector," not in driver

    # Check that all module dependencies have been inlined:
    for mod in ["read_kernel_data_mod", "constants_mod", "kernel_mod",
                "argument_mod", "log_mod", "fs_continuity_mod",
                "testkern_mod"]:
        assert f"module {mod}" in driver
        assert f"end module {mod}" in driver

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.compile_file("driver-field-test.F90")


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager_lfric")
def test_lfric_driver_external_symbols():
    '''Test the handling of symbols imported from other modules, or calls to
    external functions that use module variables.

    '''
    psy, invoke = get_invoke("driver_creation/invoke_kernel_with_imported_"
                             "symbols.f90", "lfric", dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("import", "test")})
    code = psy.gen
    assert ('CALL extract_psy_data % PreDeclareVariable("'
            'module_var_a_post@module_with_var_mod", module_var_a)' in code)
    assert ('CALL extract_psy_data % ProvideVariable("'
            'module_var_a_post@module_with_var_mod", module_var_a)' in code)

    # Check that const-size arrays are exported:
    expected = [
      'use module_with_var_mod, only : const_size_array',
      'CALL extract_psy_data % PreDeclareVariable("const_size_array@'
      'module_with_var_mod", const_size_array)',
      'CALL extract_psy_data % PreDeclareVariable("const_size_array_post@'
      'module_with_var_mod", const_size_array)',
      'CALL extract_psy_data % ProvideVariable("const_size_array@'
      'module_with_var_mod", const_size_array)',
      'CALL extract_psy_data % ProvideVariable("const_size_array_post@'
      'module_with_var_mod", const_size_array)']
    for line in expected:
        assert line in code, line

    filename = "driver-import-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    assert ("call extract_psy_data%ReadVariable('module_var_a_post@"
            "module_with_var_mod', module_var_a_post)" in driver)
    assert ("call compare('module_var_a', module_var_a, module_var_a_post)"
            in driver)

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.compile_file("driver-import-test.F90")


@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager_lfric")
def test_base_driver_structure_accesses():
    '''This test creates a structure access in an LFRic example.
    The base class driver creator should flag this:
    '''

    psy, invoke = get_invoke("26.6_mixed_precision_solver_vector.f90", "lfric",
                             dist_mem=True, idx=0)
    extract = LFRicExtractTrans()

    kern_call = invoke.schedule.children[-1]
    extract.apply(kern_call,
                  options={"create_driver": True,
                           "region_name": ("field", "test")})

    # If there is a method call other than the ones from the DM infrastructure
    # it will fail
    base_symbol = invoke.schedule.symbol_table.lookup('x_ptr_vector')
    kern_call.parent.addchild(
        Assignment.create(
            StructureReference.create(base_symbol, ["method"]),
            StructureReference.create(base_symbol, ["method"])))
    with pytest.raises(VisitorError) as err:
        _ = psy.gen
    assert ("The DriverCreator does not support StructureReferences, any such "
            "references in the extraction region should have been flattened "
            "by the ExtractNode, but found: 'x_ptr_vector%method'"
            in str(err.value))


# ----------------------------------------------------------------------------
def test_lfric_driver_import_modules():
    '''Tests that adding a call detects errors as expected.
    '''
    program = Routine.create("routine", is_program=True)
    _, invoke = get_invoke("8_vector_field_2.f90", "lfric",
                           dist_mem=False, idx=0)

    sched = invoke.schedule
    # We need to lower to convert the kernels to calls
    sched.lower_to_language_level()
    # and add them to the extraction driver
    program.children.extend([node.copy() for node in sched.children])

    driver_creator = LFRicDriverCreator()

    # Initially we should only have no symbol other than the routine:
    assert ['routine'] == [sym.name for sym in program.symbol_table.symbols]

    driver_creator.import_modules(program)
    # We should now have two more symbols:
    all_symbols = ["routine",
                   "testkern_coord_w0_2_mod",
                   "testkern_coord_w0_2_code"]
    assert (all_symbols == [sym.name for sym in program.symbol_table.symbols])

    # Import twice so we test the handling of symbols that
    # are already in the symbol table:
    driver_creator.import_modules(program)

    # The symbol table should be the same as it was before:
    assert (all_symbols == [sym.name for sym in program.symbol_table.symbols])
