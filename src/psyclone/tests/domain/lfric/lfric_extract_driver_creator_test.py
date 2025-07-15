# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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

from pathlib import Path
import pytest

from psyclone.domain.lfric import LFRicExtractDriverCreator
from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.line_length import FortLineLength
from psyclone.parse import ModuleManager
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import (
    Literal, Routine, Schedule, Call, StructureReference)
from psyclone.psyir.symbols import INTEGER_TYPE
from psyclone.tests.utilities import (
    Compile, get_base_path, get_infrastructure_path, get_invoke)


API = "lfric"


@pytest.fixture(scope='function')
def init_module_manager():
    ''' The tests in this module all assume that there is no pre-existing
    ModuleManager object, so this fixture ensures that the module manager
    instance is deleted before and after each test function. The latter
    makes sure that any other test executed next will automatically reload
    the default ModuleManager file.
    '''

    test_files_dir = get_base_path(API)
    infrastructure_path = Path(get_infrastructure_path(API))
    # Define the path to the ReadKernelData module (which contains functions
    # to read extracted data from a file) relative to the infrastructure path:
    psyclone_root = infrastructure_path.parents[2]
    read_mod_path = (psyclone_root / "lib" / "extract" /
                     "standalone" / "lfric")
    # Enforce loading of the default ModuleManager
    ModuleManager._instance = None

    module_manager = ModuleManager.get()
    module_manager.add_search_path(test_files_dir)
    module_manager.add_search_path(str(infrastructure_path))
    module_manager.add_search_path(str(read_mod_path))

    # Now execute all tests
    yield

    # Enforce loading of the default ModuleManager
    ModuleManager._instance = None


# ----------------------------------------------------------------------------
def test_lfric_driver_valid_unit_name():
    '''Tests that we create valid unit names, i.e. less than 64 characters,
    and no ":" in name.'''

    long_name = "A"*100
    new_name = LFRicExtractDriverCreator._make_valid_unit_name(long_name)
    assert new_name == "A"*63

    special_characters = "aaa-bbb"
    new_name = \
        LFRicExtractDriverCreator._make_valid_unit_name(special_characters)
    assert new_name == "aaabbb"


# ----------------------------------------------------------------------------
def test_lfric_driver_add_call(fortran_writer):
    '''Tests that adding a call detects errors and adds calls
    with and without parameters as expected.
    '''
    program = Routine.create("routine", is_program=True)
    program.symbol_table.find_or_create_tag("test")
    driver_creator = LFRicExtractDriverCreator()
    with pytest.raises(TypeError) as err:
        driver_creator.add_call(program, "test", [])
    assert ("Error creating call to 'test' - existing symbol is of type "
            "'Symbol', not a 'RoutineSymbol'" in str(err.value))
    # Clean up previous invalid test symbol
    del program.symbol_table._symbols['test']
    del program.symbol_table._tags['test']

    driver_creator.add_call(program, "my_sub", [])
    driver_creator.add_call(program, "my_sub_2", [Literal("1", INTEGER_TYPE)])
    out = fortran_writer(program)
    assert "call my_sub()" in out
    assert "call my_sub_2(1)" in out


# ----------------------------------------------------------------------------
def test_lfric_driver_import_modules():
    '''Tests that adding a call detects errors as expected.
    '''
    program = Routine.create("routine", is_program=True)
    _, invoke = get_invoke("8_vector_field_2.f90", API,
                           dist_mem=False, idx=0)

    sched = invoke.schedule
    # We need to lower to convert the kernels to calls
    sched.lower_to_language_level()
    # and add them to the extraction driver
    program.children.extend([node.copy() for node in sched.children])

    driver_creator = LFRicExtractDriverCreator()

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


# ----------------------------------------------------------------------------
def test_lfric_driver_import_modules_no_import_interface(fortran_reader):
    '''This test checks the import_modules method if there is a call
    that has no ImportInterface by calling an unknown function.'''

    code = """subroutine test()
              call something()
              end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    # Find the schedule:
    sched = psyir.walk(Schedule)[0]
    sched.lower_to_language_level()
    driver_creator = LFRicExtractDriverCreator()
    program = Routine.create("routine", is_program=True)
    driver_creator.import_modules(program)
    # No symbols other than the routine should be in the symbol table after
    # calling `import_modules`.
    assert (['routine'] == [sym.name for sym in program.symbol_table.symbols])


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_simple_test():
    '''Test the full pipeline: Add kernel extraction to a kernel and
    request driver creation. Read in the written driver, and make sure
    any variable that is provided in the kernel call is also read
    in the driver. '''

    psy, invoke = get_invoke("26.6_mixed_precision_solver_vector.f90", API,
                             dist_mem=False, idx=0)

    extract = LFRicExtractTrans()

    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    _ = psy.gen

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

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
        "call extract_psy_data%ReadVariable('cell_post', cell_post)"
    ]:
        assert line.lower() in driver.lower(), line

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


@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_dm_test():
    '''Test the full pipeline with DM:  '''

    psy, invoke = get_invoke("26.6_mixed_precision_solver_vector.f90", API,
                             dist_mem=True, idx=0)

    extract = LFRicExtractTrans()

    kern_call = invoke.schedule.children[-1]
    extract.apply(kern_call,
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    code = psy.gen
    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    # Check that DM infrastructure calls such as "set_dirty" are still in the
    # psylayer, so following extraction regions still have the correct values,
    # while it has been removed from the driver (because it is executed in a
    # single rank and without the infrastructure imported).
    assert "set_dirty" in code
    assert "set_dirty" not in driver

    # If there is a method call other than the ones from the DM infrastructure
    # it will fail
    base_symbol = invoke.schedule.symbol_table.lookup('x_ptr_vector')
    kern_call.parent.addchild(
        Call.create(
            StructureReference.create(base_symbol, ["method"])))
    with pytest.raises(VisitorError) as err:
        code = psy.gen
    assert ("The provided PSyIR should not have StructureReferences, "
            "but found: x_ptr_vector%method" in str(err.value))


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_import_precision():
    '''Test that all required precision symbols are imported from
    constants_mod'''

    psy, invoke = get_invoke("26.6_mixed_precision_solver_vector.f90", API,
                             dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    _ = psy.gen

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()
    assert ("use constants_mod, only : i_def, l_def, r_bl, r_def, "
            "r_double, r_ncdf, r_second, r_single, r_solver, "
            "r_tran, r_um" in driver)

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
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_field_arrays():
    '''Test handling of array of fields: they are written in one call to
    the extraction library, but the library will write each array member
    as an individual field. The driver needs to read in each individual
    array member into distinct variables.'''

    psy, invoke = get_invoke("8_vector_field_2.f90", API,
                             dist_mem=False, idx=0)

    extract = LFRicExtractTrans()

    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "array")})
    # The extraction provides each array individual fields.
    out = psy.gen
    for idx in range(1, 4):
        assert f"ProvideVariable(\"chi_{idx}_data\", chi_{idx}_data)" in out

    filename = "driver-field-array.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    # Check that the driver reads the three individual fields
    assert "ReadVariable('chi_1_data', chi_1_data)" in driver
    assert "ReadVariable('chi_2_data', chi_2_data)" in driver
    assert "ReadVariable('chi_3_data', chi_3_data)" in driver

    for mod in ["read_kernel_data_mod", "constants_mod", "kernel_mod",
                "argument_mod", "log_mod", "fs_continuity_mod",
                "testkern_coord_w0_2_mod"]:
        assert f"module {mod}" in driver
        assert f"end module {mod}" in driver

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.compile_file("driver-field-array.F90")


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_operator():
    '''Test handling of operators, including the structure members
    that are implicitly added.'''

    psy, invoke = get_invoke("10.7_operator_read.f90", API,
                             dist_mem=False, idx=0)

    extract = LFRicExtractTrans()

    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("operator", "test")})
    out = psy.gen
    # Check the structure members that are added for operators:
    assert "mm_w3_proxy_ncell_3d = mm_w3_proxy%ncell_3d" in out
    assert ("ProvideVariable(\"mm_w3_local_stencil\", "
            "mm_w3_local_stencil)" in out)
    assert ("ProvideVariable(\"mm_w3_proxy_ncell_3d\", "
            "mm_w3_proxy_ncell_3d)" in out)

    filename = "driver-operator-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    # Check that the user defined variables that are added for
    # operators are flattened correctly:
    assert ("ReadVariable('mm_w3_local_stencil', "
            "mm_w3_local_stencil" in driver)
    assert ("ReadVariable('mm_w3_proxy_ncell_3d', "
            "mm_w3_proxy_ncell_3d" in driver)
    # And check the field arrays just in case
    for i in range(1, 4):
        assert (f"ReadVariable('coord_{i}_data', coord_{i}_data"
                in driver)

    for mod in ["read_kernel_data_mod", "constants_mod", "kernel_mod",
                "argument_mod", "log_mod", "fs_continuity_mod",
                "testkern_operator_read_mod"]:
        assert f"module {mod}" in driver
        assert f"end module {mod}" in driver

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.compile_file("driver-operator-test.F90")


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_extract_some_kernels_only():
    '''Test that we can extract only some kernels of an invoke, but still
    get the right loop boundaries (which are dependent on the index
    of the kernels in the tree). This test can potentially be removed
    when TODO #1731 is done.'''

    psy, invoke = get_invoke("4.5.2_multikernel_invokes.f90", API,
                             dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[2],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    code = psy.gen

    # We only extract the third loop, which uses the index '2' for
    # loop boundaries. So none of the previous loop indices should
    # be in the extract code:
    assert "PreDeclareVariable(\"loop0_start\", loop0_start)" not in code
    assert "PreDeclareVariable(\"loop1_start\", loop1_start)" not in code
    assert "PreDeclareVariable(\"loop2_start\", loop2_start)" in code
    assert "PreDeclareVariable(\"loop2_stop\", loop2_stop)" in code

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    # Make sure the driver does not have any information about other
    # kernels added, and that it uses index 2 for loop boundaries.
    assert "loop0_start" not in driver
    assert "loop1_start" not in driver
    assert "ReadVariable('loop2_start', loop2_start)" in driver
    assert "ReadVariable('loop2_stop', loop2_stop)" in driver

    for mod in ["read_kernel_data_mod", "constants_mod", "kernel_mod",
                "argument_mod", "testkern_any_space_2_mod"]:
        assert f"module {mod}" in driver
        assert f"end module {mod}" in driver

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.compile_file("driver-field-test.F90")


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_field_array_write():
    '''Test the handling of arrays of fields which are written.'''

    psy, invoke = get_invoke("10.7_operator_read.f90", API,
                             dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    code = psy.gen
    for i in range(1, 4):
        # The variable coord is an output variable, but it still must
        # be provided as input field (since a kernel might only write
        # some parts of a field - e.g. most kernels won't update halo
        # regions) to make sure a driver can reproduce the values
        # ProvideVariable("coord_1_data", coord_1_data)
        # of the elements that are not being updated.
        assert (f"ProvideVariable(\"coord_{i}_data\", coord_{i}_data)"
                in code)
        # The variable coord is an output value, but we still need to provide
        # its input value (for kernels that only update partial fields)
        assert (f"ProvideVariable(\"coord_{i}_data_post\", coord_{i}_data)"
                in code)

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    for i in range(1, 4):
        assert (f"ReadVariable('coord_{i}_data_post', coord_{i}_data_post)"
                in driver)
        assert (f"ReadVariable('coord_{i}_data', coord_{i}_data)"
                in driver)
        assert (f"compare('coord_{i}_data', coord_{i}_data, "
                f"coord_{i}_data_post)" in driver)

    for mod in ["read_kernel_data_mod", "constants_mod", "kernel_mod",
                "argument_mod", "log_mod", "fs_continuity_mod",
                "testkern_operator_read_mod"]:
        assert f"module {mod}" in driver
        assert f"end module {mod}" in driver

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.compile_file("driver-field-test.F90")


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_field_array_inc():
    '''Test the handling of arrays of fields which are incremented (i.e.
    read and written).'''

    psy, invoke = get_invoke("8_vector_field_2.f90", API,
                             dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    code = psy.gen
    for idx in range(1, 4):
        assert f'ProvideVariable("chi_{idx}_data", chi_{idx}_data)' in code
    assert 'ProvideVariable("f1_data", f1_data)' in code
    for idx in range(1, 4):
        assert f'videVariable("chi_{idx}_data_post", chi_{idx}_data)' in code
    assert 'ProvideVariable("f1_data_post", f1_data)' in code

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    assert "ReadVariable('f1_data', f1_data)" in driver
    assert "ReadVariable('chi_1_data', chi_1_data)" in driver
    assert "ReadVariable('chi_2_data', chi_2_data)" in driver
    assert "ReadVariable('chi_3_data', chi_3_data)" in driver
    assert "ReadVariable('chi_1_data_post', chi_1_data_post)" in driver
    assert "ReadVariable('chi_2_data_post', chi_2_data_post)" in driver
    assert "ReadVariable('chi_3_data_post', chi_3_data_post)" in driver
    assert "ReadVariable('f1_data_post', f1_data_post)" in driver

    # Check that the required modules are inlined
    for mod in ["read_kernel_data_mod", "constants_mod", "kernel_mod",
                "argument_mod", "log_mod", "fs_continuity_mod",
                "testkern_coord_w0_2_mod"]:
        assert f"module {mod}" in driver, driver
        assert f"end module {mod}" in driver

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.compile_file("driver-field-test.F90")


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_external_symbols():
    '''Test the handling of symbols imported from other modules, or calls to
    external functions that use module variables.

    '''
    psy, invoke = get_invoke("driver_creation/invoke_kernel_with_imported_"
                             "symbols.f90", API, dist_mem=False, idx=0)

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


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_external_symbols_name_clash():
    '''Test the handling of symbols imported from other modules, or calls to
    external functions that use module variables. In this example the external
    module uses a variable with the same name as the user code, which causes
    a name clash.

    '''
    psy, invoke = get_invoke("driver_creation/invoke_kernel_with_imported_"
                             "symbols.f90", API, dist_mem=False, idx=1)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("import", "test")})
    code = psy.gen

    # Make sure the imported, clashing symbol 'f1_data' is renamed:
    assert "use module_with_name_clash_mod, only : f1_data_1=>f1_data" in code
    assert ('CALL extract_psy_data % PreDeclareVariable("f1_data@'
            'module_with_name_clash_mod", f1_data_1)' in code)
    assert ('CALL extract_psy_data % ProvideVariable("f1_data@'
            'module_with_name_clash_mod", f1_data_1)' in code)

    # Even though PSyclone cannot find the variable, it should still be
    # extracted:
    filename = "driver-import-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    assert ("call extract_psy_data%ReadVariable("
            "'f1_data@module_with_name_clash_mod', f1_data_1)" in driver)
    assert ("call extract_psy_data%ReadVariable("
            "'f2_data@module_with_name_clash_mod', f2_data_1)" in driver)
    assert ("call extract_psy_data%ReadVariable("
            "'f2_data_post@module_with_name_clash_mod', f2_data_1_post)"
            in driver)

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.compile_file("driver-import-test.F90")


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_external_symbols_error():
    '''Test the handling of symbols imported from other modules, or calls to
    external functions that use module variables. In this example, the
    external module cannot be parsed by fparser (it contains syntax errors),
    resulting in external functions and variables that cannot be found.

    '''
    psy, invoke = get_invoke("driver_creation/invoke_kernel_with_imported_"
                             "symbols_error.f90", API, dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("import", "test")})

    with pytest.raises(VisitorError) as err:
        _ = psy.gen
    assert ("Could not find the tag 'non_existent_var@module_with_"
            "error_mod'" in str(err.value))


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_rename_externals():
    '''Tests that we get the used non-local symbols from a routine that
    renames a symbol reported correctly. Additionally, this also tests
    a potential name clash, if the renamed symbol should already exist
    in the PSy layer: in this case, the symbol also needs to be renamed
    on import

    '''
    # This example calls a subroutine that renames a symbol used from
    # a different module, i.e.:
    #     use module_with_var_mod, only: renamed_var => module_var_a

    psy, invoke = get_invoke("driver_creation/"
                             "invoke_kernel_rename_symbols.f90",
                             API, dist_mem=False, idx=0)

    extract = LFRicExtractTrans()

    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    _ = psy.gen

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    # The invoking program also contains a variable `module_var_a`. So
    # the `module_var_a` from the module must be renamed on import and it
    # becomes `module_var_a_1`.
    assert ("use module_with_var_mod, only : module_var_a_1=>module_var_a"
            in driver)
    assert ("call extract_psy_data%ReadVariable("
            "'module_var_a@module_with_var_mod', module_var_a_1)" in driver)

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files. The string also needs
    # to be wrapped explicitly (which the driver creation only does
    # when writing the result to a file).
    build = Compile(".")
    fll = FortLineLength()
    code = fll.process(driver)
    build.string_compiles(code)
