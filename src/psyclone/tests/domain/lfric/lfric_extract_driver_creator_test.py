# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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

import os
import pytest

from psyclone.core import Signature
from psyclone.domain.lfric import LFRicExtractDriverCreator
from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.errors import InternalError
from psyclone.parse import ModuleManager
from psyclone.psyir.nodes import Literal, Routine, Schedule
from psyclone.psyir.symbols import INTEGER_TYPE
from psyclone.psyir.tools import DependencyTools, ReadWriteInfo
from psyclone.tests.utilities import Compile, get_base_path, get_invoke


API = "dynamo0.3"


@pytest.fixture(scope='function')
def init_module_manager():
    ''' The tests in this module all assume that there is no pre-existing
    ModuleManager object, so this fixture ensures that the module manager
    instance is deleted before and after each test function. The latter
    makes sure that any other test executed next will automatically reload
    the default ModuleManager file.
    '''

    infrastructure_path = get_base_path(API)
    # Define the path to the ReadKernelData module (which contains functions
    # to read extracted data from a file) relative to the infrastructure path:
    psyclone_root = os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.dirname(os.path.dirname(infrastructure_path)))))
    read_mod_path = os.path.join(psyclone_root, "lib", "extract", "standalone")
    # Enforce loading of the default ModuleManager
    ModuleManager._instance = None

    module_manager = ModuleManager.get()
    module_manager.add_search_path(infrastructure_path)
    module_manager.add_search_path(read_mod_path)

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

    special_characters = "aaa:bbb"
    new_name = \
        LFRicExtractDriverCreator._make_valid_unit_name(special_characters)
    assert new_name == "aaabbb"


# ----------------------------------------------------------------------------
def test_lfric_driver_flatten_signature():
    '''Tests that a user-defined type access is correctly converted
    to a 'flattened' string.'''

    new_name = LFRicExtractDriverCreator._flatten_signature(Signature("a%b%c"))
    assert new_name == "a_b_c"


# ----------------------------------------------------------------------------
def test_lfric_driver_get_proxy_mapping():
    '''Tests that a kernel returns the right proxy mapping.'''

    _, invoke = get_invoke("26.6_mixed_precision_solver_vector.f90", API,
                           dist_mem=False, idx=0)
    driver_creator = LFRicExtractDriverCreator()

    mapping = driver_creator._get_proxy_name_mapping(invoke.schedule)
    assert mapping == ({'x_ptr_vector_proxy': 'x_ptr_vector',
                        'self_vec_type_vector_proxy': 'self_vec_type_vector',
                        'm1_proxy': 'm1',
                        'm2_proxy': 'm2'})


# ----------------------------------------------------------------------------
def test_lfric_driver_flatten_reference_error():
    '''Tests errors when flattening user defined symbols.'''
    driver_creator = LFRicExtractDriverCreator()

    with pytest.raises(InternalError) as err:
        driver_creator._flatten_reference("NoUserType", symbol_table=None,
                                          proxy_name_mapping={})
    assert ("Unexpected type 'str' in _flatten_reference, it must be a "
            "'StructureReference'" in str(err.value))


# ----------------------------------------------------------------------------
def test_lfric_driver_add_call(fortran_writer):
    '''Tests that adding a call detects errors and adds calls
    with and without parameters as expected.
    '''
    program = Routine("routine", is_program=True)
    program.symbol_table.find_or_create_tag("test")
    driver_creator = LFRicExtractDriverCreator()
    with pytest.raises(TypeError) as err:
        driver_creator._add_call(program, "test", [])
    assert ("Routine 'test' is a symbol of type 'Symbol', not a "
            "'RoutineSymbol'" in str(err.value))
    # Clean up previous invalid test symbol
    del program.symbol_table._symbols['test']
    del program.symbol_table._tags['test']

    driver_creator._add_call(program, "my_sub", [])
    driver_creator._add_call(program, "my_sub_2", [Literal("1", INTEGER_TYPE)])
    out = fortran_writer(program)
    assert "call my_sub()" in out
    assert "call my_sub_2(1)" in out


# ----------------------------------------------------------------------------
def test_lfric_driver_import_modules():
    '''Tests that adding a call detects errors as expected.
    '''
    program = Routine("routine", is_program=True)
    _, invoke = get_invoke("8_vector_field_2.f90", API,
                           dist_mem=False, idx=0)

    sched = invoke.schedule
    # We need to lower to convert the kernels to calls
    sched.lower_to_language_level()

    driver_creator = LFRicExtractDriverCreator()

    # Initially we should only have one symbol:
    assert ["routine"] == [sym.name for sym in program.symbol_table.symbols]

    driver_creator._import_modules(program.scope.symbol_table, sched)
    # We should now have two more symbols:
    all_symbols = ["routine", "testkern_coord_w0_2_mod",
                   "testkern_coord_w0_2_code"]
    assert (all_symbols == [sym.name for sym in program.symbol_table.symbols])

    # Import twice so we test the handling of symbols that
    # are already in the symbol table:
    driver_creator._import_modules(program.scope.symbol_table, sched)

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
    program = Routine("routine", is_program=True)
    driver_creator._import_modules(program.scope.symbol_table, sched)
    # Only the program routine itself should be in the symbol table after
    # calling `import_modules`.
    assert (["routine"] == [sym.name for sym in program.symbol_table.symbols])


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_simple_test():
    '''Test the full pipeline: Add kernel extraction to a kernel and
    request driver creation. Read in the written driver, and make sure
    any variable that is provided in the kernel call is also read
    in the driver. '''

    _, invoke = get_invoke("26.6_mixed_precision_solver_vector.f90", API,
                           dist_mem=False, idx=0)

    extract = LFRicExtractTrans()

    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    for line in ["call extract_psy_data%OpenRead('field', 'test')",
                 "call extract_psy_data%ReadVariable('a', a)",
                 "call extract_psy_data%ReadVariable('loop0_start', "
                 "loop0_start)",
                 "call extract_psy_data%ReadVariable('loop0_stop', "
                 "loop0_stop)",
                 "call extract_psy_data%ReadVariable('m1_data', m1_data)",
                 "call extract_psy_data%ReadVariable('m2_data', m2_data)",
                 "call extract_psy_data%ReadVariable('map_w1', map_w1)",
                 "call extract_psy_data%ReadVariable('map_w2', map_w2)",
                 "call extract_psy_data%ReadVariable('map_w3', map_w3)",
                 "call extract_psy_data%ReadVariable('ndf_w1', ndf_w1)",
                 "call extract_psy_data%ReadVariable('ndf_w2', ndf_w2)",
                 "call extract_psy_data%ReadVariable('ndf_w3', ndf_w3)",
                 "call extract_psy_data%ReadVariable('nlayers', nlayers)",
                 "call extract_psy_data%ReadVariable('"
                 "self_vec_type_vector_data', self_vec_type_vector_data)",
                 "call extract_psy_data%ReadVariable('undf_w1', undf_w1)",
                 "call extract_psy_data%ReadVariable('undf_w2', undf_w2)",
                 "call extract_psy_data%ReadVariable('undf_w3', undf_w3)",
                 "call extract_psy_data%ReadVariable('x_ptr_vector_data', "
                 "x_ptr_vector_data)",
                 "call extract_psy_data%ReadVariable('cell_post', cell_post)"]:
        assert line in driver

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
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_import_precision():
    '''Test that all required precision symbols are imported from
    constants_mod'''

    _, invoke = get_invoke("26.6_mixed_precision_solver_vector.f90", API,
                           dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()
    assert ("use constants_mod, only : i_def, l_def, r_bl, r_def, "
            "r_double, r_ncdf, r_phys, r_second, r_single, r_solver, "
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

    _, invoke = get_invoke("8_vector_field_2.f90", API,
                           dist_mem=False, idx=0)

    extract = LFRicExtractTrans()

    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "array")})
    # The extraction provides the array once, it is the responsibility
    # of the extraction library to create the individual fields.
    out = str(invoke.gen())
    assert "ProvideVariable(\"chi\", chi)" in out

    filename = "driver-field-array.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    # Check that the driver reads the three individual fields
    assert "ReadVariable('chi%1', chi_1_data)" in driver
    assert "ReadVariable('chi%2', chi_2_data)" in driver
    assert "ReadVariable('chi%3', chi_3_data)" in driver

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

    _, invoke = get_invoke("10.7_operator_read.f90", API,
                           dist_mem=False, idx=0)

    extract = LFRicExtractTrans()

    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("operator", "test")})
    out = str(invoke.gen())
    # Check the structure members that are added for operators:
    assert ("ProvideVariable(\"mm_w3_local_stencil\", "
            "mm_w3_local_stencil)" in out)
    assert ("ProvideVariable(\"mm_w3_proxy%ncell_3d\", "
            "mm_w3_proxy%ncell_3d)" in out)
    assert "ProvideVariable(\"coord_post\", coord)" in out

    filename = "driver-operator-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    # Check that the user defined variables that are added for
    # operators are flattened correctly:
    assert ("ReadVariable('mm_w3_local_stencil', "
            "mm_w3_local_stencil" in driver)
    assert ("ReadVariable('mm_w3_proxy%ncell_3d', "
            "mm_w3_proxy_ncell_3d" in driver)
    # And check the field arrays just in case
    for i in range(1, 4):
        assert (f"ReadVariable('coord_post%{i}', coord_{i}_data"
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
def test_lfric_driver_removing_structure_data():
    '''Check that array accesses correctly remove the `%data`(which would be
    added for builtins using f1_proxy$data(df)). E.g. the following code needs
    to be created:
        do df = loop0_start, loop0_stop, 1
            f2(df) = a + f1(df)
        enddo
    As opposed to a normal kernel (where the whole field is passed in),
    here we need explicit array accesses.
    '''

    _, invoke = get_invoke("15.1.8_a_plus_X_builtin_array_of_fields.f90",
                           API, dist_mem=False, idx=0)
    dep = DependencyTools()
    read_write_info = dep.get_in_out_parameters(invoke.schedule)
    driver_creator = LFRicExtractDriverCreator()

    driver = driver_creator.\
        get_driver_as_string(invoke.schedule, read_write_info, "extract",
                             "_post", region_name=("region", "name"))

    assert "call extract_psy_data%ReadVariable('f1_data', f1_data)" in driver
    assert ("call extract_psy_data%ReadVariable('f2_data_post', f2_data_post)"
            in driver)
    assert "ALLOCATE(f2_data, mold=f2_data_post)" in driver
    assert "f2_data(df) = a + f1_data(df)" in driver
    assert "if (ALL(f2_data - f2_data_post == 0.0)) then" in driver

    for mod in ["read_kernel_data_mod", "constants_mod"]:
        assert f"module {mod}" in driver
        assert f"end module {mod}" in driver

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.string_compiles(driver)


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "init_module_manager")
def test_lfric_driver_extract_some_kernels_only():
    '''Test that we can extract only some kernels of an invoke, but still
    get the right loop boundaries (which are dependent on the index
    of the kernels in the tree). This test can potentially be removed
    when TODO #1731 is done.'''

    _, invoke = get_invoke("4.5.2_multikernel_invokes.f90", API,
                           dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[2],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    code = str(invoke.gen())

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

    _, invoke = get_invoke("10.7_operator_read.f90", API,
                           dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    code = str(invoke.gen())
    # The variable coord is an output variable, it should
    # be provided once, the extraction library will write these
    # accesses as individual fields using the names "coord_post%1",
    # ..., "coord_post%3"
    assert "ProvideVariable(\"coord_post\", coord)" in code
    # The variable is not read, so it shouldn't be listed:
    assert "ProvideVariable(\"coord\", coord)" not in code

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    for i in range(1, 4):
        assert (f"ReadVariable('coord_post%{i}', coord_{i}_data_post)"
                in driver)
        assert f"ALL(coord_{i}_data - coord_{i}_data_post == 0.0))" in driver

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

    _, invoke = get_invoke("8_vector_field_2.f90", API,
                           dist_mem=False, idx=0)

    extract = LFRicExtractTrans()
    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("field", "test")})
    code = str(invoke.gen())
    assert 'ProvideVariable("chi", chi)' in code
    assert 'ProvideVariable("f1_data", f1_data)' in code
    assert 'ProvideVariable("chi_post", chi)' in code
    assert 'ProvideVariable("f1_data_post", f1_data)' in code

    filename = "driver-field-test.F90"
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    assert "ReadVariable('f1_data', f1_data)" in driver
    assert "ReadVariable('chi%1', chi_1_data)" in driver
    assert "ReadVariable('chi%2', chi_2_data)" in driver
    assert "ReadVariable('chi%3', chi_3_data)" in driver
    assert "ReadVariable('chi_post%1', chi_1_data_post)" in driver
    assert "ReadVariable('chi_post%2', chi_2_data_post)" in driver
    assert "ReadVariable('chi_post%3', chi_3_data_post)" in driver
    assert "ReadVariable('f1_data_post', f1_data_post)" in driver

    # Check that the required modules are inlined
    for mod in ["read_kernel_data_mod", "constants_mod", "kernel_mod",
                "argument_mod", "log_mod", "fs_continuity_mod",
                "testkern_coord_w0_2_mod"]:
        assert f"module {mod}" in driver
        assert f"end module {mod}" in driver

    # While the actual code is LFRic, the driver is stand-alone, and as such
    # does not need any of the infrastructure files
    build = Compile(".")
    build.compile_file("driver-field-test.F90")
