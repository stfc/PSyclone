# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology

''' This module tests the driver creation for extracted kernels.'''

import re

import pytest

from psyclone.domain.lfric import LFRicConstants, LFRicExtractDriverCreator
from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Literal, Routine, Schedule
from psyclone.psyir.symbols import INTEGER_TYPE
from psyclone.psyir.tools.dependency_tools import DependencyTools
from psyclone.tests.utilities import get_invoke


API = "dynamo0.3"


def test_lfric_driver_constructor():
    '''Tests the constructor of the LFRic driver creator.'''

    driver_creator = LFRicExtractDriverCreator()
    default_precision = {"i_def": "int32", "r_def": "real64",
                         "r_second": "real64", "r_solver": "real32",
                         "r_tran": "real32"}

    assert driver_creator._precision == default_precision

    # Check that we can modify the default precisions:
    new_precision = {"r_solver": "real64", "r_tran": "real64"}
    driver_creator = LFRicExtractDriverCreator(new_precision)
    assert (driver_creator._precision ==
            {"i_def": "int32", "r_def": "real64",
             "r_second": "real64", "r_solver": "real64",
             "r_tran": "real64"})


# ----------------------------------------------------------------------------
def test_lfric_driver_field_mapping():
    '''Tests that the mapping of fields to precision is as expected.'''
    mapping = LFRicConstants().DATA_TYPE_MAP
    correct = {}

    for field in ["columnwise_operator",
                  "field", "integer_field",
                  "operator", "r_solver_field",
                  "r_solver_operator", "r_tran_field"]:
        correct[mapping[field]["proxy_type"]] = mapping[field]["kind"]

    driver_creator = LFRicExtractDriverCreator()
    assert driver_creator._map_fields_to_precision == correct


# ----------------------------------------------------------------------------
def test_lfric_driver_constructor_error():
    '''Tests the error handling of the constructor of the LFRic driver
    creator.'''

    # Wrong argument type:
    with pytest.raises(InternalError) as err:
        _ = LFRicExtractDriverCreator(precision=1)
    assert ("The precision argument of the LFRic driver creator must be a "
            "dictionary, but got 'int'." in str(err.value))


# ----------------------------------------------------------------------------
def test_lfric_driver_valid_unit_name():
    '''Tests that we create valid unit names, i.e. less than 64 characters,
    and no ":" in name.'''

    long_name = "A"*100
    new_name = LFRicExtractDriverCreator.make_valid_unit_name(long_name)
    assert new_name == "A"*63

    special_characters = "aaa:bbb"
    new_name = \
        LFRicExtractDriverCreator.make_valid_unit_name(special_characters)
    assert new_name == "aaabbb"


# ----------------------------------------------------------------------------
def test_lfric_driver_flatten_string():
    '''Tests that a user-defined type access is correctly converted
    to a 'flattened' string.'''

    new_name = LFRicExtractDriverCreator.flatten_string("a%b%c")
    assert new_name == "a_b_c"


# ----------------------------------------------------------------------------
def test_lfric_driver_get_proxy_mapping():
    '''Tests that a kernel returns the right proxy mapping.'''

    _, invoke = get_invoke("26.6_mixed_precision_solver_vector.f90", API,
                           dist_mem=False, idx=0)
    driver_creator = LFRicExtractDriverCreator()

    mapping = driver_creator.get_proxy_name_mapping(invoke.schedule)
    assert mapping == ({'x_ptr_vector_proxy': 'x_ptr_vector',
                        'self_vec_type_vector_proxy': 'self_vec_type_vector',
                        'm1_proxy': 'm1',
                        'm2_proxy': 'm2'})


# ----------------------------------------------------------------------------
def test_lfric_driver_flatten_reference_error():
    '''Tests errors when flattening user defined symbols.'''
    driver_creator = LFRicExtractDriverCreator()

    with pytest.raises(InternalError) as err:
        driver_creator.flatten_reference("NoUserType", symbol_table=None,
                                         proxy_name_mapping={})
    assert "PSyclone internal error: " in str(err.value)


# ----------------------------------------------------------------------------
def test_lfric_driver_add_call(fortran_writer):
    '''Tests that adding a call detects errors and adds calls
    with and without parameters as expected.
    '''
    program = Routine("routine", is_program=True)
    program.symbol_table.find_or_create_tag("test")
    driver_creator = LFRicExtractDriverCreator()
    with pytest.raises(TypeError) as err:
        driver_creator.add_call(program, "test", [])
    assert ("Routine 'test' is a symbol of type 'Symbol', not a "
            "'RoutineSymbol'" in str(err.value))

    driver_creator.add_call(program, "my_sub", [])
    driver_creator.add_call(program, "my_sub_2", [Literal("1", INTEGER_TYPE)])
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
    assert ["routine"] == [sym.name for sym in program.symbol_table.symbols]

    driver_creator.import_modules(program, sched)
    # We should now have two more symbols:
    all_symbols = ["routine", "testkern_coord_w0_2_mod",
                   "testkern_coord_w0_2_code"]
    assert (all_symbols == [sym.name for sym in program.symbol_table.symbols])

    # Import twice so we test the handling of symbols that
    # are already in the symbol table:
    driver_creator.import_modules(program, sched)

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
    driver_creator.import_modules(program, sched)
    # Only the program routine itself should be in the symbol table after
    # calling `import_modules`.
    assert (["routine"] == [sym.name for sym in program.symbol_table.symbols])


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
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
    out = str(invoke.gen())

    filename = ("driver-field-test.f90")
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    # Now get all the variable names and variables listed in ProvideVariable
    # in extracting code, and make sure the same appear in the driver:
    # This re extracts the quoted variable name and the variable itself,
    # e.g. '"a", a'. The ReadVariable function takes exactly the same
    # parameters.
    provide = re.compile(r"ProvideVariable\((.*)\)")
    for line in out.split("\n"):
        # The current `gen` created double quotes, while using PSyIR
        # produces single quotes. So convert them first:
        line = line.replace('"', "'")
        grp = provide.search(line)
        if grp:
            # A special handling is required for output variables:
            # The output value of 'a' is written as '"a_post", a'. But the
            # driver needs to read this into a different variable called
            # 'a_post', so we also need to test if appending 'post''
            params = grp.groups(0)[0]
            if "_post', " in params:
                # It's an output variable, so the driver u
                assert f"({grp.groups(0)[0]}_post)" in driver
            else:
                assert f"({grp.groups(0)[0]})" in driver


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
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
    out = str(invoke.gen())

    filename = ("driver-field-array.f90")
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    assert "ProvideVariable(\"chi\", chi)" in out
    assert "ReadVariable('chi%1', chi_1_data)" in driver
    assert "ReadVariable('chi%2', chi_2_data)" in driver
    assert "ReadVariable('chi%3', chi_3_data)" in driver


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_lfric_driver_operator():
    '''Test handling of array of fields: they are written in one call to
    the extraction library, but the library will write each array member
    as an individual field. The driver needs to read in each individual
    array member into distinct variables.'''

    _, invoke = get_invoke("10.7_operator_read.f90", API,
                           dist_mem=False, idx=0)

    extract = LFRicExtractTrans()

    extract.apply(invoke.schedule.children[0],
                  options={"create_driver": True,
                           "region_name": ("operator", "test")})
    out = str(invoke.gen())
    # Check the structure members that are added for operators:
    assert ("ProvideVariable(\"mm_w3_proxy%local_stencil\", "
            "mm_w3_proxy%local_stencil)" in out)
    assert ("ProvideVariable(\"mm_w3_proxy%ncell_3d\", "
            "mm_w3_proxy%ncell_3d)" in out)
    # Check handling of field arrays
    assert "ProvideVariable(\"coord_post\", coord)" in out

    filename = ("driver-operator-test.f90")
    with open(filename, "r", encoding='utf-8') as my_file:
        driver = my_file.read()

    # Check that the user defined variables that are added for
    # operators are flattened correctly:
    assert ("ReadVariable('mm_w3_proxy%local_stencil', "
            "mm_w3_proxy_local_stencil" in driver)
    assert ("ReadVariable('mm_w3_proxy%ncell_3d', "
            "mm_w3_proxy_ncell_3d" in driver)
    # And check the field arrays just in case
    for i in range(1, 4):
        assert f"ReadVariable('coord_{i}_data', coord_{i}_data" in driver


# ----------------------------------------------------------------------------
@pytest.mark.parametrize("name, filename",
                         [("x_innerproduct_x",
                           "15.9.2_X_innerproduct_X_builtin.f90"),
                          ("sum_x", "15.14.3_sum_setval_field_builtin.f90"),
                          ("int_x", "15.10.3_int_X_builtin.f90"),
                          ("real_x", "15.28.2_real_X_builtin.f90")
                          ])
def test_lfric_driver_unsupported_builtins(name, filename, capsys):
    '''The following builtins do not have a proper lower_to_language_level
    method to create the PSyIR, so they are not supported in the driver
    creation: LFRicXInnerproductXKern, LFRicSumXKern, LFRicIntXKern,
    LFRicRealXKern. This tests also the error handling of the functions
    write_driver, get_driver_as_string, create. '''

    _, invoke = get_invoke(filename, API, dist_mem=False, idx=0)

    driver_creator = LFRicExtractDriverCreator()

    # The create method should raise an exception
    # -------------------------------------------
    with pytest.raises(NotImplementedError) as err:
        # The parameters do not really matter
        driver_creator.create(invoke.schedule, [], [], "extract", "_post",
                              region_name=("region", "name"))
    assert f"LFRic builtin '{name}' is not supported" in str(err.value)

    # The get_driver_as_string method returns
    # an empty string, but prints an error message
    # --------------------------------------------
    code = driver_creator.get_driver_as_string(invoke.schedule, [], [],
                                               "extract", "_post",
                                               ("region", "name"))
    assert code == ""
    out, _ = capsys.readouterr()
    assert (f"Cannot create driver for 'region-name' because:\nLFRic builtin "
            f"'{name}' is not supported" in out)

    # write_driver prints an error message, and does not return an error
    # ------------------------------------------------------------------
    driver_creator.write_driver(invoke.schedule, [], [],
                                "extract", "_post", ("region", "name"))
    assert (f"Cannot create driver for 'region-name' because:\nLFRic builtin "
            f"'{name}' is not supported" in out)


# ----------------------------------------------------------------------------
def test_lfric_driver_array_of_fields():
    '''Check that array accesses are created correctly, which is required
    for builtins. E.g. the following code needs to be created:
        do df = loop0_start, loop0_stop, 1
            f2(df) = a + f1(df)
        enddo
    As opposed to a normal kernel (where the whole field is passed in),
    here we need explicit array accesses.
    '''

    _, invoke = get_invoke("15.1.8_a_plus_X_builtin_array_of_fields.f90",
                           API, dist_mem=False, idx=0)
    dep = DependencyTools()
    input_list, output_list = dep.get_in_out_parameters(invoke.schedule)
    driver_creator = LFRicExtractDriverCreator()

    driver = driver_creator.\
        get_driver_as_string(invoke.schedule, input_list, output_list,
                             "extract", "_post",
                             region_name=("region", "name"))

    assert "f2(df) = a + f1(df)" in driver
