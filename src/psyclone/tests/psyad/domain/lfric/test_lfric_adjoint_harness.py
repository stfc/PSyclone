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
# Authors: R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab

'''Provides py.test tests of LFRic-specific PSyclone adjoint test-harness
   functionality.'''

import pytest
from fparser import api as fpapi

from psyclone.domain.lfric import LFRicSymbolTable, LFRicConstants, LFRicTypes
from psyclone.domain.lfric.algorithm import (
    LFRicBuiltinFunctor, LFRicAlg, LFRicBuiltinFunctorFactory,
    LFRicKernelFunctor)
from psyclone.errors import InternalError, GenerationError
from psyclone.psyad.domain.lfric import lfric_adjoint_harness
from psyclone.psyad.domain.lfric.lfric_adjoint_harness import (
    _compute_lfric_inner_products,
    _compute_field_inner_products,
    _init_fields_random,
    _init_operators_random,
    _init_scalar_value,
    _validate_geom_arg,
    _lfric_create_real_comparison,
    generate_lfric_adjoint_harness)
from psyclone.psyir import nodes
from psyclone.psyir.symbols import (DataSymbol, REAL_TYPE, BOOLEAN_TYPE,
                                    ArrayType, DataTypeSymbol, UnresolvedType,
                                    INTEGER_TYPE, ContainerSymbol,
                                    ImportInterface, ScalarType, SymbolTable)


@pytest.fixture(name="type_map", scope="module")
def lfric_consts_fixture():
    '''pytest fixture that returns the DATA_TYPE_MAP from LFRicConstants.'''
    return LFRicConstants().DATA_TYPE_MAP


# _compute_lfric_inner_products

def test_compute_inner_products_scalars(fortran_writer):
    '''Test that _compute_lfric_inner_products generates the expected code
    for scalars and ignores any of boolean type.'''
    table = LFRicSymbolTable()
    prog = nodes.Routine.create("test_prog", table, [], is_program=True)
    sum_sym = table.new_symbol(root_name="my_sum",
                               symbol_type=DataSymbol, datatype=REAL_TYPE)
    sym1 = table.new_symbol(root_name="var1", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    sym2 = table.new_symbol(root_name="var2", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    sym3 = table.new_symbol(root_name="var3", symbol_type=DataSymbol,
                            datatype=BOOLEAN_TYPE)
    _compute_lfric_inner_products(
        prog, [(sym1, sym1), (sym1, sym2), (sym3, sym3)], [], sum_sym)
    gen = fortran_writer(prog)
    # The resulting code should not include var3 since it is boolean.
    assert ("  my_sum = 0.0\n"
            "  my_sum = my_sum + var1 * var1\n"
            "  my_sum = my_sum + var1 * var2\n\n"
            "end program" in gen)


def test_compute_inner_products_fields(fortran_writer):
    '''Test that _compute_lfric_inner_products generates the expected code
    when supplied with symbols representing the innerproducts of fields.'''
    table = LFRicSymbolTable()
    prog = nodes.Routine.create("test_prog", table, [], is_program=True)
    sum_sym = table.new_symbol(root_name="my_sum",
                               symbol_type=DataSymbol, datatype=REAL_TYPE)
    sym1 = table.new_symbol(root_name="ip1", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    sym2 = table.new_symbol(root_name="ip2", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    # For field vectors we have an array of inner-product values to sum.
    atype = ArrayType(REAL_TYPE, [3])
    sym3 = table.new_symbol(root_name="ip3", symbol_type=DataSymbol,
                            datatype=atype)
    _compute_lfric_inner_products(prog, [], [sym1, sym2, sym3], sum_sym)
    gen = fortran_writer(prog)
    assert ("  my_sum = 0.0\n"
            "  my_sum = my_sum + ip1\n"
            "  my_sum = my_sum + ip2\n"
            "  my_sum = my_sum + ip3(1_i_def)\n"
            "  my_sum = my_sum + ip3(2_i_def)\n"
            "  my_sum = my_sum + ip3(3_i_def)\n" in gen)


# _compute_field_inner_products

def test_compute_field_inner_products(fortran_writer, type_map):
    '''Check that _compute_field_inner_products generates the expected symbols,
    assignments and functors for fields.'''
    bin_factory = LFRicBuiltinFunctorFactory.get()
    table = LFRicSymbolTable()
    prog = nodes.Routine.create("test_prog", table, [], is_program=True)
    csym = table.new_symbol(type_map["field"]["module"],
                            symbol_type=ContainerSymbol)
    fld_type = table.new_symbol(type_map["field"]["type"],
                                symbol_type=DataTypeSymbol,
                                datatype=UnresolvedType(),
                                interface=ImportInterface(csym))
    fld1 = table.new_symbol("field1", symbol_type=DataSymbol,
                            datatype=fld_type)
    fld2 = table.new_symbol("field2", symbol_type=DataSymbol,
                            datatype=fld_type)
    sums, functors = _compute_field_inner_products(prog, [(fld1, fld1),
                                                          (fld1, fld2)])
    assert len(sums) == 2
    assert isinstance(sums[0], DataSymbol)
    assert sums[0].name.endswith("_inner_prod")
    assert sums[0].name in table
    assert len(functors) == 2
    assert isinstance(functors[0],
                      bin_factory._get_builtin_class("x_innerproduct_x"))
    assert isinstance(functors[1],
                      bin_factory._get_builtin_class("x_innerproduct_y"))
    assert isinstance(prog.children[0], nodes.Assignment)
    code = fortran_writer(prog)
    assert "field1_inner_prod = 0.0_r_def" in code
    assert "field1_field2_inner_prod = 0.0_r_def" in code


def test_compute_field_vector_inner_products(fortran_writer, type_map):
    '''Check that _compute_field_inner_products generates the expected symbols,
    assignments and functors for field vectors.'''
    bin_factory = LFRicBuiltinFunctorFactory.get()
    table = LFRicSymbolTable()
    prog = nodes.Routine.create("test_prog", table, [], is_program=True)
    csym = table.new_symbol(type_map["field"]["module"],
                            symbol_type=ContainerSymbol)
    fld_type = table.new_symbol(type_map["field"]["type"],
                                symbol_type=DataTypeSymbol,
                                datatype=UnresolvedType(),
                                interface=ImportInterface(csym))
    fld1 = table.new_symbol("field1", symbol_type=DataSymbol,
                            datatype=ArrayType(fld_type, [3]))
    fld2 = table.new_symbol("field2", symbol_type=DataSymbol,
                            datatype=ArrayType(fld_type, [3]))
    fld3 = table.new_symbol("field3", symbol_type=DataSymbol,
                            datatype=fld_type)
    sums, functors = _compute_field_inner_products(prog, [(fld1, fld1),
                                                          (fld3, fld3),
                                                          (fld2, fld1)])
    assert len(sums) == 3
    assert sums[0].is_array
    assert not sums[1].is_array
    assert sums[2].is_array
    assert len(functors) == 7
    for dim in range(4):
        assert isinstance(functors[dim],
                          bin_factory._get_builtin_class("x_innerproduct_x"))
    code = fortran_writer(prog)
    for dim in range(1, 4):
        assert f"field1_inner_prod({dim}_i_def) = 0.0_r_def" in code
        assert f"field2_field1_inner_prod({dim}_i_def) = 0.0_r_def" in code


def test_compute_field_inner_products_errors(type_map):
    '''Check that _compute_field_inner_products raises the expected errors
    when passed incorrect arguments.'''
    table = LFRicSymbolTable()
    prog = nodes.Routine.create("test_prog", table, [], is_program=True)
    csym = table.new_symbol(type_map["field"]["module"],
                            symbol_type=ContainerSymbol)
    fld_type = table.new_symbol(type_map["field"]["type"],
                                symbol_type=DataTypeSymbol,
                                datatype=UnresolvedType(),
                                interface=ImportInterface(csym))
    fld1 = table.new_symbol("field1", symbol_type=DataSymbol,
                            datatype=ArrayType(fld_type, [3]))
    fld3 = table.new_symbol("field3", symbol_type=DataSymbol,
                            datatype=fld_type)
    # Check that an inner product of a field vector with a field is rejected.
    with pytest.raises(InternalError) as err:
        _compute_field_inner_products(prog, [(fld1, fld3)])
    assert ("Cannot compute the inner product of fields 'field1' and 'field3' "
            "because they are of different types: Array<field_type: "
            "DataTypeSymbol, shape=[3]> and field_type: DataTypeSymbol" in
            str(err.value))
    with pytest.raises(TypeError) as err:
        _compute_field_inner_products(prog, [(fld1, "hello")])
    assert ("Each pair of fields/field-vectors must be supplied as "
            "DataSymbols but got:" in str(err.value))
    # Break the datatype of one of the fields
    fld1.datatype = INTEGER_TYPE
    with pytest.raises(InternalError) as err:
        _compute_field_inner_products(prog, [(fld1, fld1)])
    assert ("Expected a field symbol to either be of ArrayType or have a type "
            "specified by a DataTypeSymbol but found Scalar<INTEGER, "
            "UNDEFINED> for field 'field1'" in str(err.value))


# _init_fields_random

def test_init_fields_random(type_map):
    '''Check that the _init_fields_random() routine works as expected.'''
    table = LFRicSymbolTable()
    fld_type = DataTypeSymbol(type_map["field"]["type"],
                              datatype=UnresolvedType())
    table.add(fld_type)
    fld1 = DataSymbol("field1", datatype=fld_type)
    fields = [fld1]
    fld1_input = DataSymbol("field1_input", datatype=fld_type)
    input_syms = {"field1": fld1_input}
    table.add(fld1)
    table.add(fld1_input)
    kernels = _init_fields_random(fields, input_syms, table)
    assert len(kernels) == 2
    assert isinstance(kernels[0], LFRicBuiltinFunctor)
    assert kernels[0].symbol.name == "setval_random"
    assert kernels[0].children[0].symbol.name == "field1"
    assert isinstance(kernels[1], LFRicBuiltinFunctor)
    assert kernels[1].symbol.name == "setval_x"
    assert kernels[1].children[1].symbol.name == "field1"
    assert kernels[1].children[0].symbol.name == "field1_input"


def test_init_fields_random_vector(type_map):
    '''Check that the _init_fields_random() routine works as expected for
    a field vector.

    '''
    table = LFRicSymbolTable()
    idef_sym = table.add_lfric_precision_symbol("i_def")
    idef_type = ScalarType(ScalarType.Intrinsic.REAL, idef_sym)

    fld_type = DataTypeSymbol(type_map["field"]["type"],
                              datatype=UnresolvedType())
    table.add(fld_type)
    fld1 = DataSymbol("field1", datatype=ArrayType(fld_type, [3]))
    fields = [fld1]
    fld1_input = DataSymbol("field1_input", datatype=ArrayType(fld_type, [3]))
    table.add(fld1)
    table.add(fld1_input)
    input_syms = {"field1": fld1_input}
    kernels = _init_fields_random(fields, input_syms, table)
    assert len(kernels) == 6
    for idx in range(3):
        kidx = 2*idx
        lit = nodes.Literal(f"{idx+1}", idef_type)
        assert isinstance(kernels[kidx], LFRicBuiltinFunctor)
        assert kernels[kidx].symbol.name == "setval_random"
        assert kernels[kidx].children[0].symbol.name == "field1"
        assert kernels[kidx].children[0].indices == [lit]
        kidx += 1
        assert isinstance(kernels[kidx], LFRicBuiltinFunctor)
        assert kernels[kidx].symbol.name == "setval_x"
        assert kernels[kidx].children[1].symbol.name == "field1"
        assert kernels[kidx].children[1].indices == [lit]
        assert kernels[kidx].children[0].symbol.name == "field1_input"
        assert kernels[kidx].children[0].indices == [lit]


def test_init_fields_random_error():
    '''Check that _init_fields_random raises the expected error if the supplied
    field is not of the correct type.

    '''
    fld1 = DataSymbol("field1", datatype=INTEGER_TYPE)
    fields = [fld1]
    inputs = {"field1": fld1}
    with pytest.raises(InternalError) as err:
        _init_fields_random(fields, inputs, LFRicSymbolTable())
    assert ("Expected a field symbol to either be of ArrayType or have a type "
            "specified by a DataTypeSymbol but found Scalar<INTEGER, "
            "UNDEFINED> for field 'field1'" in str(err.value))


# _init_operators_random

def test_init_operators_random(type_map):
    '''Check that the _init_operators_random() routine works as expected.'''
    table = LFRicSymbolTable()
    op_type = DataTypeSymbol(type_map["operator"]["type"],
                             datatype=UnresolvedType())
    table.add(op_type)
    op1 = DataSymbol("op1", datatype=op_type)
    op2 = DataSymbol("op2", datatype=op_type)
    ops = [op1, op2]
    table.add(op1)
    kernels = _init_operators_random(ops, table)
    assert len(kernels) == 2
    assert isinstance(kernels[0], LFRicKernelFunctor)
    assert kernels[0].symbol.name == "setop_random_kernel_type"
    assert kernels[0].children[0].symbol is op1
    assert kernels[1].symbol.name == "setop_random_kernel_type"
    assert kernels[1].children[0].symbol is op2
    csym = table.lookup("setop_random_kernel_mod")
    assert isinstance(csym, ContainerSymbol)
    assert (table.lookup("setop_random_kernel_type").interface.container_symbol
            is csym)


# _init_scalar_values

def test_init_scalar_value(monkeypatch):
    '''Check that _init_scalar_value() adds the expected nodes to the supplied
    Routine.'''
    table = LFRicSymbolTable()
    routine = nodes.Routine("testkern_code", symbol_table=table)
    sym1 = DataSymbol("my_real1", LFRicTypes("LFRicRealScalarDataType")())
    sym2 = DataSymbol("my_int2", LFRicTypes("LFRicIntegerScalarDataType")())
    sym2_input = DataSymbol("my_int2_input",
                            LFRicTypes("LFRicIntegerScalarDataType")())
    table.add(sym2_input)
    _init_scalar_value(sym1, routine, {})
    assert len(routine.children) == 1
    # We should get a call to random_number for a real scalar.
    assert isinstance(routine[0], nodes.Call)
    _init_scalar_value(sym2, routine, {"my_int2": sym2_input})
    assert len(routine.children) == 3
    # An integer should just be assigned the value 1 (TODO #2087)
    assert isinstance(routine[1], nodes.Assignment)
    assert routine[1].rhs.value == "1"
    # and we should store this value as it's listed as an 'input'.
    assert isinstance(routine[2], nodes.Assignment)
    assert routine[2].lhs.symbol.name == "my_int2_input"
    # A logical argument should just be assigned False (TODO #2087)
    sym3 = DataSymbol("my_bool", LFRicTypes("LFRicLogicalScalarDataType")())
    _init_scalar_value(sym3, routine, {})
    assert isinstance(routine[3], nodes.Assignment)
    assert routine[3].rhs.value == "false"
    # Unrecognised type of scalar. This is tricky to reproduce so we create
    # a new class that has a 'name' attribute and monkeypatch the 'intrinsic'
    # property of the datatype.
    sym4 = DataSymbol("my_var", LFRicTypes("LFRicRealScalarDataType")())

    class BrokenType:
        '''Utility class to provide an unsupported type.'''
        def __init__(self):
            self.name = "wrong"
    monkeypatch.setattr(sym4.datatype, "intrinsic", BrokenType())
    with pytest.raises(InternalError) as err:
        _init_scalar_value(sym4, routine, {})
    assert ("scalars of REAL, INTEGER or BOOLEAN type are supported but got "
            "symbol 'my_var' of type 'Scalar<wrong" in str(err.value))


# _validate_geom_arg

def test_validate_geom_arg():
    '''
    Tests for the _validate_geom_arg method.
    '''
    code = '''\
module testkern_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_type
     type(arg_type), dimension(3) :: meta_args =        &
          (/ arg_type(gh_scalar, gh_real, gh_read),     &
             arg_type(gh_field,  gh_real, gh_inc,  w1), &
             arg_type(gh_field*2, gh_real, gh_read,w1)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_code
  end type testkern_type

contains

  subroutine testkern_code()
  end subroutine testkern_code
end module testkern_mod
'''
    ptree = fpapi.parse(code)
    kern = LFRicAlg().kernel_from_metadata(ptree, "testkern_type")
    # Invalid argument index.
    with pytest.raises(ValueError) as err:
        _validate_geom_arg(kern, -1, "var", None, None)
    assert ("LFRic TL kernel 'testkern_code' has 3 arguments specified in its "
            "metadata. Therefore, the index of the argument containing the "
            "'var' field must be between 1 and 3 (inclusive) but got -1" in
            str(err.value))
    with pytest.raises(ValueError) as err:
        _validate_geom_arg(kern, 4, "var", None, None)
    assert ("LFRic TL kernel 'testkern_code' has 3 arguments specified in its "
            "metadata. Therefore, the index of the argument containing the "
            "'var' field must be between 1 and 3 (inclusive) but got 4" in
            str(err.value))
    # Inconsistent function space.
    with pytest.raises(ValueError) as err:
        _validate_geom_arg(kern, 2, "var", ["w2"], None)
    assert ("The 'var' field argument to kernel 'testkern_code' is expected "
            "to be on one of the ['w2'] spaces but the argument at the "
            "specified position (2) is on the 'w1' space" in str(err.value))
    # Inconsistent vector length.
    with pytest.raises(ValueError) as err:
        _validate_geom_arg(kern, 3, "var", ["w1"], 3)
    assert ("The 'var' field argument to kernel 'testkern_code' is expected "
            "to be a field vector of length 3 but the argument at the "
            "specified position (3) has a length of 2" in str(err.value))
    # Inconsistent vector length.
    with pytest.raises(ValueError) as err:
        _validate_geom_arg(kern, 3, "var", ["w1"], 1)
    assert ("The 'var' field argument to kernel 'testkern_code' is expected "
            "to be a field but the argument at the specified position (3) is "
            "a field vector of length 2" in str(err.value))
    # Everything validates OK.
    _validate_geom_arg(kern, 3, "var", ["w1"], 2)

# _lfric_create_real_comparison


def test_lfric_create_real_comparison(fortran_writer):
    '''Test for the _lfric_create_real_comparison method.'''
    symbol_table = SymbolTable()
    var1_symbol = symbol_table.new_symbol(
        "var1", symbol_type=DataSymbol, datatype=REAL_TYPE)
    var2_symbol = symbol_table.new_symbol(
        "var2", symbol_type=DataSymbol, datatype=REAL_TYPE)
    routine = nodes.Routine.create("test", symbol_table, [])
    stmt_list = _lfric_create_real_comparison(
        symbol_table, routine, var1_symbol, var2_symbol)
    routine.children = stmt_list
    result = fortran_writer(routine)
    expected = (
        "  use log_mod, only : log_event, log_level_error, log_level_info, "
        "log_scratch_space\n"
        "  real, parameter :: overall_tolerance = 1500.0\n"
        "  real :: var1\n"
        "  real :: var2\n"
        "  real :: MachineTol\n"
        "  real :: relative_diff\n\n"
        "  ! Test the inner-product values for equality, allowing for the "
        "precision of the active variables\n"
        "  MachineTol = SPACING(MAX(ABS(var1), ABS(var2)))\n"
        "  relative_diff = ABS(var1 - var2) / MachineTol\n"
        "  if (relative_diff < overall_tolerance) then\n"
        "    WRITE(log_scratch_space, *) \"PASSED test:\", var1, var2, "
        "relative_diff\n"
        "    call log_event(log_scratch_space, log_level_info)\n"
        "  else\n"
        "    WRITE(log_scratch_space, *) \"FAILED test:\", var1, var2, "
        "relative_diff\n"
        "    call log_event(log_scratch_space, log_level_error)\n"
        "  end if\n")
    assert expected in result

# _lfric_log_write


def test_lfric_log_write(fortran_writer):
    '''Test for the _lfric_log_write method.'''
    symbol_table = SymbolTable()
    var1_symbol = symbol_table.new_symbol(
        "var1", symbol_type=DataSymbol, datatype=REAL_TYPE)
    var2_symbol = symbol_table.new_symbol(
        "var2", symbol_type=DataSymbol, datatype=REAL_TYPE)
    routine = nodes.Routine.create("test", symbol_table, [])
    stmt_list = _lfric_create_real_comparison(
        symbol_table, routine, var1_symbol, var2_symbol)
    routine.children = stmt_list
    result = fortran_writer(routine)
    expected = (
        "  use log_mod, only : log_event, log_level_error, log_level_info, "
        "log_scratch_space\n"
        "  real, parameter :: overall_tolerance = 1500.0\n"
        "  real :: var1\n"
        "  real :: var2\n"
        "  real :: MachineTol\n"
        "  real :: relative_diff\n\n"
        "  ! Test the inner-product values for equality, allowing for the "
        "precision of the active variables\n"
        "  MachineTol = SPACING(MAX(ABS(var1), ABS(var2)))\n"
        "  relative_diff = ABS(var1 - var2) / MachineTol\n"
        "  if (relative_diff < overall_tolerance) then\n"
        "    WRITE(log_scratch_space, *) \"PASSED test:\", var1, var2, "
        "relative_diff\n"
        "    call log_event(log_scratch_space, log_level_info)\n"
        "  else\n"
        "    WRITE(log_scratch_space, *) \"FAILED test:\", var1, var2, "
        "relative_diff\n"
        "    call log_event(log_scratch_space, log_level_error)\n"
        "  end if\n")
    assert expected in result

# generate_lfric_adjoint_harness


def test_generate_lfric_adjoint_harness_invalid_code(fortran_reader):
    '''Test that the generate_lfric_adjoint_harness() function raises the
    expected errors if passed invalid/unsupported source code.'''
    with pytest.raises(TypeError) as err:
        _ = generate_lfric_adjoint_harness(None)
    assert "Expected a PSyIR Node but got 'NoneType'" in str(err.value)
    psyir = fortran_reader.psyir_from_source("program oops\n"
                                             "end program oops\n")
    with pytest.raises(ValueError) as err:
        _ = generate_lfric_adjoint_harness(psyir)
    assert ("generated if the supplied TL kernel is within a module "
            "(Container) but the supplied PSyIR does not have a Container "
            "node:\nFileContainer[]\n    Routine[name:'oops']" in
            str(err.value))
    psyir = fortran_reader.psyir_from_source(
        TL_CODE.replace("testkern_mod", "wrong"))
    with pytest.raises(ValueError) as err:
        _ = generate_lfric_adjoint_harness(psyir)
    assert ("The supplied LFRic TL kernel is contained within a module named "
            "'wrong'. This does not end in '_mod' and as such does not comply "
            "with the LFRic naming convention." in str(err.value))


TL_CODE = (
    "module testkern_mod\n"
    "  use kinds_mod, only: i_def, r_def\n"
    "  use kernel_mod, only: kernel_type, arg_type, gh_field, gh_real, "
    "gh_write, w3, cell_column\n"
    "  type, extends(kernel_type) :: testkern_type\n"
    "     type(arg_type), dimension(2) :: meta_args =          & \n"
    "          (/ arg_type(gh_scalar, gh_real, gh_read),       & \n"
    "             arg_type(gh_field,  gh_real, gh_write,  w3)  & \n"
    "           /)\n"
    "     integer :: operates_on = cell_column\n"
    "   contains\n"
    "     procedure, nopass :: code => testkern_code\n"
    "  end type testkern_type\n"
    "contains\n"
    "  subroutine testkern_code(nlayers, ascalar, "
    "field, ndf_w3, undf_w3, map_w3)\n"
    "    integer(kind=i_def), intent(in) :: nlayers\n"
    "    integer(kind=i_def), intent(in) :: ndf_w3, undf_w3\n"
    "    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
    "    real(kind=r_def), intent(in) :: ascalar\n"
    "    real(kind=r_def), intent(inout), dimension(undf_w3) :: field\n"
    "    field = ascalar\n"
    "  end subroutine testkern_code\n"
    "end module testkern_mod\n"
)


def test_generate_lfric_adjoint_harness(fortran_reader, fortran_writer):
    '''Test that the generate_lfric_adjoint_harness() function generates the
    expected test-harness code.'''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    psyir = generate_lfric_adjoint_harness(tl_psyir)
    gen = fortran_writer(psyir).lower()
    assert "use finite_element_config_mod, only : element_order" in gen
    assert "module adjoint_test_mod" in gen
    assert "subroutine adjoint_test(mesh, chi, panel_id)" in gen
    # We should have a field, a copy of that field and an inner-product value
    # for that field.
    assert ("    real(kind=r_def) :: ascalar\n"
            "    type(field_type) :: field\n"
            "    real(kind=r_def) :: ascalar_input\n"
            "    type(field_type) :: field_input\n"
            "    real(kind=r_def) :: field_inner_prod\n" in gen)
    # The field and its copy must be initialised.
    assert ("call field % initialise(vector_space=vector_space_w3_ptr, "
            "name='field')" in gen)
    assert ("call field_input % initialise(vector_space=vector_space_w3_ptr,"
            " name='field_input')" in gen)
    # So too must the scalar argument.
    assert ("    call random_number(ascalar)\n"
            "    ascalar_input = ascalar\n" in gen)

    # The field must be given random values and those copied into the copy.
    # The TL kernel must then be called and the inner-product of the result
    # computed.
    assert "field_inner_prod = 0.0_r_def" in gen
    assert ("    ! initialise arguments and call the tangent-linear kernel.\n"
            "    call invoke(setval_random(field), setval_x(field_input, "
            "field), testkern_type(ascalar, field), x_innerproduct_x("
            "field_inner_prod, field))\n" in gen)
    # Compute and store the sum of all inner products.
    assert ("    inner1 = 0.0_r_def\n"
            "    inner1 = inner1 + ascalar * ascalar\n"
            "    inner1 = inner1 + field_inner_prod\n"
            "    field_field_input_inner_prod = 0.0_r_def\n" in gen)
    # Run the adjoint of the kernel and compute the inner products of its
    # outputs with the inputs to the TL kernel.
    assert ("call invoke(adj_testkern_type(ascalar, field), "
            "x_innerproduct_y(field_field_input_inner_prod, field, "
            "field_input))"
            in gen)
    assert ("    inner2 = 0.0_r_def\n"
            "    inner2 = inner2 + ascalar * ascalar_input\n"
            "    inner2 = inner2 + field_field_input_inner_prod\n" in gen)


def test_generate_lfric_adj_test_quadrature(fortran_reader):
    '''Check that input copies of quadrature arguments are not created.'''
    # Change the metadata so that it requires quadrature.
    new_code = TL_CODE.replace(
        "     integer :: operates_on = cell_column\n",
        "     type(func_type) :: meta_funcs(1) = (/               &\n"
        "          func_type(W3, gh_basis)                        &\n"
        "          /)\n"
        "     integer :: operates_on = cell_column\n"
        "     integer :: gh_shape = gh_quadrature_xyoz\n")
    new_code = new_code.replace(
        "field, ndf_w3, undf_w3, map_w3)\n",
        "field, ndf_w3, undf_w3, map_w3, basis_w3_qr_xyoz, np_xy_qr_xyoz, "
        "np_z_qr_xyoz, weights_xy_qr_xyoz, weights_z_qr_xyoz)\n"
        "      INTEGER(KIND=i_def), intent(in) :: np_xy_qr_xyoz, "
        "np_z_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w3_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_xy_qr_xyoz) :: "
        "weights_xy_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_z_qr_xyoz) :: "
        "weights_z_qr_xyoz\n")

    tl_psyir = fortran_reader.psyir_from_source(new_code)
    psyir = generate_lfric_adjoint_harness(tl_psyir)
    routine = psyir.walk(nodes.Routine)[0]
    for sym in routine.symbol_table.datasymbols:
        # All input variables should be either scalars or fields (operators
        # are never active).
        if sym.name.endswith("_input"):
            assert (sym.name.startswith("field") or
                    sym.name.startswith("ascalar"))


def test_generate_lfric_adjoint_harness_operator(fortran_reader,
                                                 fortran_writer):
    '''Check the test harness generation for a kernel that has an operator
    as argument.

    '''
    # Alter the metadata so that the kernel expects an operator that maps
    # from W0 to W3.
    code = TL_CODE.replace("arg_type(gh_field,  gh_real, gh_write,  w3)  &",
                           "arg_type(gh_field,  gh_real, gh_write,  w3), &\n"
                           "arg_type(gh_operator,gh_real,gh_read,w3,w0) &")
    code = code.replace("dimension(2)", "dimension(3)")
    code = code.replace("nlayers, ascalar", "cell, nlayers, ascalar")
    code = code.replace(
        "field, ndf_w3, undf_w3, map_w3",
        "field, op_ncell_3d, op, ndf_w3, undf_w3, map_w3, ndf_w0")
    code = code.replace(
        "    field = ascalar\n",
        "    INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "    INTEGER(KIND=i_def), intent(in) :: cell\n"
        "    INTEGER(KIND=i_def), intent(in) :: op_ncell_3d\n"
        "    REAL(KIND=r_def), intent(in), dimension(ndf_w3,ndf_w0,"
        "op_ncell_3d) :: op\n"
        "    field = ascalar\n")

    tl_psyir = fortran_reader.psyir_from_source(code)
    psyir = generate_lfric_adjoint_harness(tl_psyir)
    gen = fortran_writer(psyir)
    assert "type(operator_type) :: op\n" in gen
    assert ("vector_space_w0_ptr => function_space_collection % get_fs(mesh, "
            "element_order, w0)\n" in gen)
    assert ("vector_space_w3_ptr => function_space_collection % get_fs(mesh, "
            "element_order, w3)\n" in gen)
    # Initialise takes the *to* and *from* spaces as arguments in that order.
    assert ("call op % initialise(vector_space_w3_ptr, vector_space_w0_ptr)"
            in gen)
    # Operator is given random values and passed to the TL kernel.
    assert ("setop_random_kernel_type(op), "
            "testkern_type(ascalar, field, op)" in gen)
    # Operator is passed to the Adjoint kernel too.
    assert "call invoke(adj_testkern_type(ascalar, field, op)" in gen


def test_gen_lfric_adjoint_harness_written_operator(fortran_reader,):
    '''Check that the test-harness generation rejects a kernel that writes to
    an operator argument.

    '''
    # Alter the metadata so that the kernel writes to an operator.
    code = TL_CODE.replace("arg_type(gh_field,  gh_real, gh_write,  w3)  &",
                           "arg_type(gh_field,  gh_real, gh_write,  w3), &\n"
                           "arg_type(gh_operator,gh_real,gh_write,w3,w0) &")
    code = code.replace("dimension(2)", "dimension(3)")

    code = code.replace("nlayers, ascalar", "cell, nlayers, ascalar")
    code = code.replace(
        "field, ndf_w3, undf_w3, map_w3",
        "field, op_ncell_3d, op, ndf_w3, undf_w3, map_w3, ndf_w0")
    code = code.replace(
        "    field = ascalar\n",
        "    INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "    INTEGER(KIND=i_def), intent(in) :: cell\n"
        "    INTEGER(KIND=i_def), intent(in) :: op_ncell_3d\n"
        "    REAL(KIND=r_def), intent(out), dimension(ndf_w3,ndf_w0,"
        "op_ncell_3d) :: op\n"
        "    field = ascalar\n")
    tl_psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(GenerationError) as err:
        generate_lfric_adjoint_harness(tl_psyir)
    assert ("Operator argument 'op' to TL kernel 'testkern_type' is written "
            "to. This is not supported." in str(err.value))


def test_generate_lfric_adjoint_harness_invalid_geom_arg(fortran_reader):
    '''
    Check that generate_lfric_adjoint_harness() calls _validate_geom_arg.
    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    with pytest.raises(ValueError) as err:
        _ = generate_lfric_adjoint_harness(tl_psyir, coord_arg_idx=1)
    assert ("The 'coordinate' argument is expected to be a field but argument "
            "1 to kernel 'testkern_code' is a 'gh_scalar'" in str(err.value))
    # Recreate the tl_psyir as it gets raised by this routine.
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    with pytest.raises(ValueError) as err:
        _ = generate_lfric_adjoint_harness(tl_psyir, panel_id_arg_idx=1)
    assert ("The 'panel-id' argument is expected to be a field but argument 1 "
            "to kernel 'testkern_code' is a 'gh_scalar'" in str(err.value))


TL_CODE_WITH_GEOM = (
    "module testkern_mod\n"
    "  use kinds_mod, only: i_def, r_def\n"
    "  use kernel_mod, only: kernel_type, arg_type, gh_field, gh_real, "
    "gh_write, w3, cell_column\n"
    "  type, extends(kernel_type) :: testkern_type\n"
    "     type(arg_type), dimension(4) :: meta_args =          & \n"
    "          (/ arg_type(gh_scalar, gh_real, gh_read),       & \n"
    "             arg_type(gh_field*3,gh_real, gh_read, wchi), & \n"
    "             arg_type(gh_field,  gh_real, gh_write,  w3), & \n"
    "             arg_type(gh_field,  gh_integer, gh_read,     & \n"
    "                      any_discontinuous_space_1)  & \n"
    "           /)\n"
    "     integer :: operates_on = cell_column\n"
    "   contains\n"
    "     procedure, nopass :: code => testkern_code\n"
    "  end type testkern_type\n"
    "contains\n"
    "  subroutine testkern_code(nlayers, ascalar, cfield1, cfield2, cfield3, &"
    "\n"
    "field, pids, ndf_wchi, undf_wchi, map_wchi, ndf_w3, undf_w3, &\n"
    "map_w3, ndf_adspace1, undf_adspace1, map_adspace1)\n"
    "    integer(kind=i_def), intent(in) :: nlayers\n"
    "    integer(kind=i_def), intent(in) :: ndf_w3, undf_w3\n"
    "    integer(kind=i_def), intent(in) :: ndf_wchi, undf_wchi\n"
    "    integer(kind=i_def), intent(in) :: ndf_adspace1, undf_adspace1\n"
    "    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
    "    integer(kind=i_def), intent(in), dimension(ndf_wchi) :: map_wchi\n"
    "    integer(kind=i_def), intent(in), dimension(ndf_adspace1) :: "
    "map_adspace1\n"
    "    real(kind=r_def), intent(in) :: ascalar\n"
    "    real(kind=r_def), intent(inout), dimension(undf_wchi) :: cfield1, "
    "cfield2, cfield3\n"
    "    real(kind=r_def), intent(inout), dimension(undf_w3) :: field\n"
    "    integer(kind=i_def), intent(in), dimension(undf_adspace1) :: pids\n"
    "    field = ascalar\n"
    "  end subroutine testkern_code\n"
    "end module testkern_mod\n"
)


def test_generate_lfric_adjoint_harness_chi_arg(fortran_reader,
                                                fortran_writer):
    '''
    Check that generate_lfric_adjoint_harness() creates correct code when one
    of the kernel arguments is identified as being the chi (coordinate) field.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE_WITH_GEOM)
    psyir = generate_lfric_adjoint_harness(tl_psyir, coord_arg_idx=2)
    gen = fortran_writer(psyir)
    # chi is passed in as an argument to the algorithm routine.
    assert "type(field_type), dimension(3), intent(in), optional :: chi" in gen
    # chi should not be initialised or given values.
    assert "chi(1) % initialise" not in gen
    assert "setval_random(chi(1))" not in gen
    # chi should be passed as the second argument to the TL and adjoint
    # kernels.
    assert "testkern_type(ascalar, chi, field, pids)" in gen
    assert "invoke(adj_testkern_type(ascalar, chi, field, pids)" in gen


def test_generate_lfric_adjoint_harness_panel_id_arg(fortran_reader,
                                                     fortran_writer):
    '''
    Check that generate_lfric_adjoint_harness() creates correct code when one
    of the kernel arguments is identified as being the panel-id field.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE_WITH_GEOM)
    psyir = generate_lfric_adjoint_harness(tl_psyir, panel_id_arg_idx=4)
    gen = fortran_writer(psyir)
    # panel id is passed in as an argument to the algorithm routine.
    assert "type(field_type), intent(in), optional :: panel_id" in gen
    # It should not be initialised or written to.
    assert "panel_id % initialise" not in gen
    assert "setval_random(panel_id)" not in gen
    # panel id should be passed as the 4th argument to both the TL and adjoint
    # kernels.
    assert "testkern_type(ascalar, cfield3, field, panel_id)" in gen
    assert ("invoke(adj_testkern_type(ascalar, cfield3, field, panel_id)"
            in gen)


def test_generate_lfric_adjoint_harness_geom_args(fortran_reader,
                                                  fortran_writer):
    '''
    Check that generate_lfric_adjoint_harness() creates correct code when
    both chi and panel-id are present.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE_WITH_GEOM)
    psyir = generate_lfric_adjoint_harness(tl_psyir, panel_id_arg_idx=4,
                                           coord_arg_idx=2)
    gen = fortran_writer(psyir)
    assert "testkern_type(ascalar, chi, field, panel_id)" in gen
    assert ("invoke(adj_testkern_type(ascalar, chi, field, panel_id)"
            in gen)


def test_generate_lfric_adj_harness_scalar_geom_arg(fortran_reader,
                                                    fortran_writer,
                                                    monkeypatch):
    '''
    Check that the code correctly handles the case where a scalar argument
    is marked as holding geometric information. This is for future-proofing
    in case such an argument is added in the future.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE_WITH_GEOM)
    # Currently there are no scalar, geometry arguments so monkeypatch the
    # _validate_geom_arg method so that it doesn't complain.
    monkeypatch.setattr(lfric_adjoint_harness,
                        "_validate_geom_arg",
                        lambda _1, _2, _3, _4, _5: None)
    psyir = generate_lfric_adjoint_harness(tl_psyir, panel_id_arg_idx=1)
    gen = fortran_writer(psyir)
    assert "call random_number(rscalar_1)" not in gen
    assert "call random_number(panel_id)" not in gen
