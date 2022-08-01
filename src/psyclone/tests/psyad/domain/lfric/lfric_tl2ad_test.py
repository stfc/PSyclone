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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Provides py.test tests of LFRic-specific PSyclone adjoint functionality.'''

import pytest
from psyclone.domain.lfric import KernCallInvokeArgList
from psyclone.domain.lfric.algorithm import LFRicBuiltinFunctor
from psyclone.errors import InternalError
from psyclone.psyad.domain.lfric.tl2ad import (_compute_lfric_inner_products,
                                               _compute_field_inner_products,
                                               _init_fields_random,
                                               generate_lfric_adjoint_test)
from psyclone.psyir.nodes import Routine, Literal, Assignment
from psyclone.psyir.symbols import (SymbolTable, DataSymbol, REAL_TYPE,
                                    ArrayType, DataTypeSymbol, DeferredType,
                                    INTEGER_TYPE)


# _compute_lfric_inner_products

def test_compute_inner_products_scalars(fortran_writer):
    '''Test that _compute_lfric_inner_products generates the expected code
    for scalars.'''
    table = SymbolTable()
    prog = Routine.create("test_prog", table, [], is_program=True)
    sum_sym = table.new_symbol(root_name="my_sum",
                               symbol_type=DataSymbol, datatype=REAL_TYPE)
    s1 = table.new_symbol(root_name="var1", symbol_type=DataSymbol,
                          datatype=REAL_TYPE)
    s2 = table.new_symbol(root_name="var2", symbol_type=DataSymbol,
                          datatype=REAL_TYPE)
    _compute_lfric_inner_products(prog, [(s1, s1), (s1, s2)], [], sum_sym)
    gen = fortran_writer(prog)
    assert ("  my_sum = 0.0\n"
            "  my_sum = my_sum + var1 * var1\n"
            "  my_sum = my_sum + var1 * var2\n" in gen)


def test_compute_inner_products_fields(fortran_writer):
    '''Test that _compute_lfric_inner_products generates the expected code
    when supplied with symbols representing the innerproducts of fields.'''
    table = SymbolTable()
    prog = Routine.create("test_prog", table, [], is_program=True)
    sum_sym = table.new_symbol(root_name="my_sum",
                               symbol_type=DataSymbol, datatype=REAL_TYPE)
    s1 = table.new_symbol(root_name="ip1", symbol_type=DataSymbol,
                          datatype=REAL_TYPE)
    s2 = table.new_symbol(root_name="ip2", symbol_type=DataSymbol,
                          datatype=REAL_TYPE)
    # For field vectors we have an array of inner-product values to sum.
    atype = ArrayType(REAL_TYPE, [3])
    s3 = table.new_symbol(root_name="ip3", symbol_type=DataSymbol,
                          datatype=atype)
    _compute_lfric_inner_products(prog, [], [s1, s2, s3], sum_sym)
    gen = fortran_writer(prog)
    assert ("  my_sum = 0.0\n"
            "  my_sum = my_sum + ip1\n"
            "  my_sum = my_sum + ip2\n"
            "  my_sum = my_sum + ip3(1)\n"
            "  my_sum = my_sum + ip3(2)\n"
            "  my_sum = my_sum + ip3(3)\n" in gen)


# _compute_field_inner_products

def test_compute_field_inner_products(fortran_writer):
    '''Check that _compute_field_inner_products generates the expected symbols,
    assignments and functors.'''
    table = SymbolTable()
    prog = Routine.create("test_prog", table, [], is_program=True)
    fld_type = DataTypeSymbol("field_type", datatype=DeferredType())
    table.add(fld_type)
    fld1 = DataSymbol("field1", datatype=fld_type)
    fld2 = DataSymbol("field2", datatype=fld_type)
    table.add(fld1)
    table.add(fld2)
    sums, functors = _compute_field_inner_products(prog, [(fld1, fld2)])
    assert len(sums) == 1
    assert isinstance(sums[0], DataSymbol)
    assert sums[0].name.endswith("_inner_prod")
    assert sums[0].name in table
    assert len(functors) == 1
    assert isinstance(functors[0], LFRicBuiltinFunctor)
    assert isinstance(prog.children[0], Assignment)
    code = fortran_writer(prog)
    assert "hello" in code


# _init_fields_random

def test_init_fields_random(fortran_writer):
    '''Check that the _init_fields_random() routine works as expected.'''
    table = SymbolTable()
    fld_type = DataTypeSymbol("field_type", datatype=DeferredType())
    table.add(fld_type)
    fld1 = DataSymbol("field1", datatype=fld_type)
    fields = [(fld1, "w1")]
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


def test_init_fields_random_vector(fortran_writer):
    '''Check that the _init_fields_random() routine works as expected for
    a field vector.

    '''
    table = SymbolTable()
    fld_type = DataTypeSymbol("field_type", datatype=DeferredType())
    table.add(fld_type)
    fld1 = DataSymbol("field1", datatype=ArrayType(fld_type, [3]))
    fields = [(fld1, "w1")]
    fld1_input = DataSymbol("field1_input", datatype=ArrayType(fld_type, [3]))
    table.add(fld1)
    table.add(fld1_input)
    input_syms = {"field1": fld1_input}
    kernels = _init_fields_random(fields, input_syms, table)
    assert len(kernels) == 6
    for idx in range(3):
        kidx = 2*idx
        lit = Literal(f"{idx+1}", INTEGER_TYPE)
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
    fields = [(fld1, "w1")]
    inputs = {"field1": fld1}
    with pytest.raises(InternalError) as err:
        _init_fields_random(fields, inputs, SymbolTable())
    assert ("Expected a field symbol to either be of ArrayType or have a type "
            "specified by a DataTypeSymbol but found Scalar<INTEGER, "
            "UNDEFINED> for field 'field1'" in str(err.value))

# generate_lfric_adjoint_test


def test_generate_lfric_adjoint_test_invalid_code():
    '''Test that the generate_lfric_adjoint_test() function raises the
    expected errors if passed invalid/unsupported source code.'''
    with pytest.raises(ValueError) as err:
        _ = generate_lfric_adjoint_test("")
    assert "Supplied TL code ('') is empty" in str(err.value)
    with pytest.raises(ValueError) as err:
        _ = generate_lfric_adjoint_test("program oops\nend program oops\n")
    assert ("generated if the supplied TL kernel is within a module but got: "
            "'program oops" in str(err.value))
    with pytest.raises(ValueError) as err:
        _ = generate_lfric_adjoint_test("module oops\nend module oops\n")
    assert ("Failed to parse kernel metadata in supplied code:"
            in str(err.value))


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
    "  subroutine testkern_code(nlayers, ascalar, field, ndf_w3, undf_w3, "
    "map_w3)\n"
    "    integer(kind=i_def), intent(in) :: nlayers\n"
    "    integer(kind=i_def), intent(in) :: ndf_w3, undf_w3\n"
    "    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
    "    real(kind=r_def), intent(in) :: ascalar\n"
    "    real(kind=r_def), intent(inout), dimension(undf_w3) :: field\n"
    "    field = ascalar\n"
    "  end subroutine testkern_code\n"
    "end module testkern_mod\n"
)


def test_generate_lfric_adjoint_test(fortran_writer):
    '''Test that the generate_lfric_adjoint_test() function generates the
    expected test-harness code.'''
    psyir = generate_lfric_adjoint_test(TL_CODE)
    gen = fortran_writer(psyir)
    assert "module adjoint_test_mod" in gen
    assert "subroutine adjoint_test(mesh, chi, panel_id)" in gen
    # We should have a field, a copy of that field and an inner-product value
    # for that field.
    assert ("    real(kind=r_def) :: rscalar_1\n"
            "    type(field_type) :: field_2\n"
            "    real(kind=r_def) :: rscalar_1_input\n"
            "    type(field_type) :: field_2_input\n"
            "    real(kind=r_def) :: field_2_inner_prod\n" in gen)
    # The field and its copy must be initialised.
    assert ("call field_2 % initialise(vector_space=vector_space_w3_ptr, "
            "name='field_2')" in gen)
    assert ("call field_2_input % initialise(vector_space=vector_space_w3_ptr,"
            " name='field_2_input')" in gen)
    # So too must the scalar argument.
    assert ("    call random_number(rscalar_1)\n"
            "    rscalar_1_input = rscalar_1\n" in gen)

    # The field must be given random values and those copied into the copy.
    # The TL kernel must then be called and the inner-product of the result
    # computed.
    assert "field_2_inner_prod = 0.0_r_def" in gen
    assert ("    ! Initialise arguments and call the tangent-linear kernel.\n"
            "    call invoke(setval_random(field_2), setval_x(field_2_input, "
            "field_2), testkern_type(rscalar_1, field_2), x_innerproduct_x("
            "field_2_inner_prod, field_2))\n" in gen)
    # Compute and store the sum of all inner products.
    assert ("    inner1 = 0.0_r_def\n"
            "    inner1 = inner1 + rscalar_1 * rscalar_1\n"
            "    inner1 = inner1 + field_2_inner_prod\n"
            "    field_2_inner_prod = 0.0_r_def\n" in gen)
    # Run the adjoint of the kernel and compute the inner products of its
    # outputs with the inputs to the TL kernel.
    assert ("call invoke(adj_testkern_type(rscalar_1, field_2), "
            "x_innerproduct_y(field_2_inner_prod, field_2, field_2_input))"
            in gen)
    assert ("    inner2 = 0.0_r_def\n"
            "    inner2 = inner2 + rscalar_1 * rscalar_1_input\n"
            "    inner2 = inner2 + field_2_inner_prod\n" in gen)


def test_generate_lfric_adjoint_test_no_operators(monkeypatch):
    '''Check that a kernel that has an operator as argument raises the
    expected error.

    '''
    code = TL_CODE.replace("arg_type(gh_field,  gh_real, gh_write,  w3)",
                           "arg_type(gh_operator,gh_real,gh_write,w0,w0)")
    # We have to monkeypatch KernCallInvokeArgList as that too doesn't yet
    # support operators.
    monkeypatch.setattr(KernCallInvokeArgList, "operator",
                        lambda _1, _2, var_accesses=None: None)
    monkeypatch.setattr(KernCallInvokeArgList, "operators",
                        lambda: [1])
    with pytest.raises(NotImplementedError) as err:
        _ = generate_lfric_adjoint_test(code)
    assert ("Kernel testkern_type has one or more operator arguments. Test "
            "harness creation for such a kernel is not yet supported." in
            str(err.value))
