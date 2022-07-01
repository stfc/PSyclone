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
from psyclone.psyad.domain.lfric.tl2ad import (_compute_lfric_inner_products,
                                               generate_lfric_adjoint_test)
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import (SymbolTable, DataSymbol, REAL_TYPE,
                                    ArrayType)


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

# generate_lfric_adjoint_test


def test_generate_lfric_adjoint_invalid_code():
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
    assert ("Failed to parse kernel metadata in supplied tangent-linear code:"
            in str(err.value))

TL_CODE = (
    "module testkern_mod\n"
    "  use kinds_mod, only: i_def, r_def\n"
    "  use kernel_mod, only: kernel_type, arg_type, gh_field, gh_real, "
    "gh_write, w3, cell_column\n"
    "  type, extends(kernel_type) :: testkern_type\n"
    "     type(arg_type), dimension(1) :: meta_args =          & \n"
    "          (/ arg_type(gh_field,  gh_real, gh_write,  w3)  & \n"
    "           /)\n"
    "     integer :: operates_on = cell_column\n"
    "   contains\n"
    "     procedure, nopass :: code => testkern_code\n"
    "  end type testkern_type\n"
    "contains\n"
    "  subroutine testkern_code(nlayers, field, ndf_w3, undf_w3, map_w3)\n"
    "    integer(kind=i_def), intent(in) :: nlayers\n"
    "    integer(kind=i_def), intent(in) :: ndf_w3, undf_w3\n"
    "    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
    "    real(kind=r_def), intent(inout), dimension(undf_w3) :: field\n"
    "    field = 0.0\n"
    "  end subroutine testkern_code\n"
    "end module testkern_mod\n"
)


def test_generate_lfric_adjoint_test(fortran_writer):
    '''Test that the generate_lfric_adjoint_test() function generates the
    expected test-harness code.'''
    psyir = generate_lfric_adjoint_test(TL_CODE)
    gen = fortran_writer(psyir)
    print(gen)
    assert "module adjoint_test_mod" in gen
    assert "subroutine adjoint_test(mesh, chi, panel_id)" in gen
    # We should have a field, a copy of that field and an inner-product value
    # for that field.
    assert ("    type(field_type) :: field_1\n"
            "    type(field_type) :: field_1_input\n"
            "    real(kind=r_def) :: field_1_inner_prod\n" in gen)
    # The field and its copy must be initialised.
    assert ("call field_1 % initialise(vector_space=vector_space_w3_ptr, "
            "name='field_1')" in gen)
    assert ("call field_1_input % initialise(vector_space=vector_space_w3_ptr,"
            " name='field_1_input')" in gen)
    # The field must be given random values and those copied into the copy.
    # The TL kernel must then be called and the inner-product of the result
    # computed.
    assert "field_1_inner_prod = 0.0_r_def" in gen
    assert ("    ! Initialise arguments and call the tangent-linear kernel.\n"
            "    call invoke(setval_random(field_1), setval_x(field_1_input, "
            "field_1), testkern_type(field_1), x_innerproduct_x("
            "field_1_inner_prod, field_1))\n" in gen)
    # Compute and store the sum of all inner products.
    assert ("    inner1 = inner1 + field_1_inner_prod\n"
            "    field_1_inner_prod = 0.0_r_def\n" in gen)
    # Run the adjoint of the kernel and compute the inner products of its
    # outputs with the inputs to the TL kernel.
    assert ("call invoke(adj_testkern_type(field_1), "
            "x_innerproduct_y(field_1_inner_prod, field_1, field_1_input))"
            in gen)
