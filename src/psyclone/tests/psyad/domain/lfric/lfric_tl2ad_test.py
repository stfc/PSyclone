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

from psyclone.psyad.domain.lfric.tl2ad import _compute_lfric_inner_products
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import SymbolTable, DataSymbol, REAL_TYPE


# _compute_lfric_inner_products

def test_compute_inner_products_scalars(fortran_writer):
    '''Test that _compute_inner_products generates the expected code
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


# generate_lfric_adjoint_test

