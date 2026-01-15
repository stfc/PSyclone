# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2026, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

'''
pytest tests for the lfric.HaloDepth class.

'''

from psyclone.lfric import HaloDepth
from psyclone.psyir import symbols, nodes
from psyclone.tests.utilities import get_invoke


def test_halo_depth_ctor():
    '''
    Basic test that we can construct a HaloDepth object.

    '''
    _, invoke = get_invoke("14.4.2_halo_vector_xory.f90",
                           "lfric", idx=0)
    hdepth = HaloDepth(invoke.schedule)
    assert hdepth.max_depth is False
    assert hdepth.max_depth_m1 is False
    assert hdepth.annexed_only is False
    assert hdepth.var_depth is None
    assert hdepth._parent is invoke.schedule
    assert hdepth.psyir_expression() is None


def test_halo_depth_set_by_value():
    '''
    Test for the set_by_value() method of HaloDepth. Also indirectly tests
    the psyir_expression() method by checking the result of str().

    '''
    _, invoke = get_invoke("14.4.2_halo_vector_xory.f90",
                           "lfric", idx=0)
    hdepth = HaloDepth(invoke.schedule)
    # Halo is accessed to max depth.
    hdepth.set_by_value(True, None, False, False)
    assert hdepth.var_depth is None
    # Halo is accessed to max-depth minus 1.
    hdepth.set_by_value(False, None, False, True)
    assert (hdepth.psyir_expression().debug_string() ==
            "max_halo_depth_mesh - 1")
    # Annexed dofs only.
    hdepth.set_by_value(False, None, True, False)
    assert hdepth.var_depth is None
    # PSyIR expression.
    my_depth = symbols.DataSymbol("my_depth", symbols.INTEGER_TYPE)
    invoke.schedule.symbol_table.add(my_depth)
    exprn = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.MUL,
        nodes.Literal("2", symbols.INTEGER_TYPE),
        nodes.Reference(my_depth))
    hdepth.set_by_value(False, exprn, False, False)
    assert hdepth.var_depth.debug_string() == "2 * my_depth"
    # Check that the PSyIR expression is simplified where possible.
    exprn2 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.MUL,
        nodes.Literal("2", symbols.INTEGER_TYPE),
        nodes.Literal("2", symbols.INTEGER_TYPE))
    hdepth.set_by_value(False, exprn2, False, False)
    assert hdepth.var_depth.debug_string() == "4"
