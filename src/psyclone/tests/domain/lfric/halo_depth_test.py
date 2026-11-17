# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
    my_depth = symbols.DataSymbol(
        "my_depth", symbols.ScalarType.integer_type())
    invoke.schedule.symbol_table.add(my_depth)
    exprn = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.MUL,
        nodes.Literal("2", symbols.ScalarType.integer_type()),
        nodes.Reference(my_depth))
    hdepth.set_by_value(False, exprn, False, False)
    assert hdepth.var_depth.debug_string() == "2 * my_depth"
    # Check that the PSyIR expression is simplified where possible.
    exprn2 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.MUL,
        nodes.Literal("2", symbols.ScalarType.integer_type()),
        nodes.Literal("2", symbols.ScalarType.integer_type()))
    hdepth.set_by_value(False, exprn2, False, False)
    assert hdepth.var_depth.debug_string() == "4"
