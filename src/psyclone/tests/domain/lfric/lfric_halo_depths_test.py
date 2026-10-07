# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
This module contains pytest tests for the LFRicHaloDepths class.

'''
import pytest

from psyclone.domain.lfric import LFRicHaloDepths, LFRicKern
from psyclone.psyir.nodes import BinaryOperation, Literal
from psyclone.psyir.symbols import DataSymbol, ScalarType
from psyclone.tests.utilities import get_invoke

API = "lfric"


def test_lfric_halo_depth_ctor():
    '''
    Test that the constructor creates the set of Symbols representing any
    halo depths.

    '''
    # An invoke with no kernels that operate on halo cells.
    _, invoke = get_invoke("1.2_multi_invoke.f90", API, dist_mem=True, idx=0)
    hdepths = LFRicHaloDepths(invoke)
    assert not hdepths._halo_depth_vars
    # An invoke with three kernels, two of which operate on halo cells
    # to different depths.
    _, invoke2 = get_invoke("1.4.2_multi_into_halos_invoke.f90", API,
                            dist_mem=True, idx=0)
    hdepths2 = LFRicHaloDepths(invoke2)
    assert len(hdepths2._halo_depth_vars) == 2
    for sym in hdepths2._halo_depth_vars:
        assert isinstance(sym, DataSymbol)
        assert sym.name in ["hdepth", "other_depth"]
    # The same invoke but without distributed memory (so we have no halos)
    _, invoke3 = get_invoke("1.4.2_multi_into_halos_invoke.f90", API,
                            dist_mem=False, idx=0)
    hdepths3 = LFRicHaloDepths(invoke3)
    assert len(hdepths3._halo_depth_vars) == 0


def test_lfric_halo_depth_invoke_declns():
    '''
    Test that the correct invoke declarations are constructed for any
    halo-depth arguments.

    '''
    # No distributed memory so no halo-depth arguments.
    _, invoke3 = get_invoke("1.4.2_multi_into_halos_invoke.f90", API,
                            dist_mem=False, idx=0)
    hdepths3 = LFRicHaloDepths(invoke3)
    hdepths3.invoke_declarations()
    args = [x.name for x in invoke3.schedule.symbol_table.argument_datasymbols]
    assert "depth" not in args

    # Now with distributed memory - should have two halo-depth arguments.
    _, invoke4 = get_invoke("1.4.2_multi_into_halos_invoke.f90", API,
                            dist_mem=True, idx=0)
    hdepths4 = LFRicHaloDepths(invoke4)
    hdepths4.invoke_declarations()
    args = [x.name for x in invoke4.schedule.symbol_table.argument_datasymbols]
    assert "hdepth" in args
    assert "other_depth" in args


def test_lfric_halo_depth_no_stub_gen():
    '''
    Test that the stub_declarations() method does nothing (because whether
    or not a kernel operates on halo cells does not affect the signature).

    '''
    _, invoke2 = get_invoke("1.4.2_multi_into_halos_invoke.f90", API, idx=0)
    hdepths2 = LFRicHaloDepths(invoke2.schedule.kernels()[0])
    hdepths2.stub_declarations()


def test_no_exprn_for_halo_depth():
    '''
    Test that an expression for a halo depth is rejected. This is normally
    caught earlier (at the Algorithm-processing stage) but the LFRicHaloDepths
    class also checks.

    '''
    _, invoke = get_invoke("1.4.3_literal_depth_into_halos_invoke.f90",
                           API, dist_mem=True, idx=0)
    # Change one of the halo depths so that it is an expression
    kern = invoke.schedule.walk(LFRicKern)[0]
    kern._halo_depth = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        Literal("1", ScalarType.integer_type()),
        Literal("1", ScalarType.integer_type()))
    with pytest.raises(NotImplementedError) as err:
        _ = LFRicHaloDepths(invoke)
    assert ("halo-depth argument must currently be a scalar reference or "
            "literal but Kernel 'testkern_halo_only_code' is passed a depth "
            "given by '1 + 1'" in str(err.value))
