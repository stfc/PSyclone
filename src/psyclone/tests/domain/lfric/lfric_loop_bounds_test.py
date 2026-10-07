# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the LFRicLoopBounds collection
class. '''

import os

from psyclone.domain.lfric import LFRicLoopBounds
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir import symbols


BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))), "test_files", "lfric")
TEST_API = "lfric"


def test_lbounds_construction():
    ''' Check that we can create an LFRicLoopBounds object. '''
    # We need a valid LFRicInvoke node and the easiest way to get one of
    # those is to create one from code.
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    lbounds = LFRicLoopBounds(invoke)
    assert isinstance(lbounds, LFRicLoopBounds)


def test_lbounds_initialise(monkeypatch, fortran_writer):
    ''' Test the initialise method of LFRicLoopBounds. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]

    table = invoke.schedule.symbol_table
    invoke.setup_psy_layer_symbols()
    lbounds = LFRicLoopBounds(invoke)
    lbounds.initialise(0)

    # Check that new symbols exist
    start_sym = table.lookup("loop0_start")
    assert start_sym.datatype.intrinsic == symbols.ScalarType.Intrinsic.INTEGER
    stop_sym = table.lookup("loop0_stop")
    assert stop_sym.datatype.intrinsic == symbols.ScalarType.Intrinsic.INTEGER

    assert "Set-up all of the loop bounds" in fortran_writer(invoke.schedule)
    # Monkeypatch the schedule so that it appears to have no loops.
    monkeypatch.setattr(invoke.schedule, "loops", lambda: [])
    lbounds = LFRicLoopBounds(invoke)
    # The initialise() should not raise an error but nothing should be
    # added to the PSyIR tree.
    lbounds.initialise(0)
    # Symbols representing loop bounds should be unaffected.
    assert table.lookup("loop0_start") is start_sym
    assert table.lookup("loop0_stop") is stop_sym
