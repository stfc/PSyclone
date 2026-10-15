# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' pytest tests for the GOInvokeSchedule class. '''

import os
from pathlib import Path

import pytest

from psyclone.gocean1p0 import GOInvokeSchedule
from psyclone.parse.algorithm import parse
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.tests.utilities import get_base_path, get_invoke

API = "gocean"


def test_gosched_parent():
    ''' Check that the GOInvokeSchedule constructor allows the parent node
    to be supplied or omitted. '''

    _, invoke_info = parse(os.path.join(get_base_path(API),
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    kcalls = invoke_info.calls[0].kcalls
    # With no parent specified
    symbol = RoutineSymbol("my_sched")
    gsched = GOInvokeSchedule(symbol, kcalls)
    assert gsched.parent is None
    # With a parent
    cont = Container("my_mod")
    gsched = GOInvokeSchedule(symbol, kcalls, parent=cont)
    assert gsched.parent is cont


@pytest.mark.usefixtures("change_into_tmpdir")
def test_writetoread_dag(have_graphviz):
    ''' Test that the GOInvokeSchedule::dag() method works as expected when we
    have two kernels with a write -> read dependency.
    '''

    _, invoke = get_invoke("single_invoke_write_to_read.f90", API, idx=0)
    invoke.schedule.dag()
    if have_graphviz:
        dot_file = Path("dag")
        assert dot_file.is_file()
        with dot_file.open("r", encoding="utf-8") as dfile:
            dot = dfile.read()
        assert dot.startswith("digraph")
        # write -> read means that the second loop can only begin once the
        # first loop is complete. Check that we have the correct forwards
        # dependence (green) and backwards dependence (red).
        assert ('"loop_[outer]_1_end" -> "loop_[outer]_36_start" '
                '[color=red]' in dot or
                '"loop_[outer]_1_end" -> "loop_[outer]_36_start" '
                '[color=#ff0000]' in dot)
        assert ('"loop_[outer]_1_end" -> "loop_[outer]_36_start" '
                '[color=green]' in dot or
                '"loop_[outer]_1_end" -> "loop_[outer]_36_start" '
                '[color=#00ff00]' in dot)


@pytest.mark.usefixtures("change_into_tmpdir")
def test_dag(have_graphviz):
    ''' Test that the GOInvokeSchedule::dag() method works as expected '''

    _, invoke = get_invoke("nemolite2d_alg_mod.f90", API, idx=0)
    invoke.schedule.dag()
    if have_graphviz:
        assert Path("dag.svg").is_file()
        dot_file = Path("dag")
        assert dot_file.is_file()
        with dot_file.open("r", encoding="utf-8") as dfile:
            dot = dfile.read()
        # The two kernels in this example are independent so we should
        # have no forwards/backwards dependencies
        for col in ["red", "#ff0000", "green", "#00ff00"]:
            assert f'[color={col}]' not in dot
