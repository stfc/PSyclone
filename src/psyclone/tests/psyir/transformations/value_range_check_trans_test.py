# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing tests for ValueRangeCheckTrans and ValueRangeCheckNode
'''

import pytest


from psyclone.errors import InternalError
from psyclone.psyir.nodes import Node, ValueRangeCheckNode, Schedule
from psyclone.psyir.transformations import (ValueRangeCheckTrans,
                                            TransformationError)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelLoopTrans
from psyclone.utils import colored


# ---------------------------------------------------------------------------
def test_value_range_check_trans():
    '''Tests basic functions in ValueRangeCheckTrans.'''
    value_range = ValueRangeCheckTrans()
    assert str(value_range) == ("Create a sub-tree of the PSyIR that has a "
                                "node of type ValueRangeCheckNode at its "
                                "root.")
    assert value_range.name == "ValueRangeCheckTrans"


# -----------------------------------------------------------------------------
def test_malformed_value_range_check_node(monkeypatch):
    ''' Check that we raise the expected error if a ValueRangeCheckNode does
    not have a single Schedule node as its child. '''
    value_range_check_node = ValueRangeCheckNode()
    monkeypatch.setattr(value_range_check_node, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = value_range_check_node.value_range_check_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(value_range_check_node, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = value_range_check_node.value_range_check_body
    assert "malformed or incomplete. It should have a " in str(err.value)


# -----------------------------------------------------------------------------
def test_value_range_check_basic():
    '''Check basic functionality: node names, schedule view.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean", idx=0, dist_mem=False)
    value_range_check = ValueRangeCheckTrans()
    value_range_check.apply(invoke.schedule[0].loop_body[0])
    result = invoke.schedule.view()

    # Create the coloured text (if required)
    value_range_check_node = colored("ValueRangeCheck",
                                     ValueRangeCheckNode._colour)
    sched_node = colored("Schedule", Schedule._colour)
    assert f"""{sched_node}[]
            0: {value_range_check_node}[]
                {sched_node}[]""" in result


# -----------------------------------------------------------------------------
def test_value_range_check_options(fortran_writer):
    '''Check that options are passed to the ValueRangeCheckNode and trigger
    the use of the newly defined names.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean", idx=0, dist_mem=False)
    value_range_check = ValueRangeCheckTrans()
    value_range_check.apply(invoke.schedule[0],
                            options={"region_name": ("a", "b")})
    code = fortran_writer(invoke.schedule)
    assert 'CALL value_range_check_psy_data % PreStart("a", "b", 6, 3)' in code


# -----------------------------------------------------------------------------
def test_invalid_apply():
    '''Test the exceptions that should be raised by ValueRangeCheckTrans.

    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean", idx=0)
    value_range_check = ValueRangeCheckTrans()
    omp = OMPParallelLoopTrans()
    omp.apply(invoke.schedule[0])
    with pytest.raises(TransformationError) as err:
        value_range_check.apply(invoke.schedule[0].dir_body[0],
                                options={"region_name": ("a", "b")})

    assert ("Error in ValueRangeCheckTrans: Application to a Loop without "
            "its parent Directive is not allowed." in str(err.value))

    with pytest.raises(TransformationError) as err:
        value_range_check.apply(invoke.schedule[0].dir_body[0].loop_body[0],
                                options={"region_name": ("a", "b")})

    assert ("Error in ValueRangeCheckTrans: Application to Nodes enclosed "
            "within a thread-parallel region is not allowed."
            in str(err.value))


# -----------------------------------------------------------------------------
def test_value_range_check_psyir_visitor(fortran_writer):
    '''Check that options are passed to the ValueRangeCheckNode and trigger
    the use of the newly defined names. This test uses the FortranWriter
    for creating output, which triggers a different code path
    (it is based on lower_to_language_level).

    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean", idx=0, dist_mem=False)

    value_range_check = ValueRangeCheckTrans()
    value_range_check.apply(invoke.schedule,
                            options={"region_name": ("a", "b")})

    code = fortran_writer(invoke.schedule)
    # Test only some of the lines to keep this test short:
    expected = ['CALL value_range_check_psy_data % PreStart("a", "b", 12, 4)',
                'CALL value_range_check_psy_data % PreDeclareVariable("cv_fld%'
                'internal%xstart", cv_fld % internal % xstart)',
                'CALL value_range_check_psy_data % PreDeclareVariable('
                '"ncycle", ncycle)',
                'CALL value_range_check_psy_data % PreEndDeclaration',
                'CALL value_range_check_psy_data % ProvideVariable("ncycle", '
                'ncycle)',
                'CALL value_range_check_psy_data % PreEnd',
                'CALL value_range_check_psy_data % PostStart',
                'CALL value_range_check_psy_data % ProvideVariable('
                '"cv_fld%data", cv_fld % data)',
                'CALL value_range_check_psy_data % ProvideVariable("i", i)',
                'CALL value_range_check_psy_data % ProvideVariable("j", j)',
                'CALL value_range_check_psy_data % ProvideVariable('
                '"p_fld%data", p_fld % data)',
                'CALL value_range_check_psy_data % PostEnd',
                ]

    for line in expected:
        assert line in code, str(line) + "\n --- \n" + str(code)


# -----------------------------------------------------------------------------
def test_value_range_check_lfric():
    '''Check that the value range check transformation works in LFRic.'''
    psy, invoke = get_invoke("1.2_multi_invoke.f90", api="lfric",
                             idx=0, dist_mem=False)

    value_range_check = ValueRangeCheckTrans()
    value_range_check.apply(invoke.schedule)

    code = str(psy.gen)

    # Test some lines - make sure that the number of variables is correct
    # (first line), and some declaration and provide variable before and
    # after the kernel:
    expected = [
        'CALL value_range_check_psy_data % PreStart("multi_invoke_psy", '
        '"invoke_0-r0", 20, 2)',
        'CALL value_range_check_psy_data % PreDeclareVariable("a", a)',
        'CALL value_range_check_psy_data % ProvideVariable("m1_data", '
        'm1_data)',
        'CALL value_range_check_psy_data % ProvideVariable("f1_data", '
        'f1_data)']

    for line in expected:
        assert line in code
