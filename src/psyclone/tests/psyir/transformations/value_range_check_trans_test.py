# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology
# Modified by: R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Module containing tests for ValueRangeCheckTrans and ValueRangeCheckNode
'''

import pytest


from psyclone.errors import InternalError
from psyclone.psyir.nodes import colored, Node, ValueRangeCheckNode, Schedule
from psyclone.psyir.transformations import (ValueRangeCheckTrans,
                                            TransformationError)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelLoopTrans


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
    value_range_check.apply(invoke.schedule[0].loop_body[0],
                            options={"region_name": ("a", "b")})
    code = fortran_writer(invoke.schedule)
    assert 'CALL value_range_check_psy_data % PreStart("a", "b", 4, 2)' in code


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
                'CALL value_range_check_psy_data % ProvideVariable("cv_fld", '
                'cv_fld)',
                'CALL value_range_check_psy_data % ProvideVariable("i", i)',
                'CALL value_range_check_psy_data % ProvideVariable("j", j)',
                'CALL value_range_check_psy_data % ProvideVariable("p_fld", '
                'p_fld)',
                'CALL value_range_check_psy_data % PostEnd',
                ]

    for line in expected:
        assert line in code


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
