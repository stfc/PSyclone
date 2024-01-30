# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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

''' Module containing tests for NanTestTrans and NanTestNode
'''

from __future__ import absolute_import

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import colored, Node, NanTestNode, Schedule
from psyclone.psyir.transformations import (NanTestTrans,
                                            TransformationError)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelLoopTrans


# --------------------------------------------------------------------------- #
def test_extract_trans():
    '''Tests basic functions in NanTestTrans.'''
    nan_test = NanTestTrans()
    assert str(nan_test) == "Create a sub-tree of the PSyIR that has " \
                            "a node of type NanTestNode at its root."
    assert nan_test.name == "NanTestTrans"


# -----------------------------------------------------------------------------
def test_malformed_extract_node(monkeypatch):
    ''' Check that we raise the expected error if a NanTestNode does
    not have a single Schedule node as its child. '''
    read_node = NanTestNode()
    monkeypatch.setattr(read_node, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = read_node.nan_test_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(read_node, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = read_node.nan_test_body
    assert "malformed or incomplete. It should have a " in str(err.value)


# -----------------------------------------------------------------------------
def test_nan_test_basic():
    '''Check basic functionality: node names, schedule view.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    nan_test = NanTestTrans()
    nan_test.apply(invoke.schedule[0].loop_body[0])
    result = invoke.schedule.view()

    # Create the coloured text (if required)
    read_node = colored("NanTest", NanTestNode._colour)
    sched_node = colored("Schedule", Schedule._colour)
    assert f"""{sched_node}[]
            0: {read_node}[]
                {sched_node}[]""" in result


# -----------------------------------------------------------------------------
def test_nan_test_options():
    '''Check that options are passed to the NanTestNode and trigger
    the use of the newly defined names.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    nan_test = NanTestTrans()
    nan_test.apply(invoke.schedule[0].loop_body[0],
                   options={"region_name": ("a", "b")})
    code = str(invoke.gen())
    assert 'CALL nan_test_psy_data%PreStart("a", "b", 4, 2)' in code


# -----------------------------------------------------------------------------
def test_invalid_apply():
    '''Test the exceptions that should be raised by NanTestTrans.

    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    nan_test = NanTestTrans()
    omp = OMPParallelLoopTrans()
    omp.apply(invoke.schedule[0])
    with pytest.raises(TransformationError) as err:
        nan_test.apply(invoke.schedule[0].dir_body[0],
                       options={"region_name": ("a", "b")})

    assert "Error in NanTestTrans: Application to a Loop without its "\
           "parent Directive is not allowed." in str(err.value)

    with pytest.raises(TransformationError) as err:
        nan_test.apply(invoke.schedule[0].dir_body[0].loop_body[0],
                       options={"region_name": ("a", "b")})

    assert "Error in NanTestTrans: Application to Nodes enclosed within a "\
           "thread-parallel region is not allowed." in str(err.value)


# -----------------------------------------------------------------------------
def test_nan_test_psyir_visitor(fortran_writer):
    '''Check that options are passed to the NanTestNode and trigger
    the use of the newly defined names. This test uses the FortranWriter
    for creating output, which triggers a different code path
    (it is based on lower_to_language_level).

    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)

    nan_test = NanTestTrans()
    nan_test.apply(invoke.schedule, options={"region_name": ("a", "b")})

    code = fortran_writer(invoke.schedule)
    # Test only some of the lines to keep this test short:
    expected = ['CALL nan_test_psy_data % PreStart("a", "b", 12, 4)',
                'CALL nan_test_psy_data % PreDeclareVariable("cv_fld%'
                'internal%xstart", cv_fld % internal % xstart)',
                'CALL nan_test_psy_data % PreDeclareVariable("ncycle", '
                'ncycle)',
                'CALL nan_test_psy_data % PreEndDeclaration',
                'CALL nan_test_psy_data % ProvideVariable("ncycle", ncycle)',
                'CALL nan_test_psy_data % PreEnd',
                'CALL nan_test_psy_data % PostStart',
                'CALL nan_test_psy_data % ProvideVariable("cv_fld", cv_fld)',
                'CALL nan_test_psy_data % ProvideVariable("i", i)',
                'CALL nan_test_psy_data % ProvideVariable("j", j)',
                'CALL nan_test_psy_data % ProvideVariable("p_fld", p_fld)',
                'CALL nan_test_psy_data % PostEnd',
                ]

    for line in expected:
        assert line in code
