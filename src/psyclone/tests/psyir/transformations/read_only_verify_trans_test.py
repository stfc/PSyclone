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

''' Module containing tests for ReadOnlyVerifyTrans and ReadOnlyVerifyNode
'''

from __future__ import absolute_import

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import colored, Node, ReadOnlyVerifyNode, Schedule
from psyclone.psyir.transformations import (ReadOnlyVerifyTrans,
                                            TransformationError)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelLoopTrans

# --------------------------------------------------------------------------- #
# ================== ReadOnly Transformation tests ========================== #
# --------------------------------------------------------------------------- #


def test_readonly_trans():
    '''Tests basic functions in ReadOnlyVerifyTrans.'''
    read_only = ReadOnlyVerifyTrans()
    assert str(read_only) == "Create a sub-tree of the PSyIR that has " \
                             "a node of type ReadOnlyVerifyNode at its root."
    assert read_only.name == "ReadOnlyVerifyTrans"


# -----------------------------------------------------------------------------
def test_malformed_readonly_node(monkeypatch):
    ''' Check that we raise the expected error if a ReadOnlyVerifyNode does
    not have a single Schedule node as its child. '''
    read_node = ReadOnlyVerifyNode()
    monkeypatch.setattr(read_node, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = read_node.read_only_verify_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(read_node, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = read_node.read_only_verify_body
    assert "malformed or incomplete. It should have a " in str(err.value)


# -----------------------------------------------------------------------------
def test_read_only_basic():
    '''Check basic functionality: node names, schedule view.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    read_only = ReadOnlyVerifyTrans()
    read_only.apply(invoke.schedule[0].loop_body[0])
    result = invoke.schedule.view()

    # Create the coloured text (if required)
    read_node = colored("ReadOnlyVerify", ReadOnlyVerifyNode._colour)
    sched_node = colored("Schedule", Schedule._colour)
    assert f"""{sched_node}[]
            0: {read_node}[]
                {sched_node}[]""" in result


# -----------------------------------------------------------------------------
def test_read_only_options():
    '''Check that options are passed to the ReadOnly Node and trigger
    the use of the newly defined names.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    read_only = ReadOnlyVerifyTrans()
    read_only.apply(invoke.schedule[0].loop_body[0],
                    options={"region_name": ("a", "b")})
    code = str(invoke.gen())

    assert 'CALL read_only_verify_psy_data%PreStart("a", "b", 4, 4)' in code


# -----------------------------------------------------------------------------
def test_invalid_apply():
    '''Test the exceptions that should be raised by ReadOnlyVerifyTrans.

    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    read_only = ReadOnlyVerifyTrans()
    omp = OMPParallelLoopTrans()
    omp.apply(invoke.schedule[0])
    with pytest.raises(TransformationError) as err:
        read_only.apply(invoke.schedule[0].dir_body[0],
                        options={"region_name": ("a", "b")})
    assert "Error in ReadOnlyVerifyTrans: Application to a Loop without its "\
           "parent Directive is not allowed." in str(err.value)

    with pytest.raises(TransformationError) as err:
        read_only.apply(invoke.schedule[0].dir_body[0].loop_body[0],
                        options={"region_name": ("a", "b")})
    assert "Error in ReadOnlyVerifyTrans: Application to Nodes enclosed " \
           "within a thread-parallel region is not allowed." in str(err.value)
