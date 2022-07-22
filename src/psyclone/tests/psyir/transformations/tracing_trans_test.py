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
# Author: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Module containing tests for TracingTrans and TracingNode
'''

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import (colored, CodeBlock, Node, TracingNode,
                                  Schedule)
from psyclone.psyir.transformations import (TracingTrans,
                                            TransformationError)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelLoopTrans

# -------------------------------------------------------------------------- #
# ================== Tracing Transformation tests ========================== #
# -------------------------------------------------------------------------- #


def test_tracing_trans():
    '''Tests basic functions in TracingTrans.'''
    tracing = TracingTrans()
    assert str(tracing) == "Create a sub-tree of the PSyIR that has " \
                           "a node of type TracingNode at its root."
    assert tracing.name == "TracingTrans"


# -----------------------------------------------------------------------------
def test_tracing_node(monkeypatch):
    ''' Check that we raise the expected error if a TracingNode does
    not have a single Schedule node as its child. '''
    tracing_node = TracingNode()
    monkeypatch.setattr(tracing_node, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = tracing_node.tracing_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(tracing_node, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = tracing_node.tracing_body
    assert "malformed or incomplete. It should have a " in str(err.value)


# -----------------------------------------------------------------------------
def test_tracing_basic():
    '''Check basic functionality: node names, schedule view.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    tracing = TracingTrans()
    tracing.apply(invoke.schedule[0].loop_body[0])

    # Create the coloured text (if required)
    tracing_node = colored("Tracing", TracingNode._colour)
    sched_node = colored("Schedule", Schedule._colour)
    assert f"""{sched_node}[]
            0: {tracing_node}[]
                {sched_node}[]"""


# -----------------------------------------------------------------------------
def test_tracing_options():
    '''Check that options are passed to the Tracing Node and trigger
    the use of the newly defined names.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    tracing = TracingTrans()
    tracing.apply(invoke.schedule[0].loop_body[0],
                  options={"region_name": ("a", "b")})
    code = str(invoke.gen())

    # Also check for 4 read variables: 2 fields and 2 loop boundaries,
    # and 2 written variables: one field and the loop variable:
    assert 'CALL tracing_psy_data%PreStart("a", "b", 4, 2)' in code


# -----------------------------------------------------------------------------
def test_invalid_apply():
    '''Test the exceptions that should be raised by TracingTrans.

    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    tracing = TracingTrans()
    omp = OMPParallelLoopTrans()
    omp.apply(invoke.schedule[0])
    with pytest.raises(TransformationError) as err:
        tracing.apply(invoke.schedule[0].dir_body[0],
                      options={"region_name": ("a", "b")})
    assert "Error in TracingTrans: Application to a Loop without its "\
           "parent Directive is not allowed." in str(err.value)

    with pytest.raises(TransformationError) as err:
        tracing.apply(invoke.schedule[0].dir_body[0].loop_body[0],
                      options={"region_name": ("a", "b")})
    assert "Error in TracingTrans: Application to Nodes enclosed " \
           "within a thread-parallel region is not allowed." in str(err.value)


# -----------------------------------------------------------------------------
def test_tracing_lower_to_language():
    '''Check that lower_to_language_level behaves as expected.

    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    tracing = TracingTrans()
    loop = invoke.schedule[0]
    tracing.apply(loop.loop_body[0], options={"region_name": ("a", "b")})
    loop.lower_to_language_level()
    codeblocks = loop.walk(CodeBlock)

    expected = ['CALL tracing_psy_data % PreStart("a", "b", 4, 2)',
                'CALL tracing_psy_data % PreDeclareVariable('
                '"cv_fld%internal%xstart", cv_fld % internal % xstart)',
                'CALL tracing_psy_data % PreDeclareVariable("'
                'cv_fld%internal%xstop", cv_fld % internal % xstop)',
                'CALL tracing_psy_data % PreDeclareVariable("p_fld", p_fld)',
                'CALL tracing_psy_data % PreDeclareVariable("v_fld", v_fld)',
                'CALL tracing_psy_data % PreDeclareVariable("cv_fld", cv_fld)',
                'CALL tracing_psy_data % PreDeclareVariable("i", i)',
                'CALL tracing_psy_data % PreEndDeclaration',
                'CALL tracing_psy_data % ProvideVariable("'
                'cv_fld%internal%xstart", cv_fld % internal % xstart)',
                'CALL tracing_psy_data % ProvideVariable("'
                'cv_fld%internal%xstop", cv_fld % internal % xstop)',
                'CALL tracing_psy_data % ProvideVariable("p_fld", p_fld)',
                'CALL tracing_psy_data % ProvideVariable("v_fld", v_fld)',
                'CALL tracing_psy_data % PreEnd',
                'CALL tracing_psy_data % PostStart',
                'CALL tracing_psy_data % ProvideVariable("cv_fld", cv_fld)',
                'CALL tracing_psy_data % ProvideVariable("i", i)',
                'CALL tracing_psy_data % PostEnd']
    for codeblock, string in zip(codeblocks, expected):
        assert string == str(codeblock.ast)
