# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Author I. Kavcic, Met Office
# Modified by A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Module containing tests for PSyclone ExtractTrans
and ExtractNode.
'''

import pytest

from psyclone.core import Signature
from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.errors import InternalError
from psyclone.psyir.nodes import ExtractNode, Loop, Node
from psyclone.psyir.tools import ReadWriteInfo
from psyclone.psyir.transformations import ExtractTrans, TransformationError
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import (ACCParallelTrans, ACCLoopTrans,
                                      DynamoOMPParallelLoopTrans)


# --------------------------------------------------------------------------- #
# ================== Extract Transformation tests =========================== #
# --------------------------------------------------------------------------- #


def test_extract_trans():
    '''Tests basic functions in ExtractTrans.'''
    etrans = ExtractTrans()
    assert str(etrans) == "Create a sub-tree of the PSyIR that has " \
                          "a node of type ExtractNode at its root."
    assert etrans.name == "ExtractTrans"

    ltrans = LFRicExtractTrans()
    assert str(ltrans) == "Create a sub-tree of the PSyIR that has " \
                          "a node of type ExtractNode at its root."


# --------------------------------------------------------------------------- #
def test_determine_postfix():
    '''Test that a unique postfix is determined.
    '''

    # Test if there is no clash that the specified postfix is returned as is:
    read_write_info = ReadWriteInfo()
    postfix = ExtractTrans.determine_postfix(read_write_info)
    assert postfix == "_post"
    postfix = ExtractTrans.determine_postfix(read_write_info,
                                             postfix="_new_postfix")
    assert postfix == "_new_postfix"

    # Clash between input variable and a created output variable:
    read_write_info = ReadWriteInfo()
    read_write_info.add_read(Signature("var_post"))
    read_write_info.add_write(Signature("var"))
    postfix = ExtractTrans.determine_postfix(read_write_info)
    assert postfix == "_post0"

    # Two clashes between input variable and a created output variable:
    read_write_info.add_read(Signature("var_post0"))
    postfix = ExtractTrans.determine_postfix(read_write_info)
    assert postfix == "_post1"

    # Two clashes between different input variables and created output
    # variables: 'var1' prevents the '_post' to be used, 'var2'
    # prevents "_post0" to be used, 'var3' prevents "_post1":
    read_write_info = ReadWriteInfo()
    read_write_info.add_read(Signature("var1_post"))
    read_write_info.add_read(Signature("var2_post0"))
    read_write_info.add_read(Signature("var3_post1"))
    read_write_info.add_write(Signature("var1"))
    read_write_info.add_write(Signature("var2"))
    read_write_info.add_write(Signature("var3"))
    postfix = ExtractTrans.determine_postfix(read_write_info)
    assert postfix == "_post2"

    # Handle clash between output variables: the first variable will
    # create "var" and var_post", the second "var_post" and "var_post_post".
    read_write_info = ReadWriteInfo()
    read_write_info. add_write(Signature("var"))
    read_write_info. add_write(Signature("var_post"))
    postfix = ExtractTrans.determine_postfix(read_write_info)
    assert postfix == "_post0"
    read_write_info.add_write(Signature("var_post0"))
    postfix = ExtractTrans.determine_postfix(read_write_info)
    assert postfix == "_post1"


# --------------------------------------------------------------------------- #
def test_malformed_extract_node(monkeypatch):
    ''' Check that we raise the expected error if an ExtractNode does not have
    a single Schedule node as its child. '''
    enode = ExtractNode()
    monkeypatch.setattr(enode, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = enode.extract_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(enode, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = enode.extract_body
    assert "malformed or incomplete. It should have a " in str(err.value)


# --------------------------------------------------------------------------- #
def test_get_default_options():
    '''Check the default options.'''

    etrans = ExtractTrans()
    assert etrans.get_default_options() == {"COLLECT-ARRAY-SHAPE-READS": True}


# -----------------------------------------------------------------------------
def test_extract_validate():
    '''Test that the validate function can successfully finish.'''

    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    etrans = ExtractTrans()
    etrans.validate(invoke.schedule.children)


# -----------------------------------------------------------------------------
def test_extract_distributed_memory():
    '''Test that distributed memory must be disabled.'''

    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           "gocean1.0", idx=0, dist_mem=True)
    etrans = ExtractTrans()
    with pytest.raises(TransformationError) as excinfo:
        etrans.validate(invoke.schedule.children[3])
    assert ("Error in ExtractTrans: Distributed memory is "
            "not supported.") in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_extract_kern_builtin_no_loop():
    ''' Test that applying Extract Transformation on a Kernel or Built-in
    call without its parent Loop raises a TransformationError. '''

    gocetrans = ExtractTrans()
    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Test Kernel call
    kernel_call = schedule.children[0].loop_body[0].loop_body[0]
    with pytest.raises(TransformationError) as excinfo:
        gocetrans.validate([kernel_call])
    assert "Error in ExtractTrans: Application to a Kernel or a " \
           "Built-in call without its parent Loop is not allowed." \
           in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_extract_loop_no_directive_dynamo0p3():
    ''' Test that applying Extract Transformation on a Loop without its
    parent Directive when optimisations are applied in Dynamo0.3 API
    raises a TransformationError. '''
    etrans = LFRicExtractTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    _, invoke = get_invoke("4.13_multikernel_invokes_w3_anyd.f90",
                           "dynamo0.3", idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Apply DynamoOMPParallelLoopTrans to the second Loop
    otrans = DynamoOMPParallelLoopTrans()
    otrans.apply(schedule[1])
    loop = schedule.children[1].dir_body[0]
    # Try extracting the Loop inside the OMP Parallel DO region
    with pytest.raises(TransformationError) as excinfo:
        etrans.validate([loop])
    assert "Error in LFRicExtractTrans: Application to a Loop without its " \
           "parent Directive is not allowed." in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_extract_directive_no_loop():
    ''' Test that applying Extract Transformation on an orphaned
    ACCLoopDirective without its ancestor ACCParallelDirective
    when optimisations are applied raises a TransformationError. '''

    etrans = ExtractTrans()
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()

    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply the OpenACC Loop transformation to every loop in the Schedule
    for child in schedule.children:
        if isinstance(child, Loop):
            acclpt.apply(child)
    # Enclose all of these loops within a single ACC Parallel region
    accpara.apply(schedule.children)

    orphaned_directive = schedule.children[0].children[0]
    with pytest.raises(TransformationError) as excinfo:
        etrans.validate(orphaned_directive)
    assert "Error in ExtractTrans: Application to Nodes enclosed " \
           "within a thread-parallel region is not allowed." \
           in str(excinfo.value)
