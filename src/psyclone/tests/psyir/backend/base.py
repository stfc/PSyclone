# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs py.test tests on the psyclond.psyir.backend.base module'''

from psyclone.psyir.backend.base import PSyIRVisitor
import pytest


def test_psyirvisitor_defaults():
    '''Check the PSyIRVisitor class can be instantiated and the default
    values are set appropriately.

    '''
    visitor = PSyIRVisitor()
    assert not visitor._skip_nodes
    assert visitor._indent == "  "
    assert visitor._depth == 0


def test_psyirvisitor_init():
    '''Check the PSyIRVisitor class __init__ arguments are stored.'''
    visitor = PSyIRVisitor(skip_nodes=True, indent=" ", start_depth=1)
    assert visitor._skip_nodes
    assert visitor._indent == " "
    assert visitor._depth == 1


def test_psyirvisitor_init_error1():
    '''Check that the expected error is raised if the PSyIRVisitor class
    __init__ skip_node argument is not a boolean.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = PSyIRVisitor(skip_nodes="hello")
    assert "skip_nodes should be a boolean but found 'str'." \
        in str(excinfo.value)


def test_psyirvisitor_init_error2():
    '''Check that the expected error is raised if the PSyIRVisitor class
    __init__ indent argument is not a string.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = PSyIRVisitor(indent=True)
    assert "indent should be a str but found 'bool'." in str(excinfo.value)


def test_psyirvisitor_init_error3():
    '''Check that the expected error is raised if the PSyIRVisitor class
    __init__ start_depth argument is not an integer.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = PSyIRVisitor(start_depth="2")
    assert "start_depth should be an integer but found 'str'." \
        in str(excinfo.value)


def test_psyirvisitor_init_error4():
    '''Check that the expected error is raised if the PSyIRVisitor class
    __init__ start_depth argument is negative.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = PSyIRVisitor(start_depth=-1)
    assert "start_depth should not be negative, but found '-1'." \
        in str(excinfo.value)

