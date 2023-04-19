# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
from psyclone.psyir.nodes import ExtractNode, Node
from psyclone.psyir.transformations import ExtractTrans

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
    postfix = ExtractTrans.determine_postfix([], [])
    assert postfix == "_post"
    postfix = ExtractTrans.determine_postfix([], [], postfix="_new_postfix")
    assert postfix == "_new_postfix"

    # Clash between input variable and a created output variable:
    postfix = ExtractTrans.determine_postfix([Signature("var_post")],
                                             [Signature("var")])
    assert postfix == "_post0"

    # Two clashes between input variable and a created output variable:
    postfix = ExtractTrans.determine_postfix([Signature("var_post"),
                                              Signature("var_post0")],
                                             [Signature("var")])
    assert postfix == "_post1"

    # Two clashes between different input variables and created output
    # variables: 'var1' prevents the '_post' to be used, 'var2'
    # prevents "_post0" to be used, 'var3' prevents "_post1":
    postfix = ExtractTrans.determine_postfix([Signature("var1_post"),
                                              Signature("var2_post0"),
                                              Signature("var3_post1")],
                                             [Signature("var1"),
                                              Signature("var2"),
                                              Signature("var3")])
    assert postfix == "_post2"

    # Handle clash between output variables: the first variable will
    # create "var" and var_post", the second "var_post" and "var_post_post".
    postfix = ExtractTrans.determine_postfix([],
                                             [Signature("var"),
                                              Signature("var_post")])
    assert postfix == "_post0"
    postfix = ExtractTrans.determine_postfix([],
                                             [Signature("var"),
                                              Signature("var_post"),
                                              Signature("var_post0")])
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
