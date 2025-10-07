# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified: I. Kavcic, L. Turner, O. Brunt and J. G. Wallwork, Met Office
# Modified: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''
Module containing pytest tests for the GlobalReduction class.
'''

import pytest

from psyclone.domain.common.psylayer.global_reduction import (
    GlobalReduction, ReductionOp)
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import colored, Literal, Reference
from psyclone.psyir.symbols import INTEGER_TYPE, Symbol


def test_globalsum_node_str():
    '''test the node_str method in the GlobalSum class. The simplest way
    to do this is to use an LFRic builtin example which contains a
    scalar and then call node_str() on that.

    '''
    gred = GlobalReduction(ReductionOp.SUM,
                           operand=Reference(Symbol("a")))
    output = str(gred)
    expected_output = (colored("GlobalReduction", GlobalReduction._colour) +
                       "[SUM, operand='a']")
    assert expected_output in output


def test_globalsum_children_validation():
    '''Test that children added to GlobalSum are validated. A GlobalSum node
    does not accept any children.

    '''
    gsum = GlobalReduction(ReductionOp.SUM,
                           operand=Reference(Symbol("a")))
    with pytest.raises(GenerationError) as excinfo:
        gsum.addchild(Literal("2", INTEGER_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'GlobalReduction'. "
            "GlobalReduction is a LeafNode and doesn't accept children."
            in str(excinfo.value))
