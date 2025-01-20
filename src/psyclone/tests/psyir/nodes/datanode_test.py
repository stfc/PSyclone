# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified J. G. Wallwork, University of Cambridge
# -----------------------------------------------------------------------------

'''Performs pytest tests on the PSyIR DataNode.

'''
import pytest

from psyclone.psyir.nodes import DataNode, Reference, BinaryOperation
from psyclone.psyir.symbols import (CHARACTER_TYPE, DataSymbol, UnresolvedType,
                                    INTEGER_SINGLE_TYPE, REAL_TYPE)


def test_datanode_datatype():
    '''
    Test that the base implementation of datatype just returns UnresolvedType.

    '''
    dnode = DataNode()
    assert isinstance(dnode.datatype, UnresolvedType)


def test_datanode_is_character():
    '''Test that character expressions are marked correctly.
    '''
    reference = Reference(DataSymbol("char", CHARACTER_TYPE))
    assert reference.is_character()

    reference = Reference(DataSymbol("int", INTEGER_SINGLE_TYPE))
    reference2 = Reference(DataSymbol("int2", INTEGER_SINGLE_TYPE))
    bop = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                 reference, reference2)
    assert not reference.is_character()
    assert not reference2.is_character()
    assert not bop.is_character()

    reference = Reference(DataSymbol("real", REAL_TYPE))
    assert not reference.is_character()

    reference = Reference(DataSymbol("unknown", UnresolvedType()))
    with pytest.raises(ValueError) as excinfo:
        _ = reference.is_character()
    assert ("is_character could not resolve whether the expression 'unknown'"
            " operates on characters." in str(excinfo.value))
    reference = Reference(DataSymbol("unknown", UnresolvedType()))
    assert not reference.is_character(unknown_as=False)
    assert reference.is_character(unknown_as=True)
