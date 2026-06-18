# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2026, Science and Technology Facilities Council.
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
# Authors: M. Naylor, University of Cambridge
# -----------------------------------------------------------------------------

''' Performs py.test tests on the ArrayConstructor PSyIR node. '''

import pytest
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    ArrayConstructor, Literal, BinaryOperation, Assignment,
    Reference, IfBlock)
from psyclone.core import AccessType, Signature
from psyclone.psyir.symbols import ScalarType, DataSymbol
from psyclone.psyir.nodes.node import colored


def test_array_construction_valid():
    '''Test array construction and addition of children.'''
    expr1 = Literal("1", ScalarType.integer_type())
    expr2 = Literal("2", ScalarType.integer_type())
    array_cons = ArrayConstructor.create(expr1, expr2)
    expr3 = Literal("3", ScalarType.integer_type())
    expr4 = Literal("4", ScalarType.integer_type())
    expr5 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                   expr3, expr4)
    array_cons.children.append(expr5)
    assert isinstance(array_cons.children[0], Literal)
    assert isinstance(array_cons.children[1], Literal)
    assert isinstance(array_cons.children[2], BinaryOperation)


def test_array_construction_invalid():
    '''Test invalid array construction.'''
    # Construct an IfBlock
    if_condition = Literal('true', ScalarType.boolean_type())
    if_body = [Assignment.create(
        Reference(DataSymbol("tmp", ScalarType.real_single_type())),
        Literal("10", ScalarType.integer_type()))]
    ifblock = IfBlock.create(if_condition, if_body)
    # Check that IfBlock cannot be an element of an ArrayConstructor
    with pytest.raises(GenerationError) as err:
        ArrayConstructor.create(ifblock)
    assert ("Generation Error: Item 'IfBlock' can't be child 0 of "
            "'ArrayConstructor'. The valid format is: '[DataNode]*"
            in str(err.value))


def test_array_construction_reference_accesses():
    '''Test the reference_accesses() method of an array constructor'''
    ref = Reference(DataSymbol("tmp", ScalarType.integer_type()))
    arr = ArrayConstructor.create(ref)
    accs = arr.reference_accesses()
    access_seq = accs[Signature("tmp")]
    assert len(access_seq) == 1
    assert access_seq[0].access_type is AccessType.READ


def test_array_constructor_node_str():
    ''' Check the node_str method of the ArrayConstructor class.'''
    lit = Literal("1", ScalarType.integer_single_type())
    array_cons = ArrayConstructor.create(lit)
    coloured_array_cons = colored("ArrayConstructor", ArrayConstructor._colour)
    assert (f"{coloured_array_cons}[]" in str(array_cons))
