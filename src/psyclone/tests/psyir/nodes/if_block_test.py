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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the IfBlock PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import IfBlock, Literal, Reference, Schedule, \
    Return, Assignment
from psyclone.psyir.symbols import DataSymbol, REAL_SINGLE_TYPE, BOOLEAN_TYPE
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links
from psyclone.psyir.nodes.node import colored


def test_ifblock_invalid_annotation():
    ''' Test that initialising IfBlock with invalid annotations produce the
    expected error.'''

    with pytest.raises(InternalError) as err:
        _ = IfBlock(annotations=["invalid"])
    assert ("IfBlock with unrecognised annotation 'invalid', valid "
            "annotations are:") in str(err.value)


def test_ifblock_node_str():
    ''' Check the node_str method of the IfBlock class.'''
    colouredif = colored("If", IfBlock._colour)

    ifblock = IfBlock()
    output = ifblock.node_str()
    assert colouredif+"[]" in output

    ifblock = IfBlock(annotations=['was_elseif'])
    output = ifblock.node_str()
    assert colouredif+"[annotations='was_elseif']" in output


def test_ifblock_view_indices():
    ''' Check that the view() method only displays indices on the nodes
    in the body (and else body) of an IfBlock. '''
    colouredif = colored("If", IfBlock._colour)
    colouredreturn = colored("Return", Return._colour)
    colouredref = colored("Reference", Reference._colour)
    condition = Reference(DataSymbol('condition1', REAL_SINGLE_TYPE))
    then_content = [Return()]
    ifblock = IfBlock.create(condition, then_content)
    output = ifblock.view()
    # Check that we only prepend child indices where it makes sense
    assert colouredif + "[]" in output
    assert "0: " + colouredreturn in output
    assert ": " + colouredref not in output


def test_ifblock_can_be_printed():
    '''Test that an IfBlock instance can always be printed (i.e. is
    initialised fully)'''
    condition = Reference(DataSymbol('condition1', BOOLEAN_TYPE))
    then_content = [Return()]
    ifblock = IfBlock.create(condition, then_content)

    assert "If[]\n" in str(ifblock)
    assert "condition1" in str(ifblock)  # Test condition is printed
    assert "Return[]" in str(ifblock)  # Test if_body is printed


def test_ifblock_properties():
    '''Test that an IfBlock node properties can be retrieved'''
    ifblock = IfBlock()

    # Condition can't be retrieved before it is added as a child.
    with pytest.raises(InternalError) as err:
        _ = ifblock.condition
    assert ("IfBlock malformed or incomplete. It should have "
            "at least 2 children, but found 0." in str(err.value))

    ref1 = Reference(DataSymbol('condition1', BOOLEAN_TYPE),
                     parent=ifblock)
    ifblock.addchild(ref1)

    # If_body can't be retrieved before is added as a child.
    with pytest.raises(InternalError) as err:
        _ = ifblock.if_body
    assert ("IfBlock malformed or incomplete. It should have "
            "at least 2 children, but found 1." in str(err.value))

    sch = Schedule()
    ifblock.addchild(sch)
    ret = Return()
    sch.addchild(ret)

    # Now we can retrieve the condition and the if_body, but else is empty
    assert ifblock.condition is ref1
    assert ifblock.if_body[0] is ret
    assert not ifblock.else_body

    sch2 = Schedule()
    ifblock.addchild(sch2)
    ret2 = Return()
    sch2.addchild(ret2)

    # Now we can retrieve else_body
    assert ifblock.else_body[0] is ret2


def test_ifblock_create():
    '''Test that the create method in an IfBlock class correctly creates
    an IfBlock instance.

    '''
    # Without an else clause.
    if_condition = Literal('true', BOOLEAN_TYPE)
    if_body = [Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Literal("0.0", REAL_SINGLE_TYPE)),
               Assignment.create(
                   Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE)),
                   Literal("1.0", REAL_SINGLE_TYPE))]
    ifblock = IfBlock.create(if_condition, if_body)
    if_schedule = ifblock.children[1]
    assert isinstance(if_schedule, Schedule)
    check_links(ifblock, [if_condition, if_schedule])
    check_links(if_schedule, if_body)
    result = FortranWriter().ifblock_node(ifblock)
    assert result == ("if (.true.) then\n"
                      "  tmp = 0.0\n"
                      "  tmp2 = 1.0\n"
                      "end if\n")

    # With an else clause.
    if_condition = Literal('true', BOOLEAN_TYPE)
    if_body = [Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Literal("0.0", REAL_SINGLE_TYPE)),
               Assignment.create(
                   Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE)),
                   Literal("1.0", REAL_SINGLE_TYPE))]
    else_body = [Assignment.create(Reference(DataSymbol("tmp",
                                                        REAL_SINGLE_TYPE)),
                                   Literal("1.0", REAL_SINGLE_TYPE)),
                 Assignment.create(Reference(DataSymbol("tmp2",
                                                        REAL_SINGLE_TYPE)),
                                   Literal("0.0", REAL_SINGLE_TYPE))]
    ifblock = IfBlock.create(if_condition, if_body, else_body)
    if_schedule = ifblock.children[1]
    assert isinstance(if_schedule, Schedule)
    else_schedule = ifblock.children[2]
    assert isinstance(else_schedule, Schedule)
    check_links(ifblock, [if_condition, if_schedule, else_schedule])
    check_links(if_schedule, if_body)
    check_links(else_schedule, else_body)
    result = FortranWriter().ifblock_node(ifblock)
    assert result == ("if (.true.) then\n"
                      "  tmp = 0.0\n"
                      "  tmp2 = 1.0\n"
                      "else\n"
                      "  tmp = 1.0\n"
                      "  tmp2 = 0.0\n"
                      "end if\n")


def test_ifblock_create_invalid():
    '''Test that the create method in an IfBlock class raises the expected
    exception if the provided input is invalid.

    '''
    if_condition = Literal('true', BOOLEAN_TYPE)
    if_body = [Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Literal("0.0", REAL_SINGLE_TYPE))]

    # if_condition not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = IfBlock.create("True", if_body)
    assert ("Item 'str' can't be child 0 of 'If'. The valid format is: "
            "'DataNode, Schedule [, Schedule]'.") in str(excinfo.value)

    # One or more if body not a Node.
    if_body_err = [Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Literal("0.0", REAL_SINGLE_TYPE)), "invalid"]
    with pytest.raises(GenerationError) as excinfo:
        _ = IfBlock.create(if_condition, if_body_err)
    assert ("Item 'str' can't be child 1 of 'Schedule'. The valid format is: "
            "'[Statement]*'.") in str(excinfo.value)

    # If body not a list.
    with pytest.raises(GenerationError) as excinfo:
        _ = IfBlock.create(if_condition, "invalid")
    assert ("if_body argument in create method of IfBlock class should be a "
            "list but found 'str'.") in str(excinfo.value)

    # One of more of else_body not a Node.
    if_condition = Literal('true', BOOLEAN_TYPE)
    if_body = [Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Literal("0.0", REAL_SINGLE_TYPE))]

    else_body_err = [Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Literal("1.0", REAL_SINGLE_TYPE)), "invalid"]
    with pytest.raises(GenerationError) as excinfo:
        _ = IfBlock.create(if_condition, if_body, else_body_err)
    assert ("Item 'str' can't be child 1 of 'Schedule'. The valid format is: "
            "'[Statement]*'.") in str(excinfo.value)

    # Else body not a list.
    with pytest.raises(GenerationError) as excinfo:
        _ = IfBlock.create(if_condition, if_body, "invalid")
    assert ("else_body argument in create method of IfBlock class should be a "
            "list but found 'str'.") in str(excinfo.value)


def test_ifblock_children_validation():
    '''Test that children added to IfBlock are validated. IfBlock accepts
    DataNodes for the children 0 to 2 and a Shcedule for child 3.

    '''
    ifblock = IfBlock()
    if_condition = Literal('true', BOOLEAN_TYPE)
    if_body = Schedule()
    else_body = Schedule()

    # First child
    with pytest.raises(GenerationError) as excinfo:
        ifblock.addchild(if_body)
    assert ("Item 'Schedule' can't be child 0 of 'If'. The valid format is: "
            "'DataNode, Schedule [, Schedule]'." in str(excinfo.value))
    ifblock.addchild(if_condition)

    # Second child
    with pytest.raises(GenerationError) as excinfo:
        ifblock.addchild(if_condition)
    assert ("Item 'Literal' can't be child 1 of 'If'. The valid format is: "
            "'DataNode, Schedule [, Schedule]'." in str(excinfo.value))
    ifblock.addchild(if_body)

    # Third child
    with pytest.raises(GenerationError) as excinfo:
        ifblock.addchild(if_condition)
    assert ("Item 'Literal' can't be child 2 of 'If'. The valid format is: "
            "'DataNode, Schedule [, Schedule]'." in str(excinfo.value))
    ifblock.addchild(else_body)

    # Additional children
    with pytest.raises(GenerationError) as excinfo:
        ifblock.addchild(else_body)
    assert ("Item 'Schedule' can't be child 3 of 'If'. The valid format is: "
            "'DataNode, Schedule [, Schedule]'." in str(excinfo.value))
