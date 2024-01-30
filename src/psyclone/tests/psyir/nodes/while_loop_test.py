# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Authors: N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the WhileLoop PSyIR node. '''

import pytest
from psyclone.core import VariablesAccessInfo
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Assignment, BinaryOperation, Literal, \
                                 Reference, Return, Schedule, WhileLoop
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import DataSymbol, REAL_SINGLE_TYPE, BOOLEAN_TYPE
from psyclone.tests.utilities import check_links


def test_whileloop_invalid_annotation():
    ''' Test that initialising WhileLoop with invalid annotations produces the
    expected error.'''

    with pytest.raises(InternalError) as err:
        _ = WhileLoop(annotations=["invalid"])
    assert ("WhileLoop with unrecognised annotation 'invalid', valid "
            "annotations are:") in str(err.value)


def test_whileloop_node_str():
    ''' Check the node_str method of the WhileLoop class.'''

    coloured_loop = colored("WhileLoop", "red")

    loop = WhileLoop()
    output = loop.node_str()
    assert coloured_loop+"[]" in output

    loop = WhileLoop(annotations=['was_unconditional'])
    output = loop.node_str()
    assert coloured_loop+"[annotations='was_unconditional']" in output


def test_whileloop_create_and_refence_accesses():
    '''Test that the create method in the WhileLoop class correctly creates
    a WhileLoop instance and that the reference_access method correctly
    captures variable accesses.'''

    ref1 = Reference(DataSymbol("tmp", REAL_SINGLE_TYPE))
    ref2 = Reference(DataSymbol("pmt", REAL_SINGLE_TYPE))
    loop_condition = BinaryOperation.create(BinaryOperation.Operator.GT, ref1,
                                            Literal("0.0", REAL_SINGLE_TYPE))
    loop_body = [Assignment.create(ref2, Literal("1.0", REAL_SINGLE_TYPE))]
    loop = WhileLoop.create(loop_condition, loop_body)
    loop_schedule = loop.children[1]
    assert isinstance(loop_schedule, Schedule)
    check_links(loop, [loop_condition, loop_schedule])
    check_links(loop_schedule, loop_body)
    result = FortranWriter().whileloop_node(loop)
    assert result == ("do while (tmp > 0.0)\n"
                      "  pmt = 1.0\n"
                      "end do\n")
    var_access_info = VariablesAccessInfo()
    loop.reference_accesses(var_access_info)
    assert (str(var_access_info)) == "pmt: WRITE, tmp: READ"


def test_whileloop_create_invalid():
    '''Test that the create method in the WhileLoop class raises the expected
    exception if the provided input is invalid.'''

    loop_condition = Literal('true', BOOLEAN_TYPE)
    loop_body = [Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Literal("0.0", REAL_SINGLE_TYPE))]

    # Loop condition not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = WhileLoop.create("True", loop_body)
    assert ("Item 'str' can't be child 0 of 'WhileLoop'. The valid format is: "
            "'DataNode, Schedule'.") in str(excinfo.value)

    # Loop body not a Node.
    loop_body_err = [Assignment.create(
                                Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
                                Literal("0.0", REAL_SINGLE_TYPE)), "invalid"]
    with pytest.raises(GenerationError) as excinfo:
        _ = WhileLoop.create(loop_condition, loop_body_err)
    assert ("Item 'str' can't be child 1 of 'Schedule'. The valid format is: "
            "'[Statement]*'.") in str(excinfo.value)

    # While loop body not a list.
    with pytest.raises(GenerationError) as excinfo:
        _ = WhileLoop.create(loop_condition, "invalid")
    assert ("loop_body argument in create method of WhileLoop class should be "
            "a list but found 'str'.") in str(excinfo.value)


def test_whileloop_properties():
    '''Test that a WhileLoop node properties can be retrieved.'''

    loop = WhileLoop()

    # Loop condition can't be retrieved before it is added as a child.
    with pytest.raises(InternalError) as err:
        _ = loop.condition
    assert ("WhileLoop malformed or incomplete. It should have "
            "2 children, but found 0." in str(err.value))

    ref1 = Reference(DataSymbol('condition1', BOOLEAN_TYPE),
                     parent=loop)
    loop.addchild(ref1)

    # Loop body can't be retrieved before is added as a child.
    with pytest.raises(InternalError) as err:
        _ = loop.loop_body
    assert ("WhileLoop malformed or incomplete. It should have "
            "2 children, but found 1." in str(err.value))

    sch = Schedule()
    loop.addchild(sch)
    ret = Return()
    sch.addchild(ret)

    # Now we can retrieve the condition and the loop body.
    assert loop.condition is ref1
    assert loop.loop_body[0] is ret


def test_whileloop_can_be_printed():
    '''Test that a WhileLoop instance can be printed.'''

    loop_condition = Reference(DataSymbol('condition1', BOOLEAN_TYPE))
    loop_body = [Return()]
    loop = WhileLoop.create(loop_condition, loop_body)

    assert "WhileLoop[]\n" in str(loop)
    assert "condition1" in str(loop)  # Test condition is printed
    assert "Return[]" in str(loop)  # Test loop body is printed
