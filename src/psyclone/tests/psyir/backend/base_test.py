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

'''Performs pytest tests on the psyclond.psyir.backend.base module'''

from __future__ import print_function
import pytest
from psyclone.psyir.backend.base import PSyIRVisitor, VisitorError
from psyclone.psyGen import Node


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
    visitor = PSyIRVisitor(skip_nodes=True, indent_string=" ",
                           initial_indent_depth=1)
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
    __init__ indent_string argument is not a string.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = PSyIRVisitor(indent_string=True)
    assert ("indent_string should be a str but found "
            "'bool'." in str(excinfo.value))


def test_psyirvisitor_init_error3():
    '''Check that the expected error is raised if the PSyIRVisitor class
    __init__ initial_indent_depth argument is not an integer.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = PSyIRVisitor(initial_indent_depth="2")
    assert "initial_indent_depth should be an integer but found 'str'." \
        in str(excinfo.value)


def test_psyirvisitor_init_error4():
    '''Check that the expected error is raised if the PSyIRVisitor class
    __init__ initial_indent_depth argument is negative.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = PSyIRVisitor(initial_indent_depth=-1)
    assert "initial_indent_depth should not be negative, but found '-1'." \
        in str(excinfo.value)


def test_psyirvisitor_nindent():
    '''Check that the PSyIRVisitor _nindent method returns the product of
    the supplied depth and indent values.

    '''
    visitor = PSyIRVisitor(indent_string=" ", initial_indent_depth=4)
    assert visitor._nindent == "    "


def test_psyirvisitor_visit_arg_error():
    '''Check that an exception is raised if the argument to the
    PSyIRVisitor visit method is not a Node or subclass of Node.

    '''
    visitor = PSyIRVisitor(indent_string=" ", initial_indent_depth=4)
    with pytest.raises(VisitorError) as excinfo:
        visitor.visit("hello")
    assert ("Visitor Error: Expected argument to be of type 'Node' but found "
            "'str'." in str(excinfo.value))


def test_psyirvisitor_visit_no_method1():
    '''Check that an exception is raised if the method for the Node class
    does not exist.

    '''
    visitor = PSyIRVisitor()
    with pytest.raises(VisitorError) as excinfo:
        visitor.visit(Node())
    assert (
        "Visitor Error: Unsupported node 'Node' found: method names "
        "attempted were ['node', 'object']." in str(excinfo.value))


def test_psyirvisitor_visit_no_method2():
    '''Check that an exception is not raised if the method for the Node class
    does not exist and skip_nodes is set to True.

    '''
    visitor = PSyIRVisitor(skip_nodes=True)
    result = visitor.visit(Node())
    assert result is None


def test_psyirvisitor_visit_attribute_error():
    '''Check that an Attribute Error is raised if the method for the Node
    class does exist and the method itself raises an Attribute
    Error. This is checked because AttributeError is used to catch a
    method not existing. Therefore an AttributeError raised by a
    method that does exist could be mistaken as meaning that the
    method does not exist.

    '''
    class MyPSyIRVisitor(PSyIRVisitor):
        '''Subclass PSyIRVisitor to make the node method exist but it itself
        raises an attribute error.'''
        def node(self, _):
            ''' Raise an AttributeError for testing purposes '''
            raise AttributeError("Error")

    visitor = MyPSyIRVisitor()
    with pytest.raises(AttributeError) as excinfo:
        _ = visitor.visit(Node())
    assert str(excinfo.value) == "Error"


def test_psyirvisitor_visit_all_parents():
    '''Check that the names of the class and all ancestors of the class
    are tried when looking to find a method.

    '''
    class Unsupported(Node):
        '''Subclass of node used to check that the names of all ancestors of a
        node are called as methods, in method resolution order (mro).

        '''
    visitor = PSyIRVisitor()
    with pytest.raises(VisitorError) as excinfo:
        visitor.visit(Unsupported())
    assert (
        "Visitor Error: Unsupported node 'Unsupported' found: method names "
        "attempted were ['unsupported', 'node', 'object']."
        "" in str(excinfo.value))


def test_psyirvisitor_visit_skip_nodes(capsys):
    '''Check that when the skip_nodes variable is set to true then child
    nodes are called irrespective of whether a parent node has a
    corresponding match in the visitor class.

    '''
    class TestVisitor(PSyIRVisitor):
        '''Subclass of PSyIRVisitor used to check that the skip_nodes variable
        works as expected.

        '''
        def testnode2(self, _):
            '''Match with class TestNode2. The print is used to check that this
            method is called.

            '''
            print("testnode2 called")

    class TestNode1(Node):
        '''Subclass of Node used to check that the skip_nodes variable in
        TestVisitor works correctly.

        '''

    class TestNode2(Node):
        '''Subclass of Node used to check that the skip_nodes variable in
        TestVisitor works correctly.

        '''

    # Create a simple Node hierachy with an instance of class
    # TestNode2 being the child of an instance of class TestNode1.
    test_node2 = TestNode2()
    test_node1 = TestNode1(children=[test_node2])

    # Visit the node hierarchy skipping the parent node (an instance
    # of class TestNode1) then visiting the child node (an instance of
    # class TestNode2) if setting skip_nodes to True works as
    # expected.
    test_visitor = TestVisitor(skip_nodes=True)
    _ = test_visitor.visit(test_node1)

    # Success, the child node (an instance of class TestNode2) has
    # been visited.
    output, _ = capsys.readouterr()
    assert output == "testnode2 called\n"


def test_psyirvisitor_visit_return_node():
    '''Check that when a return PSyIR node is found the actual method
    called is 'return_node'. This is done to avoid clashing with the
    Python keyword.

    '''
    from psyclone.psyGen import Return
    return_node = Return()
    test_visitor = PSyIRVisitor()
    with pytest.raises(VisitorError) as excinfo:
        _ = test_visitor.visit(return_node)
    assert ("Visitor Error: Unsupported node 'Return' found: method names "
            "attempted were ['return_node', 'node', 'object']."
            ""in str(excinfo))
