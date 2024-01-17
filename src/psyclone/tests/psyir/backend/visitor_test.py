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
# Author: R. W. Ford, STFC Daresbury Laboratory
# Modified: A. R. Porter and S. Siso, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.visitor module'''

from __future__ import print_function, absolute_import
import pytest
from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyir.nodes import Node, Reference, ArrayReference, Return, \
    Statement, Schedule, Container, Routine
from psyclone.psyir.symbols import DataSymbol, ArrayType, REAL_TYPE
from psyclone.errors import GenerationError


def test_psyirvisitor_defaults():
    '''Check the PSyIRVisitor class can be instantiated and the default
    values are set appropriately.

    '''
    visitor = PSyIRVisitor()
    assert not visitor._skip_nodes
    assert visitor._indent == "  "
    assert visitor._depth == 0
    assert visitor._validate_nodes is True


def test_psyirvisitor_init():
    '''Check the PSyIRVisitor class __init__ arguments are stored.'''
    visitor = PSyIRVisitor(skip_nodes=True, indent_string=" ",
                           initial_indent_depth=1,
                           check_global_constraints=False)
    assert visitor._skip_nodes
    assert visitor._indent == " "
    assert visitor._depth == 1
    assert visitor._validate_nodes is False


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


def test_psyirvisitor_init_error5():
    '''Check that the expected error is raised if the check_global_constraints
    argument to the PSyIRVisitor constructor is not a bool.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = PSyIRVisitor(check_global_constraints=-1)
    assert ("check_global_constraints should be a boolean but found 'int'" in
            str(excinfo.value))


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
    with pytest.raises(TypeError) as excinfo:
        visitor("hello")
    assert ("The PSyIR visitor functor method only accepts a PSyIR Node "
            "as argument, but found 'str'." in str(excinfo.value))
    with pytest.raises(VisitorError) as excinfo:
        visitor._visit("hello")
    assert ("Visitor Error: Expected argument to be of type 'Node' but found "
            "'str'." in str(excinfo.value))


def test_psyirvisitor_visit_no_string():
    '''Check that the visitor supports non-strings being returned.

    '''
    class MyVisitor(PSyIRVisitor):
        '''Test visitor to check that non-strings can be returned by a visitor
        and are returned as a hierarchy. In this example the supplied
        nodes are returned unmodified.

        '''
        def node_node(self, node):
            '''Return the supplied node unmodified.

            :param node: the supplied PSyIR node.
            :type node: :py:class:`psyclone.psyir.nodes.Node`

            '''
            return node

    visitor = MyVisitor()
    my_node = Container("blah")
    my_child_node = Routine("hmm")
    my_node.children = [my_child_node]
    result = visitor._visit(my_node)
    assert result is my_node
    assert len(result.children) == 1
    assert result.children[0] is my_child_node


def test_psyirvisitor_lower_dsl_concepts():
    ''' Test that DSL concepts are lowered by the visitors but the node is not
    modified after the visitor has finished. '''

    class MyDSLNode(Statement):
        ''' DSL Concept that lowers to a return statement '''
        _text_name = "MyDSLNode"

        def lower_to_language_level(self):
            ''' MyDSLNode lowers to a return statement and adds a symbol
            if it is inside an scoping region.

            :returns: the lowered return statement.
            :rtype: :py:class:`psyclone.psyir.nodes.Node`

            '''
            # This will break if this Node does not have a parent with
            # a scope. This is intentional to cause an error during the
            # lowering step.
            self.scope.symbol_table.add(DataSymbol("val", REAL_TYPE))
            new_node = Return()
            self.replace_with(new_node)
            return new_node

    class MyVisitor(PSyIRVisitor):
        ''' Simple Visitor for Schedules and Return statements '''
        def return_node(self, _):
            ''' Return node visitor '''
            return "return"

        def schedule_node(self, node):
            ''' Schedule node visitor '''
            return "schedule(" + self._visit(node.children[0]) + ")"

    # Create a custom visitor and dsl-level node
    visitor = MyVisitor(indent_string=" ", initial_indent_depth=4)
    schedule = Schedule()
    my_dsl_node = MyDSLNode()
    schedule.addchild(my_dsl_node)

    # Visit DSL Node in a tree (the tree should not be modified)
    assert visitor(schedule) == "schedule(return)"
    assert isinstance(schedule.children[0], MyDSLNode)
    assert schedule.children[0] is my_dsl_node
    assert len(schedule.symbol_table.symbols) == 0

    # Visit DSL Node directly (the tree is also not modified)
    assert visitor(my_dsl_node) == "return"
    assert isinstance(my_dsl_node, MyDSLNode)
    assert isinstance(schedule.children[0], MyDSLNode)
    assert len(my_dsl_node.scope.symbol_table.symbols) == 0

    # Visit DSL node without a parent, which is an invalid state to
    # lower this node
    my_dsl_node.detach()
    with pytest.raises(VisitorError) as excinfo:
        visitor(my_dsl_node)
    assert (
        "Failed to lower 'MyDSLNode[]'. Note that some nodes need to be "
        "lowered from an ancestor in order to properly apply their in-tree "
        "modifications. Original error was 'PSyclone SymbolTable error: "
        "Unable to find the scope of node 'MyDSLNode[]' as none of its "
        "ancestors are Container or Schedule nodes.'." in str(excinfo.value))


def test_psyirvisitor_visit_no_method1():
    '''Check that an exception is raised if the method for the Node class
    does not exist.

    '''
    visitor = PSyIRVisitor()
    with pytest.raises(VisitorError) as excinfo:
        visitor(Node())
    assert (
        "Visitor Error: Unsupported node 'Node' found: method names "
        "attempted were ['node_node']." in str(excinfo.value))


def test_psyirvisitor_visit_no_method2():
    '''Check that no exception is raised if the method for the Node class
    does not exist and skip_nodes is set to True.

    '''
    visitor = PSyIRVisitor(skip_nodes=True)
    result = visitor(Node())
    assert result == ""


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
        def node_node(self, _):
            ''' Raise an AttributeError for testing purposes '''
            raise AttributeError("Error")

    visitor = MyPSyIRVisitor()
    with pytest.raises(AttributeError) as excinfo:
        _ = visitor(Node())
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
        visitor(Unsupported())
    assert (
        "Visitor Error: Unsupported node 'Unsupported' found: method names "
        "attempted were ['unsupported_node', 'node_node']."
        "" in str(excinfo.value))


def test_psyirvisitor_visit_skip_nodes():
    '''Check that when the skip_nodes variable is set to true then child
    nodes are called irrespective of whether a parent node has a
    corresponding match in the visitor class.

    '''
    class TestVisitor(PSyIRVisitor):
        '''Subclass of PSyIRVisitor used to check that the skip_nodes variable
        works as expected.

        '''
        def testnode2_node(self, _):
            ''' Match with class TestNode2. '''
            return "testnode2\n"

    class TestNode2(Node):
        '''Subclass of Node used to check that the skip_nodes variable in
        TestVisitor works correctly.

        '''

    class TestNode1(Node):
        '''Subclass of Node used to check that the skip_nodes variable in
        TestVisitor works correctly.

        '''
        @staticmethod
        def _validate_child(_, child):
            return isinstance(child, TestNode2)

    # Create a simple Node hierarchy with an instance of class
    # TestNode2 being the child of an instance of class TestNode1.
    test_node2 = TestNode2()
    test_node1 = TestNode1(children=[test_node2])

    # Visit the node hierarchy skipping the parent node (an instance
    # of class TestNode1) then visiting the child node (an instance of
    # class TestNode2) if setting skip_nodes to True works as
    # expected.
    test_visitor = TestVisitor(skip_nodes=True)
    result = test_visitor(test_node1)

    # Success, the child node (an instance of class TestNode2) has
    # been visited.
    assert result == "testnode2\n"


def test_psyir_comments_no_prefix():
    ''' Test the visitor behaviour for PSyIR with comments '''

    class TestVisitorNoPrefix(PSyIRVisitor):
        ''' Subclass of PSyIRVisitor with a statement visitor '''
        def statement_node(self, _):
            ''' Statement node visitor. '''
            return "statement\n"

    class TestVisitorPrefix(TestVisitorNoPrefix):
        ''' Same as above but with a _COMMENT_PREFIX '''
        _COMMENT_PREFIX = ":) "

    statement = Return()
    statement.preceding_comment = "C1"
    statement.inline_comment = "C2"
    visitor_no_prefix = TestVisitorNoPrefix()
    visitor_prefix = TestVisitorPrefix()
    # If the backend doesn't have a prefix, comments are ignored
    assert visitor_no_prefix(statement) == "statement\n"
    # Otherwise comments are inserted
    assert visitor_prefix(statement) == ":) C1\nstatement  :) C2\n"


def test_psyir_inline_comments_error():
    ''' Test that a Visitor don't allow inline comments if the produced
    output doesn't terminate with a line break '''

    class TestVisitor(PSyIRVisitor):
        ''' Subclass of PSyIRVisitor used to check that inline statements
        with non-terminated constructs would fail '''
        _COMMENT_PREFIX = ":) "

        def statement_node(self, _):
            ''' Statement node visitor. '''
            return "statement"

    statement = Return()
    statement.inline_comment = "An inline comment"
    test_visitor = TestVisitor()
    with pytest.raises(VisitorError) as err:
        _ = test_visitor(statement)
    assert ("An inline_comment can only be added to a construct that finishes "
            "with a '\\n', indicating that the line has ended, but node "
            "'Return[]' results in 'statement'." in str(err.value))


def test_psyirvisitor_validation():
    ''' Check that validation of the tree is performed and can be
    switched off. '''

    class Node2(Node):
        '''Subclass of Node used to check that the validate_nodes
        variable in MyVisitor works correctly.

        '''
        def validate_global_constraints(self):
            raise GenerationError("Fail for testing purposes")

    class Node1(Node):
        '''Subclass of Node used to check that the validate_nodes variable in
        TestVisitor works correctly.

        '''
        @staticmethod
        def _validate_child(_, child):
            return isinstance(child, Node2)

    class MyVisitor(PSyIRVisitor):
        '''Subclass of PSyIRVisitor used to check that the validate_nodes
        variable works as expected.

        '''
        def node1_node(self, node):
            ''' Match with class Node1. '''
            result = "node1\n"
            for child in node.children:
                result += self._visit(child)
            return result

        def node2_node(self, _):
            ''' Match with class Node2. '''
            return "node2\n"

    # Create a simple Node hierarchy with an instance of class
    # Node2 being the child of an instance of class Node1.
    test_node2 = Node2()
    test_node1 = Node1(children=[test_node2])
    # Visit the node hierarchy with node validation disabled
    test_visitor = MyVisitor(check_global_constraints=False)
    output = test_visitor(test_node1)
    assert output == "node1\nnode2\n"
    # Repeat but with tree validation enabled - this should raise an exception
    test_visitor = MyVisitor(check_global_constraints=True)
    with pytest.raises(GenerationError) as err:
        _ = test_visitor(test_node1)
    assert "Fail for testing purposes" in str(err.value)


def test_psyirvisitor_visit_return_node():
    '''Check that when a return PSyIR node is found the actual method
    called is 'return_node'. This is done to avoid clashing with the
    Python keyword.

    '''
    return_node = Return()
    test_visitor = PSyIRVisitor()
    with pytest.raises(VisitorError) as excinfo:
        _ = test_visitor(return_node)
    assert ("Visitor Error: Unsupported node 'Return' found: method names "
            "attempted were ['return_node', " in str(excinfo.value))


def test_reference():
    '''Test that the common reference visitor writes the referenced symbol name
    or raises an error if the node is not Leaf node reference.

    '''
    # Generate PSyIR Reference
    test_visitor = PSyIRVisitor()
    reference = Reference(DataSymbol('a', REAL_TYPE))
    result = test_visitor(reference)
    assert result == "a"

    # Generate PSyIR ArrayReference
    reference2 = ArrayReference.create(
        DataSymbol('b', ArrayType(REAL_TYPE, shape=[1])), [reference])
    with pytest.raises(VisitorError) as err:
        result = test_visitor(reference2)
    assert "Expecting a Reference with no children but found: " \
        in str(err.value)
