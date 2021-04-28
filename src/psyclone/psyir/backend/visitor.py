# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab.
# Modified: J. Henrichs, Bureau of Meteorology
#           A. R. Porter, STFC Daresbury Lab


'''Generic PSyIR visitor code that can be specialised by different
back ends.

'''

import inspect
import six
from psyclone.psyir.nodes import Node


class VisitorError(Exception):
    '''Provides a PSyclone-specific error class for errors related to a
    PSyIR visitor.

    :param str value: Error message.

    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Visitor Error: "+value

    def __str__(self):
        return str(self.value)


class PSyIRVisitor(object):
    '''A generic PSyIR visitor. This is designed to be specialised by
    a particular back end. By default, global constraints are enforced by
    calling the `validate_global_constraints()` method of each Node visited.

    :param bool skip_nodes: If skip_nodes is False then an exception \
        is raised if a visitor method for a PSyIR node has not been \
        implemented, otherwise the visitor silently continues. This is an \
        optional argument which defaults to False.
    :param indent_string: Specifies what to use for indentation. This \
        is an optional argument that defaults to two spaces.
    :type indent_string: str or NoneType
    :param int initial_indent_depth: Specifies how much indentation to \
        start with. This is an optional argument that defaults to 0.
    :param bool check_global_constraints: whether or not to validate all \
        global constraints when walking the tree.

    :raises TypeError: if any of the supplied parameters are of the wrong type.

    '''
    def __init__(self, skip_nodes=False, indent_string="  ",
                 initial_indent_depth=0, check_global_constraints=True):

        if not isinstance(skip_nodes, bool):
            raise TypeError(
                "skip_nodes should be a boolean but found '{0}'."
                "".format(type(skip_nodes).__name__))
        if indent_string is not None and not isinstance(indent_string, str):
            raise TypeError(
                "indent_string should be a str but found '{0}'."
                "".format(type(indent_string).__name__))
        if not isinstance(initial_indent_depth, int):
            raise TypeError(
                "initial_indent_depth should be an integer but found '{0}'."
                "".format(type(initial_indent_depth).__name__))
        if initial_indent_depth < 0:
            raise TypeError(
                "initial_indent_depth should not be negative, but found '{0}'."
                "".format(initial_indent_depth))
        if not isinstance(check_global_constraints, bool):
            raise TypeError("check_global_constraints should be a boolean but "
                            "found '{0}'.".format(
                                type(check_global_constraints).__name__))

        self._skip_nodes = skip_nodes
        self._indent = indent_string
        self._depth = initial_indent_depth
        #: If validate_nodes is True then each node visited will have any
        #: global constraints validated.
        self._validate_nodes = check_global_constraints

    def reference_node(self, node):
        # pylint: disable=no-self-use
        '''This method is called when a Reference instance is found in the
        PSyIR tree.

        :param node: a Reference PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`

        :returns: the text representation of this reference.
        :rtype: str

        :raises VisitorError: if this node has children.

        '''
        if node.children:
            raise VisitorError(
                "Expecting a Reference with no children but found: {0}"
                "".format(str(node)))
        return node.name

    @property
    def _nindent(self):
        '''
        :returns: the current indentation string.
        :rtype: str

        '''
        return self._depth * self._indent

    def __call__(self, node):
        '''This method is called when an instance of the class is called
        directly (like a function). This implementation is known as
        a functor. It makes sense for this class as there is only one
        main method - the `visit` method.

        :param node: A PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: text representation of the PSyIR tree.
        :rtype: str

        '''
        return self._visit(node)

    def _visit(self, node):
        '''Implements the PSyIR callbacks. Callbacks are implemented by using
        the class hierarchy names of the object in the PSyIR tree as
        the candidate method names. The class name of the object is
        tried first, then the class name of its parent, and so on
        until there are no more parent classes. Names are not
        modified, other than making them lower case, apart from the
        `Return` class which is changed to `return_node` because
        `return` is a Python keyword.

        :param node: A PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: text representation of the PSyIR node sub-tree.
        :rtype: str

        :raises VisitorError: if a node is found that does not have \
            associated call back methods (and skip_nodes is not set).
        :raises AttributeError: if a call back method is found but it \
            raises an AttributeError.

        '''
        if not isinstance(node, Node):
            raise VisitorError(
                "Expected argument to be of type 'Node' but found '{0}'."
                "".format(type(node).__name__))

        # Check global constraints for this node (if validation enabled).
        if self._validate_nodes:
            node.validate_global_constraints()

        # Make a list of the node's ancestor classes (including
        # itself) in method resolution order (mro), apart from the
        # base "object" class.
        possible_method_names = [curr_class.__name__.lower()+"_node"
                                 for curr_class in inspect.getmro(type(node))]
        possible_method_names.remove("object_node")

        # Try to call methods using the class names in the order of
        # the class hierarchy (starting from the current class name).
        for method_name in possible_method_names:
            try:
                # pylint: disable=eval-used
                return eval("self.{0}(node)".format(method_name))

            except AttributeError as excinfo:
                if "attribute '{0}'".format(method_name) in str(excinfo):
                    # This attribute error is because the method that
                    # was tried does not exist.
                    pass
                else:
                    # The method does exist but it has raised an
                    # attribute error so re-raise it here.
                    six.raise_from(AttributeError(excinfo), excinfo)

        if self._skip_nodes:
            # We haven't found a handler for this node but '_skip_nodes' is
            # set so we ignore it and continue on down to any children.
            results = []
            for child in node.children:
                results.append(self._visit(child))
            return "".join(results)

        raise VisitorError(
            "Unsupported node '{0}' found: method names attempted were "
            "{1}.".format(type(node).__name__, str(possible_method_names)))


# For AutoAPI documentation generation
__all__ = ['VisitorError', 'PSyIRVisitor']
