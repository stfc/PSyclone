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
#           A. R. Porter and N. Nobre, STFC Daresbury Lab


'''Generic PSyIR visitor code that can be specialised by different
back ends.

'''

import inspect

from psyclone.errors import PSycloneError
from psyclone.psyir.nodes import Node
from psyclone.psyir.nodes.commentable_mixin import CommentableMixin


class VisitorError(PSycloneError):
    '''Provides a PSyclone-specific error class for errors related to a
    PSyIR visitor.

    :param str value: Error message.

    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "Visitor Error: "+str(value)


class PSyIRVisitor(object):
    '''A generic PSyIR visitor. This is designed to be specialised by
    a particular back end. By default, global constraints are enforced by
    calling the `validate_global_constraints()` method of each Node visited.

    :param bool skip_nodes: If skip_nodes is False then an exception \
        is raised if a visitor method for a PSyIR node has not been \
        implemented, otherwise the visitor silently continues. This is an \
        optional argument which defaults to False.
    :param str indent_string: Specifies what to use for indentation. This \
        is an optional argument that defaults to two spaces.
    :param int initial_indent_depth: Specifies how much indentation to \
        start with. This is an optional argument that defaults to 0.
    :param bool check_global_constraints: whether or not to validate all \
        global constraints when walking the tree. Defaults to True.

    :raises TypeError: if any of the supplied parameters are of the wrong type.

    '''
    _COMMENT_PREFIX = None

    def __init__(self, skip_nodes=False, indent_string="  ",
                 initial_indent_depth=0, check_global_constraints=True):

        if not isinstance(skip_nodes, bool):
            raise TypeError(
                f"skip_nodes should be a boolean but found "
                f"'{type(skip_nodes).__name__}'.")
        if not isinstance(indent_string, str):
            raise TypeError(
                f"indent_string should be a str but found "
                f"'{type(indent_string).__name__}'.")
        if not isinstance(initial_indent_depth, int):
            raise TypeError(
                f"initial_indent_depth should be an integer but found "
                f"'{type(initial_indent_depth).__name__}'.")
        if initial_indent_depth < 0:
            raise TypeError(
                f"initial_indent_depth should not be negative, but found "
                f"'{initial_indent_depth}'.")
        if not isinstance(check_global_constraints, bool):
            raise TypeError(f"check_global_constraints should be a boolean "
                            f"but found "
                            f"'{type(check_global_constraints).__name__}'.")

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
                f"Expecting a Reference with no children but found: {node}")
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
        directly (like a function). It creates a copy of the whole tree of
        the provided node (in order to return without any side-effects to
        the original tree), then lower the DSL concepts into language level
        nodes, and finally recurse down the node using the visitors defined
        in this Visitor class.

        :param node: A PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: text representation of the PSyIR tree.
        :rtype: str

        :raises TypeError: if the provided argument is not a PSyIR Node.

        '''
        if not isinstance(node, Node):
            raise TypeError(
                f"The PSyIR visitor functor method only accepts a PSyIR Node "
                f"as argument, but found '{type(node).__name__}'.")

        # The visitor must not alter the provided node but if there are any
        # DSL concepts then these will need to be lowered in-place and this
        # operation often modifies the tree. Therefore, we first create a
        # copy of the full provided tree (as modifications can be above the
        # provided node - e.g. adding a symbol in the scope)
        tree_copy = node.root.copy()

        # Get the node in the new tree with equivalent position to the provided
        # node
        node_copy = tree_copy.walk(Node)[node.abs_position]

        # Lower the DSL concepts starting from the selected node.
        # pylint: disable=broad-except
        try:
            node_copy.lower_to_language_level()
        except Exception as err:
            raise VisitorError(
                f"Failed to lower '{node}'. Note that some nodes need to be "
                f"lowered from an ancestor in order to properly apply their "
                f"in-tree modifications.") from err

        # Find again the equivalent node in the lowered tree in case that it
        # has been replaced
        lowered_node = tree_copy.walk(Node)[node.abs_position]

        return self._visit(lowered_node)

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
        # pylint: disable=too-many-branches
        if not isinstance(node, Node):
            raise VisitorError(
                f"Expected argument to be of type 'Node' but found "
                f"'{type(node).__name__}'.")

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
                node_result = eval(f"self.{method_name}(node)")

                # We can only proceed to add comments if the Visitor
                # returned a string, otherwise we just return
                if not isinstance(node_result, str):
                    return node_result

                result = ""

                # Add preceding comment if available
                if isinstance(node, CommentableMixin):
                    if node.preceding_comment and self._COMMENT_PREFIX:
                        result += (self._nindent + self._COMMENT_PREFIX +
                                   node.preceding_comment + "\n")

                result += node_result

                # Add inline comment if available
                if isinstance(node, CommentableMixin):
                    if node.inline_comment and self._COMMENT_PREFIX:
                        if result[-1] != "\n":
                            raise VisitorError(
                                f"An inline_comment can only be added to a "
                                f"construct that finishes with a '\\n', "
                                f"indicating that the line has ended, but"
                                f" node '{node}' results in '{result}'.")
                        # Add the comment before the last line break
                        result = (result[:-1] + "  " + self._COMMENT_PREFIX +
                                  node.inline_comment + "\n")

                return result

            except AttributeError as excinfo:
                if f"attribute '{method_name}'" in str(excinfo):
                    # This attribute error is because the method that
                    # was tried does not exist.
                    pass
                else:
                    # The method does exist but it has raised an
                    # attribute error so re-raise it here.
                    raise AttributeError(excinfo) from excinfo

        if self._skip_nodes:
            # We haven't found a handler for this node but '_skip_nodes' is
            # set so we ignore it and continue on down to any children.
            results = []
            for child in node.children:
                results.append(self._visit(child))
            return "".join(results)

        raise VisitorError(
            f"Unsupported node '{type(node).__name__}' found: method names "
            f"attempted were {possible_method_names}.")


# For AutoAPI documentation generation
__all__ = ['VisitorError', 'PSyIRVisitor']
