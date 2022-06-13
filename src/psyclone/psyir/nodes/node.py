# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Modified A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''
This module contains the abstract Node implementation.

'''
import copy

from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.symbols import SymbolError

# We use the termcolor module (if available) to enable us to produce
# coloured, textual representations of Invoke schedules. If it's not
# available then we don't use colour.
try:
    # pylint disable=import-outside-toplevel
    from termcolor import colored
except ImportError:
    # We don't have the termcolor package available so provide
    # alternative routine
    def colored(text, _):
        '''
        Returns the supplied text argument unchanged. This is a swap-in
        replacement for when termcolor.colored is not available.

        :param str text: text to return.
        :param _: fake argument, only required to match interface \
                  provided by termcolor.colored.

        :returns: the supplied text, unchanged.
        :rtype: str
        '''
        return text


def _graphviz_digraph_class():
    '''
    Wrapper that returns the graphviz Digraph type if graphviz is installed
    and None otherwise.

    :returns: the graphviz Digraph type or None.
    :rtype: :py:class:`graphviz.Digraph` or NoneType.

    '''
    try:
        # pylint: disable=import-outside-toplevel
        import graphviz as gv
        return gv.Digraph
    except ImportError:
        # TODO #11 add a warning to a log file here
        # silently return if graphviz bindings are not installed
        return None


class ChildrenList(list):
    '''
    Customized list to keep track of the children nodes. It is initialised with
    a callback function that allows the validation of the inserted children.
    Since this is a subclass of the standard list, all operations (e.g. append,
    insert, extend, comparisons, list arithmetic operations) are conserved and
    making use of the validation.

    :param node: reference to the node where the list belongs.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    :param validation_function: callback function to the validation method.
    :type validation_function: \
            function(int, :py:class:`psyclone.psyir.nodes.Node`)
    :param str validation_text: textual representation of the valid children.

    '''
    def __init__(self, node, validation_function, validation_text):
        super(ChildrenList, self).__init__()
        self._node_reference = node
        self._validation_function = validation_function
        self._validation_text = validation_text

    def _validate_item(self, index, item):
        '''
        Validates the provided index and item before continuing inserting the
        item into the list.

        :param int index: position where the item is inserted into.
        :param item: object that needs to be validated in the given position.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        :raises GenerationError: if the given index and item are not valid \
            children for this list.
        '''
        if not self._validation_function(index, item):
            errmsg = (f"Item '{item.__class__.__name__}' can't be child "
                      f"{index} of "
                      f"'{self._node_reference.coloured_name(False)}'.")

            if self._validation_text == "<LeafNode>":
                errmsg = (f"{errmsg} "
                          f"{self._node_reference.coloured_name(False)} is a "
                          f"LeafNode and doesn't accept children.")
            else:
                errmsg = (f"{errmsg} The valid format is: "
                          f"'{self._validation_text}'.")

            raise GenerationError(errmsg)

    def _check_is_orphan(self, item):
        '''
        Checks that the provided item is an orphan (has no parent or the parent
        was predefined in the constructor but the other end of connection has
        not finalised until now).

        :param item: object that needs to be validated.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        :raises GenerationError: if the given item is not an orphan.
        :raises GenerationError: if the item had been provided with a parent \
            argument on its constructor and this operation is trying to \
            make its parent a different node.

        '''
        if item.parent and not item.has_constructor_parent:
            raise GenerationError(
                f"Item '{item.coloured_name(False)}' can't be added as child "
                f"of '{self._node_reference.coloured_name(False)}' because "
                f"it is not an orphan. It already has a "
                f"'{item.parent.coloured_name(False)}' as a parent.")

        if item.parent and item.has_constructor_parent:
            if item.parent is not self._node_reference:
                raise GenerationError(
                    f"'{self._node_reference.coloured_name(False)}' cannot be "
                    f"set as parent of '{item.coloured_name(False)}' because "
                    f"its constructor predefined the parent reference to a "
                    f"different '{item.parent.coloured_name(False)}' node.")

    def _set_parent_link(self, node):
        '''
        Set parent connection of the given node to this ChildrenList's node.

        :param node: node for which the parent connection need to be updated.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # pylint: disable=protected-access
        node._parent = self._node_reference
        node._has_constructor_parent = False

    @staticmethod
    def _del_parent_link(node):
        '''
        Delete parent connection of the given node.

        :param node: node for which the parent connection need to be updated.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # This is done from here with protected access because it's the parent
        # which is in charge of maintaining its children connections.
        # pylint: disable=protected-access
        node._parent = None
        node._has_constructor_parent = False

    def append(self, item):
        ''' Extends list append method with children node validation.

        :param item: item to be appened to the list.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._validate_item(len(self), item)
        self._check_is_orphan(item)
        super(ChildrenList, self).append(item)
        self._set_parent_link(item)

    def __setitem__(self, index, item):
        ''' Extends list __setitem__ method with children node validation.

        :param int index: position where to insert the item.
        :param item: item to be inserted to the list.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._validate_item(index, item)
        self._check_is_orphan(item)
        self._del_parent_link(self[index])
        super(ChildrenList, self).__setitem__(index, item)
        self._set_parent_link(item)

    def insert(self, index, item):
        ''' Extends list insert method with children node validation.

        :param int index: position where to insert the item.
        :param item: item to be inserted to the list.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        '''
        positiveindex = index if index >= 0 else len(self) - index
        self._validate_item(positiveindex, item)
        self._check_is_orphan(item)
        # Check that all displaced items will still in valid positions
        for position in range(positiveindex, len(self)):
            self._validate_item(position + 1, self[position])
        super(ChildrenList, self).insert(index, item)
        self._set_parent_link(item)

    def extend(self, items):
        ''' Extends list extend method with children node validation.

        :param items: list of items to be appened to the list.
        :type items: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        for index, item in enumerate(items):
            self._validate_item(len(self) + index, item)
            self._check_is_orphan(item)
        super(ChildrenList, self).extend(items)
        for item in items:
            self._set_parent_link(item)

    # Methods below don't insert elements but have the potential to displace
    # or change the order of the items in-place.
    def __delitem__(self, index):
        ''' Extends list __delitem__ method with children node validation.

        :param int index: position where to insert the item.

        '''
        positiveindex = index if index >= 0 else len(self) - index
        for position in range(positiveindex + 1, len(self)):
            self._validate_item(position - 1, self[position])
        self._del_parent_link(self[index])
        super(ChildrenList, self).__delitem__(index)

    def remove(self, item):
        ''' Extends list remove method with children node validation.

        :param item: item to be deleted the list.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        '''
        for position in range(self.index(item) + 1, len(self)):
            self._validate_item(position - 1, self[position])
        self._del_parent_link(item)
        super(ChildrenList, self).remove(item)

    def pop(self, index=-1):
        ''' Extends list pop method with children node validation.

        :param int index: position of the item that is popped out, if not \
            given, the last element is popped out.

        :returns: the last value or the given index value from the list.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        positiveindex = index if index >= 0 else len(self) - index
        # Check if displaced items after 'positiveindex' will still be valid
        for position in range(positiveindex + 1, len(self)):
            self._validate_item(position - 1, self[position])
        self._del_parent_link(self[index])
        return super(ChildrenList, self).pop(index)

    def reverse(self):
        ''' Extends list reverse method with children node validation. '''
        for index, item in enumerate(self):
            self._validate_item(len(self) - index - 1, item)
        super(ChildrenList, self).reverse()


class Node(object):
    '''
    Base class for a PSyIR node.

    :param ast: reference into the fparser2 AST corresponding to this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: that parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param annotations: Tags that provide additional information about \
        the node. The node should still be functionally correct when \
        ignoring these tags.
    :type annotations: list of str

    :raises TypeError: if a parent is supplied that is not an instance of Node.
    :raises InternalError: if an invalid annotation tag is supplied.

    '''
    # pylint: disable=too-many-public-methods
    # Define two class constants: START_DEPTH and START_POSITION
    # START_DEPTH is used to calculate depth of all Nodes in the tree
    # (1 for main Nodes and increasing for their descendants).
    START_DEPTH = 0
    # START_POSITION is used to to calculate position of all Nodes in
    # the tree (absolute or relative to a parent).
    START_POSITION = 0
    # The list of valid annotations for this Node. Populated by sub-class.
    valid_annotations = tuple()
    # Textual description of the node. (Set up with None since this is
    # an abstract class, subclasses need to initialize them with strings.)
    # In python >= 3 this can be better implemented by creating @classmethod
    # properties for each of them and chain the ABC @abstractmethod annotation.
    _children_valid_format = None
    _text_name = None
    _colour = None

    def __init__(self, ast=None, children=None, parent=None, annotations=None):
        self._children = ChildrenList(self, self._validate_child,
                                      self._children_valid_format)
        if children:
            self._children.extend(children)
        if parent and not isinstance(parent, Node):
            raise TypeError(f"The parent of a Node must also be a Node but "
                            f"got '{type(parent).__name__}'")
        # Keep a record of whether a parent node was supplied when constructing
        # this object. In this case it still won't appear in the parent's
        # children list. When both ends of the reference are connected this
        # will become False.
        self._has_constructor_parent = parent is not None
        self._parent = parent
        # Reference into fparser2 AST (if any)
        self._ast = ast
        # Ref. to last fparser2 parse tree node associated with this Node.
        # This is required when adding directives.
        self._ast_end = None
        # List of tags that provide additional information about this Node.
        self._annotations = []
        if annotations:
            for annotation in annotations:
                if annotation in self.valid_annotations:
                    self._annotations.append(annotation)
                else:
                    raise InternalError(
                        f"{self.__class__.__name__} with unrecognised "
                        f"annotation '{annotation}', valid "
                        f"annotations are: {self.valid_annotations}.")

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. The basic implementation of this
        checks whether the nodes are the same type, and whether all children
        of the nodes are equal, and if so then
        they are considered equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        super().__eq__(other)
        is_eq = type(self) is type(other)
        is_eq = is_eq and (len(self.children) == len(other.children))
        for index, child in enumerate(self.children):
            is_eq = is_eq and child == other.children[index]

        return is_eq

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         The generic implementation always returns False, this simplifies the
         specializations as Leaf nodes will have by default the expected
         behaviour, and non-leaf nodes need to modify this method to its
         particular constrains anyway. Issue #765 explores if this method
         can be auto-generated using the _children_valid_format string.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # pylint: disable=unused-argument
        # Position and child argument names are kept for better documentation,
        # but the generic method always returns False.
        return False

    def coloured_name(self, colour=True):
        '''
        Returns the display name of this Node, optionally with colour control
        codes (requires that the termcolor package be installed).

        :param bool colour: whether or not to include colour control codes \
                            in the result.

        :returns: the name of this node, optionally with colour control codes.
        :rtype: str
        '''
        if not self._text_name:
            name_string = type(self).__name__
        else:
            name_string = self._text_name
        if colour:
            if self._colour is None:
                raise NotImplementedError(
                    f"The _colour attribute is abstract so needs to be given "
                    f"a string value in the concrete class "
                    f"'{type(self).__name__}'.")
            try:
                return colored(name_string, self._colour)
            except KeyError as info:
                raise InternalError(
                    f"The _colour attribute in class '{type(self).__name__}' "
                    f"has been set to a colour ('{self._colour}') that is not "
                    f"supported by the termcolor package.") from info
        return name_string

    def node_str(self, colour=True):
        '''
        :param bool colour: whether or not to include control codes for \
                            coloured text.

        :returns: a text description of this node. Will typically be \
                  overridden by sub-class.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[]"

    def __str__(self):
        return self.node_str(False)

    @property
    def ast(self):
        '''
        :returns: a reference to that part of the fparser2 parse tree that \
                  this node represents or None.
        :rtype: sub-class of :py:class:`fparser.two.utils.Base`
        '''
        return self._ast

    @property
    def ast_end(self):
        '''
        :returns: a reference to the last node in the fparser2 parse tree \
                  that represents a child of this PSyIR node or None.
        :rtype: sub-class of :py:class:`fparser.two.utils.Base`
        '''
        return self._ast_end

    @ast.setter
    def ast(self, ast):
        '''
        Set a reference to the fparser2 node associated with this Node.

        :param ast: fparser2 node associated with this Node.
        :type ast: :py:class:`fparser.two.utils.Base`
        '''
        self._ast = ast

    @ast_end.setter
    def ast_end(self, ast_end):
        '''
        Set a reference to the last fparser2 node associated with this Node.

        :param ast: last fparser2 node associated with this Node.
        :type ast: :py:class:`fparser.two.utils.Base`
        '''
        self._ast_end = ast_end

    @property
    def annotations(self):
        ''' Return the list of annotations attached to this Node.

        :returns: List of anotations
        :rtype: list of str
        '''
        return self._annotations

    def dag(self, file_name='dag', file_format='svg'):
        '''
        Create a dag of this node and its children, write it to file and
        return the graph object.

        :param str file_name: name of the file to create.
        :param str file_format: format of the file to create. (Must be one \
                                recognised by Graphviz.)

        :returns: the graph object or None (if the graphviz bindings are not \
                  installed).
        :rtype: :py:class:`graphviz.Digraph` or NoneType

        :raises GenerationError: if the specified file format is not \
                                 recognised by Graphviz.

        '''
        digraph = _graphviz_digraph_class()
        if digraph is None:
            return None
        try:
            graph = digraph(format=file_format)
        except ValueError as err:
            raise GenerationError(f"unsupported graphviz file format "
                                  f"'{file_format}' provided") from err
        self.dag_gen(graph)
        graph.render(filename=file_name)
        return graph

    def dag_gen(self, graph):
        '''Output my node's graph (dag) information and call any
        children. Nodes with children are represented as two vertices,
        a start and an end. Forward dependencies are represented as
        green edges, backward dependencies are represented as red
        edges (but their direction is reversed so the layout looks
        reasonable) and parent child dependencies are represented as
        blue edges.'''
        # Import outside top-level to avoid circular dependencies.
        # pylint: disable=too-many-branches, import-outside-toplevel
        from psyclone.psyir.nodes.loop import Loop
        from psyclone.psyir.nodes.routine import Routine

        # names to append to my default name to create start and end vertices
        start_postfix = "_start"
        end_postfix = "_end"
        if self.children:
            # I am represented by two vertices, a start and an end
            graph.node(self.dag_name+start_postfix)
            graph.node(self.dag_name+end_postfix)
        else:
            # I am represented by a single vertex
            graph.node(self.dag_name)
        # first deal with forward dependencies
        remote_node = self.forward_dependence()
        local_name = self.dag_name
        if self.children:
            # edge will come from my end vertex as I am a forward dependence
            local_name += end_postfix
        if remote_node:
            # this node has a forward dependence
            remote_name = remote_node.dag_name
            if remote_node.children:
                # the remote node has children so I will connect to
                # its start vertex
                remote_name += start_postfix
            # Create the forward dependence edge in green
            graph.edge(local_name, remote_name, color="green")
        elif not isinstance(self, Routine):
            # If this node is not a Routine (where the DAG context finishes)
            # and has no forward dependence, connect it to the end vertex
            # of its parent. Use blue to indicate a parent child
            # relationship.
            remote_name = self.parent.dag_name + end_postfix
            graph.edge(local_name, remote_name, color="blue")
        # now deal with backward dependencies. When creating the edges
        # we reverse the direction of the dependence (place
        # remote_node before local_node) to help with the graph
        # layout
        remote_node = self.backward_dependence()
        local_name = self.dag_name
        if self.children:
            # the edge will come from my start vertex as I am a
            # backward dependence
            local_name += start_postfix
        if remote_node:
            # this node has a backward dependence.
            remote_name = remote_node.dag_name
            if remote_node.children:
                # the remote node has children so I will connect to
                # its end vertex
                remote_name += end_postfix
            # Create the backward dependence edge in red.
            graph.edge(remote_name, local_name, color="red")
        elif not isinstance(self, Routine):
            # If this node is not a Routine (where the DAG context finishes)
            # and has no backward dependence, connect it to the start vertex
            # of its parent. Use blue to indicate a parent child
            # relationship.
            remote_name = self.parent.dag_name + start_postfix
            graph.edge(remote_name, local_name, color="blue")
        # now call any children so they can add their information to
        # the graph
        if isinstance(self, Loop):
            # In case of a loop only look at the body (the other part
            # of the tree contain start, stop, step values):
            self.loop_body.dag_gen(graph)
        else:
            for child in self.children:
                child.dag_gen(graph)

    @property
    def dag_name(self):
        '''Return the dag name for this node. This includes the name of the
        class and the index of its relative position to the parent Routine. If
        no parent Routine is found, the index used is the absolute position
        in the tree.

        :returns: the dag name for this node.
        :rtype: str

        '''
        # Import here to avoid circular dependencies
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Routine
        if self.ancestor(Routine):
            _, position = self._find_position(self.ancestor(Routine))
        else:
            position = self.abs_position
        return self.coloured_name(False) + "_" + str(position)

    @property
    def args(self):
        '''Return the list of arguments associated with this Node. The default
        implementation assumes the Node has no directly associated
        arguments (i.e. is not a Kern class or subclass). Arguments of
        any of this nodes descendants are considered to be
        associated. '''
        args = []
        for call in self.kernels():
            args.extend(call.args)
        return args

    def backward_dependence(self):
        '''Returns the closest preceding Node that this Node has a direct
        dependence with or None if there is not one. Only Nodes with
        the same parent as self are returned. Nodes inherit their
        descendants' dependencies. The reason for this is that for
        correctness a node must maintain its parent if it is
        moved. For example a halo exchange and a kernel call may have
        a dependence between them but it is the loop body containing
        the kernel call that the halo exchange must not move beyond
        i.e. the loop body inherits the dependencies of the routines
        within it.'''
        dependence = None
        # look through all the backward dependencies of my arguments
        for arg in self.args:
            dependent_arg = arg.backward_dependence()
            if dependent_arg:
                # this argument has a backward dependence
                node = dependent_arg.call
                # if the remote node is deeper in the tree than me
                # then find the ancestor that is at the same level of
                # the tree as me.
                while node.depth > self.depth:
                    node = node.parent
                if self.sameParent(node) and node is not self:
                    # The remote node (or one of its ancestors) shares
                    # the same parent as me (but its not me)
                    if not dependence:
                        # this is the first dependence found so keep it
                        dependence = node
                    else:
                        # we have already found a dependence
                        if dependence.position < node.position:
                            # the new dependence is closer to me than
                            # the previous dependence so keep it
                            dependence = node
        return dependence

    def forward_dependence(self):
        '''Returns the closest following Node that this Node has a direct
        dependence with or None if there is not one. Only Nodes with
        the same parent as self are returned. Nodes inherit their
        descendants' dependencies. The reason for this is that for
        correctness a node must maintain its parent if it is
        moved. For example a halo exchange and a kernel call may have
        a dependence between them but it is the loop body containing
        the kernel call that the halo exchange must not move beyond
        i.e. the loop body inherits the dependencies of the routines
        within it.'''
        dependence = None
        # look through all the forward dependencies of my arguments
        for arg in self.args:
            dependent_arg = arg.forward_dependence()
            if dependent_arg:
                # this argument has a forward dependence
                node = dependent_arg.call
                # if the remote node is deeper in the tree than me
                # then find the ancestor that is at the same level of
                # the tree as me.
                while node.depth > self.depth:
                    node = node.parent
                if self.sameParent(node) and node is not self:
                    # The remote node (or one of its ancestors) shares
                    # the same parent as me (but its not me)
                    if not dependence:
                        # this is the first dependence found so keep it
                        dependence = node
                    else:
                        if dependence.position > node.position:
                            # the new dependence is closer to me than
                            # the previous dependence so keep it
                            dependence = node
        return dependence

    def is_valid_location(self, new_node, position="before"):
        '''If this Node can be moved to the new_node
        (where position determines whether it is before of after the
        new_node) without breaking any data dependencies then return True,
        otherwise return False.

        :param new_node: Node to which this node should be moved.
        :type new_node: :py:class:`psyclone.psyir.nodes.Node`
        :param str position: either 'before' or 'after'.

        :raises GenerationError: if new_node is not an\
                instance of :py:class:`psyclone.psyir.nodes.Node`.
        :raises GenerationError: if position is not 'before' or 'after'.
        :raises GenerationError: if self and new_node do not have the same\
                parent.
        :raises GenerationError: self and new_node are the same Node.

        :returns: whether or not the specified location is valid for this node.
        :rtype: bool

        '''
        # First perform correctness checks
        # 1: check new_node is a Node
        if not isinstance(new_node, Node):
            raise GenerationError(
                f"In the psyir.nodes.Node.is_valid_location() method the "
                f"supplied argument is not a Node, it is a "
                f"'{type(new_node).__name__}'.")

        # 2: check position has a valid value
        valid_positions = ["before", "after"]
        if position not in valid_positions:
            raise GenerationError(
                f"The position argument in the psyGenNode.is_valid_location() "
                f"method must be one of {valid_positions} but found "
                f"'{position}'")

        # 3: check self and new_node have the same parent
        if not self.sameParent(new_node):
            raise GenerationError(
                "In the psyir.nodes.Node.is_valid_location() method "
                "the node and the location do not have the same parent")

        # 4: check proposed new position is not the same as current position
        new_position = new_node.position
        if new_position < self.position and position == "after":
            new_position += 1
        elif new_position > self.position and position == "before":
            new_position -= 1

        if self.position == new_position:
            raise GenerationError(
                "In the psyir.nodes.Node.is_valid_location() method, the "
                "node and the location are the same so this transformation "
                "would have no effect.")

        # Now determine whether the new location is valid in terms of
        # data dependencies
        # Treat forward and backward dependencies separately
        if new_position < self.position:
            # the new_node is before this node in the schedule
            prev_dep_node = self.backward_dependence()
            if not prev_dep_node:
                # There are no backward dependencies so the move is valid
                return True
            # return (is the dependent node before the new_position?)
            return prev_dep_node.position < new_position
        # new_node.position > self.position
        # the new_node is after this node in the schedule
        next_dep_node = self.forward_dependence()
        if not next_dep_node:
            # There are no forward dependencies so the move is valid
            return True
        # return (is the dependent node after the new_position?)
        return next_dep_node.position > new_position

    @property
    def depth(self):
        '''
        Returns this Node's depth in the tree: 1 for the Schedule
        and increasing for its descendants at each level.
        :returns: depth of the Node in the tree
        :rtype: int
        '''
        my_depth = self.START_DEPTH
        node = self
        while node is not None:
            node = node.parent
            my_depth += 1
        return my_depth

    def view(self, depth=0, colour=True, indent="    ", _index=None):
        '''Output a human readable description of the current node and all of
        its descendents as a string.

        :param int depth: depth of the tree hierarchy for output \
            text. Defaults to 0.
        :param bool colour: whether to include colour coding in the \
            output. Defaults to True.
        :param str indent: the indent to apply as the depth \
            increases. Defaults to 4 spaces.
        :param int _index: the position of this node wrt its siblings \
            or None. Defaults to None.

        :returns: a representation of this node and its descendents.
        :rtype: str

        :raises TypeError: if one of the arguments is the wrong type.
        :raises ValueError: if the depth argument is negative.

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Schedule

        if not isinstance(depth, int):
            raise TypeError(
                f"depth argument should be an int but found "
                f"{type(depth).__name__}.")
        if depth < 0:
            raise ValueError(
                f"depth argument should be a positive integer but "
                f"found {depth}.")
        if not isinstance(colour, bool):
            raise TypeError(
                f"colour argument should be a bool but found "
                f"{type(colour).__name__}.")
        if not isinstance(indent, str):
            raise TypeError(
                f"indent argument should be a str but found "
                f"{type(indent).__name__}.")

        full_indent = depth*indent
        description = self.node_str(colour=colour)
        if not isinstance(self.parent, Schedule) or _index is None:
            result = f"{full_indent}{description}\n"
        else:
            result = f"{full_indent}{_index}: {description}\n"
        children_result_list = []
        for idx, node in enumerate(self._children):
            children_result_list.append(
                node.view(
                    depth=depth + 1, _index=idx, colour=colour, indent=indent))
        result = result + "".join(children_result_list)
        return result

    def addchild(self, child, index=None):
        '''
        Adds the supplied node as a child of this node (at position index if
        supplied). The supplied node must not have an existing parent.

        :param child: the node to add as a child of this one.
        :type child: :py:class:`psyclone.psyir.nodes.Node`
        :param index: optional position at which to insert new child. Default \
                      is to append new child to the list of existing children.
        :type index: Optional[int]

        '''
        if index is not None:
            self._children.insert(index, child)
        else:
            self._children.append(child)

    @property
    def children(self):
        '''
        :returns: the immediate children of this Node.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]
        '''
        return self._children

    @children.setter
    def children(self, my_children):
        ''' Set a new children list.

        :param my_children: new list of children.
        :type my_children: list

        :raises TypeError: if the given children parameter is not a list.
        '''
        if isinstance(my_children, list):
            self.pop_all_children()  # First remove existing children if any
            self._children = ChildrenList(self, self._validate_child,
                                          self._children_valid_format)
            self._children.extend(my_children)
        else:
            raise TypeError("The 'my_children' parameter of the node.children"
                            " setter must be a list.")

    @property
    def parent(self):
        '''
        :returns: the parent node.
        :rtype: :py:class:`psyclone.psyir.nodes.Node` or NoneType
        '''
        return self._parent

    @property
    def has_constructor_parent(self):
        '''
        :returns: whether the constructor has predefined a parent connection
            but the parent's children list doesn't include this node yet.
        :rtype: bool
        '''
        return self._has_constructor_parent

    @property
    def position(self):
        '''
        Find a Node's position relative to its parent Node (starting
        with 0 if it does not have a parent).

        :returns: relative position of a Node to its parent
        :rtype: int
        '''
        if self.parent is None:
            return self.START_POSITION
        for index, child in enumerate(self.parent.children):
            if child is self:
                return index

    @property
    def abs_position(self):
        '''
        Find a Node's absolute position in the tree (starting with 0 if
        it is the root). Needs to be computed dynamically from the
        starting position (0) as its position may change.

        :returns: absolute position of a Node in the tree.
        :rtype: int

        :raises InternalError: if the absolute position cannot be found.

        '''
        if self.root is self:
            return self.START_POSITION
        found, position = self._find_position(self.root.children,
                                              self.START_POSITION)
        if not found:
            raise InternalError("Error in search for Node position "
                                "in the tree")
        return position

    def _find_position(self, children, position=None):
        '''
        Recurse through the tree depth first returning position of
        a Node if found.

        :param children: list of Nodes which are children of this Node.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int position: start counting from this position. Defaults to \
            START_POSITION.

        :returns: if the self has been found in the provided children list \
            and its relative position.
        :rtype: bool, int

        :raises InternalError: if the starting position is < 0.

        '''
        if position is None:
            position = self.START_POSITION
        elif position < self.START_POSITION:
            raise InternalError(
                f"Search for Node position started from {position} "
                f"instead of {self.START_POSITION}.")
        for child in children:
            position += 1
            if child is self:
                return True, position
            if child.children:
                found, position = self._find_position(child.children, position)
                if found:
                    return True, position
        return False, position

    @property
    def root(self):
        '''
        :returns: the root node of the PSyIR tree.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # Starting with 'self.parent' instead of 'node = self' avoids many
        # false positive pylint issues that assume self.root type would be
        # the same as self type.
        if self.parent is None:
            return self
        node = self.parent
        while node.parent is not None:
            node = node.parent
        return node

    def sameParent(self, node_2):
        '''
        :returns: True if `node_2` has the same parent as this node, False \
                  otherwise.
        :rtype: bool
        '''
        if self.parent is None or node_2.parent is None:
            return False
        return self.parent is node_2.parent

    def walk(self, my_type, stop_type=None):
        ''' Recurse through the PSyIR tree and return all objects that are
        an instance of 'my_type', which is either a single class or a tuple
        of classes. In the latter case all nodes are returned that are
        instances of any classes in the tuple. The recursion into the tree
        is stopped if an instance of 'stop_type' (which is either a single
        class or a tuple of classes) is found. This can be used to avoid
        analysing e.g. inlined kernels, or as performance optimisation to
        reduce the number of recursive calls.

        :param my_type: the class(es) for which the instances are collected.
        :type my_type: type | Tuple[type, ...]
        :param stop_type: class(es) at which recursion is halted (optional).
        :type stop_type: Optional[type | Tuple[type, ...]]

        :returns: list with all nodes that are instances of my_type \
                  starting at and including this node.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

        '''
        local_list = []
        if isinstance(self, my_type):
            local_list.append(self)

        # Stop recursion further into the tree if an instance of a class
        # listed in stop_type is found.
        if stop_type and isinstance(self, stop_type):
            return local_list
        for child in self.children:
            local_list += child.walk(my_type, stop_type)
        return local_list

    def ancestor(self, my_type, excluding=None, include_self=False,
                 limit=None):
        '''
        Search back up the tree and check whether this node has an ancestor
        that is an instance of the supplied type. If it does then we return
        it otherwise we return None. An individual (or tuple of) (sub-)
        class(es) to ignore may be provided via the `excluding` argument. If
        `include_self` is True then the current node is included in the search.
        If `limit` is provided then the search ceases if/when the supplied
        node is encountered.

        :param my_type: class(es) to search for.
        :type my_type: type | Tuple[type, ...]
        :param excluding: (sub-)class(es) to ignore or None.
        :type excluding: Optional[type | Tuple[type, ...]]
        :param bool include_self: whether or not to include this node in the \
                                  search.
        :param limit: an optional node at which to stop the search.
        :type limit: Optional[:py:class:`psyclone.psyir.nodes.Node`]

        :returns: First ancestor Node that is an instance of any of the \
                  requested classes or None if not found.
        :rtype: Optional[:py:class:`psyclone.psyir.nodes.Node`]

        :raises TypeError: if `excluding` is provided but is not a type or \
                           tuple of types.
        :raises TypeError: if `limit` is provided but is not an instance \
                           of Node.
        '''
        if include_self:
            myparent = self
        else:
            myparent = self.parent

        if excluding is not None:
            if isinstance(excluding, type):
                excludes = (excluding, )
            elif isinstance(excluding, tuple):
                excludes = excluding
            else:
                raise TypeError(
                    f"The 'excluding' argument to ancestor() must be a type or"
                    f" a tuple of types but got: '{type(excluding).__name__}'")

        if limit and not isinstance(limit, Node):
            raise TypeError(
                f"The 'limit' argument to ancestor() must be an instance of "
                f"Node but got '{type(limit).__name__}'")

        while myparent not in [None, limit]:
            if isinstance(myparent, my_type):
                if not (excluding and isinstance(myparent, excludes)):
                    # This parent node is not an instance of an excluded
                    # sub-class so return it
                    return myparent
            myparent = myparent.parent
        return None

    def kernels(self):
        '''
        :returns: all kernels that are descendants of this node in the PSyIR.
        :rtype: List[:py:class:`psyclone.psyGen.Kern`]
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import Kern
        return self.walk(Kern)

    def following(self, routine=True):
        '''Return all :py:class:`psyclone.psyir.nodes.Node` nodes after this
        node. Ordering is depth first. If the `routine` argument is
        set to `True` then nodes are only returned if they are
        descendents of this node's closest ancestor routine if one
        exists.

        :param bool routine: an optional (default `True`) argument \
            that only returns nodes that are within this node's \
            closest ancestor Routine node if one exists.

        :returns: a list of nodes.
        :rtype: :func:`list` of :py:class:`psyclone.psyir.nodes.Node`

        '''
        root = self.root
        if routine:
            # Import here to avoid circular dependencies
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.nodes import Routine
            # If there is an ancestor Routine node then only return nodes
            # that are within it.
            routine_node = self.ancestor(Routine)
            if routine_node:
                root = routine_node
        all_nodes = root.walk(Node)
        position = None
        for index, node in enumerate(all_nodes):
            if node is self:
                position = index
                break

        return all_nodes[position+1:]

    def preceding(self, reverse=False, routine=True):
        '''Return all :py:class:`psyclone.psyir.nodes.Node` nodes before this
        node. Ordering is depth first. If the `reverse` argument is
        set to `True` then the node ordering is reversed
        i.e. returning the nodes closest to this node first. if the
        `routine` argument is set to `True` then nodes are only
        returned if they are descendents of this node's closest
        ancestor routine if one exists.

        :param bool reverse: an optional (default `False`) argument \
            that reverses the order of any returned nodes (i.e. makes \
            them 'closest first' if set to true.
        :param bool routine: an optional (default `True`) argument \
            that only returns nodes that are within this node's \
            closest ancestor Routine node if one exists.

        :returns: a list of nodes.
        :rtype: :func:`list` of :py:class:`psyclone.psyir.nodes.Node`

        '''
        root = self.root
        if routine:
            # Import here to avoid circular dependencies
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.nodes import Routine
            # If there is an ancestor Routine node then only return nodes
            # that are within it.
            routine_node = self.ancestor(Routine)
            if routine_node:
                root = routine_node
        all_nodes = root.walk(Node)
        position = None
        for index, node in enumerate(all_nodes):
            if node is self:
                position = index
                break
        nodes = all_nodes[:position]
        if reverse:
            nodes.reverse()
        return nodes

    def coded_kernels(self):
        '''
        Returns a list of all of the user-supplied kernels (as opposed to
        builtins) that are beneath this node in the PSyIR.

        :returns: all user-supplied kernel calls below this node.
        :rtype: List[:py:class:`psyclone.psyGen.CodedKern`]

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import CodedKern
        return self.walk(CodedKern)

    def loops(self):
        '''
        :returns: all loops currently in this schedule.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Loop`]
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Loop
        return self.walk(Loop)

    def reductions(self, reprod=None):
        '''
        Return all kernels that have reductions and are decendents of this
        node. If reprod is not provided, all reductions are
        returned. If reprod is False, all builtin reductions that are
        not set to reproducible are returned. If reprod is True, all
        builtins that are set to reproducible are returned.

        :param reprod: if provided, filter reductions by whether or not they \
                       are set to be reproducible.
        :type param: Optional[bool]

        :returns: all kernels involving reductions that are descendants of \
                  this node.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Kern`]

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import Kern

        call_reduction_list = []
        for call in self.walk(Kern):
            if call.is_reduction:
                if reprod is None:
                    call_reduction_list.append(call)
                elif reprod:
                    if call.reprod_reduction:
                        call_reduction_list.append(call)
                else:
                    if not call.reprod_reduction:
                        call_reduction_list.append(call)
        return call_reduction_list

    def is_openmp_parallel(self):
        '''
        :returns: True if this Node is within an OpenMP parallel region, \
                  False otherwise.
        :rtype: bool

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import OMPParallelDirective
        omp_dir = self.ancestor(OMPParallelDirective)
        if omp_dir:
            return True
        return False

    def lower_to_language_level(self):
        '''
        In-place replacement of DSL or high-level concepts into generic
        PSyIR constructs. The generic implementation only recurses down
        to its children, but this method must be re-implemented by Nodes
        that represent high-level concepts.

        '''
        # We recurse only over the original children (hence [:]), this is
        # because new nodes may be inserted during the lowering, but these
        # must already be language-level.
        for child in self.children[:]:
            child.lower_to_language_level()

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. The default implementation
        just recurses down to all children.

        :param var_accesses: Stores the output results.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        for child in self._children:
            child.reference_accesses(var_accesses)

    @property
    def scope(self):
        ''' Some nodes (e.g. Schedule and Container) allow symbols to be
        scoped via an attached symbol table. This property returns the closest
        ScopingNode node including self.

        :returns: the closest ancestor ScopingNode node.
        :rtype: :py:class:`psyclone.psyir.node.ScopingNode`

        :raises SymbolError: if there is no ScopingNode ancestor.

        '''
        # These imports have to be local to this method to avoid circular
        # dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.scoping_node import ScopingNode
        node = self.ancestor(ScopingNode, include_self=True)
        if node:
            return node
        raise SymbolError(
            f"Unable to find the scope of node '{self}' as none of its "
            f"ancestors are Container or Schedule nodes.")

    def replace_with(self, node):
        '''Removes self, and its descendants, from the PSyIR tree to which it
        is connected, and replaces it with the supplied node (and its
        descendants).

        :param node: the node that will replace self in the PSyIR \
            tree.
        :type node: :py:class:`psyclone.psyir.nodes.node`

        :raises TypeError: if the argument 'node' is not a Node.
        :raises GenerationError: if this node does not have a parent.
        :raises GenerationError: if the argument 'node' has a parent.

        '''
        if not isinstance(node, Node):
            raise TypeError(
                f"The argument node in method replace_with in the Node class "
                f"should be a Node but found '{type(node).__name__}'.")
        if not self.parent:
            raise GenerationError(
                "This node should have a parent if its replace_with method "
                "is called.")
        if node.parent is not None:
            raise GenerationError(
                f"The parent of argument node in method replace_with in the "
                f"Node class should be None but found "
                f"'{type(node.parent).__name__}'.")

        self.parent.children[self.position] = node

    def pop_all_children(self):
        ''' Remove all children of this node and return them as a list.

        :returns: all the children of this node as orphan nodes.
        :rtype: list of :py:class:`psyclone.psyir.node.Node`

        '''
        free_children = []
        while self.children:
            free_children.insert(0, self.children.pop())
        return free_children

    def detach(self):
        ''' Detach this node from the tree it belongs to. This is necessary
        as a precursor to inserting it as the child of another node.

        :returns: this node detached from its parent.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        if self.parent:
            index = self.position
            self.parent.children.pop(index)
        return self

    def _refine_copy(self, other):
        ''' Refine the object attributes when a shallow copy is not the most
        appropriate operation during a call to the copy() method.

        :param other: object we are copying from.
        :type other: :py:class:`psyclone.psyir.node.Node`

        '''
        self._parent = None
        self._has_constructor_parent = False
        self._annotations = other.annotations[:]
        # Invalidate shallow copied children list
        self._children = ChildrenList(self, self._validate_child,
                                      self._children_valid_format)
        # And make a recursive copy of each child instead
        self.children.extend([child.copy() for child in other.children])

    def copy(self):
        ''' Return a copy of this node. This is a bespoke implementation for
        PSyIR nodes that will deepcopy some of its recursive data-structure
        (e.g. the children tree), while not copying other attributes (e.g.
        top-level parent reference).

        :returns: a copy of this node and its children.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Start with a shallow copy of the object
        new_instance = copy.copy(self)
        # and then refine the elements that shouldn't be shallow copied
        # pylint: disable=protected-access
        new_instance._refine_copy(self)
        return new_instance

    def validate_global_constraints(self):
        ''' Validates this Node in the context of the whole PSyIR tree.
        Although there are validation checks for the parent<->child
        relationships, there are other constraints that can only be
        checked once the tree is complete and all transformations have
        been applied. (One example is that an OMP Do directive must be
        within the scope of an OMP Parallel directive.)

        By default, this routine does nothing. It must be overridden
        appropriately in any sub-classes to which constraints apply.
        If an error is found then a GenerationError should be raised.

        '''


# For automatic documentation generation
# TODO #913 the 'colored' routine shouldn't be in this module.
__all__ = ["colored",
           "ChildrenList",
           "Node"]
