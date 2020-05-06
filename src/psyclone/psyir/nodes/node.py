# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the abstract Node implementation.'''

import abc
from psyclone.psyir.symbols import SymbolError
from psyclone.errors import GenerationError, InternalError

# Colour map to use when writing Invoke schedule to terminal. (Requires
# that the termcolor package be installed. If it isn't then output is not
# coloured.) See https://pypi.python.org/pypi/termcolor for details.
SCHEDULE_COLOUR_MAP = {"Schedule": "white",
                       "Loop": "red",
                       "GlobalSum": "cyan",
                       "Directive": "green",
                       "HaloExchange": "blue",
                       "HaloExchangeStart": "yellow",
                       "HaloExchangeEnd": "yellow",
                       "BuiltIn": "magenta",
                       "CodedKern": "magenta",
                       "InlinedKern": "magenta",
                       "PSyData": "green",
                       "Profile": "green",
                       "Extract": "green",
                       "If": "red",
                       "Assignment": "blue",
                       "Range": "white",
                       "Reference": "yellow",
                       "Operation": "blue",
                       "Literal": "yellow",
                       "Return": "yellow",
                       "CodeBlock": "red",
                       "Container": "green"}


# Default indentation string
INDENTATION_STRING = "    "

# We use the termcolor module (if available) to enable us to produce
# coloured, textual representations of Invoke schedules. If it's not
# available then we don't use colour.
try:
    from termcolor import colored
except ImportError:
    # We don't have the termcolor package available so provide
    # alternative routine
    def colored(text, _):
        '''
        Returns the supplied text argument unchanged. This is a swap-in
        replacement for when termcolor.colored is not available.

        :param text: Text to return
        :type text: string
        :param _: Fake argument, only required to match interface
                  provided by termcolor.colored
        :returns: The supplied text, unchanged
        :rtype: string
        '''
        return text


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
            errmsg = "Item '{0}' can't be child {1} of '{2}'.".format(
                item.__class__.__name__, index,
                self._node_reference.coloured_name(False))
            if self._validation_text == "<LeafNode>":
                errmsg = errmsg + " {0} is a LeafNode and doesn't accept " \
                    "children.".format(
                        self._node_reference.coloured_name(False))
            else:
                errmsg = errmsg + " The valid format is: '{0}'.".format(
                    self._validation_text)

            raise GenerationError(errmsg)

    def append(self, item):
        ''' Extends list append method with children node validation.

        :param item: item to be appened to the list.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._validate_item(len(self), item)
        super(ChildrenList, self).append(item)

    def __setitem__(self, index, item):
        ''' Extends list __setitem__ method with children node validation.

        :param int index: position where to insert the item.
        :param item: item to be inserted to the list.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._validate_item(index, item)
        super(ChildrenList, self).__setitem__(index, item)

    def insert(self, index, item):
        ''' Extends list insert method with children node validation.

        :param int index: position where to insert the item.
        :param item: item to be inserted to the list.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        '''
        positiveindex = index if index >= 0 else len(self) - index
        self._validate_item(positiveindex, item)
        # Check that all displaced items will still in valid positions
        for position in range(positiveindex, len(self)):
            self._validate_item(position + 1, self[position])
        super(ChildrenList, self).insert(index, item)

    def extend(self, items):
        ''' Extends list extend method with children node validation.

        :param items: list of items to be appened to the list.
        :type items: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        for index, item in enumerate(items):
            self._validate_item(len(self) + index, item)
        super(ChildrenList, self).extend(items)

    # Methods below don't insert elements but have the potential to displace
    # or change the order of the items in-place.
    def __delitem__(self, index):
        ''' Extends list __delitem__ method with children node validation.

        :param int index: position where to insert the item.

        '''
        positiveindex = index if index >= 0 else len(self) - index
        for position in range(positiveindex + 1, len(self)):
            self._validate_item(position - 1, self[position])
        super(ChildrenList, self).__delitem__(index)

    def remove(self, item):
        ''' Extends list remove method with children node validation.

        :param item: item to be deleted the list.
        :type item: :py:class:`psyclone.psyir.nodes.Node`

        '''
        for position in range(self.index(item) + 1, len(self)):
            self._validate_item(position - 1, self[position])
        super(ChildrenList, self).remove(item)

    def pop(self, index=-1):
        ''' Extends list pop method with children node validation.

        :param int index: position of the item that is popped out, if not \
            given, the last element is popped out.

        :returns: the last value or the given index value from the list.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        positiveindex = index if index >= 0 else len(self) - index
        for position in range(positiveindex, len(self)):
            self._validate_item(position - 1, self[position])
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
    _colour_key = None

    def __init__(self, ast=None, children=None, parent=None, annotations=None):
        self._children = ChildrenList(self, self._validate_child,
                                      self._children_valid_format)
        if children:
            self._children.extend(children)
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
                        "{0} with unrecognised annotation '{1}', valid "
                        "annotations are: {2}.".format(
                            self.__class__.__name__, annotation,
                            self.valid_annotations))

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
            raise NotImplementedError(
                "_text_name is an abstract attribute which needs to be "
                "given a string value in the concrete class '{0}'."
                "".format(type(self).__name__))
        if colour:
            try:
                return colored(self._text_name,
                               SCHEDULE_COLOUR_MAP[self._colour_key])
            except KeyError:
                pass
        return self._text_name

    def node_str(self, colour=True):
        '''
        :param bool colour: whether or not to include control codes for \
                            coloured text.

        :returns: a text description of this node. Will typically be \
                  overridden by sub-class.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[]"

    @abc.abstractmethod
    def __str__(self):
        return self.node_str(False)

    def math_equal(self, other):
        '''Returns True if the self has the same results as other. The
        implementation in the base class just confirms that the type is the
        same, and the number of children as well.

        :param other: the node to compare self with.
        :type other: py:class:`psyclone.psyir.nodes.Node`.

        :returns: whether self has the same result as other.
        :rtype: bool
        '''

        # pylint: disable=unidiomatic-typecheck
        if type(self) != type(other):
            return False

        if len(self.children) != len(other.children):
            return False

        for i, entity in enumerate(self.children):
            if not entity.math_equal(other.children[i]):
                return False
        return True

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
        '''Create a dag of this node and its children.'''
        try:
            import graphviz as gv
        except ImportError:
            # TODO: #11 add a warning to a log file here
            # silently return if graphviz bindings are not installed
            return
        try:
            graph = gv.Digraph(format=file_format)
        except ValueError:
            raise GenerationError(
                "unsupported graphviz file format '{0}' provided".
                format(file_format))
        self.dag_gen(graph)
        graph.render(filename=file_name)

    def dag_gen(self, graph):
        '''Output my node's graph (dag) information and call any
        children. Nodes with children are represented as two vertices,
        a start and an end. Forward dependencies are represented as
        green edges, backward dependencies are represented as red
        edges (but their direction is reversed so the layout looks
        reasonable) and parent child dependencies are represented as
        blue edges.'''
        # pylint: disable=too-many-branches
        from psyclone.psyir.nodes.loop import Loop
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
        elif self.parent:
            # this node is a child of another node and has no forward
            # dependence. Therefore connect it to the the end vertex
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
        elif self.parent:
            # this node has a parent and has no backward
            # dependence. Therefore connect it to the the start vertex
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
        '''Return the base dag name for this node.'''
        return "node_" + str(self.abs_position)

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
                if self.sameParent(node):
                    # The remote node (or one of its ancestors) shares
                    # the same parent as me
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
                if self.sameParent(node):
                    # The remote node (or one of its ancestors) shares
                    # the same parent as me
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
                "In the psyir.nodes.Node.is_valid_location() method the "
                "supplied argument is not a Node, it is a '{0}'.".
                format(type(new_node).__name__))

        # 2: check position has a valid value
        valid_positions = ["before", "after"]
        if position not in valid_positions:
            raise GenerationError(
                "The position argument in the psyGenNode.is_valid_location() "
                "method must be one of {0} but found '{1}'".format(
                    valid_positions, position))

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

    def view(self, indent=0, index=None):
        ''' Print out description of current node to stdout and
        then call view() on all child nodes.

        :param int indent: depth of indent for output text.
        :param int index: the position of this Node wrt its siblings or None.

        '''
        from psyclone.psyir.nodes import Schedule
        if not isinstance(self.parent, Schedule) or index is None:
            print("{0}{1}".format(self.indent(indent),
                                  self.node_str(colour=True)))
        else:
            print("{0}{1}: {2}".format(self.indent(indent), index,
                                       self.node_str(colour=True)))
        for idx, entity in enumerate(self._children):
            entity.view(indent=indent + 1, index=idx)

    @staticmethod
    def indent(count, indent=INDENTATION_STRING):
        '''
        Helper function to produce indentation strings.

        :param int count: Number of indentation levels.
        :param str indent: String representing one indentation level.
        :returns: Complete indentation string.
        :rtype: str
        '''
        return count * indent

    def list(self, indent=0):
        result = ""
        for entity in self._children:
            result += str(entity)+"\n"
        return result

    def addchild(self, child, index=None):
        if index is not None:
            self._children.insert(index, child)
        else:
            self._children.append(child)

    @property
    def children(self):
        return self._children

    @children.setter
    def children(self, my_children):
        ''' Set a new children list.

        :param my_children: new list of children.
        :type my_children: list or NoneType

        :raises TypeError: if the given children parameter is not a list \
            nor NoneType.
        '''
        if my_children is None:
            self._children = None
        elif isinstance(my_children, list):
            self._children = ChildrenList(self, self._validate_child,
                                          self._children_valid_format)
            self._children.extend(my_children)
        else:
            raise TypeError("The 'my_children' parameter of the node.children"
                            " setter must be a list or None.")

    @property
    def parent(self):
        return self._parent

    @parent.setter
    def parent(self, my_parent):
        self._parent = my_parent

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
        return self.parent.children.index(self)

    @property
    def abs_position(self):
        '''
        Find a Node's absolute position in the tree (starting with 0 if
        it is the root). Needs to be computed dynamically from the
        starting position (0) as its position may change.

        :returns: absolute position of a Node in the tree
        :rtype: int

        :raises InternalError: if the absolute position cannot be found
        '''
        from psyclone.psyir.nodes import Schedule
        if self.root == self and isinstance(self.root, Schedule):
            return self.START_POSITION
        found, position = self._find_position(self.root.children,
                                              self.START_POSITION)
        if not found:
            raise InternalError("Error in search for Node position "
                                "in the tree")
        return position

    def _find_position(self, children, position):
        '''
        Recurse through the tree depth first returning position of
        a Node if found.
        :param children: list of Nodes which are children of this Node
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :returns: position of the Node in the tree
        :rtype: int
        :raises InternalError: if the starting position is < 0
        '''
        if position < self.START_POSITION:
            raise InternalError(
                "Search for Node position started from {0} "
                "instead of {1}.".format(position, self.START_POSITION))
        for child in children:
            position += 1
            if child == self:
                return True, position
            if child.children:
                found, position = self._find_position(child.children, position)
                if found:
                    return True, position
        return False, position

    @property
    def root(self):
        node = self
        while node.parent is not None:
            node = node.parent
        return node

    def sameRoot(self, node_2):
        if self.root == node_2.root:
            return True
        return False

    def sameParent(self, node_2):
        if self.parent is None or node_2.parent is None:
            return False
        if self.parent == node_2.parent:
            return True
        return False

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
        :type my_type: either a single :py:class:`psyclone.Node` class \
            or a tuple of such classes
        :param stop_type: class(es) at which recursion is halted (optional)."

        :type stop_type: None or a single :py:class:`psyclone.Node` \
            class or a tuple of such classes

        :returns: list with all nodes that are instances of my_type \
            starting at and including this node.
        :rtype: list of :py:class:`psyclone.Node` instances.
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

    def ancestor(self, my_type, excluding=None, include_self=False):
        '''
        Search back up tree and check whether we have an ancestor that is
        an instance of the supplied type. If we do then we return
        it otherwise we return None. An individual (or tuple of) (sub-)
        class(es) to ignore may be provided via the `excluding` argument. If
        include_self is True then the current node is included in the search.

        :param my_type: class(es) to search for.
        :type my_type: type or tuple of types
        :param tuple excluding: individual (or tuple of) (sub-)class(es) to \
                                ignore or None.
        :param bool include_self: whether or not to include this node in the \
                                  search.

        :returns: First ancestor Node that is an instance of any of the \
                  requested classes or None if not found.
        :rtype: :py:class:`psyclone.psyir.nodes.Node` or NoneType

        :raises TypeError: if `excluding` is provided but is not a type or \
                           tuple of types.
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
                    "The 'excluding' argument to ancestor() must be a type or "
                    "a tuple of types but got: '{0}'".format(
                        type(excluding).__name__))

        while myparent is not None:
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
        :rtype: list of :py:class:`psyclone.psyGen.Kern` sub-classes.
        '''
        from psyclone.psyGen import Kern
        return self.walk(Kern)

    def following(self):
        '''Return all :py:class:`psyclone.psyir.nodes.Node` nodes after me in
        the schedule. Ordering is depth first.

        :returns: a list of nodes
        :rtype: :func:`list` of :py:class:`psyclone.psyir.nodes.Node`

        '''
        all_nodes = self.root.walk(Node)
        position = all_nodes.index(self)
        return all_nodes[position+1:]

    def preceding(self, reverse=None):
        '''Return all :py:class:`psyclone.psyir.nodes.Node` nodes before me
        in the schedule. Ordering is depth first. If the `reverse` argument
        is set to `True` then the node ordering is reversed
        i.e. returning the nodes closest to me first

        :param: reverse: An optional, default `False`, boolean flag
        :type: reverse: bool
        :returns: A list of nodes
        :rtype: :func:`list` of :py:class:`psyclone.psyir.nodes.Node`

        '''
        all_nodes = self.root.walk(Node)
        position = all_nodes.index(self)
        nodes = all_nodes[:position]
        if reverse:
            nodes.reverse()
        return nodes

    def coded_kernels(self):
        '''
        Returns a list of all of the user-supplied kernels (as opposed to
        builtins) that are beneath this node in the PSyIR.

        :returns: all user-supplied kernel calls below this node.
        :rtype: list of :py:class:`psyclone.psyGen.CodedKern`
        '''
        from psyclone.psyGen import CodedKern
        return self.walk(CodedKern)

    def loops(self):
        '''Return all loops currently in this schedule.'''
        from psyclone.psyir.nodes import Loop
        return self.walk(Loop)

    def reductions(self, reprod=None):
        '''Return all calls that have reductions and are decendents of this
        node. If reprod is not provided, all reductions are
        returned. If reprod is False, all builtin reductions that are
        not set to reproducible are returned. If reprod is True, all
        builtins that are set to reproducible are returned.'''
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
        ''':returns: True if this Node is within an OpenMP parallel region.

        '''
        from psyclone.psyGen import OMPParallelDirective
        omp_dir = self.ancestor(OMPParallelDirective)
        if omp_dir:
            return True
        return False

    def gen_code(self, parent):
        '''Abstract base class for code generation function.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        '''
        raise NotImplementedError("Please implement me")

    def update(self):
        ''' By default we assume there is no need to update the existing
        fparser2 AST which this Node represents. We simply call the update()
        method of any children. '''
        for child in self._children:
            child.update()

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. The default implementation
        just recurses down to all children.

        :param var_accesses: Stores the output results.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        for child in self._children:
            child.reference_accesses(var_accesses)

    def _insert_schedule(self, children=None, ast=None):
        '''
        Utility method to insert a Schedule between this Node and the
        supplied list of children.

        :param children: nodes which will become children of the \
                         new Schedule.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param ast: reference to fparser2 parse tree for associated \
                    Fortran code.
        :type ast: :py:class:`fparser.two.utils.Base`

        :returns: the new Schedule node.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`
        '''
        from psyclone.psyir.nodes import Schedule
        sched = Schedule(children=children, parent=self)
        if children:
            # If we have children then set the Schedule's AST pointer to
            # point to the AST associated with them.
            sched.ast = children[0].ast
            for child in children:
                child.parent = sched
        else:
            sched.ast = ast
        return sched

    def find_symbol_table(self):
        '''
        :returns: the symbol table attached to the nearest ancestor \
            node (including self).
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :raises InternalError: if no symbol table is found.

        '''
        current = self
        while current and not hasattr(current, "symbol_table"):
            current = current.parent
        if current:
            return current.symbol_table
        raise InternalError("Symbol table not found in any ancestor nodes.")

    def find_or_create_symbol(self, name, scope_limit=None):
        '''Returns the symbol with the name 'name' from a symbol table
        associated with this node or one of its ancestors.  If the symbol
        is not found and there are no ContainerSymbols with wildcard imports
        then an exception is raised. However, if there are one or more
        ContainerSymbols with wildcard imports (which could therefore be
        bringing the symbol into scope) then a new DataSymbol of unknown type
        and interface is created and inserted in the most local SymbolTable
        that has such an import.
        The scope_limit variable further limits the symbol table search so
        that the search through ancestor nodes stops when the scope_limit node
        is reached i.e. ancestors of the scope_limit node are not searched.

        :param str name: the name of the symbol.
        :param scope_limit: optional Node which limits the symbol \
            search space to the symbol tables of the nodes within the \
            given scope. If it is None (the default), the whole \
            scope (all symbol tables in ancestor nodes) is searched \
            otherwise ancestors of the scope_limit node are not \
            searched.
        :type scope_limit: :py:class:`psyclone.psyir.nodes.Node` or \
            `NoneType`

        :returns: the matching symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises SymbolError: if no matching symbol is found and there are \
            no ContainerSymbols from which it might be brought into scope.

        '''
        from psyclone.psyir.symbols import DataSymbol, UnresolvedInterface, \
            DeferredType
        if scope_limit:
            # Validate the supplied scope_limit
            if not isinstance(scope_limit, Node):
                raise TypeError(
                    "The scope_limit argument '{0}' provided to the "
                    "find_or_create_symbol method, is not of type `Node`."
                    "".format(str(scope_limit)))

            # Check that the scope_limit Node is an ancestor of this
            # Reference Node and raise an exception if not.
            mynode = self.parent
            while mynode is not None:
                if mynode is scope_limit:
                    # The scope_limit node is an ancestor of the
                    # supplied node.
                    break
                mynode = mynode.parent
            else:
                # The scope_limit node is not an ancestor of the
                # supplied node so raise an exception.
                raise ValueError(
                    "The scope_limit node '{0}' provided to the "
                    "find_or_create_symbol method, is not an ancestor of this "
                    "node '{1}'.".format(str(scope_limit), str(self)))

        # Keep a list of (symbol table, container) tuples for containers that
        # have wildcard imports into the current scope and therefore may
        # contain the symbol we're searching for.
        possible_containers = []
        # Keep a reference to the most local SymbolTable with a wildcard
        # import in case we need to create a Symbol.
        first_symbol_table = None
        test_node = self

        # Iterate over ancestor Nodes of this Node.
        while test_node:
            # For simplicity, test every Node for the existence of a
            # SymbolTable (rather than checking for the particular
            # Node types which we know to have SymbolTables).
            if hasattr(test_node, 'symbol_table'):
                # This Node does have a SymbolTable.
                symbol_table = test_node.symbol_table

                try:
                    # If the reference matches a Symbol in this
                    # SymbolTable then return the Symbol.
                    return symbol_table.lookup(name)
                except KeyError:
                    # The Reference Node does not match any Symbols in
                    # this SymbolTable. Does this SymbolTable have any
                    # wildcard imports?
                    for csym in symbol_table.containersymbols:
                        if csym.wildcard_import:
                            possible_containers.append((symbol_table, csym))
                            if not first_symbol_table:
                                first_symbol_table = symbol_table

            if test_node is scope_limit:
                # The ancestor scope/top-level Node has been reached and
                # nothing has matched.
                break

            # Move on to the next ancestor.
            test_node = test_node.parent

        if possible_containers:
            # No symbol found but there are one or more Containers from which
            # it may be being brought into scope. Therefore create a DataSymbol
            # of unknown type and deferred interface and add it to the most
            # local SymbolTable.
            symbol = DataSymbol(name, DeferredType(),
                                interface=UnresolvedInterface())
            first_symbol_table.add(symbol)
            return symbol

        # All requested Nodes have been checked but there has been no
        # match and there are no wildcard imports so raise an exception.
        raise SymbolError(
            "No Symbol found for name '{0}'.".format(name))
