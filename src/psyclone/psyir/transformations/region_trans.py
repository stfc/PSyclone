# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
#        J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

'''This module contains the base class RegionTrans. All transformations which
   act on a region of code (list of PSyIR nodes) sub-class this one.
'''

import abc
import six

from psyclone.psyGen import Kern, Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.nodes import Schedule, Node


@six.add_metaclass(abc.ABCMeta)
class RegionTrans(Transformation):
    # Avoid pylint warning about abstract functions (apply, name) not
    # overwritten:
    # pylint: disable=abstract-method,arguments-differ
    '''
    This abstract class is a base class for all transformations that act
    on a list of nodes. It gives access to a validate function that
    makes sure that the nodes in the list are in the same order as in
    the original AST, no node is duplicated, and that all nodes have
    the same parent. We also check that all nodes to be enclosed are
    valid for this transformation - this requires that the sub-class
    populate the `excluded_node_types` tuple.

    '''
    # The types of Node that are excluded from within this region. Must be
    # populated by sub-class.
    excluded_node_types = ()

    def get_node_list(self, nodes):
        '''This is a helper function for region based transformations.
        The parameter for any of those transformations is either a single
        node, a schedule, or a list of nodes. This function converts this
        into a list of nodes according to the parameter type. This function
        will always return a copy, to avoid issues e.g. if a child list
        of a node should be provided, and a transformation changes the order
        in this list (which would then also change the order of the
        nodes in the tree).

        :param nodes: can be a single node, a schedule or a list of nodes.
        :type nodes: :py:obj:`psyclone.psyir.nodes.Node` or  \
            :py:obj:`psyclone.psyir.nodes.Schedule or a list of \
            :py:obj:`psyclone.psyir.nodes.Node`

        :returns: a list of nodes.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        :raises TransformationError: if the supplied parameter is neither a \
            single Node, nor a Schedule, nor a list of Nodes.

        '''
        if isinstance(nodes, list) and \
                all(isinstance(node, Node) for node in nodes):
            # We still need to return a copy, since the user might have
            # provided Node.children as parameter.
            return nodes[:]
        if isinstance(nodes, Schedule):
            # We've been passed a Schedule so default to enclosing its
            # children.
            return nodes.children[:]
        if isinstance(nodes, Node):
            # Single node that's not a Schedule
            return [nodes]

        arg_type = str(type(nodes))
        raise TransformationError(f"Error in {self.name}: "
                                  f"Argument must be a single Node in a "
                                  f"Schedule, a Schedule or a list of Nodes "
                                  f"in a Schedule but have been passed an "
                                  f"object of type: {arg_type}")

    def validate(self, nodes, options=None):
        '''Checks that the nodes in node_list are valid for a region
        transformation.

        :param nodes: list of PSyIR nodes or a single node.
        :type nodes: subclass of :py:class:`psyclone.psyir.nodes.Node` or \
                a list of subclasses of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["node-type-check"]: this flag controls if the \
                type of the nodes enclosed in the region should be tested \
                to avoid using unsupported nodes inside a region.

        :raises TransformationError: if the nodes in the list are not \
                in the original order in which they are in the AST, \
                a node is duplicated or the nodes have different parents.
        :raises TransformationError: if any of the nodes to be enclosed in \
                the region are of an unsupported type.
        :raises TransformationError: if the parent of the supplied Nodes is \
                not a Schedule or a Directive.
        :raises TransformationError: if the nodes are in a NEMO \
                Schedule and the transformation acts on the child of a \
                single-line If or Where statment.
        :raises TransformationError: if the supplied options are not a \
                dictionary.

        '''
        # pylint: disable=too-many-branches

        # Ensure we are always validating a list of nodes, even if only
        # one was supplied via the `node_list` argument.
        node_list = self.get_node_list(nodes)

        if not options:
            options = {}
        if not isinstance(options, dict):
            raise TransformationError(
                f"Transformation apply method options argument must be a "
                f"dictionary but found '{type(options).__name__}'.")
        node_parent = node_list[0].parent
        prev_position = -1
        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    f"Error in {self.name} transformation: supplied nodes "
                    f"are not children of the same parent.")
            if prev_position >= 0 and prev_position+1 != child.position:
                raise TransformationError(
                    f"Children are not consecutive children of one parent: "
                    f"child '{child}' has position {child.position}, but "
                    f"previous child had position {prev_position}.")
            prev_position = child.position

        # Check that the proposed region contains only supported node types
        if options.get("node-type-check", True):
            for child in node_list:
                # Stop at any instance of Kern to avoid going into the
                # actual kernels, e.g. in Nemo inlined kernels
                flat_list = [item for item in child.walk(object, Kern)
                             if not isinstance(item, Schedule)]
                for item in flat_list:
                    if isinstance(item, self.excluded_node_types):
                        raise TransformationError(
                            f"Nodes of type '{type(item).__name__}' cannot be "
                            f"enclosed by a {self.name} transformation")

        # If we've been passed a list that contains one or more Schedules
        # then something is wrong. e.g. two Schedules that are both children
        # of an IfBlock would imply that the transformation is being applied
        # around both the if-body and the else-body and that doesn't make
        # sense.
        if (len(node_list) > 1 and
                any([isinstance(node, Schedule) for node in node_list])):
            raise TransformationError(
                "Cannot apply a transformation to multiple nodes when one "
                "or more is a Schedule. Either target a single Schedule "
                "or the children of a Schedule.")

        # Sanity check that we've not been passed the condition part of
        # an If statement or the bounds of a Loop. If the parent node is
        # a Loop or IfBlock then we can only accept a single Schedule.
        if not isinstance(node_parent, Schedule) and \
                not isinstance(node_list[0], Schedule):
            # We've already checked for lists with len > 1 that contain a
            # Schedule above so if the first item is a Schedule then that's
            # all the list contains.
            raise TransformationError(
                "Cannot apply transformation to the immediate children of a "
                "Loop/IfBlock unless it is to a single Schedule representing"
                " the Loop/If/Else body.")
