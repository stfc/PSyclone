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
# Authors I. Kavcic, Met Office

'''This module contains the base class for extracting extracting a region
of an Invoke into a stand-alone application."
'''

from __future__ import absolute_import
from psyclone.configuration import Config
from psyclone.psyir.nodes import ExtractNode, Schedule
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.undoredo import Memento


class ExtractTrans(RegionTrans):
    '''This transformation inserts an ExtractNode or a node derived
    from ExtractNode into the PSyIR of a schedule. At code creation
    time this node will use the PSyData API to create code that can
    write the input and output parameters to a file. The node might
    also create a stand-alone driver program that can read the created
    file and then execute the instrumented region.
    Examples are given in the derived classes DynamoExtractTrans and
    GOceanExtractTrans.

    After applying the transformation the Nodes marked for extraction are
    children of the ExtractNode.
    Nodes to extract can be individual constructs within an Invoke (e.g.
    Loops containing a Kernel or BuiltIn call) or entire Invokes. This
    functionality does not support distributed memory.

    :param node_class: The Node class of which an instance will be inserted
        into the tree (defaults to ExtractNode), but can be any derived class.
    :type node_class: :py:class:`psyclone.psyir.nodes.ExtractNode` or derived
        class

    '''
    from psyclone.psyir import nodes
    from psyclone import psyGen
    # The types of node that this transformation can enclose
    valid_node_types = (nodes.Loop, psyGen.Kern, psyGen.BuiltIn,
                        psyGen.Directive, nodes.Literal, nodes.Reference)

    def __init__(self, node_class=ExtractNode):
        super(ExtractTrans, self).__init__()
        self._node_class = node_class

    def __str__(self):
        return ("Create a sub-tree of the PSyIR that has ExtractNode "
                "at its root.")

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "ExtractTrans"

    def validate(self, node_list, options=None):
        ''' Perform validation checks before applying the transformation

        :param node_list: the list of Node(s) we are checking.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if distributed memory is configured.
        :raises TransformationError: if transformation is applied to a \
                                     Kernel or a BuiltIn call without its \
                                     parent Loop.
        :raises TransformationError: if transformation is applied to a Loop \
                                     without its parent Directive when \
                                     optimisations are applied.
        :raises TransformationError: if transformation is applied to an \
                                     orphaned Directive without its parent \
                                     Directive.
        '''

        # First check constraints on Nodes in the node_list common to
        # all RegionTrans transformations.
        super(ExtractTrans, self).validate(node_list, options)

        # Now check ExtractTrans specific constraints.

        # Extracting distributed memory code is not supported due to
        # generation of infrastructure calls to set halos dirty or clean.
        # This constraint covers the presence of HaloExchange and
        # GlobalSum classses as they are only generated when distributed
        # memory is enabled.
        if Config.get().distributed_memory:
            raise TransformationError(
                "Error in {0}: Distributed memory is not supported."
                .format(str(self.name)))

        # Check constraints not covered by valid_node_types for
        # individual Nodes in node_list.
        from psyclone.psyir.nodes import Loop
        from psyclone.psyGen import BuiltIn, Directive, Kern, \
            OMPParallelDirective, ACCParallelDirective

        for node in node_list:

            # Check that ExtractNode is not inserted between a Kernel or
            # a BuiltIn call and its parent Loop.
            if isinstance(node, (Kern, BuiltIn)) and \
               isinstance(node.parent.parent, Loop):
                raise TransformationError(
                    "Error in {0}: Extraction of a Kernel or a Built-in "
                    "call without its parent Loop is not allowed."
                    .format(str(self.name)))

            # Check that ExtractNode is not inserted between a Loop and its
            # parent Directive when optimisations are applied, as this may
            # result in including the end Directive for extraction but
            # not the beginning.
            if isinstance(node, Loop) and isinstance(node.parent, Schedule) \
               and isinstance(node.parent.parent, Directive):
                raise TransformationError(
                    "Error in {0}: Extraction of a Loop without its parent "
                    "Directive is not allowed.".format(str(self.name)))

            # Check that ExtractNode is not inserted within a thread
            # parallel region when optimisations are applied. For instance,
            # this may be between an orphaned Directive (e.g. OMPDoDirective,
            # ACCLoopDirective) and its ancestor Directive (e.g. ACC or OMP
            # Parallel Directive) or within an OMPParallelDoDirective.
            if node.ancestor((OMPParallelDirective, ACCParallelDirective)):
                raise TransformationError(
                    "Error in {0}: Extraction of Nodes enclosed within "
                    "a thread-parallel region is not allowed."
                    .format(str(self.name)))

    def apply(self, nodes, options=None):
        # pylint: disable=arguments-differ
        ''' Apply this transformation to a subset of the Nodes within
        a Schedule - i.e. enclose the specified Nodes in the Schedule
        within a single Extract region.

        :param nodes: a single Node or a list of Nodes.
        :type nodes: (list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations. \
            This dictionary is passed on to the validate function and also \
            to the constructor of the node that is being inserted. This way \
            the derived API-specific extract transformation can pass on \
            parameters to the API-specific node.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the `nodes` argument is not of \
                                     the correct type.

        :returns: tuple of the modified Schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                 :py:class:`psyclone.undoredo.Memento`).
        '''

        # Check whether we've been passed a list of Nodes or just a
        # single Node. If the latter then we create ourselves a list
        # containing just that Node.
        from psyclone.psyir.nodes import Node
        if isinstance(nodes, list) and isinstance(nodes[0], Node):
            node_list = nodes
        elif isinstance(nodes, Schedule):
            node_list = nodes.children
        elif isinstance(nodes, Node):
            node_list = [nodes]
        else:
            arg_type = str(type(nodes))
            raise TransformationError("Error in {0}: "
                                      "Argument must be a single Node in a "
                                      "Schedule or a list of Nodes in a "
                                      "Schedule but have been passed an "
                                      "object of type: {1}".
                                      format(str(self.name), arg_type))

        # Validate transformation
        self.validate(node_list, options)

        # Keep a reference to the parent of the Nodes that are to be
        # enclosed within an Extract region. Also keep the index of
        # the first child to be enclosed as that will be the position
        # of the ExtractNode.
        node_parent = node_list[0].parent

        # Create a Memento of the Schedule and the proposed
        # transformation
        schedule = node_list[0].root
        keep = Memento(schedule, self)

        # Create an instance of the required class that implements
        # the code extraction using the PSyData API, e.g. a
        # GOceanExtractNode. The constructor of the extraction node
        # will insert itself into the PSyIR between the specified
        # nodes to be extracted and their parent. The nodes to
        # be extracted will become children of the extraction node.
        # it also passes the user-specified options to the constructor,
        # so that the behaviour of the code extraction can be controlled.
        # An example use case of this is the 'create_driver' flag, where
        # the calling program can control if a stand-alone driver program
        # should be created or not.
        self._node_class(parent=node_parent, children=node_list[:],
                         options=options)

        return schedule, keep
