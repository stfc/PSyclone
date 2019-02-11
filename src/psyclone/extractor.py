# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' This module provides support for extraction of code within a specified
invoke. The extracted code may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke (extraction
applied to all Nodes).
'''

from __future__ import absolute_import, print_function
from psyclone.psyGen import colored, GenerationError, Kern, Node, \
     SCHEDULE_COLOUR_MAP
from psyclone.transformations import ExtractRegionTrans


class ExtractNode(Node):
    ''' This class can be inserted into a Schedule to mark Nodes for \
    code extraction using the ExtractRegionTrans transformation. By \
    applying the transformation the Nodes marked for extraction become \
    children of an ExtractNode.
    '''

    def __init__(self, children=None, parent=None):
        ''' Constructor for an ExtractNode that is inserted in a Schedule.

            :param children: a list of children nodes for this Node.
            :type children: a list of :py::class::`psyclone.psyGen.Node` \
                            or derived classes.
            :param parent: the parent of this Node.
            :type parent: a :py::class::`psyclone.psyGen.Node`.
        '''
        Node.__init__(self, children=children, parent=parent)

    def __str__(self):
        ''' Returns a string representation of the subtree starting at
        the Extract Node. '''
        result = "ExtractStart\n"
        for child in self.children:
            result += str(child)+"\n"
        return result+"ExtractEnd"

    @property
    def coloured_text(self):
        ''' Returns a string containing the name of this Node along with \
        control characters for colouring in terminals that supports it.

        :returns: the name of this Node, possibly with control codes for \
                  colouring.
        :rtype: string
        '''
        return colored("Extract", SCHEDULE_COLOUR_MAP["Extract"])

    @property
    def dag_name(self):
        ''' Return the base dag name for this ExtractNode '''
        return "extract_" + str(self.position)

    def view(self, indent=0):
        ''' Print a text representation of this Extract schedule to stdout \
        and then call the view() method of any children. The text \
        representation returns position of Extract Node(s) in the tree.

        :param int indent: depth of indent for output text.
        '''
        print(self.indent(indent) + self.coloured_text +
              "[position='" + str(self.position) +
              "',depth='" + str(self.depth) + "']")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, parent):
        ''' Marks region for code extraction as children of the \
        ExtractNode. For now it inserts comments at the position of the \
        ExtractNode and after all children of the ExtractNode. These \
        comments will later be replaced by calls to write out arguments \
        of extracted Node(s) or Kernel(s).

        :param parent: the parent of this Node.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        '''
        from psyclone.f2pygen import CommentGen
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractStart"))
        parent.add(CommentGen(
            parent, " CALL write_extract_arguments(argument_list)"))
        parent.add(CommentGen(parent, ""))
        for child in self.children:
            child.gen_code(parent)
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractEnd"))
        parent.add(CommentGen(parent, ""))


class Extractor(object):
    ''' This class contains is helper functions for code extraction. \
    For now it only provides the function to extract the specific Kernel \
    from an Invoke Schedule. Another planned functionality is to wrap \
    settings for generating driver for the extracted code. '''

    @staticmethod
    def extract_kernel(schedule, kernel_name, position=None):
        ''' This function inserts ExtractNode(s) around one or more Nodes \
        in a Schedule which contains calls to a particular Kernel. \
        First we construct the lists of relative and absolute positions \
        of root Nodes which contain the Kernel call within the Schedule. \
        The list of relative positions instructs the ExtractRegionTrans \
        where to insert an ExtractNode. The list of absolute positions is \
        used as a control mechanism for cases where two or more Kernels \
        with the same name are descendants of the same root Node (for \
        instance if they are enclosed within an OMPParallelDirective or \
        an OMPParallelDoDirective). In these cases the repeated values of \
        root Node(s)' absolute and the corresponding relative positions \
        are not counted. Otherwise the ExtractRegionTrans would try to \
        insert and ExtractNode repeatedly and fail with the appropriate \
        TransformationError. If the specified Kernel is called within \
        more than one root Node then this function will insert ExtractNodes \
        in all returned locations, unless the optional argument "position" \
        specifies just one of these locations (relative positions of the \
        root Nodes in the Schedule).

        :param schedule: the supplied Schedule within which we are \
                         extracting one or more root Nodes containing \
                         calls to the specified Kernel.
        :type schedule: :py:class:`psyclone.psyGen.Schedule`.
        :param str kernel_name: the name of the specified Kernel as \
                                represented in a Kernel call (ending in \
                                "_code", e.g. "ru_kernel_code").
        :param int position: optional argument to determine where to \
                             insert ExtractNode if there are multiple \
                             root Nodes with the specified Kernel calls.
        :returns: the modified Schedule.
        :rtype: :py:class:`psyclone.psyGen.Schedule`.
        :raises GenerationError: if there are no Kernels with the specified \
                                 name in the Schedule.
        :raises GenerationError: if the optional position argument does \
                                 not point to a location within the list \
                                 which contains the root Node(s) with \
                                 the specified Kernel calls.
         '''
        etrans = ExtractRegionTrans()

        # First construct the list of relative and absolute positions of
        # Nodes which contain the Kernel call within the Schedule.
        extract_node_pos = []
        extract_node_abspos = []
        for kernel in schedule.walk(schedule.children, Kern):
            if kernel.name.lower() == kernel_name:
                # Root at depth 2 returns the root (ancestor) Node of this
                # Kernel call in the Schedule
                extract_node = kernel.root_at_depth(2)
                # Check whether the absolute position of the root Node
                # is already in the list and add it if it is not
                if extract_node.abs_position not in extract_node_abspos:
                    extract_node_pos.append(extract_node.position)
                    extract_node_abspos.append(extract_node.abs_position)

        # Now insert ExtracNode at the relative positions returned in
        # the extract_node_pos list
        if not extract_node_pos:
            # Raise an error if there are no Kernels with the specified
            # name in the Schedule
            raise GenerationError("No Kernels with the name of '{0}' were "
                                  "found to extract.".format(kernel_name))
        else:
            # Check whether the optional relative position argument is
            # is provided and assign its value to the extract_node_pos
            # list if it points to a valid location inside the list
            if position:
                if position not in extract_node_pos:
                    # Raise an error if the optional position argument
                    # does not correspond to any of the returned relative
                    # positions of the Kernel's root Nodes
                    raise GenerationError(
                        "Provided position {0} is not the position of any "
                        "Node which contains Kernel '{1}' call."
                        .format(position, kernel_name))
                else:
                    # Assign the position argument if it is valid
                    extract_node_pos = [position]

        # Apply the ExtractRegionTrans to the selected Nodes
        for idx in extract_node_pos:
            schedule, _ = etrans.apply(schedule.children[idx])

        # Return modified Schedule
        return schedule
