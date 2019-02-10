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

'''
    This module provides support for extraction of code within a specified
    invoke. The extracted code may be a single kernel, multiple occurences
    of a kernel in an invoke, nodes in an invoke or the entire invoke
    (extraction applied to all nodes).
'''

from __future__ import absolute_import, print_function
from psyclone.psyGen import colored, GenerationError, Kern, Node, \
     SCHEDULE_COLOUR_MAP
from psyclone.transformations import ExtractRegionTrans


class ExtractNode(Node):
    ''' This class can be inserted into a Schedule to mark Nodes for code
    extraction using the ExtractRegionTrans transformation. By applying
    the transformation the Nodes marked for extraction become children
    of an ExtractNode.
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
        '''
        Returns a string containing the name of this node along with
        control characters for colouring in terminals that supports it.

        :returns: The name of this node, possibly with control codes for \
                  colouring
        :rtype: string
        '''
        return colored("Extract", SCHEDULE_COLOUR_MAP["Extract"])

    @property
    def dag_name(self):
        ''' Return the base dag name for this Extract node '''
        return "extract_" + str(self.position)

    def view(self, indent=0):
        '''
        Print a text representation of this Extract schedule to stdout \
        and then call the view() method of any children. The text \
        representation returns position of Extract Node(s) in the tree.

        :param int indent: Depth of indent for output text
        '''
        print(self.indent(indent) + self.coloured_text +
              "[position='" + str(self.position) +
              "',depth='" + str(self.depth) + "']")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, parent):
        '''
        Marks region for code extraction as children of the ExtractNode. \
        For now it inserts comments at the position of the ExtractNode
        and after all children of the ExtractNode. These comments will \
        later be replaced by calls to write out arguments of extracted \
        Node(s) or Kernel(s).

        :param parent: the parent of this Node.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        '''
        from psyclone.f2pygen import CommentGen
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(
            parent, " CALL write_extract_arguments(argument_list)"))
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractStart"))
        for child in self.children:
            child.gen_code(parent)
        parent.add(CommentGen(parent, " ExtractEnd"))


class Extractor(object):
    ''' This class
    1) wraps settings for code extraction,
    2) provides view function about which kernel to extract
    3) generates driver for the extracted code '''

    @staticmethod
    def extract_kernel(schedule, kernel_name, position=None):
        ''' Extract function for a specific kernel and invoke '''
        # Find the kernel and invoke to extract

        etrans = ExtractRegionTrans()

        # First construct the list of positions of Nodes which
        # contain the kernel
        extract_node_position = []
        extract_node_absposition = []
        for kernel in schedule.walk(schedule.children, Kern):
            if kernel.name == kernel_name:
                extract_node = kernel.root_at_depth(1)
                extract_node_position.append(extract_node.position)
                extract_node_absposition.append(extract_node.abs_position)
                print(type(extract_node))
                print(extract_node.position, extract_node.abs_position)

        if not extract_node_position:
            raise GenerationError("No Kernels with the name of {0} were "
                                  "found to extract.".format(kernel_name))
        else:
            if position:
                if position not in extract_node_position:
                    raise GenerationError(
                        "Provided position {0} is not the position of any "
                        "Node which contains Kernel {1} call."
                        .format(position, kernel_name))
                else:
                    extract_node_position = [position]
        print(extract_node_position)
        for idx in extract_node_position:
            print(idx, position)
            schedule, _ = etrans.apply(schedule.children[idx])

        return schedule
