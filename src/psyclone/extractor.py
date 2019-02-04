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
    A Python script and Python functions to generate an subroutine
    from the extracted code within a specified invoke or kernel.
'''

from __future__ import absolute_import, print_function
import os
import sys
import traceback
from psyclone.f2pygen import ProgUnitGen, CallGen, TypeDeclGen, UseGen
from psyclone.psyGen import colored, GenerationError, Kern, NameSpace, \
     NameSpaceFactory, Node, Schedule, SCHEDULE_COLOUR_MAP
from psyclone.transformations import ExtractRegionTrans


class ExtractNode(Node):

    def __init__(self, children=None, parent=None):
        ''' Constructor for an ExtractNode that is inserted in a Schedule.

            :param children: a list of children nodes for this Node.
            :type children: a list of :py::class::`psyclone.psyGen.Node` \
                            or derived classes.
            :param parent: the parent of this Node.
            :type parent: a :py::class::`psyclone.psyGen.Node`.
        '''
        Node.__init__(self, children=children, parent=parent)
        self._namespace = NameSpace()

        # Store the name of the extract variable that is used for this
        # extract region name. This allows to show the variable name in __str__
        # (and also if we would call create_name in gen(), the name would
        # change every time gen() is called).
        self._var_name = NameSpaceFactory().create().create_name("extract")

    def __str__(self):
        ''' Returns a string representation of the subtree starting at
        the Extract Node. '''
        result = "ExtractStart[var={0}]\n".format(self._var_name)
        for child in self.children:
            result += str(child)+"\n"
        return result+"ExtractEnd"

    @property
    def coloured_text(self):
        '''
        Returns a string containing the name of this node along with
        control characters for colouring in terminals that supports it.

        :return: The name of this node, possibly with control codes for
                 colouring
        :rtype: string
        '''
        return colored("Extract", SCHEDULE_COLOUR_MAP["Extract"])

    @property
    def dag_name(self):
        ''' Return the base dag name for this Extract node '''
        return "extract_" + str(self.abs_position)

    def view(self, indent=0):
        '''
        Print a text representation of this Extract schedule to stdout
        and then call the view() method of any children.

        :param int indent: Depth of indent for output text
        '''
        # Find out the name of the first (or only) kernel to be extracted
        #kernels = self.walk(self.children, Kern)
        #extrstr = "[extract='" + kernels[0].name + "']"
        extrstr = "extract_" + str(self.abs_position)
        #print(self.indent(indent) + self.coloured_text +
              #extrstr)
        print(self.indent(indent) + self.coloured_text + self._var_name)
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, parent):
        ''' Marks region for code extraction as children of the ExtractNode.
        For now it inserts comments at the position of the ExtractNode and
        after all children of the ExtractNode. These comments will later
        be replaced by calls to write out arguments of extracted kernels.
        :param parent: the parent of this Node.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        '''
        from psyclone.f2pygen import CommentGen
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " CALL write_extract_arguments(argument_list)"))
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

    # KERNEL = "kernel"
    # NODES = "nodes"
    # INVOKE = "invoke"
    # SUPPORTED_OPTIONS = [KERNEL, NODES, INVOKE]
    # _options = []
    # # A namespace manager to make sure we get unique region names
    # _namespace = NameSpace()

    # def set_options(options):
        # '''Sets the option the user required.
        # :param options: List of options selected by the user.
        # :type options: List of strings.
        # :raises GenerationError: If any option is not in SUPPORTED_OPTIONS.
        # '''
        # # Test that all options are valid
        # if options is None:
            # options = []   # Makes it easier to test
        # for index, option in enumerate(options):
            # if option not in Extractor.SUPPORTED_OPTIONS:
                # # Create a 'nice' representation of the allowed options.
                # # [1:-1] cuts out the '[' and ']' that surrounding the
                # # string of the list.
                # allowed_options = str(Extractor.SUPPORTED_OPTIONS)[1:-1]
                # raise GenerationError("Error in Extractor.setOptions: options "
                                      # "must be one of {0} but found '{1}' "
                                      # "at {2}"
                                      # .format(allowed_options,
                                              # str(option), index))
        # # When extracting a kernel code test that 
        # # a) the kernel name is provided, 
        # # b) that the invoke name is provided if the kernel is
        # #    called by more than one invoke,
        # # c) that the kernel is found in specified algorithm file.
        # if option == Extractor.KERNEL:
            # if kernel_name is "":
                # raise GenerationError("Error in Extractor.setOptions: "
                                      # "Please provide the name of the "
                                      # "kernel to extract.")
            # # elif more than one kernel with the same name in an alg file
        # # When extracting code from one or more nodes within an invoke
        # # test that the invoke name is provided.
        # if option == Extractor.NODES and invoke_name is "":
           # raise GenerationError("Error in Extractor.setOptions: "
                                 # "Please provide the name of the "
                                 # "invoke to extract the nodes from.")
        # # Test that the invoke name is provided if this option is selected
        # if option == Extractor.INVOKE and invoke_name is "":
           # raise GenerationError("Error in Extractor.setOptions: "
                                 # "Please provide the name of the "
                                 # "invoke to extract.")

        # # Store options so they can be queried later
        # Extractor._options = options


    @staticmethod
    def extract_kernel(schedule, kernel_name):
        ''' Extract function for a specific kernel and invoke '''
        # Find the kernel and invoke to extract

        etrans = ExtractRegionTrans()

        for kernel in schedule.walk(schedule.children, Kern):
            if kernel.name == kernel_name:
                extract_parent = kernel.root_at_depth(1)

        extract_schedule, _ = etrans.apply(extract_parent)

        return extract_schedule
