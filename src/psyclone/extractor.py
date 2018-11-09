# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
        ''' Constructor for an ExtractNode that is inserted in a schedule.

            :param children: A list of children nodes for this node.
            :type children: A list of :py::class::`psyclone.psyGen.Node` \
            or derived classes.
            :param parent: The parent of this node.
            :type parent: A :py::class::`psyclone.psyGen.Node`.
        '''
        Node.__init__(self, children=children, parent=parent)
        self._namespace = NameSpace()

    def __str__(self):
        ''' Returns a name for an ExtractNode. '''
        result = "Extract\n"
        for child in self.children:
            result += str(child)+"\n"
        return result+"End Extract"
        #''' Returns a string representation of the subtree starting at
        #this node. '''
        #result = "ExtractStart[var={0}]\n".format(self._var_name)
        #for child in self.children:
            #result += str(child)+"\n"
        #return result+"ExtractEnd"

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
        kernels = self.walk(self.children, Kern)
        extrstr = "[extract='" + kernels[0].name + "']"
        print(self.indent(indent) + self.coloured_text +
              extrstr)
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, parent):
        # Add comment at the position of ExtractNode for now.
        # This comment will later be replaced by calls to write statements.
        from psyclone.f2pygen import CommentGen
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractStart"))
        parent.add(CommentGen(parent, " CALL write_extract_arguments(argument_list)"))
        for child in self.children:
            child.gen_code(parent)
        parent.add(CommentGen(parent, " ExtractEnd"))
        parent.add(CommentGen(parent, ""))


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

    # # -------------------------------------------------------------------------
    #@staticmethod
    #def extract_kernel():
        #'''Returns true if kernel extraction is enabled.
        #:return: True if a kernel code should be extracted.
        #:rtype: bool'''
        #return Extractor.KERNEL in Extractor._options

    # # -------------------------------------------------------------------------
    # @staticmethod
    # def extract_nodes():
        # '''Returns true if nodes extraction is enabled.
        # :return: True if nodes within an invoke should be extracted.
        # :rtype: bool'''
        # ext
        # return Extractor.NODES in Extractor._options

    # # -------------------------------------------------------------------------
    # @staticmethod
    # def extract_invoke():
        # '''Returns true if invoke extracting is enabled.
        # :return: True if invokes should be extracted.
        # :rtype: bool'''
        # return Extractor.INVOKE in Extractor._options

    @staticmethod
    def extract_kernel(schedule, kernel_name):
        ''' Extract function for a specific kernel and invoke '''
        # Find the kernel and invoke to extract

        etrans = ExtractRegionTrans()

        for kernel in schedule.walk(schedule.children, Kern):
            if kernel.name == kernel_name:
                extract_parent = kernel.root_at_depth(1)

        modified_schedule, _ = etrans.apply(extract_parent)

        # 
        print(type(extract_parent))
        extract_schedule = extract_parent.parent
        extract_schedule.view()

        kdriver_instance = KernDriver(extract_schedule)
        kdriver_prog = kdriver_instance.gen()
        #print(str(kdriver_prog))

        return modified_schedule


class KernDriver(object):

    def __init__(self, driver_schedule):
        self.name = "kernel_driver"
        self.schedule = driver_schedule

    def gen(self):
        ''' Output the kernel driver'''
        from psyclone.f2pygen import ProgramGen, CommentGen
        # Create the program
        program = ProgramGen(name="kdriver", implicitnone=True)
        # Placeholder for the declarations
        program.add(CommentGen(program, ""))
        program.add(CommentGen(program, " Argument declarations"))
        # Placeholder for the read data statements
        program.add(CommentGen(program, ""))
        program.add(CommentGen(program, " CALL read_extract_arguments(argument_list)"))
        # Create the kernel call loop
        program.add(CommentGen(program, ""))
        program.add(CommentGen(program, " Call the kernel(s)"))
        for child in self.schedule.children:
            child.gen_code(program)
        return program.root






