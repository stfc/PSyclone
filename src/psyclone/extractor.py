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


class Extractor(object):
    ''' This class wraps settings for code extraction. '''

    KERNEL = "kernel"
    NODES = "nodes"
    INVOKE = "invoke"
    SUPPORTED_OPTIONS = [KERNEL, NODES, INVOKE]
    _options = []
    # A namespace manager to make sure we get unique region names
    _namespace = NameSpace()

    def set_options(options):
        '''Sets the option the user required.
        :param options: List of options selected by the user.
        :type options: List of strings.
        :raises GenerationError: If any option is not in SUPPORTED_OPTIONS.
        '''
        # Test that all options are valid
        if options is None:
            options = []   # Makes it easier to test
        for index, option in enumerate(options):
            if option not in Extractor.SUPPORTED_OPTIONS:
                # Create a 'nice' representation of the allowed options.
                # [1:-1] cuts out the '[' and ']' that surrounding the
                # string of the list.
                allowed_options = str(Extractor.SUPPORTED_OPTIONS)[1:-1]
                raise GenerationError("Error in Extractor.setOptions: options "
                                      "must be one of {0} but found '{1}' "
                                      "at {2}"
                                      .format(allowed_options,
                                              str(option), index))
        # When extracting a kernel code test that 
        # a) the kernel name is provided, 
        # b) that the invoke name is provided if the kernel is
        #    called by more than one invoke,
        # c) that the kernel is found in specified algorithm file.
        if option == Extractor.KERNEL:
            if kernel_name is "":
                raise GenerationError("Error in Extractor.setOptions: "
                                      "Please provide the name of the "
                                      "kernel to extract.")
            # elif more than one kernel with the same name in an alg file
        # When extracting code from one or more nodes within an invoke
        # test that the invoke name is provided.
        if option == Extractor.NODES and invoke_name is "":
           raise GenerationError("Error in Extractor.setOptions: "
                                 "Please provide the name of the "
                                 "invoke to extract the nodes from.")
        # Test that the invoke name is provided if this option is selected
        if option == Extractor.INVOKE and invoke_name is "":
           raise GenerationError("Error in Extractor.setOptions: "
                                 "Please provide the name of the "
                                 "invoke to extract.")

        # Store options so they can be queried later
        Extractor._options = options

    # -------------------------------------------------------------------------
    @staticmethod
    def extract_kernel():
        '''Returns true if kernel extraction is enabled.
        :return: True if a kernel code should be extracted.
        :rtype: bool'''
        return Extractor.KERNEL in Extractor._options

    # -------------------------------------------------------------------------
    @staticmethod
    def extract_nodes():
        '''Returns true if nodes extraction is enabled.
        :return: True if nodes within an invoke should be profiled.
        :rtype: bool'''
        ext
        return Extractor.NODES in Extractor._options

    # -------------------------------------------------------------------------
    @staticmethod
    def extract_invoke():
        '''Returns true if invoke profiling is enabled.
        :return: True if invokes should be profiled.
        :rtype: bool'''
        return Extractor.INVOKE in Extractor._options

    @staticmethod
    def add_extract_nodes(schedule, loop_class):
        '''This function inserts all required Extractor Nodes (for invokes
        and kernels, as specified on the command line) into a schedule.
        :param schedule: The schedule to instrument.
        :type schedule: :py::class::`psyclone.psyGen.Schedule` or derived class
        :param loop_class: The loop class (e.g. GOLoop, DynLoop) to instrument.
        :type loop_class: :py::class::`psyclone.psyGen.Loop` or derived class.
        '''

        from psyclone.transformations import ExtractRegionTrans
        extract_trans = ExtractRegionTrans()
        if Profiler.profile_kernels():
            for i in schedule.children:
                if isinstance(i, loop_class):
                    profile_trans.apply(i)
        if Profiler.profile_invokes():
            profile_trans.apply(schedule.children)


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
        return "extract schedule"
    ##@property
    ##def dag_name(self):
        ##''' Return the base dag name for this Extract node '''
        ##return "extract_" + str(self.abs_position)

    def view(self, indent=0):
        '''
        Print a text representation of this Extract schedule to stdout
        and then call the view() method of any children.

        :param int indent: Depth of indent for output text
        '''
        print(self.indent(indent) + self.coloured_text +
              "[extract_0]")
        for entity in self._children:
            entity.view(indent=indent + 1)
    ##def view(self, indent=0):
        ##'''
        ##Print a text representation of this Extract node to stdout.

        ##:param int indent: Depth of indent for output text
        ##'''
        ##print(self.indent(indent) + self.coloured_text)
        ##for entity in self._children:
            ##entity.view(indent=indent + 1)

    def gen(self):
        from psyclone.f2pygen import ProgramGen
        program = ProgramGen("kdriver")
        self.gen_code(program)
        return program.root

    def gen_code(self, parent):
        for child in self.children:
            child.gen_code(parent)

## Get our one and only Configuration object
#_CONFIG = ConfigFactory().create()

#def generate(filename, api=""):
    #''' 
    #Generates subroutine from the extracted code. If extraction is 
    #applied to kernels then Kernel Metadata must be presented in the
    #standard Kernel format.
    #'''