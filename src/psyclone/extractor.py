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
invoke. The extracted code may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke (extraction
applied to all Nodes).

There is currently only one class in this module: ExtractNode (see below).

Another class which contains helper functions for code extraction, such as
wrapping up settings for generating driver for the extracted code, will
be added in Issue #298.
'''

from __future__ import absolute_import, print_function
from psyclone.psyGen import colored, Node, SCHEDULE_COLOUR_MAP


class ExtractNode(Node):
    '''
    This class can be inserted into a Schedule to mark Nodes for \
    code extraction using the ExtractRegionTrans transformation. By \
    applying the transformation the Nodes marked for extraction become \
    children of an ExtractNode.
    '''
    def __str__(self):
        '''
        Returns a string representation of the subtree starting at the \
        Extract Node.

        :returns: the Extract Node with all its children.
        :rtype: str
        '''
        result = "ExtractStart\n"
        for child in self.children:
            result += str(child)+"\n"
        return result+"ExtractEnd"

    @property
    def coloured_text(self):
        '''
        Returns a string containing the name of this Node along with \
        control characters for colouring in terminals that supports it.

        :returns: the name of this Node, possibly with control codes for \
                  colouring.
        :rtype: str
        '''
        return colored("Extract", SCHEDULE_COLOUR_MAP["Extract"])

    @property
    def dag_name(self):
        '''
        Returns the name to use in a DAG for this Node

        :returns: the dag name of ExtractNode.
        :rtype: str
        '''
        return "extract_" + str(self.position)

    def view(self, indent=0):
        '''
        Prints a text representation of the Extract tree to stdout \
        and then calls the view() method of any children.

        :param int indent: depth of indent for output text.
        '''
        print(self.indent(indent) + self.coloured_text)
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, parent):
        '''
        Generates the code required for extraction of one or more Nodes. \
        For now it inserts comments before and after the code belonging \
        to all the children of this ExtractNode. These comments will be \
        replaced by calls to write out arguments of extracted Node(s) or \
        Kernel(s) in Issue #234.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        '''
        from psyclone.f2pygen import CommentGen

        # Mark the beginning of the extract region
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractStart"))
        # Placeholder for the data write statements
        parent.add(CommentGen(
            parent, " CALL write_extract_arguments(argument_list)"))
        parent.add(CommentGen(parent, ""))
        # Generate code for all enclosed calls
        for child in self.children:
            child.gen_code(parent)
        # Mark the end of the extract region
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractEnd"))
        parent.add(CommentGen(parent, ""))

    def gen_c_code(self, indent=0):
        '''
        Generates a string representation of this Node using C language
        (currently not supported).

        :param int indent: Depth of indent for the output string.
        :raises NotImplementedError: Not yet supported for code extraction.
        '''
        raise NotImplementedError("Generation of C code is not supported "
                                  "for code extraction")


class DynamoExtractNode(ExtractNode):
    '''
    This is Dynamo0.3 class to be inserted into a Dynamo0.3 Schedule \
    that handles specifics of code generation for the extracted Nodes.
    '''
    def __init__(self, parent, children):
        '''
        :param parent: the parent of this Node in the PSyIR tree.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param children: the PSyIR nodes that are children of this Node.
        :type children: list of :py:class:`psyclone.psyGen.Node`
        '''
        super(DynamoExtractNode, self).__init__(ast=None, children=children,
                                                parent=parent)
        self._extract_arg_list = []
        self._io_lib_instance = ""

    def extract_arg_list(self):
        '''
        Returns the list of arguments of all kernel calls in this Invoke.

        :returns: a list of all of the actual arguments to all \
                  kernel calls in children Nodes.
        :rtype: list of str.
        '''
        for kern_call in self.calls():
            for kern_arg in kern_call.arguments.raw_arg_list():
                if kern_arg not in self._extract_arg_list:
                    self._extract_arg_list.append(kern_arg)

        return self._extract_arg_list

    def io_lib_instance(self, parent):
        '''
        Adds information and code to utilise the IO library for code \
        extraction, "dino_mod.F90", located in the LFRic trunk in \
        "infrastructure/source/PSYKE" directory. The library can read \
        ("input"; driver generation) or write ("output"; PSy-layer \
        generation) scalars and arrays.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        :returns: name of the instance of the IO library.
        :rtype: str.
        '''
        from psyclone.f2pygen import UseGen, TypeDeclGen, AssignGen

        # Define library name from the LFRic trunk (located in
        # "infrastructure/source/PSYKE" directory)
        io_lib_name = "dino"
        io_lib_type = io_lib_name + "_type"
        self._io_lib_instance = io_lib_name + "_lib"

        # Statements to use and declare the IO library instance
        parent.add(UseGen(parent, name=io_lib_name+"_mod", only=True,
                          funcnames=[io_lib_type]))
        parent.add(TypeDeclGen(parent, datatype=io_lib_type,
                               entity_decls=[self._io_lib_instance]))
        parent.add(AssignGen(parent, lhs=self._io_lib_instance,
                             rhs=io_lib_type+"()"))

        return self._io_lib_instance

    def gen_code(self, parent):
        '''
        Generates the code required for extraction of one or more Nodes. \
        For now it inserts comments before and after the code belonging \
        to all the children of this ExtractNode. These comments will be \
        replaced by calls to write out arguments of extracted Node(s) or \
        Kernel(s) in Issue #234.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        '''
        from psyclone.f2pygen import CommentGen, CallGen

        # Mark the beginning of the extract region
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractStart"))
        parent.add(CommentGen(parent, ""))

        # Add LFRic "dino" library to write data from enclosed calls (generic
        # functions "output_scalar" and "output_array" in dino_mod.F90)
        parent.add(CommentGen(parent, " Create an instance of 'dino' library"))
        libname = self.io_lib_instance(parent)
        parent.add(CommentGen(parent, ""))
        # Generate code for all enclosed calls
        for child in self.children:
            child.gen_code(parent)
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(
            parent, " CALL write_extract_arguments(argument_list)"))
        lib_iocall = libname + "%output"
        for arg in self.extract_arg_list():
            parent.add(CallGen(parent, name=lib_iocall, args=[str(arg)]))
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Close output data file"))
        parent.add(CallGen(parent, name=libname+"%io_close()"))
        # Mark the end of the extract region
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractEnd"))
        parent.add(CommentGen(parent, ""))
        