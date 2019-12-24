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
# Modified by A. R. Porter, STFC Daresbury Lab
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
from psyclone.configuration import Config
from psyclone.psyGen import CodedKern
from psyclone.psyir.nodes.psy_data_node import PSyDataNode


class ExtractNode(PSyDataNode):
    '''
    This class can be inserted into a Schedule to mark Nodes for \
    code extraction using the ExtractRegionTrans transformation. By \
    applying the transformation the Nodes marked for extraction become \
    children of (the Schedule of) an ExtractNode.

    :param ast: reference into the fparser2 parse tree corresponding to \
                this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyGen.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    def __init__(self, ast=None, children=None, parent=None):
        super(ExtractNode, self).__init__(ast=ast, children=children,
                                          parent=parent)
        self._text_name = "Extract"
        self._colour_key = "Extract"

        # Define two postfixes that will be added to variable names
        # to make sure the names can be distinguished between pre-
        # and post-variables (i.e. here input and output).
        self._post_name = "_post"

    @property
    def extract_body(self):
        '''
        :returns: the Schedule associated with this ExtractNode.
        :rtype: :py:class:`psyclone.psyGen.Schedule`

        :raises InternalError: if this node does not have a single Schedule as\
                               its child.
        '''
        return super(ExtractNode, self).psy_data_body

    @property
    def dag_name(self):
        '''
        Returns the name to use in a DAG for this Node

        :returns: the dag name of ExtractNode.
        :rtype: str
        '''
        return "extract_" + str(self.position)

    def gen_code(self, parent):
        # pylint: disable=arguments-differ
        '''
        Generates the code required for extraction of one or more Nodes. \
        For now it inserts comments before and after the code belonging \
        to all the children of this ExtractNode. These comments will be \
        replaced by calls to write out arguments of extracted Node(s) or \
        Kernel(s) in Issue #234.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        '''

        # Determine the variables to write:
        from psyclone.psyir.tools.dependency_tools import DependencyTools
        dep = DependencyTools()
        input_list, output_list = dep.get_in_out_parameters(self)
        options = {'pre-var-list': input_list,
                   'post-var-list': output_list,
                   'post-var-postfix': self._post_name}

        from psyclone.f2pygen import CommentGen
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractStart"))
        parent.add(CommentGen(parent, ""))
        super(ExtractNode, self).gen_code(parent, options)
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractEnd"))
        parent.add(CommentGen(parent, ""))

        self.create_driver(input_list, output_list)

    # -------------------------------------------------------------------------
    def create_driver(self, input_list, output_list):
        # pylint: disable=too-many-locals, too-many-statements
        '''This function creates a driver that can read the
        output created by the extractopm process. This is a stand-alone
        program that will read the input data, calls the kernels/
        instrumented region, and then compares the results with the
        stored results in the file.

        :param input_list: list of variables that are input parameters.
        :type input_list: list of str
        :param output_list: list of variables that are output parameters.
        :type output_list: list or str
        '''

        from psyclone.f2pygen import AllocateGen, AssignGen, CallGen,\
            CommentGen, DeclGen, ModuleGen, SubroutineGen, UseGen, \
            TypeDeclGen

        all_vars = list(set(input_list).union(set(output_list)))
        all_vars.sort()

        name = self.module_name + self.region_name
        module = ModuleGen(name=name)
        prog = SubroutineGen(parent=module, name=name+"_code")
        module.add(prog)
        use = UseGen(prog, "psy_data_mod", only=True,
                     funcnames=["PSyDataType"])
        prog.add(use)

        var_decl = TypeDeclGen(prog, datatype="PSyDataType",
                               entity_decls=["psy_data"])
        prog.add(var_decl)

        call = CallGen(prog,
                       "psy_data%OpenRead(\"{0}\", \"{1}\")"
                       .format(self.module_name, self.region_name))
        prog.add(call)

        post_suffix = self._post_name
        for var_name in all_vars:
            # TODO: we need to identify arrays!!
            # Any variable used needs to be defined.
            decl = DeclGen(prog, "real", [var_name], kind="8",
                           dimension=":,:", allocatable=True)
            prog.add(decl)
            is_input = var_name in input_list
            is_output = var_name in output_list

            if is_input and not is_output:
                # We only need the pre-variable, and we can read
                # it from the file (and allocate its size)
                call = CallGen(prog,
                               "psy_data%ReadVariable(\"{0}\", {0})"
                               .format(var_name))
                prog.add(call)
            elif is_input:
                # Now must be input and output:
                # First read the pre-variable
                call = CallGen(prog,
                               "psy_data%ReadVariable(\"{0}\", {0})"
                               .format(var_name))
                prog.add(call)
                # Then declare the post variable, and allocate it using read
                decl = DeclGen(prog, "real", [var_name+post_suffix],
                               dimension=":,:", kind="8", allocatable=True)
                prog.add(decl)
                call = CallGen(prog,
                               "psy_data%ReadVariable(\"{0}\", {0})"
                               .format(var_name+post_suffix))
                prog.add(call)
            else:
                # Now the variable is output only. We need to read the
                # post variable in, and allocate the pre variable with
                # the same size as the post
                decl = DeclGen(prog, "real", [var_name+post_suffix],
                               dimension=":,:", kind="8", allocatable=True)
                prog.add(decl)
                call = CallGen(prog,
                               "psy_data%ReadVariable(\"{0}\", {0})"
                               .format(var_name+post_suffix))
                prog.add(call)
                decl = DeclGen(prog, "real", [var_name], kind="8",
                               dimension=":,:", allocatable=True)
                prog.add(decl)
                alloc = AllocateGen(prog, [var_name,
                                           "mold={0}".format(var_name +
                                                             post_suffix)])
                prog.add(alloc)
                # Initialise the variable with 0, since it might contain
                # values that are not set at all (halo regions, or a
                # kernel might not set all values). This way the array
                # comparison with the post value works as expected
                assign = AssignGen(prog, var_name, "0.0")
                prog.add(assign)

        # Now add the region that was extracted here:
        prog.add(CommentGen(prog, ""))
        prog.add(CommentGen(prog, " RegionStart"))

        # For the driver we have to re-create the code, but
        # the arguments are not fields anymore, but simple arrays.
        # So we need to make sure that the field parameters
        # are changed from "fld%data" to just "fld". This is
        # achieved by temporary changing the value of the
        # "go_grid_data" property from "{0}.%data" to just "{0}".
        # But each kernel caches the argument code, so we also
        # need to clear this cached data to make sure the new
        # value for "go_grid_data" is actually used.
        api_config = Config.get().api_conf("gocean1.0")
        props = api_config.field_properties
        old_data_property = props["go_grid_data"]
        props["go_grid_data"] = ("{0}", "array")
        for kernel in self.psy_data_body.walk(CodedKern):
            # Clear cached data in all kernels, which will
            # mean the new value for go_grid_data will be used:
            kernel.clear_cached_data()

        # Recreate the instrumented region:
        for child in self.psy_data_body:
            child.gen_code(prog)

        # Reset the go_grid_data property back to its original value.q

        props["go_grid_data"] = old_data_property

        prog.add(CommentGen(prog, " RegionEnd"))
        prog.add(CommentGen(prog, ""))

        for var_name in output_list:
            prog.add(CommentGen(prog, " Check {0}".format(var_name)))

        code = str(module.root)

        with open(name+".f90", "w") as out:
            out.write(code)
