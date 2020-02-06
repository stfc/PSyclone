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
# Author J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''
This module contains the GOcean-specific implementation of the ExtractNode.
It mostly adds code to create a driver program that can read the created
output files.
'''


from __future__ import absolute_import, print_function
from psyclone.configuration import Config
from psyclone.psyir.nodes import ExtractNode


class GOceanExtractNode(ExtractNode):
    '''
    This is the Gocean-specific implementation of the extract node.
    It adds a function 'generate_driver' which creates a GOcean-specific
    stand-alone driver program that can read the created output files.

    :param ast: reference into the fparser2 parse tree corresponding to \
        this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyGen.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyGen.Node`
    :param options: a dictionary with options for transformations.
    :type options: dictionary of string:values or None
    :param bool options["create-driver"]: If at code creation time a driver \
        program should be created. If set, the driver will be created in the \
        current working directory with the name "driver-MODULE-REGION.f90" \
        where MODULE and REGION will be the corresponding values for this \
        region.

    '''
    def __init__(self, ast=None, children=None, parent=None,
                 options=None):
        super(GOceanExtractNode, self).__init__(ast=ast, children=children,
                                                parent=parent,
                                                options=options)
        if options:
            self._create_driver = options.get("create-driver", False)
        else:
            self._create_driver = False

    @property
    def dag_name(self):
        '''
        Returns the name to use in a DAG for this Node

        :returns: the dag name of ExtractNode.
        :rtype: str
        '''
        return "gocean_extract_" + str(self.position)

    def gen_code(self, parent):
        '''
        Generates the code required for extraction of one or more Nodes. \
        For now it inserts comments before and after the code belonging \
        to all the children of this ExtractNode.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        '''

        input_list, output_list = \
            super(GOceanExtractNode, self).gen_code(parent)
        if self._create_driver:
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

        from collections import namedtuple
        from psyclone.f2pygen import AllocateGen, AssignGen, CallGen,\
            CommentGen, DeclGen, ModuleGen, SubroutineGen, UseGen, \
            TypeDeclGen

        all_vars = list(set(input_list).union(set(output_list)))
        all_vars.sort()

        module_name, region_name = self.region_identifier
        module = ModuleGen(name=module_name)
        prog = SubroutineGen(parent=module, name=module_name+"_code")
        module.add(prog)
        use = UseGen(prog, "psy_data_mod", only=True,
                     funcnames=["PSyDataType"])
        prog.add(use)

        var_decl = TypeDeclGen(prog, datatype="PSyDataType",
                               entity_decls=["psy_data"])
        prog.add(var_decl)

        call = CallGen(prog,
                       "psy_data%OpenRead(\"{0}\", \"{1}\")"
                       .format(module_name, region_name))
        prog.add(call)

        post_suffix = self._post_name
        for var_name in all_vars:
            # TODO: we need to identify arrays!!
            # Any variable used needs to be defined.

            # In case of a derived type, we need to get a 'local'
            # name (since we don't have the derived type) to be
            # used to store the values.
            last_percent = var_name.rfind("%")
            if last_percent > -1:
                local_name = var_name[last_percent+1:]
            else:
                # No derived type, so we can just use the
                # variable name directly in the driver
                local_name = var_name
            decl = DeclGen(prog, "real", [local_name], kind="8",
                           dimension=":,:", allocatable=True)
            prog.add(decl)
            is_input = var_name in input_list
            is_output = var_name in output_list

            if is_input and not is_output:
                # We only need the pre-variable, and we can read
                # it from the file (and allocate its size)
                call = CallGen(prog,
                               "psy_data%ReadVariable(\"{0}\", {1})"
                               .format(var_name, local_name))
                prog.add(call)
            elif is_input:
                # Now must be input and output:
                # First read the pre-variable
                call = CallGen(prog,
                               "psy_data%ReadVariable(\"{0}\", {1})"
                               .format(var_name, local_name))
                prog.add(call)
                # Then declare the post variable, and and read its values
                # (ReadVariable will also allocate it)
                decl = DeclGen(prog, "real", [local_name+post_suffix],
                               dimension=":,:", kind="8", allocatable=True)
                prog.add(decl)
                call = CallGen(prog,
                               "psy_data%ReadVariable(\"{0}{2}\", {1}{2})"
                               .format(var_name, local_name, post_suffix))
                prog.add(call)
            else:
                # Now the variable is output only. We need to read the
                # post variable in, and allocate the pre variable with
                # the same size as the post
                decl = DeclGen(prog, "real", [local_name+post_suffix],
                               dimension=":,:", kind="8", allocatable=True)
                prog.add(decl)
                call = CallGen(prog,
                               "psy_data%ReadVariable(\"{0}{2}\", {1}{2})"
                               .format(var_name, local_name, post_suffix))
                prog.add(call)
                decl = DeclGen(prog, "real", [local_name], kind="8",
                               dimension=":,:", allocatable=True)
                prog.add(decl)
                alloc = AllocateGen(prog, [var_name,
                                           "mold={0}".format(local_name +
                                                             post_suffix)])
                prog.add(alloc)
                # Initialise the variable with 0, since it might contain
                # values that are not set at all (halo regions, or a
                # kernel might not set all values). This way the array
                # comparison with the post value works as expected
                assign = AssignGen(prog, local_name, "0.0")
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

        props = api_config.grid_properties
        old_data_property = props["go_grid_data"]
        Property = namedtuple("Property", "fortran type")
        props["go_grid_data"] = Property("{0}", "array")

        from psyclone.psyGen import CodedKern
        for kernel in self.psy_data_body.walk(CodedKern):
            # Clear cached data in all kernels, which will
            # mean the new value for go_grid_data will be used:
            kernel.clear_cached_data()

        # Recreate the instrumented region:
        for child in self.psy_data_body:
            child.gen_code(prog)

        # Reset the go_grid_data property back to its original value.
        props["go_grid_data"] = old_data_property

        prog.add(CommentGen(prog, " RegionEnd"))
        prog.add(CommentGen(prog, ""))

        for var_name in output_list:
            prog.add(CommentGen(prog, " Check {0}".format(var_name)))

        code = str(module.root)

        with open("driver-{0}-{1}.f90".
                  format(module_name, region_name), "w") as out:
            out.write(code)
