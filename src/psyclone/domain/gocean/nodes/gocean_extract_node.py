# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council
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
from psyclone.configuration import Config, GOceanConfig
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
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param options: a dictionary with options for transformations.
    :type options: dictionary of string:values or None
    :param bool options["create-driver"]: whether or not to create a driver \
        program at code-generation time. If set, the driver will be created \
        in the current working directory with the name \
        "driver-MODULE-REGION.f90" where MODULE and REGION will be the \
        corresponding values for this region. Defaults to False.
    :param str options["prefix"]: a prefix to use for the PSyData module name \
        (``prefix_psy_data_mod``) and the PSyDataType \
        (``prefix_PSyDataType``) - a "_" will be added automatically. \
        It defaults to "extract", which is set in the base class if \
        not overwritten in the options dictionary.

    '''
    def __init__(self, ast=None, children=None, parent=None,
                 options=None):
        super(GOceanExtractNode, self).__init__(ast=ast, children=children,
                                                parent=parent,
                                                options=options)
        if options:
            self._create_driver = options.get("create_driver", False)
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

    # -------------------------------------------------------------------------
    def update_vars_and_postname(self):
        '''
        This function prevents any name clashes that can occur when adding
        the postfix to output variable names (e.g. if there is an output
        variable 'a', and an input variable 'a_post'. Writing the output value
        of 'a' would create the key 'a_post', same as the input variable).
        We change the postfix to make sure we have unique keys (by adding a
        number to the end, e.g. "post0", then "post1", ...).

        '''
        suffix = ""
        while any(out_var+self._post_name+str(suffix) in self._input_list
                  for out_var in self._output_list):
            if suffix == "":
                suffix = 0
            else:
                suffix += 1
        self._post_name = self._post_name+str(suffix)

        super(GOceanExtractNode, self).update_vars_and_postname()

    # -------------------------------------------------------------------------
    def gen_code(self, parent):
        '''
        Generates the code required for extraction of one or more Nodes.
        It uses the PSyData API (via the base class ExtractNode) to create
        the required callbacks that will allow a library to write the
        kernel data to a file. If requested, it will also trigger the
        creation of a stand-alone driver program, which can read in the
        extracted data.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`.
        '''

        super(GOceanExtractNode, self).gen_code(parent)
        if self._create_driver:
            input_list = self.input_list
            output_list = self.output_list
            self.create_driver(input_list, output_list)

    # -------------------------------------------------------------------------
    def create_driver(self, input_list, output_list):
        # pylint: disable=too-many-locals, too-many-statements
        '''This function creates a driver that can read the
        output created by the extraction code. This is a stand-alone
        program that will read the input data, calls the kernels/
        instrumented region, and then compares the results with the
        stored results in the file.

        TODO: #644: we need type information here.

        :param input_list: list of variables that are input parameters.
        :type input_list: list of str
        :param output_list: list of variables that are output parameters.
        :type output_list: list or str
        '''

        from psyclone.f2pygen import AllocateGen, AssignGen, CallGen,\
            CommentGen, DeclGen, ModuleGen, SubroutineGen, UseGen, \
            TypeDeclGen
        from psyclone.gocean1p0 import GOSymbolTable
        from psyclone.psyir.symbols import Symbol

        all_vars = list(set(input_list).union(set(output_list)))
        all_vars.sort()

        module_name, region_name = self.region_identifier
        module = ModuleGen(name=module_name)
        prog = SubroutineGen(parent=module, name=module_name+"_code",
                             implicitnone=True)
        module.add(prog)
        use = UseGen(prog, self.add_psydata_class_prefix("psy_data_mod"),
                     only=True,
                     funcnames=[self.add_psydata_class_prefix("PSyDataType")])
        prog.add(use)

        # Use a symbol table to make sure all variable names are unique
        sym_table = GOSymbolTable()
        sym = Symbol("PSyDataType")
        sym_table.add(sym)

        psy_data = sym_table.new_symbol_name(self.add_psydata_class_prefix
                                             ("psy_data"))
        sym_table.add(Symbol(psy_data))
        var_decl = TypeDeclGen(prog, datatype=self.add_psydata_class_prefix
                               ("PSyDataType"),
                               entity_decls=[psy_data])
        prog.add(var_decl)

        call = CallGen(prog,
                       "{0}%OpenRead(\"{1}\", \"{2}\")"
                       .format(psy_data, module_name, region_name))
        prog.add(call)

        post_suffix = self._post_name

        # Variables might need to be renamed in order to guarantee unique
        # variable names in the driver: An example of this would be if the
        # user code contains a variable 'dx', and the kernel takes a
        # property 'dx' as well. In the original code that is no problem,
        # since the property is used via field%grid%dx. But the stand-alone
        # driver renames field%grid%dx to dx, which can cause a name clash.
        # Similar problems can exist with any user defined type, since all
        # user defined types are rewritten to just use the field name.
        # We use a mapping to support renaming of variables: it takes as
        # key the variable as used in the original program (e.g. 'dx' from
        # an expression like field%grid%dx), and maps it to a unique local
        # name (e.g. dx_0).

        rename_variable = {}
        for var_name in all_vars:
            # TODO #644: we need to identify arrays!!
            # Support GOcean properties, which are accessed via a
            # derived type (e.g. 'fld%grid%dx'). In this stand-alone
            # driver we don't have the derived type, instead we create
            # variable based on the field in the derived type ('dx'
            # in the example above), and pass this variable to the
            # instrumented code.
            last_percent = var_name.rfind("%")
            if last_percent > -1:
                # Strip off the derived type, and only leave the last
                # field, which is used as the local variable name.
                local_name = var_name[last_percent+1:]
            else:
                # No derived type, so we can just use the
                # variable name directly in the driver
                local_name = var_name
            unique_local_name = sym_table.new_symbol_name(local_name)
            rename_variable[local_name] = unique_local_name
            sym_table.add(Symbol(unique_local_name))
            local_name = unique_local_name

            # TODO: #644 - we need to identify arrays!!
            # Any variable used needs to be defined. We also need
            # to handle the kind property better and not rely on
            # a hard-coded value.
            decl = DeclGen(prog, "real", [local_name], kind="8",
                           dimension=":,:", allocatable=True)
            prog.add(decl)
            is_input = var_name in input_list
            is_output = var_name in output_list

            if is_input and not is_output:
                # We only need the pre-variable, and we can read
                # it from the file (this call also allocates space for it).
                call = CallGen(prog,
                               "{0}%ReadVariable(\"{1}\", {2})"
                               .format(psy_data, var_name, local_name))
                prog.add(call)
            elif is_input:
                # Now must be input and output:
                # First read the pre-variable (which also allocates it):
                call = CallGen(prog,
                               "{0}%ReadVariable(\"{1}\", {2})"
                               .format(psy_data, var_name, local_name))
                prog.add(call)
                # Then declare the post variable, and and read its values
                # (ReadVariable will also allocate it)
                sym = Symbol(local_name+post_suffix)
                sym_table.add(sym)
                decl = DeclGen(prog, "real", [local_name+post_suffix],
                               dimension=":,:", kind="8", allocatable=True)
                prog.add(decl)
                call = CallGen(prog,
                               "{0}%ReadVariable(\"{1}{3}\", {2}{3})"
                               .format(psy_data, var_name, local_name,
                                       post_suffix))
                prog.add(call)
            else:
                # Now the variable is output only. We need to read the
                # post variable in, and create and allocate a pre variable
                # with the same size as the post
                sym = Symbol(local_name+post_suffix)
                sym_table.add(sym)
                decl = DeclGen(prog, "real", [local_name+post_suffix],
                               dimension=":,:", kind="8", allocatable=True)
                prog.add(decl)
                call = CallGen(prog,
                               "{0}%ReadVariable(\"{1}{3}\", {2}{3})"
                               .format(psy_data, var_name, local_name,
                                       post_suffix))
                prog.add(call)
                decl = DeclGen(prog, "real", [local_name], kind="8",
                               dimension=":,:", allocatable=True)
                prog.add(decl)
                alloc = AllocateGen(prog, [var_name],
                                    mold="{0}".format(local_name +
                                                      post_suffix))
                prog.add(alloc)
                # Initialise the variable with 0, since it might contain
                # values that are not set at all (halo regions, or a
                # kernel might not set all values). This way the array
                # comparison with the post value works as expected
                # TODO #644 - create the right "0.0" type here (e.g.
                # 0.0d0, ...)
                assign = AssignGen(prog, local_name, "0.0d0")
                prog.add(assign)

        # Now add the region that was extracted here:
        prog.add(CommentGen(prog, ""))
        prog.add(CommentGen(prog, " RegionStart"))

        # For the driver we have to re-create the code of the
        # instrumented region, but in this stand-alone driver the
        # arguments are not dl_esm_inf fields anymore, but simple arrays.
        # Similarly, for properties we cannot use e.g. 'fld%grid%dx'
        # anymore, we have to use e.g. a local variable 'dx' that has
        # been created. Since we are using the existing way of creating
        # the code for the instrumented region, we need to modify how
        # these variables are created. We do this by temporarily
        # modifying the properties in the config file.
        api_config = Config.get().api_conf("gocean1.0")
        all_props = api_config.grid_properties
        # Keep a copy of the original values, so we can restore
        # them later
        orig_props = dict(all_props)

        # 1) A grid property is defined like "{0}%grid%dx". This is
        #    changed to be just 'dx', i.e. the final component of
        #    the current value (but we also take renaming into account,
        #    so 'dx' might become 'dx_0').
        #    If a property is not used, it doesn't matter if we modify
        #    its definition, so we just change all properties.
        for name, prop in all_props.items():
            last_percent = prop.fortran.rfind("%")
            if last_percent > -1:
                # Get the last field name, which will be the
                # local variable name
                local_name = prop.fortran[last_percent+1:]
                unique_name = rename_variable.get(local_name, local_name)
                all_props[name] = GOceanConfig.make_property(unique_name,
                                                             prop.type)

        # 2) The property 'grid_data' is a reference to the data on the
        #    grid (i.e. the actual field) , and it is defined as "{0}%data".
        #    This just becomes {0} ('a_fld%data' in the original program
        #    becomes just 'a_fld', and 'a_fld' is declared to be a plain
        #    Fortran 2d-array)
        all_props["go_grid_data"] = GOceanConfig.make_property("{0}", "array")

        # Each kernel caches the argument code, so we also
        # need to clear this cached data to make sure the new
        # value for "go_grid_data" is actually used.
        from psyclone.psyGen import CodedKern
        for kernel in self.psy_data_body.walk(CodedKern):
            kernel.clear_cached_data()

        # Recreate the instrumented region. Due to the changes in the
        # config files, fields and properties will now become local
        # plain arrays and variables:
        for child in self.psy_data_body:
            child.gen_code(prog)

        # Now reset all properties back to the original values:
        for name in all_props.keys():
            all_props[name] = orig_props[name]

        prog.add(CommentGen(prog, " RegionEnd"))
        prog.add(CommentGen(prog, ""))

        for var_name in output_list:
            prog.add(CommentGen(prog, " Check {0}".format(var_name)))

        code = str(module.root)

        with open("driver-{0}-{1}.f90".
                  format(module_name, region_name), "w") as out:
            out.write(code)
