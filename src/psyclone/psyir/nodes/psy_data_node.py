# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Modified: A. R. Porter and S. Siso, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

''' This module implementes a PSyData node, i.e.a node that at code
creation time will create callbacks according to the PSyData API.
This is the base class for nodes that e.g. create kernel extraction
or profiling.'''

from __future__ import absolute_import, print_function
from psyclone.errors import InternalError
from psyclone.f2pygen import CallGen, TypeDeclGen, UseGen
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import Symbol, SymbolTable


# =============================================================================
class PSyDataNode(Statement):
    # pylint: disable=too-many-instance-attributes, too-many-locals
    '''
    This class can be inserted into a schedule to instrument a set of nodes.
    Instrument means that calls to an external library using the PSyData API
    will be inserted before and after the child nodes, which will give that
    library access to fields and the fact that a region is executed. This
    can be used, for example, to add performance profiling calls, in-situ
    visualisation of data, or for writing fields to a file (e.g. for creating
    test cases, or using driver to run a certain kernel only). The node
    allows specification of a class string which is used as a prefix for
    the PSyData module name (prefix_psy_data_mod) and for the PSyDataType
    (prefix_PSyDataType).

    :param ast: reference into the fparser2 parse tree corresponding to \
                this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: a list of child nodes for this node. These will be made \
                     children of the child Schedule of this PSyDataNode.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node` \
                    or derived classes
    :param parent: the parent of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param options: a dictionary with options for transformations.
    :type options: dictionary of string:values or None
    :param str options[prefix"]: a prefix to use for the PSyData module name \
        (``prefix_psy_data_mod``) and the PSyDataType \
        (``prefix_PSyDataType``) - a "_" will be added automatically. \
        It defaults to "", which means the module name used will just be \
        ``psy_data_mod``, and the data type ``PSyDataType``.
    :param (str,str) options["region_name"]: an optional name to \
        use for this PSyDataNode, provided as a 2-tuple containing a \
        module name followed by a local name. The pair of strings should \
        uniquely identify a region unless aggregate information is required \
        (and is supported by the runtime library).

    '''
    # PSyData interface Fortran module
    fortran_module = "psy_data_mod"
    # The symbols we import from the PSyData Fortran module
    symbols = ["PSyDataType"]
    # Textual description of the node.
    _children_valid_format = "Schedule"
    _text_name = "PSyData"
    _colour_key = "PSyData"

    def __init__(self, ast=None, children=None, parent=None, options=None):

        if not options:
            options = {}

        # This string stores a prefix to be used with all external PSyData
        # symbols (i.e. data types and module name), used in the
        # method 'add_psydata_class_prefix'.
        self._class_string = options.get("prefix", "")
        if self._class_string:
            self._class_string = self._class_string + "_"

        # Root of the name to use for variables associated with
        # PSyData regions
        self._psy_data_symbol_with_prefix = \
            self.add_psydata_class_prefix("psy_data")

        # The use statement that will be inserted. Any use of a module
        # of the same name that doesn't match this will result in a
        # NotImplementedError at code-generation time.
        self.use_stmt = "use {0}, only: "\
            .format(self.add_psydata_class_prefix("psy_data_mod")) + \
            ", ".join(self.add_psydata_class_prefix(symbol) for symbol in
                      PSyDataNode.symbols)

        if children:
            # We need to store the position of the original children,
            # i.e. before they are added to a schedule
            node_position = children[0].position

        # A PSyData node always contains a Schedule
        sched = self._insert_schedule(children)
        super(PSyDataNode, self).__init__(ast=ast, children=[sched],
                                          parent=parent)

        # Get or create a symbol table so we can avoid name clashes
        # when creating variables
        if parent and hasattr(self.root, 'symbol_table'):
            symtab = self.root.symbol_table
        else:
            # FIXME: This may not be a good solution
            symtab = SymbolTable()

        # Store the name of the PSyData variable that is used for this
        # PSyDataNode. This allows the variable name to be shown in str
        # (and also, calling create_name in gen() would result in the name
        # being changed every time gen() is called).
        self._var_name = symtab.new_symbol_name(
            self._psy_data_symbol_with_prefix)
        symtab.add(Symbol(self._var_name))

        if children and parent:
            # Correct the parent's list of children. Use a slice of the list
            # of nodes so that we're looping over a local copy of the list.
            # Otherwise things get confused when we remove children from
            # the list.
            for child in children[:]:
                # Remove child from the parent's list of children
                parent.children.remove(child)

            # Add this node as a child of the parent
            # of the nodes being enclosed and at the original location
            # of the first of these nodes
            parent.addchild(self, index=node_position)
        elif parent:
            parent.addchild(self)

        # Name of the region. In general at constructor time we might
        # not have a parent subroutine or any child nodes, so
        # the name is left empty, unless explicitly provided by the
        # user. If names are not provided here then the region and
        # module names are set the first time gen() is called (and
        # then remain unchanged).
        self._module_name = None
        self._region_name = None
        # The region identifier caches the computed module- and region-name
        # as a tuple of strings. This is required so that a derived class can
        # query the actual name of region (e.g. during generation of a driver
        # for an extract node). If the user does not define a name, i.e.
        # module_name and region_name are empty, a unique name will be
        # computed in gen_code(). If this name would then be stored in
        # module_name and region_name, and gen() is called again, the
        # names would not be computed again, since the code detects already
        # defined module and region names. This can then result in duplicated
        # region names: The test 'test_region' in profile_test triggers this.
        # gen()) is called first after one profile region is applied, then
        # another profile region is added, and gen() is called again. The
        # second profile region would compute a new name, which then happens
        # to be the same as the name computed for the first region in the
        # first gen_code call (which indeed implies that the name of the
        # first profile region is different the second time it is computed).
        # So in order to guarantee that the computed module and region names
        # are unique when gen_code is called more than once, we
        # cannot store a computed name in module_name and region_name.
        self._region_identifier = ("", "")

        name = options.get("region_name", None)

        if name:
            # pylint: disable=too-many-boolean-expressions
            if not isinstance(name, tuple) or not len(name) == 2 or \
               not name[0] or not isinstance(name[0], str) or \
               not name[1] or not isinstance(name[1], str):
                raise InternalError(
                    "Error in PSyDataNode. The name must be a "
                    "tuple containing two non-empty strings.")
            # pylint: enable=too-many-boolean-expressions
            # Valid PSyData names have been provided by the user.
            self._module_name = name[0]
            self._region_name = name[1]
            self.set_region_identifier(self._module_name, self._region_name)

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position == 0 and isinstance(child, Schedule)

    # -------------------------------------------------------------------------
    def add_psydata_class_prefix(self, symbol):
        '''Returns a string with the class-string as prefix, e.g. if the
        class string is "profile", then the symbol "PSyDataType" will
        become "profilePSyDataType". Typically the class_string will
        contain a trailing "_".

        :param str symbol: the symbol name to prefix with the class string.

        :returns: the symbol name with the class string as prefix.
        :rtype: str
        '''
        return self._class_string + symbol

    # -------------------------------------------------------------------------
    @property
    def region_identifier(self):
        ''':returns: the unique region identifier, which is a tuple \
            consisting of the module name and region name.
        :rtype: 2-tuple (str, str)'''
        return self._region_identifier

    # -------------------------------------------------------------------------
    def set_region_identifier(self, module_name, region_name):
        '''Defines a unique region identifier based on module- and region-name.
        Typically the names will be concatenated to create a file name or a
        region name. Since the region name is unique in the module,
        concatenating the strings will then result in a unique region name.

        :param str module_name: name of the module.
        :param str region_name: name of the region.
        '''
        self._region_identifier = (module_name, region_name)

    # -------------------------------------------------------------------------
    def __str__(self):
        ''' Returns a string representation of the subtree starting at
        this node. '''
        return_list = \
            ["{0}Start[var={1}]".format(self._text_name, self._var_name)] + \
            [str(child) for child in self.psy_data_body.children] + \
            ["{0}End[var={1}]".format(self._text_name, self._var_name)]

        return "\n".join(return_list)

    # -------------------------------------------------------------------------
    @property
    def psy_data_body(self):
        '''
        :returns: the Schedule associated with this PSyData region.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        :raises InternalError: if this PSyData node does not have a Schedule \
                               as its one and only child.
        '''
        if len(self.children) != 1 or not \
           isinstance(self.children[0], Schedule):
            raise InternalError(
                "PSyData node malformed or incomplete. It should have a "
                "single Schedule as a child but found: {0}"
                .format([type(child).__name__ for child in self.children]))
        return self.children[0]

    # -------------------------------------------------------------------------
    def _add_call(self, name, parent, arguments=None):
        '''This function adds a call to the specified (type-bound) method of
        self._var_name to the parent.

        :param str name: name of the method to call.
        :param parent: parent node into which to insert the calls.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param arguments: optional arguments for the method call.
        :type arguments: list of str or None
        '''
        call = CallGen(parent,
                       "{0}%{1}".format(self._var_name, name),
                       arguments)
        parent.add(call)

    # -------------------------------------------------------------------------
    def gen_code(self, parent, options=None):
        # pylint: disable=arguments-differ, too-many-branches
        '''Creates the PSyData code before and after the children
        of this node.

        :param parent: the parent of this node in the f2pygen AST.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param options["pre_var_list"]: a list of variables to be extracted \
            before the first child.
        :type options["pre_var_list"]: list of str
        :param options["post_var_list"]: a list of variables to be extracted \
            after the last child.
        :type options["post_var_list"]: list of str
        :param str options["pre_var_postfix"]: an optional postfix that will \
            be added to each variable name in the pre_var_list.
        :param str options["post_var_postfix"]: an optional postfix that will \
            be added to each variable name in the post_var_list.

        '''
        # TODO: #415 Support different classes of PSyData calls.
        module_name = self._module_name
        if module_name is None:
            # The user has not supplied a module (location) name so
            # return the psy-layer module name as this will be unique
            # for each PSyclone algorithm file.
            module_name = self.root.invoke.invokes.psy.name

        region_name = self._region_name
        if region_name is None:
            # The user has not supplied a region name (to identify
            # this particular invoke region). Use the invoke name as a
            # starting point.
            region_name = self.root.invoke.name
            from psyclone.psyGen import Kern
            kerns = self.walk(Kern)
            if len(kerns) == 1:
                # This PSyData region only has one kernel within it,
                # so append the kernel name.
                region_name += ":{0}".format(kerns[0].name)
            # Add a region index to ensure uniqueness when there are
            # multiple regions in an invoke.
            psy_data_nodes = self.root.walk(PSyDataNode)
            idx = psy_data_nodes.index(self)
            region_name += ":r{0}".format(idx)

        if not options:
            options = {}

        pre_variable_list = options.get("pre_var_list", [])
        post_variable_list = options.get("post_var_list", [])
        pre_suffix = options.get("pre_var_postfix", "")
        post_suffix = options.get("post_var_postfix", "")

        # Note that adding a use statement makes sure it is only
        # added once, so we don't need to test this here!
        symbols = [self.add_psydata_class_prefix(symbol)
                   for symbol in PSyDataNode.symbols]
        fortran_module = self.add_psydata_class_prefix(self.fortran_module)
        use = UseGen(parent, fortran_module, only=True,
                     funcnames=symbols)
        parent.add(use)
        var_decl = TypeDeclGen(parent,
                               datatype=self.add_psydata_class_prefix
                               ("PSyDataType"),
                               entity_decls=[self._var_name],
                               save=True, target=True)
        parent.add(var_decl)

        self._add_call("PreStart", parent,
                       ["\"{0}\"".format(module_name),
                        "\"{0}\"".format(region_name),
                        len(pre_variable_list),
                        len(post_variable_list)])
        self.set_region_identifier(module_name, region_name)
        has_var = pre_variable_list or post_variable_list

        # Each variable name can be given a suffix. The reason for
        # this feature is that a library might have to distinguish if
        # a variable is both in the pre- and post-variable list.
        # Consider a NetCDF file that is supposed to store a
        # variable that is read (i.e. it is in the pre-variable
        # list) and written (it is also in the post-variable
        # list). Since a NetCDF file uses the variable name as a key,
        # there must be a way to distinguish these two variables.
        # The application could for example give all variables in
        # the post-variable list a suffix like "_post" to create
        # a different key in the NetCDF file, allowing it to store
        # values of a variable "A" as "A" in the pre-variable list,
        # and store the modified value of "A" later as "A_post".
        if has_var:
            for var_name in pre_variable_list:
                self._add_call("PreDeclareVariable", parent,
                               ["\"{0}{1}\"".format(var_name, pre_suffix),
                                var_name])
            for var_name in post_variable_list:
                self._add_call("PreDeclareVariable", parent,
                               ["\"{0}{1}\"".format(var_name, post_suffix),
                                var_name])

            self._add_call("PreEndDeclaration", parent)

            for var_name in pre_variable_list:
                self._add_call("ProvideVariable", parent,
                               ["\"{0}{1}\"".format(var_name, pre_suffix),
                                var_name])

            self._add_call("PreEnd", parent)

        for child in self.psy_data_body:
            child.gen_code(parent)

        if has_var:
            # Only add PostStart() if there is at least one variable.
            self._add_call("PostStart", parent)
            for var_name in post_variable_list:
                self._add_call("ProvideVariable", parent,
                               ["\"{0}{1}\"".format(var_name, post_suffix),
                                var_name])

        self._add_call("PostEnd", parent)

    # -------------------------------------------------------------------------
    def gen_c_code(self, indent=0):
        '''
        Generates a string representation of this Node using C language
        (currently not supported).

        :param int indent: Depth of indent for the output string.
        :raises NotImplementedError: Not yet supported for PSyData nodes.
        '''
        raise NotImplementedError("Generation of C code is not supported "
                                  "for PSyDataNode.")

    # -------------------------------------------------------------------------

    def update(self):
        # pylint: disable=too-many-branches, too-many-statements
        # pylint: disable=too-many-locals
        '''
        Update the underlying fparser2 parse tree to implement the PSyData
        region represented by this Node. This involves adding the necessary
        module use statement as well as the calls to the PSyData API.

        TODO #435 - remove this whole method once the NEMO API uses the
        Fortran backend of the PSyIR.

        :raises NotImplementedError: if the routine which is to have \
                             PSyData calls added to it does not already have \
                             a Specification Part (i.e. some declarations).
        :raises NotImplementedError: if there would be a name clash with \
                             existing variable/module names in the code to \
                             be transformed.
        :raises InternalError: if we fail to find the node in the parse tree \
                             corresponding to the end of the PSyData region.

        '''
        from fparser.common.sourceinfo import FortranFormat
        from fparser.common.readfortran import FortranStringReader
        from fparser.two.utils import walk
        from fparser.two import Fortran2003
        from psyclone.psyGen import object_index, InvokeSchedule
        from psyclone.psyir.nodes import ProfileNode

        # The update function at this stage only supports profiling
        if not isinstance(self, ProfileNode):
            raise InternalError("PSyData.update is only supported for a "
                                "ProfileNode, not for a node of type {0}."
                                .format(type(self)))

        # Ensure child nodes are up-to-date
        super(PSyDataNode, self).update()

        # Get the parse tree of the routine containing this region
        routine_schedule = self.ancestor(InvokeSchedule)
        ptree = routine_schedule.ast

        # Rather than repeatedly walk the tree, we do it once for all of
        # the node types we will be interested in...
        node_list = walk([ptree], (Fortran2003.Main_Program,
                                   Fortran2003.Subroutine_Stmt,
                                   Fortran2003.Function_Stmt,
                                   Fortran2003.Specification_Part,
                                   Fortran2003.Use_Stmt,
                                   Fortran2003.Name))
        if self._module_name:
            routine_name = self._module_name
        else:
            for node in node_list:
                if isinstance(node, (Fortran2003.Main_Program,
                                     Fortran2003.Subroutine_Stmt,
                                     Fortran2003.Function_Stmt)):
                    names = walk([node], Fortran2003.Name)
                    routine_name = str(names[0]).lower()
                    break

        for node in node_list:
            if isinstance(node, Fortran2003.Specification_Part):
                spec_part = node
                break
        else:
            # This limitation will be removed when we use the Fortran
            # backend of the PSyIR (#435)
            raise NotImplementedError(
                "Addition of PSyData regions to routines without any "
                "existing declarations is not supported and '{0}' has no "
                "Specification-Part".format(routine_name))

        # TODO #703: Rename the PSyDataType instead of
        # aborting.
        # Get the existing use statements
        found = False
        fortran_module = self.add_psydata_class_prefix(self.fortran_module)
        for node in node_list[:]:
            if isinstance(node, Fortran2003.Use_Stmt) and \
               fortran_module == str(node.items[2]).lower():
                # Check that the use statement matches the one we would
                # insert (i.e. the code doesn't already contain a module
                # with the same name as that used by the PSyData API)
                if str(node).lower() != self.use_stmt.lower():
                    raise NotImplementedError(
                        "Cannot add PSyData calls to '{0}' because it "
                        "already 'uses' a module named '{1}'".format(
                            routine_name, fortran_module))
                found = True
                # To make our check on name clashes below easier, remove
                # the Name nodes associated with this use from our
                # list of nodes.
                names = walk([node], Fortran2003.Name)
                for name in names:
                    node_list.remove(name)

        if not found:
            # We don't already have a use for the PSyData module so
            # add one.
            reader = FortranStringReader(
                "use {0}, only: {1}"
                .format(self.add_psydata_class_prefix("psy_data_mod"),
                        self.add_psydata_class_prefix("PSyDataType")))
            # Tell the reader that the source is free format
            reader.set_format(FortranFormat(True, False))
            use = Fortran2003.Use_Stmt(reader)
            spec_part.content.insert(0, use)

        # Check that we won't have any name-clashes when we insert the
        # symbols required for the PSyData API. This check uses the list of
        # symbols that we created before adding the `use psy_data_mod...`
        # statement.
        if not routine_schedule.psy_data_name_clashes_checked:
            for node in node_list:
                if isinstance(node, Fortran2003.Name):
                    text = str(node).lower()
                    # Check for the symbols we import from the PSyData module
                    for symbol in self.symbols:
                        if text == symbol.lower():
                            raise NotImplementedError(
                                "Cannot add PSyData calls to '{0}' because it "
                                "already contains a symbol that clashes with "
                                "one of those ('{1}') that must be imported "
                                "from the PSyclone PSyData module.".
                                format(routine_name, symbol))
                    # Check for the name of the PSyData module itself
                    if text == self.fortran_module:
                        raise NotImplementedError(
                            "Cannot add PSyData calls to '{0}' because it "
                            "already contains a symbol that clashes with the "
                            "name of the PSyclone PSyData module "
                            "('{1}')". format(routine_name,
                                              fortran_module))
                    # Check for the names of PSyData variables
                    if text.startswith(self._psy_data_symbol_with_prefix):
                        raise NotImplementedError(
                            "Cannot add PSyData calls to '{0}' because it "
                            "already contains symbols that potentially clash "
                            "with the variables we will insert for each "
                            "PSyData region ('{1}*').".
                            format(routine_name,
                                   self._psy_data_symbol_with_prefix))
        # Flag that we have now checked for name clashes so that if there's
        # more than one PSyData node we don't fall over on the symbols
        # we've previous inserted.
        # TODO #435 the psy_data_name_clashes_checked attribute only exists
        # for a NemoInvokeSchedule. Since this whole `update()` routine will
        # be removed once we are able to use the PSyIR backend to re-generate
        # NEMO code, the pylint warning is disabled.
        # pylint: disable=attribute-defined-outside-init
        routine_schedule.psy_data_name_clashes_checked = True
        # pylint: enable=attribute-defined-outside-init

        # Create a name for this region by finding where this PSyDataNode
        # is in the list of PSyDataNodes in this Invoke.
        pnodes = routine_schedule.walk(PSyDataNode)
        region_idx = pnodes.index(self)
        if self._region_name:
            region_name = self._region_name
        else:
            region_name = "r{0}".format(region_idx)
        var_name = "{0}{1}".format(self.add_psydata_class_prefix("psy_data"),
                                   region_idx)

        # Create a variable for this PSyData region
        reader = FortranStringReader(
            "type({0}), target, save :: {1}"
            .format(self.add_psydata_class_prefix("PSyDataType"), var_name))
        # Tell the reader that the source is free format
        reader.set_format(FortranFormat(True, False))
        decln = Fortran2003.Type_Declaration_Stmt(reader)
        spec_part.content.append(decln)

        # Find the parent in the parse tree - first get a pointer to the
        # AST for the content of this region.
        content_ast = self.psy_data_body.children[0].ast
        # Now store the parent of this region
        fp_parent = content_ast.parent
        # Find the location of the AST of our first child node in the
        # list of child nodes of our parent in the fparser parse tree.
        ast_start_index = object_index(fp_parent.content,
                                       content_ast)
        # Do the same for our last child node
        if self.psy_data_body[-1].ast_end:
            ast_end = self.psy_data_body[-1].ast_end
        else:
            ast_end = self.psy_data_body[-1].ast

        if ast_end.parent is not fp_parent:
            raise InternalError(
                "The beginning ({0}) and end ({1}) nodes of the PSyData "
                "region in the fparser2 parse tree do not have the same "
                "parent.".format(content_ast, ast_end))
        ast_end_index = object_index(fp_parent.content, ast_end)

        # Add the PSyData end call
        reader = FortranStringReader(
            "CALL {0}%PostEnd".format(var_name))
        # Tell the reader that the source is free format
        reader.set_format(FortranFormat(True, False))
        pecall = Fortran2003.Call_Stmt(reader)
        pecall.parent = fp_parent
        fp_parent.content.insert(ast_end_index+1, pecall)

        # Add the PSyData start call
        reader = FortranStringReader(
            "CALL {2}%PreStart('{0}', '{1}', 0, 0)".format(
                routine_name, region_name, var_name))
        reader.set_format(FortranFormat(True, False))
        pscall = Fortran2003.Call_Stmt(reader)
        pscall.parent = fp_parent
        fp_parent.content.insert(ast_start_index, pscall)
        self.set_region_identifier(routine_name, region_name)
