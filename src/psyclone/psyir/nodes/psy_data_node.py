# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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

''' This module provides support accessing. '''

from __future__ import absolute_import, print_function
from psyclone.f2pygen import CallGen, TypeDeclGen, UseGen
from psyclone.psyGen import InternalError, Kern, NameSpace, \
     NameSpaceFactory, Node


# =============================================================================
class PSyDataNode(Node):
    # pylint: disable=too-many-instance-attributes, too-many-locals
    '''
    This class can be inserted into a schedule to instrument a set of nodes.
    Instrument means that calls to an external library will be inserted
    before and after the child nodes, which will give this library access
    to fields and the fact that a region is executed. This can be used as
    example to add performance profiling calls, in-situ visualisation
    of data, or for writing fields to a file (e.g. for creating test
    cases, or using driver to run a certain kernel only)

    :param ast: reference into the fparser2 parse tree corresponding to \
                this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: a list of child nodes for this node. These will be made \
                     children of the child Schedule of this PSyDataNode.
    :type children: list of :py::class::`psyclone.psyGen.Node` \
                    or derived classes
    :param parent: the parent of this node in the PSyIR.
    :type parent: :py::class::`psyclone.psyGen.Node`
    :param (str,str) name: an optional name to use for this PSyDataNode, \
        provided as a 2-tuple containing a module name followed by a \
        local name. The pair of strings should uniquely identify a\
        region unless aggregate information is required and supported
        by the runtime library linked in.

    '''
    # PSyData interface Fortran module
    fortran_module = "psy_data_mod"
    # The symbols we import from the PSyData Fortran module
    symbols = ["PSyDataType"]
    # The use statement that we will insert. Any use of a module of the
    # same name that doesn't match this will result in a NotImplementedError
    # at code-generation time.
    use_stmt = "use psy_data_mod, only: " + ", ".join(symbols)

    # Root of the name to use for variables associated with PSyData regions
    psy_data_var = "psy_data"

    # A namespace manager to make sure we get unique region names
    _namespace = NameSpace()

    def __init__(self, ast=None, children=None, parent=None, name=None):

        # Store the name of the PSyData variable that is used for this
        # PSyData name. This allows to show the variable name in __str__
        # (and also if we would call create_name in gen(), the name would
        # change every time gen() is called).
        self._var_name = NameSpaceFactory().create().create_name("psy_data")

        if not children:
            super(PSyDataNode, self).__init__(ast=ast, children=children,
                                              parent=parent)
        else:
            node_parent = children[0].parent
            node_position = children[0].position

            # A PSyData node always contains a Schedule
            sched = self._insert_schedule(children)

            super(PSyDataNode, self).__init__(ast=ast, children=[sched],
                                              parent=parent)

            # Correct the parent's list of children. Use a slice of the list
            # of nodes so that we're looping over a local copy of the list.
            # Otherwise things get confused when we remove children from
            # the list.
            for child in children[:]:
                # Remove child from the parent's list of children
                node_parent.children.remove(child)

            # Add this node as a child of the parent
            # of the nodes being enclosed and at the original location
            # of the first of these nodes
            node_parent.addchild(self, index=node_position)

        # Name and colour to use for this node - must be set after calling
        # the constructor
        self._text_name = "PSyData"
        self._colour_key = "PSyData"

        # Name of the region. In general at constructor time we might
        # not have a parent subroutine or a child for the kernel, so
        # the name is left empty, unless explicitly provided by the
        # user. If names are not provided here then the region and
        # module names are set the first time gen() is called (and
        # then remain unchanged).
        self._module_name = None
        self._region_name = None
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

    # -------------------------------------------------------------------------
    @property
    def region_name(self):
        ''':returns: the region name for this node.
        :rtype: str'''
        return self._region_name
    # -------------------------------------------------------------------------
    @property
    def module_name(self):
        ''':returns: the module name for this node.
        :rtype: str'''
        return self._module_name

    # -------------------------------------------------------------------------
    def __str__(self):
        ''' Returns a string representation of the subtree starting at
        this node. '''
        result = "{0}Start[var={1}]\n".format(self._text_name, self._var_name)
        for child in self.psy_data_body.children:
            result += str(child)+"\n"
        return result+"{0}End[var={1}]".format(self._text_name, self._var_name)

    # -------------------------------------------------------------------------
    @property
    def psy_data_body(self):
        '''
        :returns: the Schedule associated with this PSyData region.
        :rtype: :py:class:`psyclone.psyGen.Schedule`

        :raises InternalError: if this PSyData node does not have a Schedule \
                               as its one and only child.
        '''
        from psyclone.psyGen import Schedule
        if len(self.children) != 1 or not \
           isinstance(self.children[0], Schedule):
            raise InternalError(
                "PSyData node malformed or incomplete. It should have a "
                "single Schedule as a child but found: {0}"
                .format([type(child).__name__ for child in self.children]))
        return self.children[0]

    # -------------------------------------------------------------------------
    def _add_call(self, name, parent, arguments=None):
        '''This function adds a call to the specified method of
        self._var_name to the parent.

        :param str name: name of the method to call.
        :param parent: parent node into which to insert the calls.
        :type parent: :py:class:`psyclone.psyGen.Node`
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
        '''Creates the PSyData code before and after the surrounded children
        of this node.

        :param parent: the parent of this node.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param options["pre-var-list"]: a list of variables to be extracted \
            before the first child.
        :type options["pre-var-list"]: list of str
        :param options["post-var-list"]: a list of variables to be extracted \
            after the last child.
        :type options["post-var-list"]: list of str
        :type str options['pre-far-postfix]: an optional postfix that will \
            be added to each variable name in the pre-var-list.
        :type str options['post-var-postfix]: an optional postfix that will \
            be added to each variable name in the post-var-list.

        '''
        if self.module_name is None:
            # The user has not supplied a module (location) name so
            # return the psy-layer module name as this will be unique
            # for each PSyclone algorithm file.
            self._module_name = self.root.invoke.invokes.psy.name

        if self.region_name is None:
            # The user has not supplied a region name (to identify
            # this particular invoke region). Use the invoke name as a
            # starting point.
            self._region_name = self.root.invoke.name
            kerns = self.walk(Kern)
            if len(kerns) == 1:
                # This PSyData region only has one kernel within it,
                # so append the kernel name.
                self._region_name += ":{0}".format(kerns[0].name)
            # Add a region index to ensure uniqueness when there are
            # multiple regions in an invoke.
            psy_data_nodes = self.root.walk(PSyDataNode)
            idx = psy_data_nodes.index(self)
            self._region_name += ":r{0}".format(idx)

        if not options:
            options = {}

        pre_variable_list = options.get("pre-var-list", [])
        post_variable_list = options.get("post-var-list", [])
        pre_suffix = options.get("pre-var-postfix", "")
        post_suffix = options.get("post-var-postfix", "")

        # Note that adding a use statement makes sure it is only
        # added once, so we don't need to test this here!
        use = UseGen(parent, self.fortran_module, only=True,
                     funcnames=PSyDataNode.symbols)
        parent.add(use)
        var_decl = TypeDeclGen(parent, datatype="PSyDataType",
                               entity_decls=[self._var_name],
                               save=True)
        parent.add(var_decl)

        self._add_call("PreStart", parent,
                       ["\"{0}\"".format(self.module_name),
                        "\"{0}\"".format(self.region_name),
                        len(pre_variable_list),
                        len(post_variable_list)])
        has_var = pre_variable_list or post_variable_list

        # All variable names get a postfix of "-post" or "-pr".
        # This enables the netcdf file to store variables that
        # read read-write (i.e. pre and post), and also avoids
        # potential name clashes (e.g. when using the PSyData
        # interface to create a netcdf file), since a Fortran
        # variable name cannot contain a "-".
        if has_var:
            for var_name in pre_variable_list:
                self._add_call("PreDeclareVariable", parent,
                               ["\"{0}{1}\"".format(var_name, pre_suffix),
                                "{0}".format(var_name)])
            for var_name in post_variable_list:
                self._add_call("PreDeclareVariable", parent,
                               ["\"{0}{1}\"".format(var_name, post_suffix),
                                "{0}".format(var_name)])

            self._add_call("PreEndDeclaration", parent)

            for var_name in pre_variable_list:
                self._add_call("ProvideVariable", parent,
                               ["\"{0}{1}\"".format(var_name, pre_suffix),
                                "{0}".format(var_name)])

            self._add_call("PreEnd", parent)

        for child in self.psy_data_body:
            child.gen_code(parent)

        if has_var:
            # Only add PostStart() if there is at least one variable.
            self._add_call("PostStart", parent)
            for var_name in post_variable_list:
                self._add_call("ProvideVariable", parent,
                               ["\"{0}{1}\"".format(var_name, post_suffix),
                                "{0}".format(var_name)])

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
        from fparser.two.utils import walk_ast
        from fparser.two import Fortran2003
        from psyclone.psyGen import object_index

        # Ensure child nodes are up-to-date
        super(PSyDataNode, self).update()

        # Get the parse tree of the routine containing this region
        # pylint: disable=protected-access
        ptree = self.root.invoke._ast
        # pylint: enable=protected-access
        # Rather than repeatedly walk the tree, we do it once for all of
        # the node types we will be interested in...
        node_list = walk_ast([ptree], [Fortran2003.Main_Program,
                                       Fortran2003.Subroutine_Stmt,
                                       Fortran2003.Function_Stmt,
                                       Fortran2003.Specification_Part,
                                       Fortran2003.Use_Stmt,
                                       Fortran2003.Name])
        if self.module_name:
            routine_name = self.module_name
        else:
            for node in node_list:
                if isinstance(node, (Fortran2003.Main_Program,
                                     Fortran2003.Subroutine_Stmt,
                                     Fortran2003.Function_Stmt)):
                    names = walk_ast([node], [Fortran2003.Name])
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

        # Get the existing use statements
        found = False
        for node in node_list[:]:
            if isinstance(node, Fortran2003.Use_Stmt) and \
               self.fortran_module == str(node.items[2]).lower():
                # Check that the use statement matches the one we would
                # insert (i.e. the code doesn't already contain a module
                # with the same name as that used by the PSyData API)
                if str(node).lower() != self.use_stmt.lower():
                    raise NotImplementedError(
                        "Cannot add PSyData calls to '{0}' because it "
                        "already 'uses' a module named '{1}'".format(
                            routine_name, self.fortran_module))
                found = True
                # To make our check on name clashes below easier, remove
                # the Name nodes associated with this use from our
                # list of nodes.
                names = walk_ast([node], [Fortran2003.Name])
                for name in names:
                    node_list.remove(name)

        if not found:
            # We don't already have a use for the PSyData module so
            # add one.
            reader = FortranStringReader(
                "use psy_data_mod, only: PSyDataType")
            # Tell the reader that the source is free format
            reader.set_format(FortranFormat(True, False))
            use = Fortran2003.Use_Stmt(reader)
            spec_part.content.insert(0, use)

        # Check that we won't have any name-clashes when we insert the
        # symbols required for the PSyData API. This check uses the list of
        # symbols that we created before adding the `use psy_data_mod...`
        # statement.
        if not self.root.psy_data_name_clashes_checked:
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
                                              self.fortran_module))
                    # Check for the names of PSyData variables
                    if text.startswith(self.psy_data_var):
                        raise NotImplementedError(
                            "Cannot add PSyData calls to '{0}' because it "
                            "already contains symbols that potentially clash "
                            "with the variables we will insert for each "
                            "PSyData region ('{1}*').".
                            format(routine_name, self.psy_data_var))
        # Flag that we have now checked for name clashes so that if there's
        # more than one PSyData node we don't fall over on the symbols
        # we've previous inserted.
        self.root.psy_data_name_clashes_checked = True

        # Create a name for this region by finding where this PSyDataNode
        # is in the list of PSyDataNodes in this Invoke.
        sched = self.root
        pnodes = sched.walk(PSyDataNode)
        region_idx = pnodes.index(self)
        if self.region_name:
            region_name = self.region_name
        else:
            region_name = "r{0}".format(region_idx)
        var_name = "psy_data{0}".format(region_idx)

        # Create a variable for this PSyData region
        reader = FortranStringReader(
            "type(PSyDataType), save :: {0}".format(var_name))
        # Tell the reader that the source is free format
        reader.set_format(FortranFormat(True, False))
        decln = Fortran2003.Type_Declaration_Stmt(reader)
        spec_part.content.append(decln)

        # Find the parent in the parse tree - first get a pointer to the
        # AST for the content of this region.
        content_ast = self.psy_data_body.children[0].ast
        # Now store the parent of this region
        # pylint: disable=protected-access
        fp_parent = content_ast._parent
        # pylint: enable=protected-access
        # Find the location of the AST of our first child node in the
        # list of child nodes of our parent in the fparser parse tree.
        ast_start_index = object_index(fp_parent.content,
                                       content_ast)
        # Finding the location of the end is harder as it might be the
        # end of a clause within an If or Select block. We therefore
        # work back up the fparser2 parse tree until we find a node that is
        # a direct child of the parent node.
        ast_end_index = None
        if self.psy_data_body[-1].ast_end:
            ast_end = self.psy_data_body[-1].ast_end
        else:
            ast_end = self.psy_data_body[-1].ast
        # Keep a copy of the pointer into the parse tree in case of errors
        ast_end_copy = ast_end

        while ast_end_index is None:
            try:
                ast_end_index = object_index(fp_parent.content,
                                             ast_end)
            except ValueError:
                # ast_end is not a child of fp_parent so go up to its parent
                # and try again
                # pylint: disable=protected-access
                if hasattr(ast_end, "_parent") and ast_end._parent:
                    ast_end = ast_end._parent
                # pylint: enable=protected-access
                else:
                    raise InternalError(
                        "Failed to find the location of '{0}' in the fparser2 "
                        "Parse Tree:\n{1}\n".format(str(ast_end_copy),
                                                    str(fp_parent.content)))

        # Add the PSyData end call
        reader = FortranStringReader(
            "CALL {0}%PostEnd".format(var_name))
        # Tell the reader that the source is free format
        reader.set_format(FortranFormat(True, False))
        pecall = Fortran2003.Call_Stmt(reader)
        fp_parent.content.insert(ast_end_index+1, pecall)

        # Add the PSyData start call
        reader = FortranStringReader(
            "CALL {2}%PreStart('{0}', '{1}', 0, 0)".format(
                routine_name, region_name, var_name))
        reader.set_format(FortranFormat(True, False))
        pscall = Fortran2003.Call_Stmt(reader)
        fp_parent.content.insert(ast_start_index, pscall)
