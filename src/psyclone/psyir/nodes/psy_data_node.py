# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
# Modified: A. R. Porter, S. Siso, R. W. Ford and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module implements a PSyData node, i.e. a node that at code
creation time will create callbacks according to the PSyData API.
This is the base class for nodes that e.g. create kernel extraction
or profiling. '''

from __future__ import absolute_import, print_function
from collections import namedtuple

from fparser.common.readfortran import FortranStringReader
from fparser.common.sourceinfo import FortranFormat
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.configuration import Config
from psyclone.errors import InternalError, GenerationError
from psyclone.f2pygen import CallGen, TypeDeclGen, UseGen
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import (SymbolTable, DataTypeSymbol, DataSymbol,
                                    ContainerSymbol, DeferredType, Symbol,
                                    UnknownFortranType, ImportInterface)


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
    :param children: the PSyIR nodes that are children of this node. These \
        will be made children of the child Schedule of this PSyDataNode.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param options: a dictionary with options for transformations.
    :type options: dictionary of string:values or None
    :param str options["prefix"]: a prefix to use for the PSyData module name \
        (``prefix_psy_data_mod``) and the PSyDataType \
        (``prefix_PSyDataType``) - a "_" will be added automatically. \
        It defaults to "", which means the module name used will just be \
        ``psy_data_mod``, and the data type ``PSyDataType``.
    :param (str,str) options["region_name"]: an optional name to \
        use for this PSyDataNode, provided as a 2-tuple containing a \
        module name followed by a local name. The pair of strings should \
        uniquely identify a region unless aggregate information is required \
        (and is supported by the runtime library).

    :raises InternalError: if a prefix is specified that is not listed in the \
        configuration file.

    '''
    #: Textual description of the node.
    _children_valid_format = "Schedule"
    _text_name = "PSyData"
    _colour = "green"
    #: The default prefix to add to the PSyData module name and PSyDataType
    _default_prefix = ""

    def __init__(self, ast=None, children=None, parent=None, options=None):

        super(PSyDataNode, self).__init__(ast=ast, children=children,
                                          parent=parent)
        if not options:
            options = {}

        # _prefix stores a prefix to be used with all external PSyData
        # symbols (i.e. data types and module name), used in the
        # method 'add_psydata_class_prefix'.
        prefix = options.get("prefix", self._default_prefix)
        # Check that the prefix is one of those listed as being supported
        # in the configuration file. If it *is* listed then it is assumed
        # that a matching PSyData wrapper library is available at compile time.
        # See the User Guide for more information:
        # https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#psy-data
        if prefix and prefix not in Config.get().valid_psy_data_prefixes:
            raise InternalError(
                f"Invalid 'prefix' parameter: found '{prefix}', expected one "
                f"of {Config.get().valid_psy_data_prefixes} as defined in "
                f"{Config.get().filename}")
        if not prefix:
            self._prefix = ""
        else:
            self._prefix = prefix + "_"

        # Create the list of symbol names that will be imported from
        # the PSyData Fortran module. We use a namedtuple to improve
        # readability. Currently there is only one imported symbol (the
        # name of the PSyData derived type) but we keep a list for future
        # extensibility.
        _PSyDataSymbol = namedtuple("_PSyDataSymbol", "name symbol_type")
        self.imported_symbols = [_PSyDataSymbol(self.type_name,
                                                DataTypeSymbol)]

        # Root of the name to use for variables associated with
        # PSyData regions
        self._psy_data_symbol_with_prefix = \
            self.add_psydata_class_prefix("psy_data")

        # The name of the PSyData variable that is used for this PSyDataNode
        self._var_name = ""

        # The region identifier caches the computed module- and region-name
        # as a tuple of strings. This is required so that a derived class can
        # query the actual name of a region (e.g. during generation of a driver
        # for an extract node). If the user does not define a name, i.e.
        # module_name and region_name are empty, a unique name will be
        # computed in gen_code() or lower_to_language_level(). If this name was
        # stored in module_name and region_name, and gen() is called again, the
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
        # Name of the region.
        self._module_name = None
        self._region_name = None

        # TODO: #1394 Fix code duplication between
        # PSyDataTrans and this PSyDataNode
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
            self.set_region_identifier(self._module_name,
                                       self._region_name)

    def __eq__(self, other):
        '''
        Checks the equality of this PSyDataNode with other. PSyDataNodes are
        equal if they are the same type, and have the same prefix, var_name,
        module_name and region_name.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.prefix == other.prefix
        is_eq = is_eq and self.var_name == other.var_name
        is_eq = is_eq and self.module_name == other.module_name
        is_eq = is_eq and self.region_name == other.region_name
        return is_eq

    @property
    def prefix(self):
        '''
        Returns the _prefix member of this PSyDataNode.

        :returns: the _prefix member of this PSyDataNode.
        :rtype: str
        '''
        return self._prefix

    @property
    def var_name(self):
        '''
        Returns the _var_name member of this PSyDataNode.

        :returns: the _var_name of this PSyDataNode.
        :rtype: str
        '''
        return self._var_name

    @property
    def module_name(self):
        '''
        Returns the _module_name of this PSyDataNode.

        :returns: the _module_name of this PSyDataNode.
        :rtype: str
        '''
        return self._module_name

    @property
    def region_name(self):
        '''
        Returns the _region_name of this PSyDataNode.

        :returns: the _region_name of this PSyDataNode.
        :rtype: str
        '''
        return self._region_name

    @classmethod
    def create(cls, children, symbol_table, ast=None, options=None):
        '''
        Creates a new (sub-class of a) PSyData node with the supplied
        'children' nodes as its children. The symbols used by the PSyData API
        are added to the supplied symbol table. This is a class method so that
        it acts as a factory for the various sub-classes of PSyDataNode.

        :param children: the PSyIR nodes that will become children of the \
            new PSyData node.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param symbol_table: the associated SymbolTable to which symbols \
            must be added.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :parent ast: reference to fparser2 parse tree for the routine being \
            instrumented with PSyData calls.
        :type ast: :py:class:`fparser.two.Fortran2003.Base`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param str options[prefix"]: a prefix to use for the PSyData module \
            name (``prefix_psy_data_mod``) and the PSyDataType \
            (``prefix_PSyDataType``) - a "_" will be added automatically. \
            It defaults to "", which means the module name used will just be \
            ``psy_data_mod``, and the data type ``PSyDataType``.
        :param (str,str) options["region_name"]: an optional name to use for \
            this PSyDataNode, provided as a 2-tuple containing a module name \
            followed by a local name. The pair of strings should uniquely \
             identify a region unless aggregate information is required \
            (and is supported by the runtime library).

        :raises TypeError: if the supplied children or symbol table are not \
            of the correct type.

        '''
        if not isinstance(children, list):
            raise TypeError(f"Error in PSyDataNode.create(). The 'children' "
                            f"argument must be a list (of PSyIR nodes) but "
                            f"got '{type(children).__name__}'")
        if children and not all(isinstance(child, Node) for child in children):
            raise TypeError(
                f"Error in PSyDataNode.create(). The 'children' argument must "
                f"be a list of PSyIR nodes but it contains: "
                f"{[type(child).__name__ for child in children]}")
        if not isinstance(symbol_table, SymbolTable):
            raise TypeError(
                f"Error in PSyDataNode.create(). The 'symbol_table' argument "
                f"must be an instance of psyir.symbols.SymbolTable but got "
                f"'{type(symbol_table).__name__}'.")

        data_node = cls(ast=ast, options=options)
        data_node.generate_symbols(symbol_table)

        # A PSyData node always contains a Schedule
        sched = Schedule(children=children, parent=data_node)
        data_node.addchild(sched)

        return data_node

    def generate_symbols(self, symbol_table):
        ''' Generate the necessary symbols to import and use this PSyDataNode
        in the provided symbol_table if they don't already exist.

        :param symbol_table: the associated SymbolTable to which symbols \
            must be added.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        # Ensure that we have a container symbol for the API access
        try:
            csym = symbol_table.lookup_with_tag(self.fortran_module)
        except KeyError:
            # The tag doesn't exist which means that we haven't already added
            # this Container as part of a PSyData transformation.
            csym = ContainerSymbol(self.fortran_module)
            symbol_table.add(csym, tag=self.fortran_module)

        # Add the symbols that will be imported from the module. Use the
        # PSyData names as tags to ensure we don't attempt to add them more
        # than once if multiple transformations are applied.
        for sym in self.imported_symbols:
            symbol_table.find_or_create_tag(sym.name,
                                            symbol_type=sym.symbol_type,
                                            interface=ImportInterface(csym),
                                            datatype=DeferredType())

        # Store the name of the PSyData variable that is used for this
        # PSyDataNode. This allows the variable name to be shown in str
        # (and also, calling create_name in gen() would result in the name
        # being changed every time gen() is called).
        if not self._var_name:
            self._var_name = symbol_table.next_available_name(
                self._psy_data_symbol_with_prefix)
            psydata_type = UnknownFortranType(
                f"type({self.type_name}), save, target :: {self._var_name}")
            symbol_table.new_symbol(self._var_name, symbol_type=DataSymbol,
                                    datatype=psydata_type,
                                    visibility=Symbol.Visibility.PRIVATE)

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
        '''Returns a string prefixed with the class-specific prefix, e.g. if
        the prefix string is "profile", then the symbol "PSyDataType" will
        become "profilePSyDataType". Typically the _prefix will
        contain a trailing "_".

        :param str symbol: the symbol name to add the prefix to.

        :returns: the symbol name with the class string as prefix.
        :rtype: str

        '''
        return self._prefix + symbol

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
            [f"{self._text_name}Start[var={self._var_name}]"] + \
            [str(child) for child in self.psy_data_body.children] + \
            [f"{self._text_name}End[var={self._var_name}]"]

        return "\n".join(return_list)

    # -------------------------------------------------------------------------
    @property
    def type_name(self):
        '''
        :returns: the name of the Fortran derived type associated with this \
                  PSyData object.
        :rtype: str
        '''
        return self.add_psydata_class_prefix("PSyDataType")

    # -------------------------------------------------------------------------
    @property
    def fortran_module(self):
        '''
        :returns: name of the PSyData interface Fortran module.
        :rtype: str
        '''
        return self.add_psydata_class_prefix("psy_data_mod")

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
                f"PSyData node malformed or incomplete. It should have a "
                f"single Schedule as a child but found: "
                f"{[type(child).__name__ for child in self.children]}")
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
        call = CallGen(parent, f"{self._var_name}%{name}", arguments)
        parent.add(call)

    # -------------------------------------------------------------------------
    def gen_code(self, parent, options=None):
        # pylint: disable=arguments-differ, too-many-branches
        '''Creates the PSyData code before and after the children
        of this node.

        TODO #1010: This method and the lower_to_language_level below contain
        duplicated logic, the gen_code method will be deleted when all APIs can
        use the PSyIR backends.

        :param parent: the parent of this node in the f2pygen AST.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:value or None
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
        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import Kern, InvokeSchedule
        # TODO: #415 Support different classes of PSyData calls.
        invoke = self.ancestor(InvokeSchedule).invoke
        module_name = self._module_name
        if module_name is None:
            # The user has not supplied a module (location) name so
            # return the psy-layer module name as this will be unique
            # for each PSyclone algorithm file.
            module_name = invoke.invokes.psy.name

        region_name = self._region_name
        if region_name is None:
            # The user has not supplied a region name (to identify
            # this particular invoke region). Use the invoke name as a
            # starting point.
            region_name = invoke.name
            kerns = self.walk(Kern)
            if len(kerns) == 1:
                # This PSyData region only has one kernel within it,
                # so append the kernel name.
                region_name += f":{kerns[0].name}"
            # Add a region index to ensure uniqueness when there are
            # multiple regions in an invoke.
            psy_data_nodes = self.root.walk(PSyDataNode)
            # We can't just use .index on the list because we are searching
            # by identity, not by equality.
            idx = None
            for index, node in enumerate(psy_data_nodes):
                if node is self:
                    idx = index
                    break
            region_name += f":r{idx}"

        if not options:
            options = {}

        pre_variable_list = options.get("pre_var_list", [])
        post_variable_list = options.get("post_var_list", [])
        pre_suffix = options.get("pre_var_postfix", "")
        post_suffix = options.get("post_var_postfix", "")

        # Note that adding a use statement makes sure it is only
        # added once, so we don't need to test this here!
        use = UseGen(parent, self.fortran_module, only=True,
                     funcnames=[sym.name for sym in self.imported_symbols])
        parent.add(use)
        # We only set the visibility of this symbol if we are *not* within
        # a Routine.
        set_private = self.ancestor(Routine) is None
        var_decl = TypeDeclGen(parent,
                               datatype=self.type_name,
                               entity_decls=[self._var_name],
                               save=True, target=True, private=set_private)
        parent.add(var_decl)

        self._add_call("PreStart", parent,
                       [f"\"{module_name}\"",
                        f"\"{region_name}\"",
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
                               [f"\"{var_name}{pre_suffix}\"", var_name])
            for var_name in post_variable_list:
                self._add_call("PreDeclareVariable", parent,
                               [f"\"{var_name}{post_suffix}\"", var_name])

            self._add_call("PreEndDeclaration", parent)

            for var_name in pre_variable_list:
                self._add_call("ProvideVariable", parent,
                               [f"\"{var_name}{pre_suffix}\"", var_name])

            self._add_call("PreEnd", parent)

        for child in self.psy_data_body:
            child.gen_code(parent)

        if has_var:
            # Only add PostStart() if there is at least one variable.
            self._add_call("PostStart", parent)
            for var_name in post_variable_list:
                self._add_call("ProvideVariable", parent,
                               [f"\"{var_name}{post_suffix}\"", var_name])

        self._add_call("PostEnd", parent)

    def lower_to_language_level(self, options=None):
        '''
        Lowers this node (and all children) to language-level PSyIR. The
        PSyIR tree is modified in-place. This PSyDataNode is replaced by a
        pair of Fortran-specific CodeBlocks (representing the calls to the
        start and stop procedures) with the body (children) of the PSyDataNode
        inserted between them. This use of CodeBlocks means that currently only
        the Fortran backend is capable of producing code representing the
        PSyDataNode.

        :param options: dictionary of the PSyData generation options.
        :type options: dict of str:value or None
        :param options["pre_var_list"]: a list of variables to be supplied \
            before the first child.
        :type options["pre_var_list"]: list of str
        :param options["post_var_list"]: a list of variables to be supplied \
            after the last child.
        :type options["post_var_list"]: list of str
        :param str options["pre_var_postfix"]: an optional postfix that will \
            be added to each variable name in the pre_var_list.
        :param str options["post_var_postfix"]: an optional postfix that will \
            be added to each variable name in the post_var_list.

        :raises GenerationError: if the node is not inside a Routine.

        '''
        def gen_type_bound_call(typename, methodname, argument_list=None,
                                annotations=None):
            ''' Helper utility to generate type-bound calls. Since this is
            not directly supported in the PSyIR the call is inserted in a
            PSyIR CodeBlock.

            :param str typename: the name of the base type.
            :param str methodname: the name of the method to be called.
            :param argument_list: the list of arguments in the method call.
            :type argument_list: list of str
            :param annotations: the list of node annotations to add to the \
                                generated CodeBlock.
            :type annotations: list of str

            :returns: a CodeBlock representing the type bound call.
            :rtype: :py:class:`psyclone.psyir.nodes.CodeBlock`

            '''
            argument_str = ""
            if argument_list:
                argument_str += "("
                argument_str += ",".join([str(arg) for arg in argument_list])
                argument_str += ")"

            ParserFactory().create(std="f2008")
            reader = FortranStringReader(
                f"CALL {typename}%{methodname}{argument_str}")
            # Tell the reader that the source is free format
            reader.set_format(FortranFormat(True, False))
            fp2_node = Fortran2003.Call_Stmt(reader)
            return CodeBlock([fp2_node], CodeBlock.Structure.STATEMENT,
                             annotations=annotations)

        for child in self.children:
            child.lower_to_language_level()

        routine_schedule = self.ancestor(Routine)
        if routine_schedule is None:
            raise GenerationError(
                f"A PSyDataNode must be inside a Routine context when "
                f"lowering but '{self}' is not.")

        self.generate_symbols(routine_schedule.symbol_table)

        module_name = self._module_name
        if module_name is None:
            module_name = routine_schedule.name

        if self._region_name:
            region_name = self._region_name
        else:
            # Create a name for this region by finding where this PSyDataNode
            # is in the list of PSyDataNodes in this Invoke. We allow for any
            # previously lowered PSyDataNodes by checking for CodeBlocks with
            # the "psy-data-start" annotation.
            pnodes = routine_schedule.walk((PSyDataNode, CodeBlock))
            region_idx = 0
            for node in pnodes[0:pnodes.index(self)]:
                if (isinstance(node, PSyDataNode) or
                        "psy-data-start" in node.annotations):
                    region_idx += 1
            region_name = f"r{region_idx}"

        if not options:
            options = {}

        pre_variable_list = options.get("pre_var_list", [])
        post_variable_list = options.get("post_var_list", [])
        pre_suffix = options.get("pre_var_postfix", "")
        post_suffix = options.get("post_var_postfix", "")

        has_var = pre_variable_list or post_variable_list

        # PSyData start call (replaces existing PSyDataNode). We annotate this
        # CodeBlock call to record the fact that it represents the start of a
        # psydata region.
        start_call = gen_type_bound_call(
            self._var_name, "PreStart",
            [f"\"{module_name}\"",
             f"\"{region_name}\"",
             len(pre_variable_list),
             len(post_variable_list)],
            ["psy-data-start"])
        self.parent.children.insert(self.position, start_call)

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
                call = gen_type_bound_call(
                    self._var_name, "PreDeclareVariable",
                    [f"\"{var_name}{pre_suffix}\"", var_name])
                self.parent.children.insert(self.position, call)

            for var_name in post_variable_list:
                call = gen_type_bound_call(
                    self._var_name, "PreDeclareVariable",
                    [f"\"{var_name}{post_suffix}\"", var_name])
                self.parent.children.insert(self.position, call)

            call = gen_type_bound_call(self._var_name, "PreEndDeclaration")
            self.parent.children.insert(self.position, call)

            for var_name in pre_variable_list:
                call = gen_type_bound_call(
                    self._var_name, "ProvideVariable",
                    [f"\"{var_name}{pre_suffix}\"", var_name])
                self.parent.children.insert(self.position, call)

            call = gen_type_bound_call(self._var_name, "PreEnd")
            self.parent.children.insert(self.position, call)

        # Insert the body of the profiled region between the start and
        # end calls
        for child in self.psy_data_body.pop_all_children():
            self.parent.children.insert(self.position, child)

        if has_var:
            # Only add PostStart() if there is at least one variable.
            call = gen_type_bound_call(self._var_name, "PostStart")
            self.parent.children.insert(self.position, call)
            for var_name in post_variable_list:
                call = gen_type_bound_call(
                    self._var_name, "ProvideVariable",
                    [f"\"{var_name}{post_suffix}\"", var_name])
                self.parent.children.insert(self.position, call)

        # PSyData end call
        end_call = gen_type_bound_call(self._var_name, "PostEnd")
        self.parent.children.insert(self.position+1, end_call)

        # Finally we can remove the original PSyDataNode from here
        self.detach()


# For AutoAPI documentation generation
__all__ = ['PSyDataNode']
