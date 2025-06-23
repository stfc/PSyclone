# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module implements the stencil information and code generation
    associated with a PSy-layer routine or Kernel stub in the LFRic API.  '''

from psyclone.domain.lfric import LFRicTypes
from psyclone.domain.lfric.lfric_collection import LFRicCollection
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes import (
    Assignment, Reference, Call, StructureReference, IfBlock, BinaryOperation,
    Literal, DataNode)
from psyclone.psyir.symbols import (
    DataSymbol, UnsupportedFortranType, INTEGER_TYPE,
    ArgumentInterface, UnresolvedType, ContainerSymbol,
    ImportInterface, ArrayType, DataTypeSymbol)


class LFRicStencils(LFRicCollection):
    '''
    Stencil information and code generation associated with a PSy-layer
    routine or Kernel stub.

    :param node: the Invoke or Kernel stub for which to provide stencil info.
    :type node: :py:class:`psyclone.lfric.LFRicInvoke` or
                :py:class:`psyclone.domain.lfric.LFRicKern`

    :raises GenerationError: if a literal has been supplied for a stencil
                             direction.

    '''
    def __init__(self, node):
        # pylint: disable=too-many-branches
        super().__init__(node)

        # List of arguments which have an extent value passed to this
        # invoke routine from the algorithm layer. Duplicate argument
        # names are removed.
        self._unique_extent_args = []
        extent_names = []
        # pylint: disable=too-many-nested-blocks
        for call in self.kernel_calls:
            for arg in call.arguments.args:
                if arg.stencil:
                    # Check for the existence of arg.extent here as in
                    # the future we plan to support kernels which
                    # specify the value of extent in metadata. If this
                    # is the case then an extent argument is not
                    # required.
                    # TODO #963
                    if not arg.stencil.extent:
                        if not arg.stencil.extent_arg.is_literal():
                            if arg.stencil.extent_arg.text not in extent_names:
                                extent_names.append(
                                    arg.stencil.extent_arg.text)
                                self._unique_extent_args.append(arg)

        # A list of arguments that have a direction variable passed in
        # to this invoke routine from the algorithm layer. Duplicate
        # argument names are removed.
        self._unique_direction_args = []
        direction_names = []
        for call in self.kernel_calls:
            for idx, arg in enumerate(call.arguments.args):
                if arg.stencil and arg.stencil.direction_arg:
                    if arg.stencil.direction_arg.is_literal():
                        raise GenerationError(
                            f"Kernel {call.name}, metadata arg {idx}, a "
                            f"literal is not a valid value for a stencil "
                            f"direction.")
                    if arg.stencil.direction_arg.text.lower() not in \
                       ["x_direction", "y_direction"]:
                        if arg.stencil.direction_arg.text not in \
                           direction_names:
                            direction_names.append(
                                arg.stencil.direction_arg.text)
                            self._unique_direction_args.append(arg)

        # List of stencil args with an extent variable passed in. The same
        # field name may occur more than once here from different kernels.
        self._kern_args = []
        for call in self.kernel_calls:
            for arg in call.arguments.args:
                if arg.stencil:
                    if not arg.stencil.extent:
                        self._kern_args.append(arg)

    def extent_value(self, arg) -> DataNode:
        '''
        Returns the content of the stencil extent which may be a literal
        value (a number) or a variable name. This function simplifies this
        problem by returning a string in either case.

        :param arg: the argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`

        :returns: the content of the stencil extent.

        '''
        extent_arg = arg.stencil.extent_arg
        if extent_arg.is_literal():
            return Literal(extent_arg.text, INTEGER_TYPE)
        return Reference(self.symtab.lookup(extent_arg.varname))

    @staticmethod
    def stencil_unique_str(arg, context):
        '''
        Creates a unique identifier for a stencil. As a stencil
        differs due to the function space it operates on, type of
        stencil and extent of stencil, we concatenate these things together
        to create a unique string.

        :param arg: kernel argument with which stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param str context: a context for this stencil (e.g. "size" or
                            "direction").

        :returns: unique string identifying the stencil for this argument.
        :rtype: str

        :raises GenerationError: if an explicit stencil extent is found in
                                 the metadata for the kernel argument.

        '''
        unique = context
        unique += arg.function_space.mangled_name
        unique += arg.descriptor.stencil['type']
        if arg.descriptor.stencil['extent']:
            raise GenerationError(
                "Found a stencil with an extent specified in the metadata. "
                "This is not coded for.")
        unique += arg.stencil.extent_arg.text.lower()
        if arg.descriptor.stencil['type'] == 'xory1d':
            unique += arg.stencil.direction_arg.text.lower()
        return unique

    def map_name(self, arg):
        '''
        Creates and registers a name for the stencil map associated with the
        supplied kernel argument.

        :param arg: kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`

        :returns: a valid unique map name for a stencil in the PSy layer.
        :rtype: str

        '''
        unique = LFRicStencils.stencil_unique_str(arg, "map")
        return self.symtab.lookup_with_tag(unique).name

    @staticmethod
    def dofmap_symbol(symtab, arg):
        '''
        Creates and registers a symbol for the stencil dofmap associated with
        the supplied kernel argument.

        :param symtab: symbol table that will contain (or already contains)
                       the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`

        :returns: a dofmap symbol for a stencil in the PSy layer.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        root_name = arg.name + "_stencil_dofmap"
        unique = LFRicStencils.stencil_unique_str(arg, "dofmap")
        # The dofmap symbol type depends if it's in a stub or an invoke, so
        # don't commit to any type yet.
        return symtab.find_or_create_tag(
                unique, root_name=root_name, symbol_type=DataSymbol,
                datatype=UnresolvedType())

    @staticmethod
    def dofmap_size_symbol(symtab, arg):
        '''
        Create a valid symbol for the size (in cells) of a stencil
        dofmap in the PSy layer.

        :param symtab: symbol table that will contain (or already contains)
                       the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`

        :returns: a symbol for the stencil size.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        root_name = arg.name + "_stencil_size"
        unique = LFRicStencils.stencil_unique_str(arg, "size")
        return symtab.find_or_create_tag(
                unique, root_name=root_name, symbol_type=DataSymbol,
                # We don't commit to a type because it is different on
                # Invokes and Stubs
                datatype=UnresolvedType())

    @staticmethod
    def max_branch_length(symtab, arg) -> DataSymbol:
        '''
        Create a valid unique name for the maximum length of a stencil branch
        (in cells) of a 2D stencil dofmap in the PSy layer. This is required
        in the kernels for defining the maximum possible length of one of the
        dofmap array dimensions.

        :param symtab: symbol table that will contain (or already contains)
                       the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`

        :returns: the symbol representing the max stencil branch length.

        '''
        root_name = arg.name + "_max_branch_length"
        unique = LFRicStencils.stencil_unique_str(arg, "length")
        return symtab.find_or_create_tag(
                unique, root_name=root_name, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())

    @staticmethod
    def direction_name(symtab, arg):
        '''
        Creates a Fortran variable name to hold the direction of the stencil
        associated with the supplied kernel argument.

        :param symtab: symbol table that will contain (or already contains)
                       the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`

        :returns: a Fortran variable name for the stencil direction.
        :rtype: str

        '''
        root_name = arg.name+"_direction"
        unique = LFRicStencils.stencil_unique_str(arg, "direction")
        return symtab.find_or_create_tag(
                unique, root_name=root_name, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())

    @property
    def _unique_extent_vars(self):
        '''
        :returns: list of all the unique extent argument names in this
                  invoke or kernel call.
        :rtype: list of str

        :raises InternalError: if neither 'self._kernel' or 'self._invoke' are
                               set.

        '''
        if self._invoke:
            names = [arg.stencil.extent_arg.varname for arg in
                     self._unique_extent_args]
        elif self._kernel:
            names = [LFRicStencils.dofmap_size_symbol(self.symtab, arg).name
                     for arg in self._unique_extent_args]
        else:
            raise InternalError("LFRicStencils._unique_extent_vars: have "
                                "neither Invoke or Kernel. Should be "
                                "impossible.")
        return names

    def _declare_unique_extent_vars(self):
        '''
        Declare all unique extent arguments as integers with intent 'in'.

        '''
        if self._unique_extent_vars:
            if self._kernel:
                for arg in self._kern_args:
                    if arg.descriptor.stencil['type'] == "cross2d":
                        for var in self._unique_extent_vars:
                            symbol = self.symtab.lookup(var)
                            symbol.datatype = ArrayType(
                                    LFRicTypes(
                                        "LFRicIntegerScalarDataType")(),
                                    [Literal("4", INTEGER_TYPE)])
                            symbol.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
                            self.symtab.append_argument(symbol)
                    else:
                        for var in self._unique_extent_vars:
                            symbol = self.symtab.lookup(var)
                            symbol.datatype = LFRicTypes(
                                    "LFRicIntegerScalarDataType")()
                            symbol.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
                            self.symtab.append_argument(symbol)
            elif self._invoke:
                for var in self._unique_extent_vars:
                    symbol = self.symtab.find_or_create(
                        var, symbol_type=DataSymbol,
                        datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                    symbol.interface = ArgumentInterface(
                                ArgumentInterface.Access.READ)
                    self.symtab.append_argument(symbol)

    @property
    def _unique_direction_vars(self):
        '''
        :returns: a list of all the unique direction argument names in this
                  invoke call.
        :rtype: list of str

        '''
        names = []
        for arg in self._unique_direction_args:
            if arg.stencil.direction_arg.varname:
                names.append(arg.stencil.direction_arg.varname)
            else:
                names.append(self.direction_name(self.symtab, arg).name)
        return names

    def _declare_unique_direction_vars(self):
        '''
        Declare all unique direction arguments as integers with intent 'in'.

        '''
        for var in self._unique_direction_vars:
            symbol = self.symtab.find_or_create(
                var, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())

            if symbol not in self.symtab.argument_list:
                symbol.interface = ArgumentInterface(
                            ArgumentInterface.Access.READ)
                self.symtab.append_argument(symbol)

    @property
    def unique_alg_vars(self):
        '''
        :returns: list of the names of the extent and direction arguments
                  supplied to the PSy routine from the Algorithm layer.
        :rtype: list of str

        '''
        return self._unique_extent_vars + self._unique_direction_vars

    def invoke_declarations(self):
        '''
        Declares all stencil maps, extent and direction arguments passed into
        the PSy layer.

        '''
        super().invoke_declarations()
        self._declare_unique_extent_vars()
        self._declare_unique_direction_vars()
        self._declare_maps_invoke()

    def stub_declarations(self):
        '''
        Declares all stencil-related quanitites for a Kernel stub.

        '''
        super().stub_declarations()
        self._declare_unique_extent_vars()
        self._declare_unique_direction_vars()
        self._declare_maps_stub()

    def initialise(self, cursor: int) -> int:
        '''
        Adds in the code to initialise stencil dofmaps to the PSy layer.

        :param cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.

        :raises GenerationError: if an unsupported stencil type is encountered.

        '''
        if not self._kern_args:
            return cursor

        stencil_map_names = []
        const = LFRicConstants()
        init_cursor = cursor
        for arg in self._kern_args:
            map_name = self.map_name(arg)
            if map_name not in stencil_map_names:
                # Only initialise maps once.
                stencil_map_names.append(map_name)
                stencil_type = arg.descriptor.stencil['type']
                symtab = self.symtab
                if stencil_type == "xory1d":
                    direction_name = arg.stencil.direction_arg.varname
                    for direction in ["x", "y"]:
                        condition = BinaryOperation.create(
                            BinaryOperation.Operator.EQ,
                            Reference(symtab.lookup(direction_name)),
                            Reference(symtab.lookup(direction + "_direction")))
                        lhs = Reference(symtab.lookup(map_name))
                        rhs = arg.generate_method_call("get_stencil_dofmap")
                        rhs.addchild(Reference(
                            symtab.lookup("STENCIL_1D" + direction.upper())))
                        rhs.addchild(self.extent_value(arg))
                        stmt = Assignment.create(lhs=lhs, rhs=rhs,
                                                 is_pointer=True)
                        ifblock = IfBlock.create(condition, [stmt])
                        self._invoke.schedule.addchild(ifblock, cursor)
                        cursor += 1

                elif stencil_type == "cross2d":
                    lhs = Reference(symtab.lookup(map_name))
                    rhs = arg.generate_method_call("get_stencil_2D_dofmap")
                    rhs.addchild(Reference(
                        symtab.lookup("STENCIL_2D_CROSS")))
                    rhs.addchild(self.extent_value(arg))
                    stmt = Assignment.create(lhs=lhs, rhs=rhs,
                                             is_pointer=True)
                    self._invoke.schedule.addchild(stmt, cursor)
                    cursor += 1

                    # Max branch length in the CROSS2D stencil is used when
                    # defining the stencil_dofmap dimensions at declaration of
                    # the dummy argument in the kernel. This value is 1
                    # greater than the stencil extent as the central cell
                    # is included as part of the stencil_dofmap.
                    stmt = Assignment.create(
                        lhs=Reference(
                                self.max_branch_length(symtab, arg)),
                        rhs=BinaryOperation.create(
                            BinaryOperation.Operator.ADD,
                            self.extent_value(arg),
                            Literal("1", INTEGER_TYPE))
                        )
                    self._invoke.schedule.addchild(stmt, cursor)
                    cursor += 1
                else:
                    try:
                        stencil_name = const.STENCIL_MAPPING[stencil_type]
                    except KeyError as err:
                        raise GenerationError(
                            f"Unsupported stencil type "
                            f"'{arg.descriptor.stencil['type']}' supplied. "
                            f"Supported mappings are "
                            f"{str(const.STENCIL_MAPPING)}") from err

                    rhs = arg.generate_method_call("get_stencil_dofmap")
                    rhs.addchild(Reference(symtab.lookup(stencil_name)))
                    rhs.addchild(self.extent_value(arg))
                    self._invoke.schedule.addchild(
                        Assignment.create(
                            lhs=Reference(symtab.lookup(map_name)),
                            rhs=rhs,
                            is_pointer=True),
                        cursor)
                    cursor += 1

                self._invoke.schedule.addchild(
                    Assignment.create(
                        lhs=Reference(self.dofmap_symbol(symtab, arg)),
                        rhs=Call.create(
                            StructureReference.create(
                                symtab.lookup(map_name),
                                ["get_whole_dofmap"])),
                        is_pointer=True),
                    cursor)
                cursor += 1

                # Add look-up of stencil size
                size_symbol = self.dofmap_size_symbol(self.symtab, arg)
                if arg.descriptor.stencil['type'] == "cross2d":
                    num_dimensions = 2
                else:
                    num_dimensions = 1
                dim_string = (":," * num_dimensions)[:-1]
                size_symbol.datatype = UnsupportedFortranType(
                    f"integer(kind=i_def), pointer, "
                    f"dimension({dim_string}) :: {size_symbol.name}"
                    f" => null()")
                self._invoke.schedule.addchild(
                    Assignment.create(
                        lhs=Reference(size_symbol),
                        rhs=Call.create(
                            StructureReference.create(
                                symtab.lookup(map_name),
                                ["get_stencil_sizes"])),
                        is_pointer=True),
                    cursor)
                cursor += 1
        if cursor > init_cursor:
            self._invoke.schedule[init_cursor].preceding_comment = (
                "Initialise stencil dofmaps")
        return cursor

    def _declare_maps_invoke(self):
        '''
        Declare all stencil maps in the PSy layer.

        :raises GenerationError: if an unsupported stencil type is encountered.

        '''
        if not self._kern_args:
            return

        symtab = self.symtab
        stencil_map_names = []
        const = LFRicConstants()

        for arg in self._kern_args:
            unique_tag = LFRicStencils.stencil_unique_str(arg, "map")

            if unique_tag in stencil_map_names:
                continue
            stencil_map_names.append(unique_tag)

            symbol = self.symtab.new_symbol(
                root_name=f"{arg.name}_stencil_map", tag=unique_tag)
            name = symbol.name
            stencil_type = arg.descriptor.stencil['type']
            if stencil_type == "cross2d":
                smap_mod = self.symtab.find_or_create(
                        const.STENCIL_TYPE_MAP["stencil_2D_dofmap"]["module"],
                        symbol_type=ContainerSymbol)
                smap_type = self.symtab.find_or_create(
                        const.STENCIL_TYPE_MAP["stencil_2D_dofmap"]["type"],
                        symbol_type=DataTypeSymbol,
                        datatype=UnresolvedType(),
                        interface=ImportInterface(smap_mod))
                self.symtab.find_or_create(
                        "STENCIL_2D_CROSS",
                        symbol_type=DataSymbol,
                        datatype=UnresolvedType(),
                        interface=ImportInterface(smap_mod))

                dtype = UnsupportedFortranType(
                    f"type({smap_type.name}), pointer :: {name} => null()")
                symbol.specialise(subclass=DataSymbol, datatype=dtype)

                dofmap_symbol = self.dofmap_symbol(symtab, arg)
                dofmap_symbol.datatype = UnsupportedFortranType(
                    f"integer(kind=i_def), pointer, dimension(:,:,:,:) "
                    f":: {dofmap_symbol.name} => null()")
            else:
                smap_mod = self.symtab.find_or_create(
                        const.STENCIL_TYPE_MAP["stencil_dofmap"]["module"],
                        symbol_type=ContainerSymbol)
                smap_type = self.symtab.find_or_create(
                        const.STENCIL_TYPE_MAP["stencil_dofmap"]["type"],
                        symbol_type=DataTypeSymbol,
                        datatype=UnresolvedType(),
                        interface=ImportInterface(smap_mod))
                if stencil_type == 'xory1d':
                    drct_mod = self.symtab.find_or_create(
                            const.STENCIL_TYPE_MAP["direction"]["module"],
                            symbol_type=ContainerSymbol)
                    self.symtab.find_or_create(
                            "x_direction",
                            symbol_type=DataSymbol,
                            datatype=UnresolvedType(),
                            interface=ImportInterface(drct_mod))
                    self.symtab.find_or_create(
                            "y_direction",
                            symbol_type=DataSymbol,
                            datatype=UnresolvedType(),
                            interface=ImportInterface(drct_mod))
                    self.symtab.find_or_create(
                            "STENCIL_1DX",
                            symbol_type=DataSymbol,
                            datatype=UnresolvedType(),
                            interface=ImportInterface(smap_mod))
                    self.symtab.find_or_create(
                            "STENCIL_1DY",
                            symbol_type=DataSymbol,
                            datatype=UnresolvedType(),
                            interface=ImportInterface(smap_mod))
                else:
                    try:
                        stencil_name = const.STENCIL_MAPPING[stencil_type]
                    except KeyError as err:
                        raise GenerationError(
                            f"Unsupported stencil type "
                            f"'{arg.descriptor.stencil['type']}' supplied. "
                            f"Supported mappings are "
                            f"{const.STENCIL_MAPPING}") from err
                    self.symtab.find_or_create(
                            stencil_name,
                            symbol_type=DataSymbol,
                            datatype=UnresolvedType(),
                            interface=ImportInterface(smap_mod))

                dtype = UnsupportedFortranType(
                    f"type({smap_type.name}), pointer :: {name} => null()")
                symbol.specialise(subclass=DataSymbol, datatype=dtype)

                dofmap_symbol = self.dofmap_symbol(symtab, arg)
                dofmap_symbol.datatype = UnsupportedFortranType(
                    f"integer(kind=i_def), pointer, dimension(:,:,:) "
                    f":: {dofmap_symbol.name} => null()")

    def _declare_maps_stub(self):
        '''
        Add declarations for all stencil maps to a kernel stub. (Note that
        the order of arguments will be redefined later on by ArgOrdering)

        '''
        symtab = self.symtab
        for arg in self._kern_args:
            symbol = self.dofmap_symbol(symtab, arg)
            if arg.descriptor.stencil['type'] == "cross2d":
                max_length = self.max_branch_length(symtab, arg)
                if max_length not in symtab.argument_list:
                    max_length.interface = ArgumentInterface(
                                ArgumentInterface.Access.READ)
                    symtab.append_argument(max_length)
                symbol.datatype = ArrayType(
                        LFRicTypes("LFRicIntegerScalarDataType")(),
                        [Reference(symtab.lookup(arg.function_space.ndf_name)),
                         Reference(max_length),
                         Literal("4", INTEGER_TYPE)])
            else:
                size_symbol = self.dofmap_size_symbol(self.symtab, arg)
                size_symbol.datatype = \
                    LFRicTypes("LFRicIntegerScalarDataType")()
                size_symbol.interface = ArgumentInterface(
                                ArgumentInterface.Access.READ)
                symtab.append_argument(size_symbol)
                symbol.datatype = ArrayType(
                        LFRicTypes("LFRicIntegerScalarDataType")(),
                        [Reference(symtab.lookup(arg.function_space.ndf_name)),
                         Reference(size_symbol)])
            symbol.interface = ArgumentInterface(
                        ArgumentInterface.Access.READ)
            symtab.append_argument(symbol)


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicStencils']
