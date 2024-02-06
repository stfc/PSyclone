# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_collection import LFRicCollection
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import (AssignGen, CommentGen, DeclGen,
                              IfThenGen, TypeDeclGen, UseGen)
from psyclone.psyir.symbols import ScalarType


class LFRicStencils(LFRicCollection):
    '''
    Stencil information and code generation associated with a PSy-layer
    routine or Kernel stub.

    :param node: the Invoke or Kernel stub for which to provide stencil info.
    :type node: :py:class:`psyclone.dynamo0p3.LFRicInvoke` or
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
        for call in self._calls:
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
        for call in self._calls:
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
        for call in self._calls:
            for arg in call.arguments.args:
                if arg.stencil:
                    if not arg.stencil.extent:
                        self._kern_args.append(arg)

    @staticmethod
    def extent_value(arg):
        '''
        Returns the content of the stencil extent which may be a literal
        value (a number) or a variable name. This function simplifies this
        problem by returning a string in either case.

        :param arg: the argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: the content of the stencil extent.
        :rtype: str

        '''
        if arg.stencil.extent_arg.is_literal():
            return arg.stencil.extent_arg.text
        return arg.stencil.extent_arg.varname

    @staticmethod
    def stencil_unique_str(arg, context):
        '''
        Creates a unique identifier for a stencil. As a stencil
        differs due to the function space it operates on, type of
        stencil and extent of stencil, we concatenate these things together
        to create a unique string.

        :param arg: kernel argument with which stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
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
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a valid unique map name for a stencil in the PSy layer.
        :rtype: str

        '''
        root_name = arg.name + "_stencil_map"
        unique = LFRicStencils.stencil_unique_str(arg, "map")
        return self._symbol_table.find_or_create_tag(unique, root_name).name

    @staticmethod
    def dofmap_symbol(symtab, arg):
        '''
        Creates and registers a symbol for the stencil dofmap associated with
        the supplied kernel argument.

        :param symtab: symbol table that will contain (or already contains)
                       the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a dofmap symbol for a stencil in the PSy layer.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        root_name = arg.name + "_stencil_dofmap"
        unique = LFRicStencils.stencil_unique_str(arg, "dofmap")
        if arg.descriptor.stencil['type'] == "cross2d":
            num_dimensions = 4
        else:
            num_dimensions = 3
        return symtab.find_or_create_array(root_name, num_dimensions,
                                           ScalarType.Intrinsic.INTEGER,
                                           tag=unique)

    @staticmethod
    def dofmap_size_symbol(symtab, arg):
        '''
        Create a valid symbol for the size (in cells) of a stencil
        dofmap in the PSy layer.

        :param symtab: symbol table that will contain (or already contains)
                       the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a symbol for the stencil size.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        root_name = arg.name + "_stencil_size"
        unique = LFRicStencils.stencil_unique_str(arg, "size")
        if arg.descriptor.stencil['type'] == "cross2d":
            num_dimensions = 2
        else:
            num_dimensions = 1
        return symtab.find_or_create_array(root_name, num_dimensions,
                                           ScalarType.Intrinsic.INTEGER,
                                           tag=unique)

    @staticmethod
    def max_branch_length_name(symtab, arg):
        '''
        Create a valid unique name for the maximum length of a stencil branch
        (in cells) of a 2D stencil dofmap in the PSy layer. This is required
        in the kernels for defining the maximum possible length of one of the
        dofmap array dimensions.

        :param symtab: symbol table that will contain (or already contains)
                       the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a Fortran variable name for the max stencil branch length.
        :rtype: str

        '''
        root_name = arg.name + "_max_branch_length"
        unique = LFRicStencils.stencil_unique_str(arg, "length")
        return symtab.find_or_create_integer_symbol(root_name, tag=unique).name

    def _unique_max_branch_length_vars(self):
        '''
        :returns: list of all the unique max stencil extent argument names in
                  this kernel call for CROSS2D stencils.
        :rtype: list of str

        '''
        names = []
        for arg in self._kern_args:
            if arg.descriptor.stencil['type'] == "cross2d":
                names.append(arg.name + "_max_branch_length")

        return names

    def _declare_unique_max_branch_length_vars(self, parent):
        '''
        Declare all unique max branch length arguments as integers with intent
        'in' and add the declaration as a child of the parent argument passed
        in.

        :param parent: the node in the f2pygen AST to which to add the
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if self._unique_max_branch_length_vars():
            parent.add(DeclGen(
                parent, datatype="integer",
                kind=api_config.default_kind["integer"],
                entity_decls=self._unique_max_branch_length_vars(), intent="in"
            ))

    @staticmethod
    def direction_name(symtab, arg):
        '''
        Creates a Fortran variable name to hold the direction of the stencil
        associated with the supplied kernel argument.

        :param symtab: symbol table that will contain (or already contains)
                       the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a Fortran variable name for the stencil direction.
        :rtype: str

        '''
        root_name = arg.name+"_direction"
        unique = LFRicStencils.stencil_unique_str(arg, "direction")
        return symtab.find_or_create_integer_symbol(root_name, tag=unique).name

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
            names = [self.dofmap_size_symbol(self._symbol_table, arg).name
                     for arg in self._unique_extent_args]
        else:
            raise InternalError("LFRicStencils._unique_extent_vars: have "
                                "neither Invoke or Kernel. Should be "
                                "impossible.")
        return names

    def _declare_unique_extent_vars(self, parent):
        '''
        Declare all unique extent arguments as integers with intent 'in' and
        add the declaration as a child of the parent argument passed
        in.

        :param parent: the node in the f2pygen AST to which to add the
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if self._unique_extent_vars:
            if self._kernel:
                for arg in self._kern_args:
                    if arg.descriptor.stencil['type'] == "cross2d":
                        parent.add(DeclGen(
                            parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            dimension="4",
                            entity_decls=self._unique_extent_vars, intent="in"
                        ))
                    else:
                        parent.add(DeclGen(
                            parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            entity_decls=self._unique_extent_vars,
                            intent="in"))
            elif self._invoke:
                parent.add(DeclGen(
                    parent, datatype="integer",
                    kind=api_config.default_kind["integer"],
                    entity_decls=self._unique_extent_vars, intent="in"
                ))

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
                names.append(arg.name+"_direction")
        return names

    def _declare_unique_direction_vars(self, parent):
        '''
        Declare all unique direction arguments as integers with intent 'in'
        and add the declaration as a child of the parent argument
        passed in.

        :param parent: the node in the f2pygen AST to which to add the
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if self._unique_direction_vars:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=self._unique_direction_vars,
                               intent="in"))

    @property
    def unique_alg_vars(self):
        '''
        :returns: list of the names of the extent and direction arguments
                  supplied to the PSy routine from the Algorithm layer.
        :rtype: list of str

        '''
        return self._unique_extent_vars + self._unique_direction_vars

    def _invoke_declarations(self, parent):
        '''
        Declares all stencil maps, extent and direction arguments passed into
        the PSy layer.

        :param parent: node in the f2pygen AST to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        self._declare_unique_extent_vars(parent)
        self._declare_unique_direction_vars(parent)
        self._declare_maps_invoke(parent)

    def _stub_declarations(self, parent):
        '''
        Declares all stencil-related quanitites for a Kernel stub.

        :param parent: node in the f2pygen AST to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        self._declare_unique_extent_vars(parent)
        self._declare_unique_direction_vars(parent)
        self._declare_unique_max_branch_length_vars(parent)
        self._declare_maps_stub(parent)

    def initialise(self, parent):
        '''
        Adds in the code to initialise stencil dofmaps to the PSy layer.

        :param parent: the node in the f2pygen AST to which to add the
                       initialisations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises GenerationError: if an unsupported stencil type is encountered.

        '''
        if not self._kern_args:
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Initialise stencil dofmaps"))
        parent.add(CommentGen(parent, ""))
        api_config = Config.get().api_conf("dynamo0.3")
        stencil_map_names = []
        const = LFRicConstants()
        for arg in self._kern_args:
            map_name = self.map_name(arg)
            if map_name not in stencil_map_names:
                # Only initialise maps once.
                stencil_map_names.append(map_name)
                stencil_type = arg.descriptor.stencil['type']
                symtab = self._symbol_table
                if stencil_type == "xory1d":
                    direction_name = arg.stencil.direction_arg.varname
                    for direction in ["x", "y"]:
                        if_then = IfThenGen(parent, direction_name +
                                            " .eq. " + direction +
                                            "_direction")
                        if_then.add(
                            AssignGen(
                                if_then, pointer=True, lhs=map_name,
                                rhs=arg.proxy_name_indexed +
                                "%vspace%get_stencil_dofmap("
                                "STENCIL_1D" + direction.upper() +
                                ","+self.extent_value(arg)+")"))
                        parent.add(if_then)
                elif stencil_type == "cross2d":
                    parent.add(
                        AssignGen(parent, pointer=True, lhs=map_name,
                                  rhs=arg.proxy_name_indexed +
                                  "%vspace%get_stencil_2D_dofmap(" +
                                  "STENCIL_2D_CROSS" + "," +
                                  self.extent_value(arg) + ")"))
                    # Max branch length in the CROSS2D stencil is used when
                    # defining the stencil_dofmap dimensions at declaration of
                    # the dummy argument in the kernel. This value is 1
                    # greater than the stencil extent as the central cell
                    # is included as part of the stencil_dofmap.
                    parent.add(
                        AssignGen(parent,
                                  lhs=self.max_branch_length_name(symtab,
                                                                  arg),
                                  rhs=self.extent_value(arg) + " + 1_" +
                                  api_config.default_kind["integer"]))
                else:
                    try:
                        stencil_name = const.STENCIL_MAPPING[stencil_type]
                    except KeyError as err:
                        raise GenerationError(
                            f"Unsupported stencil type "
                            f"'{arg.descriptor.stencil['type']}' supplied. "
                            f"Supported mappings are "
                            f"{str(const.STENCIL_MAPPING)}") from err
                    parent.add(
                        AssignGen(parent, pointer=True, lhs=map_name,
                                  rhs=arg.proxy_name_indexed +
                                  "%vspace%get_stencil_dofmap(" +
                                  stencil_name + "," +
                                  self.extent_value(arg) + ")"))

                parent.add(AssignGen(parent, pointer=True,
                                     lhs=self.dofmap_symbol(symtab, arg).name,
                                     rhs=map_name + "%get_whole_dofmap()"))

                # Add declaration and look-up of stencil size
                dofmap_size_name = self.dofmap_size_symbol(symtab, arg).name
                parent.add(AssignGen(parent, pointer=True,
                                     lhs=dofmap_size_name,
                                     rhs=map_name + "%get_stencil_sizes()"))

    def _declare_maps_invoke(self, parent):
        '''
        Declare all stencil maps in the PSy layer.

        :param parent: the node in the f2pygen AST to which to add
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises GenerationError: if an unsupported stencil type is encountered.

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if not self._kern_args:
            return

        symtab = self._symbol_table
        stencil_map_names = []
        const = LFRicConstants()

        for arg in self._kern_args:
            map_name = self.map_name(arg)

            if map_name in stencil_map_names:
                continue

            stencil_map_names.append(map_name)
            stencil_type = arg.descriptor.stencil['type']
            if stencil_type == "cross2d":
                smap_type = const.STENCIL_TYPE_MAP["stencil_2D_dofmap"]["type"]
                smap_mod = const.STENCIL_TYPE_MAP[
                    "stencil_2D_dofmap"]["module"]
                parent.add(UseGen(parent, name=smap_mod, only=True,
                                  funcnames=[smap_type, "STENCIL_2D_CROSS"]))
                parent.add(TypeDeclGen(parent, pointer=True,
                                       datatype=smap_type,
                                       entity_decls=[map_name +
                                                     " => null()"]))
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   pointer=True,
                                   entity_decls=[self.dofmap_symbol(symtab,
                                                                    arg).name +
                                                 "(:,:,:,:) => null()"]))
                dofmap_size_name = self.dofmap_size_symbol(symtab, arg).name
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   pointer=True,
                                   entity_decls=[f"{dofmap_size_name}(:,:) "
                                                 f"=> null()"]))
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   entity_decls=[self.max_branch_length_name(
                                       symtab, arg)]))
            else:
                smap_type = const.STENCIL_TYPE_MAP["stencil_dofmap"]["type"]
                smap_mod = const.STENCIL_TYPE_MAP["stencil_dofmap"]["module"]
                parent.add(UseGen(parent, name=smap_mod,
                                  only=True, funcnames=[smap_type]))
                if stencil_type == 'xory1d':
                    drct_mod = const.STENCIL_TYPE_MAP["direction"]["module"]
                    parent.add(UseGen(parent, name=drct_mod,
                                      only=True, funcnames=["x_direction",
                                                            "y_direction"]))
                    parent.add(UseGen(parent, name=smap_mod,
                                      only=True, funcnames=["STENCIL_1DX",
                                                            "STENCIL_1DY"]))
                else:
                    try:
                        stencil_name = const.STENCIL_MAPPING[stencil_type]
                    except KeyError as err:
                        raise GenerationError(
                            f"Unsupported stencil type "
                            f"'{arg.descriptor.stencil['type']}' supplied. "
                            f"Supported mappings are "
                            f"{const.STENCIL_MAPPING}") from err
                    parent.add(UseGen(parent, name=smap_mod,
                                      only=True, funcnames=[stencil_name]))

                parent.add(TypeDeclGen(parent, pointer=True,
                                       datatype=smap_type,
                                       entity_decls=[map_name+" => null()"]))
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   pointer=True,
                                   entity_decls=[self.dofmap_symbol(symtab,
                                                                    arg).name +
                                                 "(:,:,:) => null()"]))
                dofmap_size_name = self.dofmap_size_symbol(symtab, arg).name
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   pointer=True,
                                   entity_decls=[f"{dofmap_size_name}(:) "
                                                 f"=> null()"]))

    def _declare_maps_stub(self, parent):
        '''
        Add declarations for all stencil maps to a kernel stub.

        :param parent: the node in the f2pygen AST representing the kernel
                       stub routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        symtab = self._symbol_table
        for arg in self._kern_args:
            if arg.descriptor.stencil['type'] == "cross2d":
                parent.add(DeclGen(
                    parent, datatype="integer",
                    kind=api_config.default_kind["integer"], intent="in",
                    dimension=",".join([arg.function_space.ndf_name,
                                        self.max_branch_length_name(
                                            symtab, arg), "4"]),
                    entity_decls=[self.dofmap_symbol(symtab, arg).name]))
            else:
                dofmap_size_name = self.dofmap_size_symbol(symtab, arg).name
                parent.add(DeclGen(
                    parent, datatype="integer",
                    kind=api_config.default_kind["integer"], intent="in",
                    dimension=",".join([arg.function_space.ndf_name,
                                        dofmap_size_name]),
                    entity_decls=[self.dofmap_symbol(symtab, arg).name]))


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['LFRicStencils']
