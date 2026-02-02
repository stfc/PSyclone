# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk STFC Daresbury Lab

'''This module contains the DataNodeExtractTrans class.'''

from psyclone.psyGen import Transformation
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import (
    DataNode,
    Reference,
    Assignment,
    Statement,
    Call
)
from psyclone.psyir.symbols.datatypes import (
    ArrayType,
    UnresolvedType,
    UnsupportedFortranType,
)
from psyclone.psyir.symbols import DataSymbol, ImportInterface
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class DataNodeExtractTrans(Transformation):
    """Provides a generic transformation for moving a datanode from a
    statement into a new standalone statement. For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import DataNodeExtractTrans
    >>>
    >>> psyir = FortranReader().psyir_from_source('''
    ...     subroutine my_subroutine()
    ...         integer :: i
    ...         integer :: j
    ...         i = j * 2
    ...     end subroutine
    ...     ''')
    >>> assign = psyir.walk(Assignment)[0]
    >>> DataNodeExtractTrans().apply(assign.rhs, storage_name="temp")
    >>> print(FortranWriter()(psyir))
    subroutine my_subroutine()
      integer, dimension(10,10) :: a
      integer :: i
      integer :: j
      integer :: temp
    <BLANKLINE>
        temp = j * 2
        i = temp
    <BLANKLINE>
    end subroutine my_subroutine
    <BLANKLINE>
    """

    def validate(self, node: DataNode, **kwargs):
        """Validity checks for input arguments

        :param node: The DataNode to be extracted.

        :raises TypeError: if the input arguments are the wrong types.
        :raises TransformationError: if the input node's datatype can't be
                                     resolved.
        :raises TransformationError: if the input node's datatype is an array
            but any of the array's dimensions are unknown.
        :raises TransformationError: if the input node doesn't have an
            ancestor statement.
        :raises TransformationError: if the input node contains a call
            that isn't guaranteed to be pure.
        """
        # Validate the input options and types.
        self.validate_options(**kwargs)

        if not isinstance(node, DataNode):
            raise TypeError(
                f"Input node to DataNodeExtractTrans should be a "
                f"DataNode but got '{type(node).__name__}'."
            )

        dtype = node.datatype

        calls = node.walk(Call)
        for call in calls:
            if not call.is_pure:
                raise TransformationError(
                    f"Input node to DataNodeExtractTrans contains a call "
                    f"that is not guaranteed to be pure. Input node is "
                    f"'{node.debug_string().strip()}'."
                )
        if isinstance(dtype, ArrayType):
            for element in dtype.shape:
                if element in [ArrayType.Extent.DEFERRED,
                               ArrayType.Extent.ATTRIBUTE]:
                    raise TransformationError(
                        f"Input node's datatype is an array of unknown size, "
                        f"so the DataNodeExtractTrans cannot be applied. "
                        f"Input node was '{node.debug_string().strip()}'."
                    )
                # Otherwise we have an ArrayBounds
                symbols = set()
                if isinstance(element.lower, DataNode):
                    symbols.update(element.lower.get_all_accessed_symbols())
                if isinstance(element.upper, DataNode):
                    symbols.update(element.upper.get_all_accessed_symbols())
                scope_symbols = node.scope.symbol_table.get_symbols()
                for sym in symbols:
                    scoped_name_sym = scope_symbols.get(sym.name, None)
                    if scoped_name_sym and sym is not scoped_name_sym:
                        # If its an imported symbol we need to check if its
                        # the same import interface.
                        if (isinstance(sym.interface, ImportInterface) and
                            isinstance(scoped_name_sym.interface,
                                       ImportInterface)):
                            # If they have the same container symbol name
                            # then its fine, otherwise we fall into the
                            # TransformationError
                            if (sym.interface.container_symbol.name ==
                                scoped_name_sym.interface.
                                    container_symbol.name):
                                continue
                        raise TransformationError(
                            f"Input node contains an imported symbol whose "
                            f"name collides with an existing symbol, so the "
                            f"DataNodeExtractTrans cannot be applied. "
                            f"Clashing symbol name is '{sym.name}'."
                        )

        if node.ancestor(Statement) is None:
            raise TransformationError(
                "Input node to DataNodeExtractTrans has no ancestor "
                "Statement node which is not supported."
            )

        if isinstance(dtype, (UnresolvedType, UnsupportedFortranType)):
            raise TransformationError(
                f"Input node's datatype cannot be computed, so the "
                f"DataNodeExtractTrans cannot be applied. Input node was "
                f"'{node.debug_string().strip()}'."
            )

    def apply(self, node: DataNode, storage_name: str = "", **kwargs):
        """Applies the DataNodeExtractTrans to the input arguments.

        :param node: The datanode to extract.
        :param storage_name: The name of the temporary variable to store
            the result of the input node in. The default is tmp(_...)
            based on the rules defined in the SymbolTable class.
        """
        # Call validate to check inputs are valid.
        self.validate(node, storage_name=storage_name, **kwargs)

        # Find the datatype
        datatype = node.datatype

        # Create a symbol of the relevant type.
        if not storage_name:
            symbol = node.scope.symbol_table.new_symbol(
                root_name="tmp",
                symbol_type=DataSymbol,
                datatype=datatype
            )
        else:
            symbol = node.scope.symbol_table.new_symbol(
                root_name=storage_name,
                symbol_type=DataSymbol,
                allow_renaming=False,
                datatype=datatype
            )

        # FIXME Make sure the shape is all in the symbol table. We know that
        # all symbols we find can be safely added as otherwise validate will
        # fail.
        # This is an oversimplification because we could have multiple
        # references to the same symbol...
        if isinstance(datatype, ArrayType):
            for element in datatype.shape:
                symbols = set()
                if isinstance(element.lower, DataNode):
                    symbols.update(element.lower.get_all_accessed_symbols())
                if isinstance(element.upper, DataNode):
                    symbols.update(element.upper.get_all_accessed_symbols())
                scope_symbols = node.scope.symbol_table.get_symbols()
                for sym in symbols:
                    scoped_name_sym = scope_symbols.get(sym.name, None)
                    # If no symbol with the name exists then create one.
                    if not scoped_name_sym:
                        sym_copy = sym.copy()
                        if isinstance(sym_copy.interface, ImportInterface):
                            # Check if the ContainerSymbol is already in the
                            # interface
                            container = scope_symbols.get(
                                sym_copy.interface.container_symbol.name,
                                None
                            )
                            if container is None:
                                # Add the container symbol the the symbol table
                                # and we're ok with this symbol.
                                node.scope.symbol_table.add(
                                     sym_copy.interface.container_symbol
                                 )
                            # If we find the container then we need to update
                            # the interface to use the container listed.
                            else:
                                sym_copy.interface.container_symbol = \
                                        container
                        node.scope.symbol_table.add(sym_copy)

        # Create a Reference to the new symbol
        new_ref = Reference(symbol)

        # Find the parent and position of the statement containing the
        # DataNode.
        parent = node.ancestor(Statement).parent
        pos = node.ancestor(Statement).position

        # Replace the datanode with the new reference
        node.replace_with(new_ref)

        # Create an assignment to set the value of the new symbol
        assign = Assignment.create(new_ref.copy(), node)

        # Add the assignment into the tree.
        parent.addchild(assign, pos)


__all__ = ["DataNodeExtractTrans"]
