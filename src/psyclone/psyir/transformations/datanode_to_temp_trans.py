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

'''This module contains the DataNodeToTempTrans class.'''

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
from psyclone.psyir.symbols.interfaces import (
    UnresolvedInterface,
    UnknownInterface
)
from psyclone.psyir.symbols import (
    DataSymbol, ImportInterface, ContainerSymbol, Symbol)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class DataNodeToTempTrans(Transformation):
    """Provides a generic transformation for moving a datanode from a
    statement into a new standalone statement. For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import DataNodeToTempTrans
    >>>
    >>> psyir = FortranReader().psyir_from_source('''
    ...     subroutine my_subroutine()
    ...         integer :: i
    ...         integer :: j
    ...         i = j * 2
    ...     end subroutine
    ...     ''')
    >>> assign = psyir.walk(Assignment)[0]
    >>> DataNodeToTempTrans().apply(assign.rhs, storage_name="temp")
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
                f"Input node to DataNodeToTempTrans should be a "
                f"DataNode but got '{type(node).__name__}'."
            )

        dtype = node.datatype

        calls = node.walk(Call)
        for call in calls:
            if not call.is_pure:
                raise TransformationError(
                    f"Input node to DataNodeToTempTrans contains a call "
                    f"'{call.debug_string().strip()}' that is not guaranteed "
                    f"to be pure. Input node is "
                    f"'{node.debug_string().strip()}'."
                )
        if isinstance(dtype, ArrayType):
            for element in dtype.shape:
                if element in [ArrayType.Extent.DEFERRED,
                               ArrayType.Extent.ATTRIBUTE]:
                    raise TransformationError(
                        f"Input node's datatype is an array of unknown size, "
                        f"so the DataNodeToTempTrans cannot be applied. "
                        f"Input node was '{node.debug_string().strip()}'."
                    )
                # The shape must now be set by ArrayBounds, we need to
                # examine the symbols used to define those bounds.
                symbols = set()
                if isinstance(element.lower, DataNode):
                    symbols.update(element.lower.get_all_accessed_symbols())
                if isinstance(element.upper, DataNode):
                    symbols.update(element.upper.get_all_accessed_symbols())
                # Compare the symbols in the array bounds with the symbols
                # already in the scope.
                scope_symbols = node.scope.symbol_table.get_symbols()
                for sym in symbols:
                    scoped_name_sym = scope_symbols.get(sym.name, None)
                    # If sym is not scoped_name_sym, then there is a
                    # symbol collision from an imported symbol.
                    if scoped_name_sym and sym is not scoped_name_sym:
                        # If the symbol in scoped is imported from the same
                        # container then we can skip this.
                        if (isinstance(scoped_name_sym.interface,
                                       ImportInterface) and
                            (scoped_name_sym.interface.container_symbol.name
                                == sym.interface.container_symbol.name)):
                            continue
                        raise TransformationError(
                            f"The type of the node supplied to {self.name} "
                            f"depends upon an imported symbol '{sym.name}' "
                            f"which has a name clash with a symbol in the "
                            f"current scope."
                        )
                    # If its not in the current scope, and its visibility is
                    # private then we can't import it.
                    if (not scoped_name_sym and sym.visibility ==
                            Symbol.Visibility.PRIVATE):
                        raise TransformationError(
                            f"The datatype of the node suppled to "
                            f"{self.name} depends upon an imported symbol "
                            f"'{sym.name}' that is declared as private in "
                            f"its containing module, so cannot be imported."
                        )
                    # If its an imported symbol we need to check if its
                    # the same import interface.
                    if isinstance(sym.interface, ImportInterface):
                        scoped_name_sym = scope_symbols.get(
                                sym.interface.container_symbol.name,
                                None
                        )
                        if scoped_name_sym and not isinstance(
                                scoped_name_sym, ContainerSymbol):
                            raise TransformationError(
                                f"Input node contains an imported symbol "
                                f"'{sym.name}' whose containing module "
                                f"collides with an existing symbol. Colliding "
                                f"name is "
                                f"'{sym.interface.container_symbol.name}'."
                                )

        if node.ancestor(Statement) is None:
            raise TransformationError(
                "Input node to DataNodeToTempTrans has no ancestor "
                "Statement node which is not supported."
            )

        if isinstance(dtype, (UnresolvedType, UnsupportedFortranType)):
            failing_symbols = []
            symbols = node.get_all_accessed_symbols()
            for sym in symbols:
                if isinstance(sym.interface, (UnresolvedInterface,
                                              UnknownInterface)):
                    failing_symbols.append(sym.name)
            # Sort the order of the list to get consistant results for tests.
            failing_symbols.sort()
            message = (
                f"Input node's datatype cannot be computed, so the "
                f"DataNodeToTempTrans cannot be applied. Input node was "
                f"'{node.debug_string().strip()}'."
            )
            if failing_symbols:
                message += (
                    f" The following symbols in the input node are not "
                    f"resolved in the scope: '{failing_symbols}'. Setting "
                    f"RESOLVE_IMPORTS in the transformation script may "
                    f"enable resolution of these symbols."
                )
            raise TransformationError(message)

    def apply(self, node: DataNode, storage_name: str = "", **kwargs):
        """Applies the DataNodeToTempTrans to the input arguments.

        :param node: The datanode to extract.
        :param storage_name: The base name of the temporary variable to store
            the result of the input node in. The default is tmp(_...)
            based on the rules defined in the SymbolTable class.
        """
        # Call validate to check inputs are valid.
        self.validate(node, storage_name=storage_name, **kwargs)

        # Find the datatype
        datatype = node.datatype

        # Make sure the shape is all in the symbol table. We know that
        # all symbols we find can be safely added as otherwise validate will
        # fail.
        # Symbols used to reference shapes that are from imported modules but
        # that aren't currently in the symbol table will be placed into the
        # symbol table with a corresponding ImportInterface so the resultant
        # symbol will reference the original definition of the shape in the
        # containing module.
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
            # Now we've created the relevant symbols, we need to update
            # the datatype to use the in-scope symbols
            datatype.replace_symbols_using(node.scope.symbol_table)

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
                datatype=datatype
            )
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


__all__ = ["DataNodeToTempTrans"]
