# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Author: H. Brunie, University of Grenoble Alpes

"""Module providing a transformation that replace PsyIR Node static const 
Reference with a Literal node when possible. """

from psyclone.psyir.symbols import (
    DataSymbol,
    Symbol,
    SymbolTable,
    ArrayType,
    DataType,
)
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    DataNode,
    Routine,
    Reference,
    Literal,
    Container,
)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError,
)

from typing import Dict, List, Union


class ReplaceReferenceByLiteralTrans(Transformation):
    """Replace Reference by Literal if the corresponding symbol from
    the symbol table is constant. That is to say this is a Fortran parameter.
    For example:

    TODO: shoule we make it also for the module symbol table parameter?

    eg:
    module toto
    integer, parameter x= 3
    integer, parameter z = x*2

    becomes
    module toto
    integer, parameter x= 3
    integer, parameter z = 3*2

    """

    def __str__(self):
        return (
            "Replaces all static const Reference"
            + " by its Literal in a subroutine."
        )

    def _update_param_table(
        self, param_table: Dict[str, Literal], symbol_table: SymbolTable
    ) -> Dict[str, Literal]:
        for sym in symbol_table.symbols:
            sym: Symbol
            if not isinstance(sym, DataSymbol):
                continue
            if sym.is_constant:
                sym_name = sym.name
                if param_table.get(sym_name):
                    raise TransformationError(
                        f"Symbol already found {sym_name}."
                    )
                if not isinstance(sym.initial_value, Literal):
                    raise TransformationError(
                        f"DataSymbol {sym_name} initial value is not "
                        + f"a Literal {type(sym.initial_value)}."
                    )
                new_literal = sym.initial_value.copy().detach()
                param_table[sym_name] = new_literal
        return param_table

    def _replace_bounds(
        self,
        current_shape: List[Union[Literal, Reference]],
        param_table: Dict[str, Literal],
    ) -> List[Union[Literal, Reference]]:
        new_shape = []
        for dim in current_shape:
            if isinstance(dim, ArrayType.ArrayBounds):
                dim_upper = dim.upper.copy()
                dim_lower = dim.lower.copy()
                ref: DataNode = dim.upper
                if isinstance(ref, Reference) and ref.name in param_table:
                    literal: Literal = param_table[ref.name]
                    dim_upper = literal.copy()
                ref = dim.lower
                if isinstance(ref, Reference) and ref.name in param_table:
                    literal: Literal = param_table[ref.name]
                    dim_lower = literal.copy()
                new_bounds = ArrayType.ArrayBounds(dim_lower, dim_upper)
                new_shape.append(new_bounds)
            else:
                # This dimension is specified with an ArrayType.Extent
                # so no need to copy.
                new_shape.append(dim)
        return new_shape

    # ------------------------------------------------------------------------
    def apply(self, node: Routine, options=None):
        self.validate(node, options)
        param_table: Dict[str, Literal] = {}
        if isinstance(node.parent, Container):
            param_table = self._update_param_table(
                param_table, node.parent.symbol_table
            )

        param_table = self._update_param_table(param_table, node.symbol_table)

        for ref in node.walk(Reference):
            ref: Reference
            if ref.name in param_table:
                literal: Literal = param_table[ref.name]
                ref.replace_with(literal.copy())

        for sym in node.symbol_table.symbols:
            sym: Symbol
            if isinstance(sym, DataSymbol) and sym.is_array:
                from psyclone.psyir.symbols.datatypes import (
                    UnsupportedFortranType,
                )

                if not isinstance(sym.datatype, UnsupportedFortranType):
                    new_shape: List[Union[Literal, Reference]] = (
                        self._replace_bounds(sym.shape, param_table)
                    )
                    sym.datatype = ArrayType(sym.datatype.datatype, new_shape)
        ## For debug and triggering raise Error.
        self._param_table = param_table

    # ------------------------------------------------------------------------
    def validate(self, node, options=None):
        """Perform various checks to ensure that it is valid to apply the
        ReplaceReferenceByLiteralTrans transformation to the supplied PSyIR
        Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`

        :raises TransformationError: if the node argument is not a \
            Routine.

        """
        if not isinstance(node, Routine):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"argument should be a PSyIR Routine, but found "
                f"'{type(node).__name__}'."
            )
        if node.symbol_table is None:
            raise TransformationError("SymbolTable is None")


__all__ = ["ReplaceReferenceByLiteralTrans"]
