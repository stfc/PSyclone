# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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

"""Module providing a transformation that replace PsyIR Node representing a
static, constant value with a Literal Node when possible. """

from typing import Union, Any, Optional, Dict, List

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Container,
    DataNode,
    Literal,
    Reference,
    Routine,
)
from psyclone.psyir.symbols import (
    ArrayType,
    DataSymbol,
    SymbolTable,
    UnsupportedFortranType,
)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError,
)


class ReplaceReferenceByLiteralTrans(Transformation):
    '''
    This transformation takes a psyir Routine and replace all Reference psyir
    Nodes by Literal if the corresponding symbol from the symbol table is
    constant. That is to say the symbol is a Fortran parameter.
    NOTE: this transformation does not handle parent scopes recursively yet.
    For example:


    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.symbols import INTEGER_TYPE
    >>> from psyclone.psyir.transformations import (
        ReplaceReferenceByLiteralTrans)
    >>> source = """program test
    ... use mymod
    ... type(my_type):: t1, t2, t3, t4
    ... integer, parameter :: x=3, y=12, z=13
    ... integer, parameter :: u1=1, u2=2, u3=3, u4=4
    ... integer i, invariant, ic1, ic2, ic3
    ... real, dimension(10) :: a
    ... invariant = 1
    ... do i = 1, 10
    ...     t1%a = z
    ...     a(ic1) = u1+(ic1+x)*ic1
    ...     a(ic2) = u2+(ic2+y)*ic2
    ...     a(ic3) = u3+(ic3+z)*ic3
    ...     a(t1%a) = u4+(t1%a+u4*z)*t1%a
    ... end do
    ... end program test"""
    >>> fortran_writer = FortranWriter()
    >>> fortran_reader = FortranReader()
    >>> psyir = fortran_reader.psyir_from_source(source)
    >>> routine = psyir.walk(Routine)[0]
    >>> rrbl = ReplaceReferenceByLiteralTrans()
    >>> rrbl.apply(routine)
    >>> written_code = fortran_writer(routine)
    >>> print(written_code)
    program test
      use mymod
      integer, parameter :: x = 3
      integer, parameter :: y = 12
      integer, parameter :: z = 13
      integer, parameter :: u1 = 1
      integer, parameter :: u2 = 2
      integer, parameter :: u3 = 3
      integer, parameter :: u4 = 4
      type(my_type) :: t1
      type(my_type) :: t2
      type(my_type) :: t3
      type(my_type) :: t4
      integer :: i
      integer :: invariant
      integer :: ic1
      integer :: ic2
      integer :: ic3
      real, dimension(10) :: a
      <BLANKLINE>
      invariant = 1
      do i = 1, 10, 1
        t1%a = 13
        a(ic1) = 1 + (ic1 + 3) * ic1
        a(ic2) = 2 + (ic2 + 12) * ic2
        a(ic3) = 3 + (ic3 + 13) * ic3
        a(t1%a) = 4 + (t1%a + 4 * 13) * t1%a
      enddo
      <BLANKLINE>
    end program test
    <BLANKLINE>

    '''
    def __init__(self) -> None:
        super().__init__()
        # Dictionary with Literal values of the corresponding symbol
        # from symbol_table (based on symbol name as a string).
        self._param_table: Dict[str, Literal] = {}

    def _update_param_table(
        self,
        param_table: Dict[str, Literal],
        symbol_table: SymbolTable,
    ) -> Dict[str, Literal]:
        """This methods takes a param_table as entry, updates this dictionary
        and then returns the same dictionary updated.

        * Goes through all datasymbols in the symbol_table.
            * if symbol already in param_table or initial_value is not Literal:
                * annotate code with warning.
            * copy and detach the symbol.initial_value (Literal)
            * update the param_table with this copy.
        * Returns the updated param_table

        :param param_table: To be updated
        :param symbol_table: scope symbol table to look for the symbols.

        :return: the updated param_table (same reference as the entry one)
        """
        for sym in symbol_table.datasymbols:
            sym: DataSymbol
            if sym.is_constant:
                sym_name = sym.name
                if param_table.get(sym_name):
                    message = (
                        f"{self.name}:"
                        + f" Symbol already found '{sym_name}'."
                        + " A conflict is possible."
                        + "To avoid replacing the symbol with the wrong value,"
                        + " we skip this symbol."
                    )
                    sym.preceding_comment += message
                    param_table.pop(sym_name)
                    continue
                if not isinstance(sym.initial_value, Literal):

                    message = (
                        f"{self.name}: only "
                        + "supports symbols which have a Literal"
                        + " as their initial value but "
                        + f"'{sym_name}' is assigned "
                        + f"a {type(sym.initial_value)}"
                    )
                    sym.preceding_comment += message
                    continue
                new_literal: Literal = sym.initial_value.copy().detach()
                param_table[sym_name] = new_literal
            else:
                if isinstance(sym.datatype, UnsupportedFortranType):
                    if "PARAMETER" in sym.datatype.declaration:
                        message = (
                            f"{self.name}:"
                            + " only support constant (parameter) but "
                            + f"{sym.datatype} is not seen by "
                            + "Psyclone as a constant."
                        )
                        sym.preceding_comment += message
        return param_table

    def _replace_bounds(
        self,
        current_shape: List[Union[Literal, Reference]],
        param_table: Dict[str, Literal],
    ) -> List[Union[Literal, Reference]]:
        """From the param_table and the current_shape of an array,
        this method create a new_shape with the reference replaced by literal
        when they are found in the param_table.

        :param current_shape: shape before transformation
        :param param_table: map of parameters to Literal values.

        :return: the new shape with any references to constants replaced by
                 their Literal values.
        """
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
    def apply(self, node: Routine, options: Optional[Dict[str, Any]] = None):
        """Applies the transformation to a Routine node:
        * First update a dictionary (param_table) with the Literal of constant
        (parameter) symbol from node.parent symbol_table, and from
        node.symbol_table.
        * Second, use this updated param_table to replace reference in node
        psyir_tree with the corresponsing Literal.
        * Third, use this updated param_table to replace reference in node
        symbol_table DataSymbol array's dimensions with the corresponding
        Literal.

        Note: If an update cannot be performed for any reason then the
        substitution is skipped and a comment starting with
        'Psyclone(ReplaceReferenceByLiteral):' is added to the generated code.

        :param node: node on which the transformation is applied
        :param options: a dictionary with options for transformations.
        """
        ## Reset the param table for the current Routine
        self._param_table = {}
        self.validate(node, options)
        ## NOTE: (From Andrew) We may want to look at all symbols in scope
        # rather than just those in the parent symbol table?
        if node.parent is not None and isinstance(node.parent, Container):
            self._param_table = self._update_param_table(
                self._param_table, node.parent.symbol_table
            )

        self._param_table = self._update_param_table(
            self._param_table, node.symbol_table
        )

        for ref in node.walk(Reference):
            ref: Reference
            if ref.name in self._param_table:
                literal: Literal = self._param_table[ref.name]
                ref.replace_with(literal.copy())

        for sym in node.symbol_table.datasymbols:
            sym: DataSymbol
            if sym.is_array:
                if not isinstance(sym.datatype, UnsupportedFortranType):
                    new_shape: List[Union[Literal, Reference]] = (
                        self._replace_bounds(sym.shape, self._param_table)
                    )
                    sym.datatype = ArrayType(sym.datatype.datatype, new_shape)

    # ------------------------------------------------------------------------
    def validate(self, node, options=None):
        """Perform various checks to ensure that it is valid to apply the
        ReplaceReferenceByLiteralTrans transformation to the supplied PSyIR
        Node.

        :param node: the node that is being checked.
        :param options: not used, defaults to None
        :type options: _type_, optional

        :raises TransformationError: if the node argument is not a Routine.
        """
        if not isinstance(node, Routine):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"argument should be a PSyIR Routine, but found "
                f"'{type(node).__name__}'."
            )


__all__ = ["ReplaceReferenceByLiteralTrans"]
