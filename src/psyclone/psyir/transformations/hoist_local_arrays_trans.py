# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

'''This module contains the HoistLocalArrayTrans transformation. 

'''

from fparser.two import Fortran2003

from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.psyGen import Transformation
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (Routine, Assignment, Schedule, Container,
                                  ArrayReference, Range, FileContainer,
                                  CodeBlock, IfBlock, UnaryOperation)
from psyclone.psyir.symbols import ArrayType, DataSymbol
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class HoistLocalArraysTrans(Transformation):
    '''This transformation takes a XXXXXX:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import HoistTrans
    >>> code = ("program test\\n"
    ...         "  integer :: i,j,n\\n"
    ...         "  real :: a(n,n)\\n"
    ...         "  real value\\n"
    ...         "  do i=1,n\\n"
    ...         "    value = 1.0\\n"
    ...         "    do j=1,n\\n"
    ...         "      a(i,j) = value\\n"
    ...         "    end do\\n"
    ...         "  end do\\n"
    ...         "end program\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> hoist = HoistTrans()
    >>> hoist.apply(psyir.walk(Assignment)[0])
    >>> print(FortranWriter()(psyir))
    program test
      integer :: i
      integer :: j
      integer :: n
      real, dimension(n,n) :: a
      real :: value
    <BLANKLINE>
      value = 1.0
      do i = 1, n, 1
        do j = 1, n, 1
          a(i,j) = value
        enddo
      enddo
    <BLANKLINE>
    end program test
    <BLANKLINE>

    '''
    def apply(self, node, options=None):
        '''Applies the transformation to the supplied Routine node,
        moving any local arrays up to Container scope and adding
        a suitable allocation when they are first accessed.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(node, options)

        container = node.ancestor(Container)

        # Identify all automatic arrays
        automatic_arrays = []
        for sym in node.symbol_table.local_datasymbols:
            if sym.is_array and all(isinstance(dim, ArrayType.ArrayBounds)
                                    for dim in sym.shape):
                automatic_arrays.append(sym)

        arefs = []
        for sym in automatic_arrays:
            orig_shape = sym.datatype.shape[:]
            # Create a new shape for the symbol with DEFERRED extent.
            new_shape = len(sym.shape)*[ArrayType.Extent.DEFERRED]
            # Modify the existing symbol so that any references to it
            # remain valid.
            sym.datatype._shape = new_shape
            # TODO handle the case where there's a clash with an existing
            # symbol at module scope.
            container.symbol_table.add(sym)
            # Create an allocate statement for the new symbol in
            # the original routine.
            dim_list = [Range.create(dim.lower, dim.upper) for dim
                        in orig_shape]
            arefs.append(ArrayReference.create(sym, dim_list))

        freader = FortranReader()
        fwriter = FortranWriter()
        # TODO #1366: we have to use a CodeBlock in order to query whether or
        # not the array has been allocated already.
        code = f"allocated({automatic_arrays[0].name})"
        expr = freader.psyir_from_expression(code, node.symbol_table)
        if_expr = UnaryOperation.create(UnaryOperation.Operator.NOT, expr)
        # TODO #1366: we also have to use a CodeBlock for the allocate().
        alloc_arg = ",".join(fwriter(aref) for aref in arefs)
        body = [freader.psyir_from_statement(f"allocate({alloc_arg})",
                                             node.symbol_table)]
        # Insert the conditional allocation at the start of the supplied
        # routine.
        node.children.insert(0, IfBlock.create(if_expr, body))

        for sym in automatic_arrays:
            # Currently the SymbolTable.remove() method does not support
            # DataSymbols.
            del node.symbol_table._symbols[sym.name]


    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target for a hoist
        transformation. At this stage only an assignment statement is
        allowed to be hoisted, see #1445. It should also be tested if
        there is a directive outside of the loop, see #1446

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        :raises TransformationError: if the supplied node is not an \
            assignment.
        :raises TransformationError: if the assignment is not within a \
            loop.
        :raises TransformationError: if the assignment is not a direct \
            child of the the loop.

        '''
        # The node should be an assignment
        if not isinstance(node, Routine):
            raise TransformationError(
                f"The target of the HoistLocalArraysTrans transformation "
                f"should be a Routine but found '{type(node).__name__}'.")

        # The assignment should be within a loop.
        container = node.ancestor(Container)
        if not container:
            raise TransformationError(
                f"The supplied routine '{node.name}' should "
                f"be within a Container but none was found.")
        if isinstance(container, FileContainer):
            raise TransformationError(
                f"The supplied routine '{node.name}' should be within a "
                f"Container but the enclosing container is a "
                f"FileContainer (named '{container.name}').")

    def __str__(self):
        return "Hoist an assignment outside of its parent loop"


# For Sphinx AutoAPI documentation generation
__all__ = ["HoistLocalArraysTrans"]
