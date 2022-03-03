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

'''
This module contains the HoistLocalArraysTrans transformation.

'''

from psyclone.psyGen import Transformation
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (Routine, Container, ArrayReference, Range,
                                  FileContainer, IfBlock, UnaryOperation)
from psyclone.psyir.symbols import ArrayType, Symbol
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class HoistLocalArraysTrans(Transformation):
    '''This transformation takes a Routine and promotes any local, 'automatic'
    arrays to Container scope:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import HoistLocalArraysTrans
    >>> code = ("module test_mod\\n"
    ...         "contains\\n"
    ...         "  subroutine test_sub(n)\\n"
    ...         "  integer :: i,j,n\\n"
    ...         "  real :: a(n,n)\\n"
    ...         "  real :: value = 1.0\\n"
    ...         "  do i=1,n\\n"
    ...         "    do j=1,n\\n"
    ...         "      a(i,j) = value\\n"
    ...         "    end do\\n"
    ...         "  end do\\n"
    ...         "  end subroutine test_sub\\n"
    ...         "end module test_mod\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> hoist = HoistLocalArraysTrans()
    >>> hoist.apply(psyir.walk(Routine)[0])
    >>> print(FortranWriter()(psyir).lower())
    module test_mod
      implicit none
      real, allocatable, dimension(:,:), private :: a
      public
    <BLANKLINE>
      public :: test_sub
    <BLANKLINE>
      contains
      subroutine test_sub(n)
        integer :: n
        integer :: i
        integer :: j
        real :: value = 1.0
    <BLANKLINE>
        if (.not.allocated(a)) then
          allocate(a(1 : n, 1 : n))
        end if
        do i = 1, n, 1
          do j = 1, n, 1
            a(i,j) = value
          enddo
        enddo
    <BLANKLINE>
      end subroutine test_sub
    <BLANKLINE>
    end module test_mod
    <BLANKLINE>

    '''
    def apply(self, node, options=None):
        '''Applies the transformation to the supplied Routine node,
        moving any local arrays up to Container scope and adding
        a suitable allocation when they are first accessed.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        '''
        self.validate(node, options)

        container = node.ancestor(Container)

        # Identify all arrays that are local to the target routine and
        # do not explicitly use dynamic memory allocation.
        automatic_arrays = []
        for sym in node.symbol_table.local_datasymbols:
            if sym.is_array and all(isinstance(dim, ArrayType.ArrayBounds)
                                    for dim in sym.shape):
                automatic_arrays.append(sym)

        # arefs will hold the list of array references to be allocated.
        arefs = []

        for sym in automatic_arrays:
            # Keep a copy of the original shape of the array.
            orig_shape = sym.datatype.shape[:]
            # Modify the *existing* symbol so that any references to it
            # remain valid.
            # pylint: disable=consider-using-enumerate
            for idx in range(len(sym.shape)):
                sym.shape[idx] = ArrayType.Extent.DEFERRED
            # Ensure that the promoted symbol is private to the container.
            sym.visibility = Symbol.Visibility.PRIVATE
            # We must allow for the situation where there's a clash with a
            # symbol already present at container scope.
            try:
                container.symbol_table.add(sym)
            except KeyError:
                new_name = container.symbol_table.next_available_name(
                    sym.name, other_table=node.symbol_table)
                node.symbol_table.rename_symbol(sym, new_name)
                container.symbol_table.add(sym)
            # Create the array reference that will be the argument to the
            # new memory allocation statement.
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

        # Finally, remove the hoisted symbols from the routine scope.
        for sym in automatic_arrays:
            # Currently the SymbolTable.remove() method does not support
            # DataSymbols.
            # pylint: disable=protected-access
            del node.symbol_table._symbols[sym.name]

    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target for a hoist-
        local-arrays transformation. It must be a Routine that is within
        a Container (that is not a FileContainer).

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        :raises TransformationError: if the supplied node is not a Routine.
        :raises TransformationError: if the Routine is not within a Container \
                                     (that is not a FileContainer).

        '''
        # The node should be a Routine.
        if not isinstance(node, Routine):
            raise TransformationError(
                f"The target of the HoistLocalArraysTrans transformation "
                f"should be a Routine but found '{type(node).__name__}'.")

        # The Routine must be within a Container (otherwise we have nowhere
        # to hoist any array declarations to).
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
        return "Hoist all local, automatic arrays to container scope."


# For Sphinx AutoAPI documentation generation
__all__ = ["HoistLocalArraysTrans"]
