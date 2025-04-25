# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
This module contains the ReplaceExprnWithTmp transformation.

'''

import copy

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    ArrayReference, Assignment, DataNode, IntrinsicCall, Range, Reference,
    Routine, Statement)
from psyclone.psyir.symbols import (
    ArrayType, Symbol, INTEGER_TYPE, DataSymbol, UnresolvedType,
    UnsupportedType)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class ReplaceExprnWithTmpTrans(Transformation):
    '''This transformation takes an expression node, creates a temporary,
    assigns the result of the expression to the temporary and then replaces
    the expression with a reference to the temporary.
    

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Expression
    >>> from psyclone.psyir.transformations import ReplaceExprnWithTmpTrans
    >>> code = ("module test_mod\\n"
    ...         "contains\\n"
    ...         "  subroutine test_sub(n)\\n"
    ...         "  integer :: i,j,n\\n"
    ...         "  real :: a(n,n)\\n"
    ...         "  real :: value = 1.0\\n"
    ...         "  ke_at_quad = dot_product( &\\n"
    ...         "    matmul( jac(:,:,qp1,qp2), ls_u_at_quad ), &\\n"
    ...         "    matmul( jac(:,:,qp1,qp2), u_at_quad ) )   &\\n"
    ...         "    / ( dj(qp1,qp2)**2 )\\n"
    ...         "  end subroutine test_sub\\n"
    ...         "end module test_mod\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> rtrans = ReplaceExprnWithTmpTrans()
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
        if (.not.allocated(a) .or. ubound(a, 1) /= n .or. ubound(a, 2) /= n) \
then
          if (allocated(a)) then
            deallocate(a)
          end if
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
    def apply(self, node: DataNode):
        '''Applies the transformation to the supplied node,
        TODO

        :param node: target PSyIR expression node.

        '''
        self.validate(node)

        datatype = node.datatype
        # Create a symbol for the temporary variable. If it's an array then
        # we make it allocatable.
        if isinstance(datatype, ArrayType):
            decl_type = datatype.copy()
            decl_type._shape = [ArrayType.Extent.DEFERRED]*len(datatype.shape)
        else:
            decl_type = datatype
        table = node.ancestor(Routine).symbol_table
        sym = table.new_symbol("ptmp", symbol_type=DataSymbol,
                               datatype=decl_type)

        alloc = None
        if isinstance(datatype, ArrayType):
            args = []
            for dim in datatype.shape:
                args.append(Range.create(dim.lower.copy(), dim.upper.copy()))
            aref = ArrayReference.create(sym, args)
            alloc = IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                                         [aref])
        # Assign the expression to the new Symbol.
        assign = Assignment.create(Reference(sym), node.copy())
        # Insert the assignment before the Statement containing the expression.
        cursor = node
        while cursor.ancestor(Statement):
            cursor = cursor.ancestor(Statement)
        preceding_stmt = cursor
        idx = preceding_stmt.parent.children.index(preceding_stmt)
        if alloc:
            preceding_stmt.parent.children.insert(idx, alloc)
            idx += 1
        preceding_stmt.parent.children.insert(idx, assign)
        # Replace the expression with the Symbol.
        node.replace_with(Reference(sym))

        return
        # Get the reversed tags map so that we can lookup the tag (if any)
        # associated with the symbol being hoisted.
        tags_dict = node.symbol_table.get_reverse_tags_dict()

        for sym in automatic_arrays:
            # Keep a copy of the original shape of the array.
            orig_shape = sym.datatype.shape[:]
            # Modify the *existing* symbol so that any references to it
            # remain valid.
            new_type = copy.copy(sym.datatype)
            # pylint: disable=protected-access
            new_type._shape = len(orig_shape)*[ArrayType.Extent.DEFERRED]
            # pylint: enable=protected-access
            sym.datatype = new_type
            # Ensure that the promoted symbol is private to the container.
            sym.visibility = Symbol.Visibility.PRIVATE
            # We must allow for the situation where there's a clash with a
            # symbol name already present at container scope. (The validate()
            # method will already have checked for tag clashes.)
            try:
                container.symbol_table.add(sym, tag=tags_dict.get(sym))
            except KeyError:
                new_name = container.symbol_table.next_available_name(
                    sym.name, other_table=node.symbol_table)
                node.symbol_table.rename_symbol(sym, new_name)
                container.symbol_table.add(sym, tag=tags_dict.get(sym))

            # Create the array reference that will be the argument to the
            # new memory allocation statement.
            dim_list = [Range.create(dim.lower.copy(), dim.upper.copy())
                        for dim in orig_shape]
            aref = ArrayReference.create(sym, dim_list)

            # Add a conditional expression to avoid repeating the allocation
            # if its already done
            allocated_expr = IntrinsicCall.create(
                    IntrinsicCall.Intrinsic.ALLOCATED,
                    [Reference(sym)])
            cond_expr = UnaryOperation.create(
                            UnaryOperation.Operator.NOT, allocated_expr)

            # Add runtime checks to verify that the boundaries haven't changed
            # (we skip literals as we know they can't have changed)
            check_added = False
            for idx, dim in enumerate(orig_shape):
                if not isinstance(dim.lower, Literal):
                    expr = BinaryOperation.create(
                            BinaryOperation.Operator.NE,
                            IntrinsicCall.create(
                                IntrinsicCall.Intrinsic.LBOUND,
                                [Reference(sym),
                                 ("dim", Literal(str(idx+1), INTEGER_TYPE))]),
                            dim.lower.copy())
                    # We chain the new check to the already existing cond_expr
                    # which starts with the 'not allocated' condition added
                    # before this loop.
                    cond_expr = BinaryOperation.create(
                                    BinaryOperation.Operator.OR,
                                    cond_expr, expr)
                    check_added = True
                if not isinstance(dim.upper, Literal):
                    expr = BinaryOperation.create(
                            BinaryOperation.Operator.NE,
                            IntrinsicCall.create(
                                IntrinsicCall.Intrinsic.UBOUND,
                                [Reference(sym),
                                 ("dim", Literal(str(idx+1), INTEGER_TYPE))]),
                            dim.upper.copy())
                    # We chain the new check to the already existing cond_expr
                    # which starts with the 'not allocated' condition added
                    # before this loop.
                    cond_expr = BinaryOperation.create(
                                    BinaryOperation.Operator.OR,
                                    cond_expr, expr)
                    check_added = True

            body = []
            if check_added:
                body.append(
                    IfBlock.create(
                        allocated_expr.copy(),
                        [IntrinsicCall.create(
                            IntrinsicCall.Intrinsic.DEALLOCATE,
                            [Reference(sym)])]))
            body.append(
                IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                                     [aref]))
            # Insert the conditional allocation at the start of the supplied
            # routine.
            node.children.insert(0, IfBlock.create(cond_expr, body))

        # Finally, remove the hoisted symbols (and any associated tags)
        # from the routine scope.
        for sym in automatic_arrays:
            # TODO #898: Currently the SymbolTable.remove() method does not
            # support DataSymbols.
            # pylint: disable=protected-access
            del node.symbol_table._symbols[sym.name]
            tag = tags_dict.get(sym)
            if tag:
                del node.symbol_table._tags[tag]

    def validate(self, node):
        '''Checks that the supplied node is a valid target for a hoist-
        local-arrays transformation. It must be a Routine that is within
        a Container (that is not a FileContainer).

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.DataNode`

        :raises TransformationError: if the supplied node is not a DataNode.

        '''
        super().validate(node)

        # The node should be a DataNode.
        if not isinstance(node, DataNode):
            raise TransformationError(
                f"The target of the {self.name} transformation "
                f"should be a DataNode but found '{type(node).__name__}'.")

        datatype = node.datatype
        if isinstance(datatype, (UnsupportedType, UnresolvedType)):
            raise TransformationError(
                f"Cannot apply {self.name} to expression "
                f"'{node.debug_string()}' because "
                f"the type of the result is unknown "
                f"({type(datatype).__name__}).")

    def __str__(self):
        return "Replace the supplied expression with a temporary."
