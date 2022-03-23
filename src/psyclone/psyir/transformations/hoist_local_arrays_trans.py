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
                                  FileContainer, IfBlock, UnaryOperation,
                                  CodeBlock)
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
        a suitable allocation when they are first accessed. If there
        are no local arrays or the supplied Routine is a program then
        this method does nothing.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        '''
        self.validate(node, options)

        if node.is_program:
            # Cannot hoist arrays out of a program so do nothing.
            return

        container = node.ancestor(Container)

        # Identify all arrays that are local to the target routine,
        # do not explicitly use dynamic memory allocation and are not
        # accessed within a CodeBlock.
        automatic_arrays = self._get_local_arrays(node)

        if not automatic_arrays:
            # No automatic arrays found so nothing to do.
            return

        # arefs will hold the list of array references to be allocated.
        arefs = []
        # Get the reversed tags map so that we can lookup the tag (if any)
        # associated with the symbol being hoisted.
        tags_dict = node.symbol_table.get_reverse_tags_dict()

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

        # Finally, remove the hoisted symbols (and any associated tags)
        # from the routine scope.
        for sym in automatic_arrays:
            # TODO #898:Currently the SymbolTable.remove() method does not
            # support DataSymbols.
            # pylint: disable=protected-access
            del node.symbol_table._symbols[sym.name]
            tag = tags_dict.get(sym)
            if tag:
                del node.symbol_table._tags[tag]

    @staticmethod
    def _get_local_arrays(node):
        '''
        Identify all arrays that are local to the target routine, do not
        represent its return value and do not explicitly use dynamic memory
        allocation. Also excludes any such arrays that are accessed within
        CodeBlocks.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`

        :returns: symbols representing routine-local arrays.
        :rtype: list[:py:class:`psyclone.psyir.symbols.DataSymbol`]

        '''
        local_arrays = {}
        for sym in node.symbol_table.local_datasymbols:
            if sym is node.return_symbol or not sym.is_array:
                continue
            # Check whether all of the bounds of the array are defined - an
            # allocatable array will have array dimensions of
            # ArrayType.Extent.DEFERRED
            if all(isinstance(dim, ArrayType.ArrayBounds)
                   for dim in sym.shape):
                local_arrays[sym.name] = sym

        # Exclude any arrays that are accessed within a CodeBlock (as they
        # may get renamed as part of the transformation).
        cblocks = node.walk(CodeBlock)
        for cblock in cblocks:
            cblock_names = set(cblock.get_symbol_names())
            array_names = set(local_arrays.keys())
            names_in_cblock = cblock_names.intersection(array_names)
            # TODO #11 - log the fact that we can't hoist the arrays
            # listed in 'names_in_cblock'.
            for name in names_in_cblock:
                del local_arrays[name]

        return list(local_arrays.values())

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
        :raises TransformationError: if any symbols corresponding to local \
            arrays have a tag that already exists in the table of the parent \
            Container.

        '''
        super().validate(node, options=options)

        # The node should be a Routine.
        if not isinstance(node, Routine):
            raise TransformationError(
                f"The target of the HoistLocalArraysTrans transformation "
                f"should be a Routine but found '{type(node).__name__}'.")

        if node.is_program:
            # We silently ignore routines that are programs - this
            # transformation will do nothing.
            return

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

        # Check for clashing tags in the container scope.
        auto_arrays = self._get_local_arrays(node)
        tags_dict = node.symbol_table.get_reverse_tags_dict()
        cont_tags_dict = container.symbol_table.tags_dict
        for sym in auto_arrays:
            tag = tags_dict.get(sym)
            if tag in cont_tags_dict:
                raise TransformationError(
                    f"The supplied routine '{node.name}' contains a local "
                    f"array '{sym.name}' with tag '{tag}' but this tag is "
                    f"also present in the symbol table of the parent "
                    f"Container (associated with variable "
                    f"'{cont_tags_dict[tag].name}').")

    def __str__(self):
        return "Hoist all local, automatic arrays to container scope."


# For Sphinx AutoAPI documentation generation
__all__ = ["HoistLocalArraysTrans"]
