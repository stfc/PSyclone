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
# Author  A. B. G. Chalk, STFC Daresbury Lab

'''This module contains the DebugChecksumTrans class.'''

from typing import Union, List

from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Assignment, Node, Reference, Routine
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.structure_accessor_mixin import (
    StructureAccessorMixin
)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, PreprocessorInterface, ScalarType
)
from psyclone.psyir.transformations.region_trans import RegionTrans


class DebugChecksumTrans(RegionTrans):
    '''
    Creates a set of checksums (written via print) for all written arrays
    inside the provided region.

    .. warning::
        This transformation generates code that will only work with .F90
        suffixed files. When using this transformation make sure the
        output file declared to PSyclone is .F90 and not .f90.

    For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.transformations import DebugChecksumTrans

    >>> psyir = FortranReader().psyir_from_source("""
    ...     subroutine mysubroutine()
    ...     integer, dimension(10,10) :: A
    ...     integer :: i
    ...     integer :: j
    ...     do i = 1, 10
    ...       do j = 1, 10
    ...         A(i,j) = A(i,k) + i-j
    ...       end do
    ...     end do
    ...     end subroutine
    ...     """)
    ... loop = psyir.children[0].children[0]
    ... DebugChecksumTrans().apply(loop)
    ... print(FortranWriter()(psyir))
    subroutine mysubroutine()
      integer, dimension(10,10) :: a
      integer :: i
      integer :: j
      integer :: PSYCLONE_INTERNAL_line_
    <BLANKLINE>
      do i = 1, 10, 1
        do j = 1, 10, 1
          a(i,j) = a(i,j) + i - j
        enddo
      enddo
      PSYCLONE_INTERNAL_line_ = __LINE__
      PRINT *, "PSyclone checksums from mysubroutine at line:", \
PSYCLONE_INTERNAL_line_ + 1
      PRINT *, "a checksum", SUM(a)
    <BLANKLINE>
    end subroutine mysubroutine
    <BLANKLINE>

    '''
    def apply(self, node: Union[Node, List[Node]], options=None) -> None:
        '''
        Applies the checksum transformation to the provided node(s).

        :param node: The node or list of nodes to apply the
                      transformation to.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options={"node-type-check": False})

        node_list = self.get_node_list(node)
        routine = node_list[0].ancestor(Routine)
        routine_table = routine.symbol_table

        fwriter = FortranWriter()
        writes = []
        # Loop over the assignments in the region
        assigns = []
        for node in node_list:
            assigns.extend(node.walk(Assignment))
        # Loop through the assignments and find the arrays
        for assign in assigns:
            # If we find a structure, we need to check that the final member
            # is the only array access and is a supported type.
            if isinstance(assign.lhs, StructureAccessorMixin):
                # If the reference at assign.lhs is both a Structure
                # and an Array (e.g. struct(i)%member...) then we
                # can't know which arrays or indexes to sum over, so
                # we need to prevent generating a Checksum for this
                # element.
                multiple_arrays = isinstance(assign.lhs, ArrayMixin)
                # Find the last member.
                member = assign.lhs.member
                while isinstance(member, StructureAccessorMixin):
                    # If we have an ArrayMixin Member that isn't the
                    # final Member in the Structure then we can't
                    # generate a checksum sensibly, as PSyclone can't know
                    # which arrays or indexes to sum over.
                    if (isinstance(member, ArrayMixin) and
                            member.member is not None):
                        multiple_arrays = True
                    member = member.member
                datatype = assign.lhs.datatype
                while not isinstance(datatype, ScalarType):
                    datatype = datatype.datatype
                # If the final member is the only array, and its a supported
                # datatype then we add it to the writes.
                if (isinstance(member, ArrayMixin) and datatype.intrinsic in
                        [ScalarType.Intrinsic.REAL,
                         ScalarType.Intrinsic.INTEGER] and
                        not multiple_arrays):
                    writes.append(assign.lhs)
            elif (isinstance(assign.lhs, ArrayMixin)
                  and assign.lhs.datatype.intrinsic in
                  [ScalarType.Intrinsic.REAL, ScalarType.Intrinsic.INTEGER]):
                writes.append(assign.lhs)

        # For each write, add a checksum after.
        checksum_nodes = []
        freader = FortranReader()
        for lhs in writes:
            copy = lhs.copy()
            name, _ = copy.get_signature_and_indices()
            # Find the section that is the array we need to checksum.
            if isinstance(lhs, StructureAccessorMixin):
                # We know this has the final member as the
                # only array.
                member = copy.member
                while isinstance(member, StructureAccessorMixin):
                    member = member.member
                array_bit = member
            else:
                array_bit = copy
            # Need to convert the lhs to a full range variant.
            for i, _ in enumerate(array_bit.indices):
                new_index = array_bit.get_full_range(i)
                array_bit.indices[i].replace_with(new_index)
            array = fwriter(copy)

            checksum = freader.psyir_from_statement(
                    f'print *, "{name} checksum", SUM({array})',
                    node_list[0].ancestor(Routine).symbol_table)
            # Remove the comment about this being a code block.
            checksum.preceding_comment = ""
            checksum_nodes.append(checksum)

        # If we didn't add any checksums then stop.
        # TODO #11: Add logging that we asked for checksums but didn't add
        # any.
        if len(checksum_nodes) == 0:
            return

        # Find the position of the last node in the region
        position = node_list[-1].position

        parent = node_list[-1].parent
        for node in checksum_nodes:
            parent.addchild(node, position+1)

        # Add a symbol to store the line number in PSyclone. This is needed as
        # fparser can't process __LINE__, so we can't use that in the print
        # statement. Instead we create an assignment with this symbol and
        # __LINE__, and use the internal symbol to create the print statement.
        internal_line = routine_table.find_or_create(
                "PSYCLONE_INTERNAL_line_", symbol_type=DataSymbol,
                datatype=INTEGER_TYPE,
                )
        line = routine_table.find_or_create(
                "__LINE__", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
                interface=PreprocessorInterface())
        # Tell us where we are to output the checksums.
        explanation_statement = freader.psyir_from_statement(
                f'print *, "PSyclone checksums from '
                f'{node_list[0].ancestor(Routine).name} at line:"'
                f', PSYCLONE_INTERNAL_line_ + 1',
                routine_table
                )
        # Remove the comment about this being a code block.
        explanation_statement.preceding_comment = \
            "PSyclone DebugChecksumTrans-generated checksums"
        assign = Assignment.create(Reference(internal_line), Reference(line))
        parent.addchild(explanation_statement, position+1)
        parent.addchild(assign, position+1)
