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
# Author: A. B. G. Chalk and A. R. Porter, STFC Daresbury Lab

'''This module contains the DebugChecksumTrans class.'''

from psyclone.core import SingleVariableAccessInfo, VariablesAccessMap
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    Assignment, IfBlock, Node, Reference, Routine, Statement)
from psyclone.psyir.nodes.structure_accessor_mixin import (
    StructureAccessorMixin
)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, PreprocessorInterface, ScalarType,
    UnsupportedType, UnresolvedType)
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class DebugChecksumTrans(RegionTrans):
    '''
    Creates a set of checksums (written via print) for all written arrays
    inside the provided region. If an array is only written conditionally
    then it is excluded (to avoid cases where arrays are not allocated or
    initialised).

    .. warning::
        This transformation generates code that will only work with .F90
        suffixed files. When using this transformation make sure the
        output file declared to PSyclone is .F90 and not .f90.

    For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.transformations import DebugChecksumTrans

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
    def _get_all_writes(
            self,
            node_list: list[Node]) -> list[SingleVariableAccessInfo]:
        '''
        Examines the supplied list of Nodes and returns information on
        those variables that are written to.

        :param node_list: the Nodes to examine for write accesses.

        :returns: information on variables that have write accesses.

        '''
        # Get all the variables that are accessed in the region.
        vam = VariablesAccessMap()
        for node in node_list:
            vam.update(node.reference_accesses())
        # Loop through the accesses and find arrays that are written.
        writes = []
        for sig in vam.all_signatures:
            if not vam[sig].is_written() or not vam[sig].is_array():
                continue
            first_write = vam[sig].all_write_accesses[0]
            datatype = first_write.node.datatype
            while not isinstance(datatype, (ScalarType, UnsupportedType,
                                            UnresolvedType)):
                datatype = datatype.datatype
            # We only support checksums for REAL and INTEGER variables.
            if isinstance(datatype, (UnsupportedType, UnresolvedType)):
                continue
            if datatype.intrinsic not in [ScalarType.Intrinsic.REAL,
                                          ScalarType.Intrinsic.INTEGER]:
                continue
            # If we find a structure, we need to check that the final member
            # is the only array access.
            for access in vam[sig].all_write_accesses:
                if [1 if indices else 0 for indices in
                        access.component_indices].count(1) > 1:
                    # This is a structure access with indexing on more than
                    # one component.
                    continue
                if not access.component_indices[-1]:
                    # This access does not have array indices on its final
                    # member.
                    continue
                writes.append(vam[sig])
                break
        return writes

    def apply(self, nodes: Node | list[Node], options=None) -> None:
        '''
        Applies the checksum transformation to the provided node(s).

        :param nodes: The node or list of nodes to apply the
                      transformation to.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(nodes, options={"node-type-check": False})

        node_list = self.get_node_list(nodes)
        writes = self._get_all_writes(node_list)

        # For each write, add a checksum after.
        checksum_nodes = []
        routine = node_list[0].ancestor(Routine)
        routine_table = routine.symbol_table
        fwriter = FortranWriter()
        freader = FortranReader()
        for varinfo in writes:
            copy = varinfo.all_write_accesses[0].node.copy()
            name, _ = copy.get_signature_and_indices()
            # Find the section that is the array we need to checksum.
            if isinstance(copy, StructureAccessorMixin):
                # We know this has the final member as the
                # only array.
                member = copy.member
                while isinstance(member, StructureAccessorMixin):
                    member = member.member
                array_bit = member
            else:
                array_bit = copy
            # Need to convert the ref to a full range variant.
            if hasattr(array_bit, "indices"):
                for i in range(len(array_bit.indices)):
                    new_index = array_bit.get_full_range(i)
                    array_bit.indices[i].replace_with(new_index)
            # Optimise the use of fwriter by detaching the copied node from
            # its parent tree.
            copy.detach()
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

    def validate(self, nodes: Node | list[Node], options: dict = None) -> None:
        '''
        Checks that the transformation can be applied to the supplied Node(s).

        :param nodes: the Node(s) for which checksums are to be generated.
        :param options: any options for the transformation.

        :raises TransformationError: if any of the variables to be checksummed
            is not unconditionally written.
        '''
        super().validate(nodes, options=options)

        node_list = self.get_node_list(nodes)
        writes = self._get_all_writes(node_list)

        # For quantity that is written, check that there is at least one write
        # in the same code path as the site at which we are going to insert
        # the checksum computation.
        parent = node_list[-1].parent
        for info in writes:
            for access in info.all_write_accesses:
                ref = access.node
                ifblock = ref.ancestor(IfBlock, limit=parent)
                if not ifblock or node_list[-1].is_descendent_of(ifblock):
                    # This write is in the same code path as the location of
                    # the checksum.
                    break
            else:
                raise TransformationError(
                    f"Cannot compute checksum of '{info.var_name}' because "
                    f"all writes to it ("
                    f"{ref.ancestor(Statement).debug_string()}) are in "
                    f"branch(es) that may not be executed.")
