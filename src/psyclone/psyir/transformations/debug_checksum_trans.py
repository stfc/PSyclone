# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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

from psyclone.core import VariablesAccessInfo
from psyclone.psyir.nodes import Assignment, Node, Reference, Routine
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, PreprocessorInterface, ScalarType
)


class DebugChecksumTrans(RegionTrans):
    '''
    Creates a set of checksums (written via print) for all written to arrays
    inside the provided region.

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
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
      PRINT *, "checksums from mysubroutine at line:", PSYCLONE_INTERNAL_line_\
+ 1
      PRINT *, "a checksum", SUM(a)
    <BLANKLINE>
    end subroutine mysubroutine
    <BLANKLINE>

    '''

    def apply(self, node: Union[Node, List[Node]], options=None) -> None:
        '''
            Applies the checksum transformation to the provided node(s).

            :param nodes: The node or list of nodes to apply the
                          transformation to.
            :param options: a dictionary with options for transformations.
            :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node)

        node_list = self.get_node_list(node)

        # Find all the writes.
        vai = VariablesAccessInfo(node_list)

        writes = []
        for sig in vai.all_data_accesses:
            if vai.is_written(sig) and vai[sig].is_array():
                sym = vai[sig].all_accesses[0].node.symbol
                if (isinstance(sym, DataSymbol) and sym.datatype.intrinsic in
                        [ScalarType.Intrinsic.REAL,
                         ScalarType.Intrinsic.INTEGER]):
                    writes.append(sym)
        # For each write, add a checksum after.
        checksum_nodes = []
        freader = FortranReader()
        for sym in writes:
            sym_name = sym.name
            checksum = freader.psyir_from_statement(
                    f'print *, "{sym_name} checksum", SUM({sym_name})',
                    node_list[0].ancestor(Routine).symbol_table)
            # Remove the comment about this being a code block.
            checksum.preceding_comment = ""
            checksum_nodes.append(checksum)

        # Find the last node in the region
        depth = 10000000
        position = -1
        for node in node_list:
            if node.depth < depth:
                depth = node.depth
                position = node.position
            elif node.depth == depth and node.position > position:
                position = node.position

        parent = node.parent
        for node in checksum_nodes:
            parent.addchild(node, position+1)

        internal_line = \
            node_list[0].ancestor(Routine).symbol_table.find_or_create(
                "PSYCLONE_INTERNAL_line_", symbol_type=DataSymbol,
                datatype=INTEGER_TYPE,
                )
        line = node_list[0].ancestor(Routine).symbol_table.find_or_create(
                "__LINE__", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
                interface=PreprocessorInterface())
        # Tell us where we are to output the checksums.
        explanation_statement = freader.psyir_from_statement(
                f'print *, "checksums from '
                f'{node_list[0].ancestor(Routine).name} at line:"'
                f', PSYCLONE_INTERNAL_line_ + 1',
                node_list[0].ancestor(Routine).symbol_table
                )
        # Remove the comment about this being a code block.
        explanation_statement.preceding_comment = ""
        assign = Assignment.create(Reference(internal_line), Reference(line))
        parent.addchild(explanation_statement, position+1)
        parent.addchild(assign, position+1)
