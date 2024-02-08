# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the OMPTargetTrans PSyIR transformation '''

from psyclone.psyir.nodes import CodeBlock, OMPTargetDirective
from psyclone.psyir.transformations.region_trans import RegionTrans


class OMPTargetTrans(RegionTrans):
    '''
    Adds an OpenMP target directive to a region of code.

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.psyir.transformations import OMPTargetTrans
    >>>
    >>> tree = FortranReader().psyir_from_source("""
    ...     subroutine my_subroutine()
    ...         integer, dimension(10, 10) :: A
    ...         integer :: i
    ...         integer :: j
    ...         do i = 1, 10
    ...             do j = 1, 10
    ...                 A(i, j) = 0
    ...             end do
    ...         end do
    ...     end subroutine
    ...     """)
    >>> OMPTargetTrans().apply(tree.walk(Loop)[0])
    >>> print(FortranWriter()(tree))
    subroutine my_subroutine()
      integer, dimension(10,10) :: a
      integer :: i
      integer :: j
    <BLANKLINE>
      !$omp target
      do i = 1, 10, 1
        do j = 1, 10, 1
          a(i,j) = 0
        enddo
      enddo
      !$omp end target
    <BLANKLINE>
    end subroutine my_subroutine
    <BLANKLINE>

    '''
    excluded_node_types = (CodeBlock, )

    def apply(self, node, options=None):
        ''' Insert an OMPTargetDirective before the provided node or list
        of nodes.

        :param node: the PSyIR node or nodes to enclose in the OpenMP \
                      target region.
        :type node: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str,Any]]

        '''
        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        node_list = self.get_node_list(node)
        self.validate(node_list, options)

        # Create a directive containing the nodes in node_list and insert it.
        parent = node_list[0].parent
        start_index = node_list[0].position
        directive = OMPTargetDirective(
            parent=parent, children=[node.detach() for node in node_list])

        parent.children.insert(start_index, directive)
