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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the OMPTargetTrans PSyIR transformation '''

from psyclone.psyir.nodes import (
    CodeBlock, OMPTargetDirective, Call, Routine, Reference,
    OMPTaskwaitDirective, Directive, Schedule)
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.async_trans_mixin import \
    AsyncTransMixin
from psyclone.psyir.transformations import TransformationError


class OMPTargetTrans(RegionTrans, AsyncTransMixin):
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

    def _add_asynchronicity(self, instance: Directive):
        '''Adds asynchronicity to the provided directive if possible. If
        PSyclone's analysis suggests that it is not possible, the directive
        is left unchanged.

        :param instance: The directive to become asynchronous if possible.
        '''
        # Nodes is all the children of the OmpTargetDirective
        nodes = instance.dir_body.children[:]
        next_depend = self._find_next_dependency(nodes, instance)

        # If find_next_dependency returns False, then this loop is its own
        # next dependency so we can't add an asynchronous clause.
        if not next_depend:
            return
        # If find next_dependency returns True there is no follow up
        # dependency, so we just need a barrier at the end of the containing
        # Routine.
        if next_depend is True:
            # Add nowait to the instance.
            instance.nowait = True
            # Add a barrier to the end of the containing Routine if there
            # isn't one already.
            containing_routine = instance.ancestor(Routine)
            # Check barrier that corresponds to self.omp_directive and add the
            # correct barrier type
            if not isinstance(containing_routine.children[-1],
                              OMPTaskwaitDirective):
                containing_routine.addchild(OMPTaskwaitDirective())
            return

        # Otherwise we have the next dependency and we need to find where the
        # correct place for the preceding barrier is. Need to find a
        # guaranteed control flow path to place it.

        # Find the deepest schedule in the tree containing both.
        sched = next_depend.ancestor(Schedule, shared_with=instance)
        routine = instance.ancestor(Routine)
        if sched and sched.is_descendent_of(routine):
            # Get the path from sched to next_depend
            path = next_depend.path_from(sched)
            # The first element of path is the position of the ancestor
            # of next_depend that is in sched, so we add the barrier there.
            sched.addchild(OMPTaskwaitDirective(), path[0])
            instance.nowait = True

        # If we didn't find anywhere to put the barrier then we just don't
        # add the nowait.
        # TODO #11: If we fail to have nowait added then log it

    def validate(self, node, options=None):
        # pylint: disable=signature-differs
        '''
        Check that we can safely enclose the supplied node or list of nodes
        within an OpenMPTargetDirective.

        :param node: the PSyIR node or nodes to enclose in the OpenMP
                      target region.
        :type node: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if it contains calls to routines that
            are not available in the accelerator device.
        :raises TransformationError: if its a function and the target region
            attempts to enclose the assingment setting the return value.
        '''
        node_list = self.get_node_list(node)
        super().validate(node, options)
        for node in node_list:
            for call in node.walk(Call):
                if not call.is_available_on_device():
                    raise TransformationError(
                        f"'{call.routine.name}' is not available on the "
                        f"accelerator device, and therefore it cannot "
                        f"be called from within an OMP Target region.")
        routine = node.ancestor(Routine)
        if routine and routine.return_symbol:
            # if it is a function, the target must not include its return sym
            for check_node in node_list:
                for ref in check_node.walk(Reference):
                    if ref.symbol is routine.return_symbol:
                        raise TransformationError(
                            f"OpenMP Target cannot enclose a region that has "
                            f"a function return value symbol, but found one in"
                            f" '{routine.return_symbol.name}'.")

    def apply(self, node, options=None):
        ''' Insert an OMPTargetDirective before the provided node or list
        of nodes.

        :param node: the PSyIR node or nodes to enclose in the OpenMP
                     target region.
        :type node: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str,Any]]
        :param bool options["nowait"]: whether to add a nowait clause and a
            corresponding barrier to enable asynchronous execution.

        '''
        if not options:
            options = {}
        nowait = options.get("nowait", False)
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

        if nowait:
            self._add_asynchronicity(directive)
