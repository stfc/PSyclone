# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
'''This module provides the OMPParallelTrans transformation.'''

from collections.abc import Iterable
from psyclone import psyGen
from psyclone.psyir.nodes import (
    ACCDirective,
    CodeBlock,
    Node,
    OMPParallelDirective,
    OMPDirective,
    Return,
    RegionDirective,
)
from psyclone.psyir.transformations.parallel_region_trans import (
    ParallelRegionTrans)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class OMPParallelTrans(ParallelRegionTrans):
    '''
    Create an OpenMP PARALLEL region by inserting directives. For
    example:

    >>> from psyclone.tests.utilities import get_psylayer_schedule
    >>> filename = "nemolite2d_alg_mod.f90"
    >>> schedule = get_psylayer_schedule(filename, api="gocean")
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> ltrans = t.get_trans_name('GOceanOMPLoopTrans')
    >>> from psyclone.psyir.transformations import OMPParallelTrans
    >>> rtrans = OMPParallelTrans()
    >>>
    >>> # Apply the OpenMP Loop transformation to *every* loop
    >>> # in the schedule
    >>> for child in schedule.children:
    ...     ltrans.apply(child)
    >>>
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> rtrans.apply(schedule.children)

    '''
    # The types of node that this transformation cannot enclose
    excluded_node_types = (CodeBlock, Return, ACCDirective,
                           psyGen.HaloExchange)

    def __init__(self):
        super().__init__()
        # Set the type of directive that the base class will use
        self._directive_factory = OMPParallelDirective.create

    def __str__(self) -> str:
        return "Insert an OpenMP Parallel region"

    @property
    def name(self) -> str:
        '''
        :returns: the name of this transformation as a string.
        '''
        return "OMPParallelTrans"

    def validate(self, nodes: list[Node], options=None, **kwargs):
        '''
        Perform OpenMP-specific validation checks.

        :param nodes: list of Nodes to put within parallel region.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the target Nodes are already within \
                                     some OMP parallel region.
        '''
        if nodes[0].ancestor(OMPDirective):
            raise TransformationError("Error in OMPParallel transformation:" +
                                      " cannot create an OpenMP PARALLEL " +
                                      "region within another OpenMP region.")

        # Now call the general validation checks
        # TODO #2668: Remove options.
        super().validate(nodes, options, **kwargs)

    def apply(
            self, nodes: list[Node],
            options=None, force_private: Iterable[str] = (),
            **kwargs):
        '''
        Surrounds the provided node list with an OpenMP Parallel region.

        :param nodes: list of Nodes to put within parallel region.
        :param force_private: list of symbols explicitly requested to
            be private.
        '''
        # TODO #2668: Remove options.
        super().apply(nodes, options, **kwargs)

        # Privatise the provided variables for the new RegionDirective, if they
        # are found within the symbol table of the ancestor Routine.
        if force_private:
            new_region_directive = nodes[0].ancestor(RegionDirective)
            if new_region_directive:
                region_set = self._check_symbol_table_vars(
                        new_region_directive,
                        force_private)
                if region_set:
                    new_region_directive.explicitly_private_symbols.update(
                        region_set)


__all__ = ["OMPParallelTrans"]
