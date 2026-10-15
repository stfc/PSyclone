# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the implementation of the OpenMP Critical
transformation.'''

from typing import Union, List

from psyclone.psyir.nodes import (Node, OMPDirective,
                                  OMPCriticalDirective, Schedule)
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class OMPCriticalTrans(RegionTrans):
    '''
    Adds an OpenMP critical directive around a region of code.

    .. note:: This transformation does not currently do any validation
              about critical regions, and their correct usage is up to the
              user. In the future, we may try to improve this (after issues
              like #3238 are resolved).
    '''
    # A critical region is not allowed to contain another OpenMP directive.
    excluded_node_types = (OMPDirective,)

    def apply(self,
              nodes: Union[Node, Schedule, List[Node]],
              **kwargs) -> None:
        '''
        Surrounds the provided nodes in a OMPCriticalDirective.

        :param nodes: the PSyIR node or nodes to enclose in the OpenMP
            critical region.
        '''

        node_list = self.get_node_list(nodes)
        # Perform validation.
        self.validate(node_list, **kwargs)

        # Create a directive containing the nodes in node_list and insert
        # it into the tree.
        parent = node_list[0].parent
        start_index = node_list[0].position
        directive = OMPCriticalDirective(
            parent=parent, children=[node.detach() for node in node_list]
        )
        parent.children.insert(start_index, directive)


__all__ = ["OMPCriticalTrans"]
