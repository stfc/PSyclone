# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2026, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk, STFC Daresbury Lab
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
