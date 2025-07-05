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
#         A. B. G. Chalk, V. K. Atkinson, STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, J. G. Wallwork, O. Brunt and L. Turner, Met Office
#          S. Valat, Inria / Laboratoire Jean Kuntzmann
#          M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann
#          J. Dendy, Met Office

'''
This module provides the implementation of ParallelRegionTrans

'''

from abc import ABC, abstractmethod
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone import psyGen
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.nodes import CodeBlock, Return


class ParallelRegionTrans(RegionTrans, ABC):
    '''
    Base class for transformations that create a parallel region.

    '''
    # The types of node that must be excluded from the section of PSyIR
    # being transformed.
    excluded_node_types = (CodeBlock, Return, psyGen.HaloExchange)

    def __init__(self):
        # Holds the class instance or create call for the type of
        # parallel region to generate
        self._directive_factory = None
        super().__init__()

    @abstractmethod
    def __str__(self) -> str:
        '''
        :returns: a string explaining what this transformation does.

        '''

    def validate(self, node_list, options=None):
        # pylint: disable=arguments-renamed
        '''
        Check that the supplied list of Nodes are eligible to be
        put inside a parallel region.

        :param list node_list: list of nodes to put into a parallel region
        :param options: a dictionary with options for transformations.\
        :type options: Optional[Dict[str, Any]]
        :param bool options["node-type-check"]: this flag controls whether \
            or not the type of the nodes enclosed in the region should be \
            tested to avoid using unsupported nodes inside a region.

        :raises TransformationError: if the supplied nodes are not all \
            children of the same parent (siblings).

        '''
        node_list = self.get_node_list(node_list)

        node_parent = node_list[0].parent

        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    f"Error in {self.name} transformation: supplied nodes are "
                    f"not children of the same parent.")
        super().validate(node_list, options)

    def apply(self, target_nodes, options=None):
        # pylint: disable=arguments-renamed
        '''
        Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Loops in the
        schedule within a single parallel region.

        :param target_nodes: a single Node or a list of Nodes.
        :type target_nodes: (list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["node-type-check"]: this flag controls if the \
                type of the nodes enclosed in the region should be tested \
                to avoid using unsupported nodes inside a region.

        '''

        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        node_list = self.get_node_list(target_nodes)
        self.validate(node_list, options)

        # Keep a reference to the parent of the nodes that are to be
        # enclosed within a parallel region. Also keep the index of
        # the first child to be enclosed as that will become the
        # position of the new !$omp parallel directive.
        node_parent = node_list[0].parent
        node_position = node_list[0].position

        # Create the parallel directive as a child of the
        # parent of the nodes being enclosed and with those nodes
        # as its children.
        # pylint: disable=not-callable
        directive = self._directive_factory(
            children=[node.detach() for node in node_list])

        # Add the region directive as a child of the parent
        # of the nodes being enclosed and at the original location
        # of the first of these nodes
        node_parent.addchild(directive, index=node_position)
