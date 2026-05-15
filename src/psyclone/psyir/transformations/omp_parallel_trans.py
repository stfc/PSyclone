# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Modified I. Kavcic, J. G. Wallwork, O. Brunt and L. Turner, Met Office
#          S. Valat, Inria / Laboratoire Jean Kuntzmann
#          M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann
#          J. Dendy, Met Office
'''This module provides the OMPParallelTrans transformation.'''
import logging
from collections.abc import Iterable
from psyclone import psyGen
from psyclone.psyir.nodes import (
    ACCDirective,
    CodeBlock,
    Node,
    OMPParallelDirective,
    OMPDirective,
    Return,
    RegionDirective
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

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory
    >>> from psyclone.errors import GenerationError
    >>> api = "gocean"
    >>> ast, invokeInfo = parse(GOCEAN_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> ltrans = t.get_trans_name('GOceanOMPLoopTrans')
    >>> rtrans = t.get_trans_name('OMPParallelTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Apply the OpenMP Loop transformation to *every* loop
    >>> # in the schedule
    >>> for child in schedule.children:
    >>>     ltrans.apply(child)
    >>>
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> rtrans.apply(schedule.children)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

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
        '''
        # TODO #2668: Remove options.
        super().apply(nodes, options, **kwargs)

        # Privatise the provided variables if they are found within the symbol
        # table of the ancestor RegionDirective to the nodes which have just
        # had a region spanned over.
        if force_private:
            new_region_directive = None
            try:
                new_region_directive = nodes[0].ancestor(RegionDirective)
            except TypeError as err:
                logging.warning(
                    "Could not find an OMPParallelTrans ancestor as %", err)
            if new_region_directive:
                new_region_directive.explicitly_private_symbols.update(
                    super()._check_symbol_table_vars(
                        new_region_directive,
                        force_private))


__all__ = ["OMPParallelTrans"]
