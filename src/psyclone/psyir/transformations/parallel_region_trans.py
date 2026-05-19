# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
import logging
from collections.abc import Iterable
from abc import ABC, abstractmethod
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone import psyGen
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.nodes import CodeBlock, Node, Return, RegionDirective
from psyclone.psyir.symbols import DataSymbol
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
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

    def _check_symbol_table_vars(
            self,
            region_node: RegionDirective,
            force_private: Iterable[str] = ()) -> set[DataSymbol]:
        '''
        Check that the symbol table of the provided region node contains the
        variable variables in the provided list. Return a set of DataSymbols.

        This is intended to be used as part of privatising the variables
        contained in the list for the provided region in the child classes.
        '''
        explicitly_private_symbols = set()

        for symbol_name in force_private:
            sym = None
            try:
                sym = region_node.scope.symbol_table.lookup(
                    symbol_name.lower())
            except KeyError as err:
                # This is not an error, but we will log the missed string
                logging.warning(
                    "%s has been provided with the '%s' symbol name in "
                    "the 'force_private' option, but there is no such "
                    "symbol in this scope.", err, symbol_name)
            if sym:
                explicitly_private_symbols.add(sym)

        return explicitly_private_symbols

    def validate(self, nodes: list[Node], options=None, **kwargs):
        # pylint: disable=arguments-renamed
        '''
        Check that the supplied list of Nodes are eligible to be
        put inside a parallel region.

        :param list nodes: list of nodes to put into a parallel region
        :param options: a dictionary with options for transformations.

        :raises TransformationError: if the supplied nodes are not all
            children of the same parent (siblings).

        '''
        node_list = self.get_node_list(nodes)

        node_parent = node_list[0].parent

        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    f"Error in {self.name} transformation: supplied nodes are "
                    f"not children of the same parent.")
        # TODO #2668: Remove options.
        super().validate(node_list, options, **kwargs)

    def apply(self, nodes: list[Node], options=None, **kwargs):
        # pylint: disable=arguments-renamed
        '''
        Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Loops in the
        schedule within a single parallel region.

        :param nodes: a single Node or a list of Nodes.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''

        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        node_list = self.get_node_list(nodes)
        # TODO #2668: Remove options.
        self.validate(node_list, options, **kwargs)

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
