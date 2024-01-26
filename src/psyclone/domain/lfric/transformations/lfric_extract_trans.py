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
# Authors I. Kavcic, Met Office
# Modified by J. Henrichs, Bureau of Meteorology
# Modified by R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified by L. Turner, Met Office

'''This module contains the LFRic-specific implementation of the ExtractTrans
transformation.
'''

from psyclone.domain.lfric import LFRicExtractDriverCreator, LFRicLoop
from psyclone.psyir.nodes import ExtractNode
from psyclone.psyir.tools import DependencyTools
from psyclone.psyir.transformations import ExtractTrans, TransformationError


class LFRicExtractTrans(ExtractTrans):
    ''' Dynamo0.3 API application of ExtractTrans transformation \
    to extract code into a stand-alone program. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>>
    >>> API = "dynamo0.3"
    >>> FILENAME = "solver_alg.x90"
    >>> ast, invokeInfo = parse(FILENAME, api=API)
    >>> psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>>
    >>> from psyclone.domain.lfric.transformations import LFRicExtractTrans
    >>> etrans = LFRicExtractTrans()
    >>>
    >>> # Apply LFRicExtractTrans transformation to selected Nodes
    >>> etrans.apply(schedule.children[0:3])
    >>> print(schedule.view())

    '''

    def __init__(self):
        super().__init__(ExtractNode)
        self._driver_creator = LFRicExtractDriverCreator()

    def validate(self, node_list, options=None):
        ''' Perform Dynamo0.3 API specific validation checks before applying
        the transformation.

        :param node_list: the list of Node(s) we are checking.
        :type node_list: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if transformation is applied to a Loop \
                                     over cells in a colour without its \
                                     parent Loop over colours.
        '''

        # First check constraints on Nodes in the node_list inherited from
        # the parent classes (ExtractTrans and RegionTrans)
        super().validate(node_list, options)

        # Check LFRicExtractTrans specific constraints
        for node in node_list:

            # Check that ExtractNode is not inserted between a Loop
            # over colours and a Loop over cells in a colour when
            # colouring is applied.
            ancestor = node.ancestor(LFRicLoop)
            if ancestor and ancestor.loop_type == 'colours':
                raise TransformationError(
                    f"Error in {self.name} for Dynamo0.3 API: Extraction of a "
                    f"Loop over cells in a colour without its ancestor Loop "
                    f"over colours is not allowed.")

    # ------------------------------------------------------------------------
    def apply(self, nodes, options=None):
        # pylint: disable=arguments-differ
        '''Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Nodes in the schedule within
        a single PSyData region. It first uses the DependencyTool to determine
        input- and output-parameters. If requested, it will then call
        the LFRicExtractDriverCreator to write the stand-alone driver
        program. Then it will call apply of the base class.

        :param nodes: can be a single node or a list of nodes.
        :type nodes: :py:class:`psyclone.psyir.nodes.Node` or \
                     List[:py:class:`psyclone.psyir.nodes.Node`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param str options["prefix"]: a prefix to use for the PSyData module \
            name (``prefix_psy_data_mod``) and the PSyDataType \
            (``prefix_PSyDataType``) - a "_" will be added automatically. \
            It defaults to "extract", resulting in e.g. \
            ``extract_psy_data_mod``.
        :param bool options["create_driver"]: whether or not to create a \
            driver program at code-generation time. If set, the driver will \
            be created in the current working directory with the name \
            "driver-MODULE-REGION.f90" where MODULE and REGION will be the \
            corresponding values for this region. Defaults to False.
        :param Tuple[str,str] options["region_name"]: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region unless aggregate information \
            is required (and is supported by the runtime library).

        '''
        if options is None:
            my_options = {}
        else:
            # We will add a default prefix, so create a copy to avoid
            # changing the user's options:
            my_options = options.copy()

        dep = DependencyTools()
        nodes = self.get_node_list(nodes)
        region_name = self.get_unique_region_name(nodes, my_options)
        my_options["region_name"] = region_name
        my_options["prefix"] = my_options.get("prefix", "extract")
        # Get the input- and output-parameters of the node list
        read_write_info = dep.get_in_out_parameters(nodes)
        # Determine a unique postfix to be used for output variables
        # that avoid any name clashes
        postfix = ExtractTrans.determine_postfix(read_write_info,
                                                 postfix="_post")
        my_options["post_var_postfix"] = postfix

        if my_options.get("create_driver", False):
            # We need to create the driver before inserting the ExtractNode
            # (since some of the visitors used in driver creation do not
            # handle an ExtractNode in the tree)
            self._driver_creator.write_driver(nodes, read_write_info,
                                              postfix=postfix,
                                              prefix=my_options["prefix"],
                                              region_name=region_name)
        # The PSyData transformation needs to pass this object to
        # the corresponding PSyData node, so add it to the option arguments.
        my_options["read_write_info"] = read_write_info
        super().apply(nodes, my_options)
