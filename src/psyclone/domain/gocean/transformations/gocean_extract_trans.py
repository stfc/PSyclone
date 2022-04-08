# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
# Modified by R. W. Ford and S. Siso, STFC Daresbury Laboratory

'''This module contains the GOcean-specific extract transformation.
'''

from psyclone.gocean1p0 import GOLoop
from psyclone.psyir.nodes import ExtractNode
from psyclone.psyir.symbols import REAL8_TYPE, INTEGER_TYPE
from psyclone.psyir.tools import DependencyTools
from psyclone.domain.common import ExtractDriverCreator
from psyclone.psyir.transformations import ExtractTrans, TransformationError


class GOceanExtractTrans(ExtractTrans):
    ''' GOcean1.0 API application of ExtractTrans transformation \
    to extract code into a stand-alone program. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>>
    >>> API = "gocean1.0"
    >>> FILENAME = "shallow_alg.f90"
    >>> ast, invokeInfo = parse(FILENAME, api=API)
    >>> psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>>
    >>> from psyclone.domain.gocean.transformations import GOceanExtractTrans
    >>> etrans = GOceanExtractTrans()
    >>>
    >>> # Apply GOceanExtractTrans transformation to selected Nodes
    >>> etrans.apply(schedule.children[0])
    >>> print(schedule.view())
    '''

    def __init__(self):
        super(GOceanExtractTrans, self).__init__(ExtractNode)
        # Set the integer and real types to use. If required, the constructor
        # could take a parameter to change these.

        self._driver_creator = ExtractDriverCreator(INTEGER_TYPE, REAL8_TYPE)

    # ------------------------------------------------------------------------
    def validate(self, node_list, options=None):
        ''' Perform GOcean1.0 API specific validation checks before applying
        the transformation.

        :param node_list: the list of Node(s) we are checking.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["create_driver"]: whether or not to create a \
            driver program at code-generation time. If set, the driver will \
            be created in the current working directory with the name \
            "driver-MODULE-REGION.f90" where MODULE and REGION will be the \
            corresponding values for this region. This flag is forwarded to \
            the ExtractNode. Its default value is False.
        :param (str,str) options["region_name"]: an optional name to \
            use for this data-extraction region, provided as a 2-tuple \
            containing a module name followed by a local name. The pair of \
            strings should uniquely identify a region unless aggregate \
            information is required (and is supported by the runtime \
            library). This option is forwarded to the PSyDataNode (where it \
            changes the region names) and to the ExtractNode (where it \
            changes the name of the created output files and the name of the \
            driver program).

        :raises TransformationError: if transformation is applied to an \
            inner Loop without its parent outer Loop.
        '''

        # First check constraints on Nodes in the node_list inherited from
        # the parent classes (ExtractTrans and RegionTrans)
        super(GOceanExtractTrans, self).validate(node_list, options)

        # Check GOceanExtractTrans specific constraints
        for node in node_list:

            # Check that ExtractNode is not inserted between an inner
            # and an outer Loop.
            ancestor = node.ancestor(GOLoop)
            if ancestor and ancestor.loop_type == 'outer':
                raise TransformationError(
                    "Error in {0}: Application to an "
                    "inner Loop without its ancestor outer Loop is not "
                    "allowed.".format(str(self.name)))

    # ------------------------------------------------------------------------
    def apply(self, nodes, options=None):
        # pylint: disable=arguments-differ
        '''Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Nodes in the schedule within
        a single PSyData region. Note that this implementation just calls
        the base class, it is only added here to provide the documentation
        for this function, since it accepts different options
        to the base class (e.g. create_driver, which is passed to the
        ExtractNode instance that will be inserted.).

        :param nodes: can be a single node or a list of nodes.
        :type nodes: :py:obj:`psyclone.psyir.nodes.Node` or list of \
                     :py:obj:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
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
        :param (str,str) options["region_name"]: an optional name to \
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
        input_list, output_list = dep.get_in_out_parameters(nodes)
        # Determine a unique postfix to be used for output variables
        # that avoid any name clashes
        postfix = ExtractTrans.determine_postfix(input_list,
                                                 output_list,
                                                 postfix="_post")
        my_options["post_var_postfix"] = postfix

        if my_options.get("create_driver", False):
            # We need to create the driver before inserting the ExtractNode
            # (since some of the visitors used in driver creation do not
            # handle an ExtractNode in the tree)
            self._driver_creator.write_driver(nodes,
                                              input_list, output_list,
                                              postfix=postfix,
                                              prefix=my_options["prefix"],
                                              region_name=region_name)

        super(GOceanExtractTrans, self).apply(nodes, my_options)
