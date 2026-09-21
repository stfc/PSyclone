# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the LFRic-specific implementation of the ExtractTrans
transformation.
'''

from psyclone.domain.lfric import LFRicDriverCreator, LFRicLoop
from psyclone.psyir.nodes import ExtractNode
from psyclone.psyir.transformations import ExtractTrans, TransformationError
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class LFRicExtractTrans(ExtractTrans):
    ''' LFRic API application of ExtractTrans transformation
    to extract code into a stand-alone program.

    '''

    def __init__(self):
        super().__init__(ExtractNode)

    def validate(self, node_list, options=None, **kwargs):
        ''' Perform LFRic API specific validation checks before applying
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
        super().validate(node_list, options, **kwargs)

        # Check LFRicExtractTrans specific constraints
        for node in node_list:

            # Check that ExtractNode is not inserted between a Loop
            # over colours and a Loop over cells in a colour when
            # colouring is applied.
            ancestor = node.ancestor(LFRicLoop)
            if ancestor and ancestor.loop_type == 'colours':
                raise TransformationError(
                    f"Error in {self.name} for LFRic API: Extraction of a "
                    f"Loop over cells in a colour without its ancestor Loop "
                    f"over colours is not allowed.")

    # ------------------------------------------------------------------------
    def apply(self, nodes, options=None, create_driver: bool = False,
              **kwargs):
        # pylint: disable=arguments-differ
        '''Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Nodes in the schedule within
        a single PSyData region. It first uses the CallTreeUtils to determine
        input- and output-parameters. If requested, it will then call
        the LFRicDriverCreator to write the stand-alone driver program.
        Then it will call apply of the base class.

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
        if options:
            # We will add a default prefix, so create a copy to avoid
            # changing the user's options:
            my_options = options.copy()
            create_driver = my_options.get("create_driver", False)
        else:
            my_options = {}

        nodes = self.get_node_list(nodes)
        super().apply(nodes, options=options, create_driver=create_driver,
                      **kwargs)
        new_node = nodes[0].ancestor(ExtractNode)
        if create_driver:
            region_name = (my_options.get("region_name", None) if options
                           else self.get_option("region_name", **kwargs))
            new_node._driver_creator = LFRicDriverCreator(region_name)
