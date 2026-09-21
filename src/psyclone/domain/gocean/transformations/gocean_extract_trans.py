# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the GOcean-specific extract transformation.
'''

from typing import Any, Optional, Union

from psyclone.gocean1p0 import GOLoop
from psyclone.psyir.nodes import ExtractNode, Node
from psyclone.psyir.symbols import ScalarType
from psyclone.domain.gocean import GOceanDriverCreator
from psyclone.psyir.transformations import ExtractTrans, TransformationError
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class GOceanExtractTrans(ExtractTrans):
    ''' GOcean API application of ExtractTrans transformation
    to extract code into a stand-alone program. For example:

    >>> from psyclone.tests.utilities import get_psylayer_schedule
    >>> filename = "eg1/shallow_alg.f90"
    >>> schedule = get_psylayer_schedule(filename, "gocean-examples")
    >>>
    >>> from psyclone.domain.gocean.transformations import GOceanExtractTrans
    >>> etrans = GOceanExtractTrans()
    >>>
    >>> # Apply GOceanExtractTrans transformation to selected Nodes
    >>> etrans.apply(schedule.children[0])

    '''

    # ------------------------------------------------------------------------
    def validate(self, node_list: list[Node],
                 options: Optional[dict[str, Any]] = None,
                 **kwargs: Any) -> None:
        ''' Perform GOcean API specific validation checks before applying
        the transformation.

        :param node_list: the list of Node(s) we are checking.
        :param options: a dictionary with options for transformations.
        :param options["create_driver"]: whether or not to create a \
            driver program at code-generation time. If set, the driver will \
            be created in the current working directory with the name \
            "driver-MODULE-REGION.f90" where MODULE and REGION will be the \
            corresponding values for this region. This flag is forwarded to \
            the ExtractNode. Its default value is False.
        :param options["region_name"]: an optional name to \
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
        super().validate(node_list, options, **kwargs)

        # Check GOceanExtractTrans specific constraints
        for node in node_list:

            # Check that ExtractNode is not inserted between an inner
            # and an outer Loop.
            ancestor = node.ancestor(GOLoop)
            if ancestor and ancestor.loop_type == 'outer':
                raise TransformationError(
                    f"Error in {self.name}: Application to an "
                    f"inner Loop without its ancestor outer Loop is not "
                    f"allowed.")

    # ------------------------------------------------------------------------
    def apply(self, nodes: Union[Node, list[Node]],
              options: Optional[dict[str, Any]] = None,
              create_driver: bool = False, **kwargs: Any) -> None:
        # pylint: disable=arguments-differ
        '''Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Nodes in the schedule within
        a single PSyData region. Note that this implementation just calls
        the base class, it is only added here to provide the documentation
        for this function, since it accepts different options
        to the base class (e.g. create_driver, which is passed to the
        ExtractNode instance that will be inserted.).

        :param nodes: can be a single node or a list of nodes.
        :param options: a dictionary with options for transformations.
        :param options["prefix"]: a prefix to use for the PSyData module \
            name (``prefix_psy_data_mod``) and the PSyDataType \
            (``prefix_PSyDataType``) - a "_" will be added automatically. \
            It defaults to "extract", resulting in e.g. \
            ``extract_psy_data_mod``.
        :param create_driver: whether or not to create a \
            driver program at code-generation time. If set, the driver will \
            be created in the current working directory with the name \
            "driver-MODULE-REGION.f90" where MODULE and REGION will be the \
            corresponding values for this region. Defaults to False.
        :param options["region_name"]: an optional name to \
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
            new_node._driver_creator = GOceanDriverCreator(
                ScalarType.integer_type(),
                ScalarType.real8_type(),
                region_name)
