# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
'''This module provides the MoveTrans transformation.'''

import warnings

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Node
from psyclone.psyir.transformations.transformation_error import (
    TransformationError
)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class MoveTrans(Transformation):
    '''Provides a transformation to move a node in the tree. For
    example:

    .. code-block :: python

        from psyclone.transformations import MoveTrans
        trans=MoveTrans()
        trans.apply(schedule.children[0], schedule.children[2],
                    options = {"position":"after")

    Nodes may only be moved to a new location with the same parent
    and must not break any dependencies otherwise an exception is
    raised.'''

    def __str__(self):
        return "Move a node to a different location"

    def validate(self, node: Node, location: Node, options=None, **kwargs):
        # pylint: disable=arguments-differ
        ''' validity checks for input arguments.

        :param node: the node to be moved.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param str options["position"]: either 'before' or 'after'.

        :raises TransformationError: if the given node is not an instance
            of :py:class:`psyclone.psyir.nodes.Node`
        :raises TransformationError: if the location is not valid.
        '''
        if not options:
            self.validate_options(**kwargs)
            position = self.get_option("position", **kwargs)
        else:
            # TODO #2668: Deprecate options dictionary.
            position = options.get("position", "before")

        # Check that the first argument is a Node
        if not isinstance(node, Node):
            raise TransformationError(
                f"The node argument to {self.name} should be a Node but got "
                f"'{type(node).__name__}'."
            )

        # Check new location conforms to any data dependencies
        # This also checks the location and position arguments
        if not node.is_valid_location(location, position=position):
            raise TransformationError(
                f"In {self.name}, data dependencies "
                f"forbid the move to the new location")

    def apply(self, node: Node, location: Node, position: str = "before",
              options=None, **kwargs):
        '''Move the node represented by :py:obj:`node` before location
        :py:obj:`location` (which is also a node) by default and after
        if the optional `position` argument is set to 'after'.

        :param node: the node to be moved.
        :param location: node before or after which the given node
            should be moved.
        :param position: whether to place the moved node before or after
            the location. This must be 'before' or 'after'.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param str options["position"]: either 'before' or 'after'.

        :raises TransformationError: if the given node is not an instance
            of :py:class:`psyclone.psyir.nodes.Node`
        :raises TransformationError: if the location is not valid.

        '''
        # pylint:disable=arguments-differ

        self.validate(node, location, options=options, position=position)

        if options:
            # TODO #2668: Deprecate options dictionary.
            position = options.get("position", "before")
            warnings.warn(self._deprecation_warning, DeprecationWarning, 2)

        parent = node.parent

        my_node = parent.children.pop(node.position)

        location_index = location.position
        if position == "before":
            location.parent.children.insert(location_index, my_node)
        else:
            location.parent.children.insert(location_index+1, my_node)


__all__ = ["MoveTrans"]
