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
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, J. G. Wallwork, O. Brunt and L. Turner, Met Office
#          S. Valat, Inria / Laboratoire Jean Kuntzmann
#          M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann
#          J. Dendy, Met Office
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

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> ast,invokeInfo=parse("lfric.F90")
    >>> psy=PSyFactory("lfric").create(invokeInfo)
    >>> schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> from psyclone.transformations import MoveTrans
    >>> trans=MoveTrans()
    >>> trans.apply(schedule.children[0], schedule.children[2],
    ...             options = {"position":"after")
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

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
