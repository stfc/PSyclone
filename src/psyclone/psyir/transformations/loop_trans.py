# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Authors: A. R. Porter, N. Nobre and S. Siso, STFC Daresbury Lab

'''This module contains the base class LoopTrans. All transformations which
   act on a loop sub-class this one.
'''

import abc

from psyclone.errors import LazyString
from psyclone.psyGen import Kern, Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.nodes import Schedule, Loop, Assignment


class LoopTrans(Transformation, metaclass=abc.ABCMeta):
    # Avoid pylint warning about abstract method (apply) not overwritten:
    # pylint: disable=abstract-method
    '''
    This abstract class is a base class for all transformations that act
    on a Loop node. It gives access to a validate function that
    makes sure that the supplied node is a Loop.
    We also check that all nodes to be enclosed are valid for this
    transformation - this requires that the sub-class populate the
    `excluded_node_types` tuple.

    '''
    # The types of Node that are excluded from within the target loop. Must be
    # populated by sub-class.
    excluded_node_types = ()

    def apply(self, node, options=None, node_type_check: bool = True,
              verbose: bool = False, **kwargs):
        '''
        Applies the transformation to the provided node.

        This function only calls the superclass method, but is required for
        option specification.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param node_type_check: If the type of nodes enclosed in the loop
                                should be tested to avoid including
                                unsupported nodes in the transformation.
        :param verbose: whether to log the reason the validation failed, at
                        the moment with a comment in the provided PSyIR node.
        '''
        super().apply(node, options=options, node_type_check=node_type_check,
                      verbose=verbose, **kwargs)

    def validate(self, node, options=None, **kwargs):
        '''Checks that the supplied node is a valid target for a loop
        transformation.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Node`

        :raises TransformationError: if the supplied node is not a (fully- \
                formed) Loop.
        :raises TransformationError: if any of the nodes within the loop are \
                of an unsupported type.
        :raises TransformationError: if the loop is of 'null' type.
        :raises TransformationError: if the supplied options are not a \
                dictionary.

        '''
        super().validate(node, options=options, **kwargs)
        # pylint: disable=too-many-branches
        if not isinstance(node, Loop):
            raise TransformationError(
                f"Target of {self.name} transformation must be a sub-class of "
                f"Loop but got '{type(node).__name__}'")

        # The loop must be fully-formed.
        if len(node.children) != 4:
            raise TransformationError(
                f"Error in {self.name} transformation. The target loop "
                f"must have four children but found: "
                f"{[type(child).__name__ for child in node.children]}.")

        # TODO 2668: options are now deprecated.
        if not options:
            self.validate_options(**kwargs)
            verbose = self.get_option("verbose", **kwargs)
            node_type_check = self.get_option("node_type_check", **kwargs)
        else:
            if not isinstance(options, dict):
                raise TransformationError(
                    f"Transformation validate method 'options' argument must "
                    f"be a dictionary but found '{type(options).__name__}'.")
            verbose = options.get("verbose", False)
            node_type_check = options.get("node-type-check", True)

        # Check that the proposed region contains only supported node types
        if node_type_check:
            # Stop at any instance of Kern to avoid going into the
            # actual kernels, e.g. in Nemo inlined kernels
            # pylint: disable=cell-var-from-loop
            flat_list = [item for item in node.walk(object, stop_type=Kern)
                         if not isinstance(item, Schedule)]
            for item in flat_list:
                if isinstance(item, self.excluded_node_types):
                    message = (
                        f"Nodes of type '{type(item).__name__}' cannot be "
                        f"enclosed by a {self.name} transformation (use "
                        f"the 'node-type-check: False' option to accept them "
                        f"at your own risk)")
                    if verbose:
                        node.append_preceding_comment(message)
                    raise TransformationError(LazyString(
                        lambda: f"{message} in:\n{node.debug_string()}"))

            for assignment in node.walk(Assignment):
                if assignment.is_pointer:
                    message = (
                        f"'{type(self).__name__}' can not be applied to nodes"
                        f" that contain pointer assignments by default (use "
                        f"the 'node-type-check: False' option to accept them "
                        f"at your own risk)")
                    if verbose:
                        node.append_preceding_comment(message)
                    raise TransformationError(LazyString(
                        lambda: f"{message} in:\n{node.debug_string()}"))

        # Disable warning to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.common.psylayer import PSyLoop

        # A 'null' loop is one which exists in the PSyIR hierarchy (mainly for
        # halo-exchange logic) but does *not* correspond to an actual loop
        # in the code that is generated for the PSy layer.

        # TODO 1756: PSyLoop is a PSy-layer concept and 'null' is only defined
        # in LFRic. Maybe a generic transformation validation is not the best
        # place for this check.
        if isinstance(node, PSyLoop) and node.loop_type == 'null':
            raise TransformationError(
                f"Cannot apply a {self.name} transformation to a 'null' loop.")

    @property
    def name(self):
        '''
        :returns: the name of this class.
        :rtype: str
        '''
        return self.__class__.__name__


# For Sphinx AutoAPI documentation generation
__all__ = ["LoopTrans"]
