# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

'''This module contains the base class LoopTrans. All transformations which
   act on a loop sub-class this one.
'''

import abc
import six

from psyclone.psyGen import Kern, Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.nodes import Schedule, Loop
from psyclone.nemo import NemoInvokeSchedule


@six.add_metaclass(abc.ABCMeta)
class LoopTrans(Transformation):
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

    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target for a loop
        transformation.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["node-type-check"]: this flag controls if the \
            type of the nodes enclosed in the loop should be tested to \
            avoid including unsupported nodes in a transformation.

        :raises TransformationError: if the supplied node is not a (fully- \
                formed) Loop.
        :raises TransformationError: if any of the nodes within the loop are \
                of an unsupported type.
        :raises TransformationError: if the loop is of 'null' type.
        :raises TransformationError: if the supplied options are not a \
                dictionary.

        '''
        # pylint: disable=too-many-branches
        if not isinstance(node, Loop):
            raise TransformationError(
                "Target of {0} transformation must be a sub-class of Loop "
                "but got '{1}'".format(self.name, type(node).__name__))

        # The loop must be fully-formed.
        if len(node.children) != 4:
            raise TransformationError(
                "Error in {0} transformation. The target loop "
                "must have four children but found: {1}.".format(
                    self.name,
                    [type(child).__name__ for child in node.children]))

        if not options:
            options = {}
        if not isinstance(options, dict):
            raise TransformationError(
                "Transformation validate method 'options' argument must be a "
                "dictionary but found '{0}'.".format(type(options).__name__))

        # Check that the proposed region contains only supported node types
        if options.get("node-type-check", True):
            # Stop at any instance of Kern to avoid going into the
            # actual kernels, e.g. in Nemo inlined kernels
            flat_list = [item for item in node.walk(object, stop_type=Kern)
                         if not isinstance(item, Schedule)]
            for item in flat_list:
                if isinstance(item, self.excluded_node_types):
                    raise TransformationError(
                        "Nodes of type '{0}' cannot be enclosed by a {1} "
                        "transformation".format(type(item).__name__,
                                                self.name))

        # A 'null' loop is one which exists in the PSyIR hierarchy (mainly for
        # halo-exchange logic) but does *not* correspond to an actual loop
        # in the code that is generated for the PSy layer.
        if node.loop_type == 'null':
            raise TransformationError(
                "Cannot apply a {0} transformation to a 'null' loop.".format(
                    self.name))

        # The checks below this point only apply to the NEMO API and can be
        # removed once #435 is done.
        if not isinstance(node.root, NemoInvokeSchedule):
            return

        if "was_where" in node.annotations:
            # This limitation is because the NEMO API currently relies on
            # manipulation of the fparser2 parse tree
            # TODO #435.
            raise TransformationError(
                "In the NEMO API a transformation cannot be applied to a "
                "PSyIR loop representing a WHERE construct.")

    @property
    def name(self):
        '''
        :returns: the name of this class.
        :rtype: str
        '''
        return self.__class__.__name__


# For Sphinx AutoAPI documentation generation
__all__ = ["LoopTrans"]
