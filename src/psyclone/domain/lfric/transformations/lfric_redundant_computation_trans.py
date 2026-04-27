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

''' This module provides the LFRic redundant-computation transformation. '''

from typing import Any, Optional, Union

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicInvokeSchedule, LFRicConstants
from psyclone.psyir.nodes import DataNode, Directive
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.transformations import check_intergrid
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class LFRicRedundantComputationTrans(LoopTrans):
    '''This transformation allows the user to modify a loop's bounds so
    that redundant computation will be performed. Redundant
    computation can result in halo exchanges being modified, new halo
    exchanges being added or existing halo exchanges being removed.

    * This transformation should be performed before any
      parallelisation transformations (e.g. for OpenMP) to the loop in
      question and will raise an exception if this is not the case.

    * This transformation can not be applied to a loop containing a
      reduction and will again raise an exception if this is the case.

    * This transformation can only be used to add redundant
      computation to a loop, not to remove it.

    * This transformation allows a loop that is already performing
      redundant computation to be modified, but only if the depth is
      increased.

    '''
    def __str__(self):
        return "Change iteration space to perform redundant computation"

    def validate(self, node: Loop, options=None, **kwargs):
        '''Perform various checks to ensure that it is valid to apply the
        RedundantComputation transformation to the supplied node

        :param node: the supplied node on which we are performing
                     validity checks.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param int options["depth"]: the depth of the stencil if the value
                                     is provided and None if not.

        :raises TransformationError: if the parent of the loop is a
            :py:class:`psyclone.psyir.nodes.Directive`.
        :raises TransformationError: if the parent of the loop is not a
            :py:class:`psyclone.psyir.nodes.Loop` or a
            :py:class:`psyclone.psyGen.LFRicInvokeSchedule`.
        :raises TransformationError: if the parent of the loop is
            :py:class:`psyclone.psyir.nodes.Loop` but the original loop does
            not iterate over 'cells_in_colour'.
        :raises TransformationError: if the parent of the loop is a
            :py:class:`psyclone.psyir.nodes.Loop` but the parent does not
            iterate over 'colours'.
        :raises TransformationError: if the parent of the loop is a
            :py:class:`psyclone.psyir.nodes.Loop` but the parent's parent is
            not a :py:class:`psyclone.psyGen.LFRicInvokeSchedule`.
        :raises TransformationError: if this transformation is applied
            when distributed memory is not switched on.
        :raises TransformationError: if the loop does not iterate over
            cells, dofs or colour.
        :raises TransformationError: if the loop contains a kernel that
            operates on halo cells or only on owned cells/dofs.
        :raises TransformationError: if the transformation is setting the
            loop to the maximum halo depth but the loop already computes
            to the maximum halo depth.
        :raises TransformationError: if the transformation is setting the
            loop to the maximum halo depth but the loop contains a stencil
            access (as this would result in the field being accessed
            beyond the halo depth).
        :raises TransformationError: if the supplied depth value is not an
            integer.
        :raises TransformationError: if the supplied depth value is less
            than 1.
        :raises TransformationError: if the supplied depth value is not
            greater than 1 when a continuous loop is modified as this is
            the minimum valid value.
        :raises TransformationError: if the supplied depth value is not
            greater than the existing depth value, as we should not need
            to undo existing transformations.
        :raises TransformationError: if a depth value has been supplied
            but the loop has already been set to the maximum halo depth.

        '''
        if not options:
            self.validate_options(**kwargs)
            depth = self.get_option("depth", **kwargs)
        else:
            # TODO #2668: Deprecate options dictionary.
            depth = options.get("depth")
        # pylint: disable=too-many-branches
        # check node is a loop
        super().validate(node, options=options)

        # Check loop's parent is the InvokeSchedule, or that it is nested
        # in a colours loop and perform other colour(s) loop checks,
        # otherwise halo exchange placement might fail. The only
        # current example where the placement would fail is when
        # directives have already been added. This could be fixed but
        # it actually makes sense to require redundant computation
        # transformations to be applied before adding directives so it
        # is not particularly important.
        dir_node = node.ancestor(Directive)
        if dir_node:
            raise TransformationError(
                f"In the LFRicRedundantComputation transformation apply "
                f"method the supplied loop is sits beneath a directive of "
                f"type {type(dir_node)}. Redundant computation must be applied"
                f" before directives are added.")
        if not (isinstance(node.parent, LFRicInvokeSchedule) or
                isinstance(node.parent.parent, Loop)):
            raise TransformationError(
                f"In the LFRicRedundantComputation transformation "
                f"apply method the parent of the supplied loop must be the "
                f"LFRicInvokeSchedule, or a Loop, but found "
                f"{type(node.parent)}")
        if isinstance(node.parent.parent, Loop):
            if node.loop_type != "cells_in_colour":
                raise TransformationError(
                    f"In the LFRicRedundantComputation transformation "
                    f"apply method, if the parent of the supplied Loop is "
                    f"also a Loop then the supplied Loop must iterate over "
                    f"'cells_in_colour', but found '{node.loop_type}'")
            if node.parent.parent.loop_type != "colours":
                raise TransformationError(
                    f"In the LFRicRedundantComputation transformation "
                    f"apply method, if the parent of the supplied Loop is "
                    f"also a Loop then the parent must iterate over "
                    f"'colours', but found '{node.parent.parent.loop_type}'")
            if not isinstance(node.parent.parent.parent, LFRicInvokeSchedule):
                raise TransformationError(
                    f"In the LFRicRedundantComputation transformation "
                    f"apply method, if the parent of the supplied Loop is "
                    f"also a Loop then the parent's parent must be the "
                    f"LFRicInvokeSchedule, but found {type(node.parent)}")
        if not Config.get().distributed_memory:
            raise TransformationError(
                "In the LFRicRedundantComputation transformation apply "
                "method distributed memory must be switched on")

        # loop must iterate over cell-column, dof or cell-in-colour. Note, an
        # empty loop_type iterates over cell-columns.
        if node.loop_type not in ["", "dof", "cells_in_colour"]:
            raise TransformationError(
                f"In the LFRicRedundantComputation transformation apply "
                f"method the loop type must be one of '' (cell-columns), 'dof'"
                f" or 'cells_in_colour', but found '{node.loop_type}'")

        const = LFRicConstants()

        for kern in node.kernels():
            if "halo" in kern.iterates_over:
                raise TransformationError(
                    f"Cannot apply the {self.name} transformation to kernels "
                    f"that operate on halo cells but kernel '{kern.name}' "
                    f"operates on '{kern.iterates_over}'.")
            if kern.iterates_over in const.NO_RC_ITERATION_SPACES:
                raise TransformationError(
                    f"Cannot apply the {self.name} transformation to kernel "
                    f"'{kern.name}' because it does not support redundant "
                    f"computation (it operates on '{kern.iterates_over}').")

        # We don't currently support the application of transformations to
        # loops containing inter-grid kernels
        check_intergrid(node)

        if not options:
            options = {}
        depth = options.get("depth")
        if depth is None:
            if node.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
                if not node.upper_bound_halo_depth:
                    raise TransformationError(
                        "In the LFRicRedundantComputation transformation "
                        "apply method the loop is already set to the maximum "
                        "halo depth so this transformation does nothing")
                for call in node.kernels():
                    for arg in call.arguments.args:
                        if arg.stencil:
                            raise TransformationError(
                                f"In the LFRicRedundantComputation "
                                f"transformation apply method the loop "
                                f"contains field '{arg.name}' with a stencil "
                                f"access in kernel '{call.name}', so it is "
                                f"invalid to set redundant computation to "
                                f"maximum depth")
        else:
            if not isinstance(depth, int):
                raise TransformationError(
                    f"In the LFRicRedundantComputation transformation "
                    f"apply method the supplied depth should be an integer but"
                    f" found type '{type(depth)}'")
            if depth < 1:
                raise TransformationError(
                    "In the LFRicRedundantComputation transformation "
                    "apply method the supplied depth is less than 1")

            if node.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
                if node.upper_bound_halo_depth:
                    if isinstance(node.upper_bound_halo_depth, Literal):
                        upper_bound = int(node.upper_bound_halo_depth.value)
                        if upper_bound >= depth:
                            raise TransformationError(
                                f"In the LFRicRedundantComputation "
                                f"transformation apply method the supplied "
                                f"depth ({depth}) must be greater than the "
                                f"existing halo depth ({upper_bound})")
                else:
                    raise TransformationError(
                        "In the LFRicRedundantComputation transformation "
                        "apply method the loop is already set to the maximum "
                        "halo depth so can't be set to a fixed value")

    def apply(self,
              node: Loop,
              options: Optional[dict[str, Any]] = None,
              depth: Optional[Union[int, DataNode]] = None,
              **kwargs):
        # pylint:disable=arguments-renamed
        '''Apply the redundant computation transformation to the loop
        :py:obj:`node`. This transformation can be applied to loops iterating
        over 'cells or 'dofs'. if :py:obj:`depth` is set to a value then the
        value will be the depth of the field's halo over which redundant
        computation will be performed. If :py:obj:`depth` is not set to a
        value then redundant computation will be performed to the full depth
        of the field's halo.

        :param node: the loop to transform.
        :param options: a dictionary with options for transformations.
        :param depth: the depth to which to perform redundant computation.
            Default is None in which case the full halo depth is used.

        '''
        # TODO #2668: remove options dictionary.
        self.validate(node, options=options, **kwargs)

        if options:
            # TODO #2668: Deprecate options dictionary.
            depth = options.get("depth")
        else:
            depth = self.get_option("depth", **kwargs)

        loop = node
        if loop.loop_type == "":
            # Loop is over cells
            loop.set_upper_bound("cell_halo", depth)
        elif loop.loop_type == "cells_in_colour":
            # Loop is over cells of a single colour
            loop.set_upper_bound("colour_halo", depth)
        elif loop.loop_type == "dof":
            loop.set_upper_bound("dof_halo", depth)
        else:
            raise TransformationError(
                f"Unsupported loop_type '{loop.loop_type}' found in "
                f"LFRicRedundant ComputationTrans.apply()")
        # Add/remove halo exchanges as required due to the redundant
        # computation
        loop.update_halo_exchanges()


# For Sphinx AutoAPI documentation generation.
__all__ = ["LFRicRedundantComputationTrans"]
