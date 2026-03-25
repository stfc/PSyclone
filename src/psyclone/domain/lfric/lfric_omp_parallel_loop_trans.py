# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
#         A. B. G. Chalk and V. K. Atkinson STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, J. G. Wallwork, O. Brunt and L. Turner, Met Office
#          S. Valat, Inria / Laboratoire Jean Kuntzmann
#          M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann
#          J. Dendy, Met Office

from psyclone.psyir.transformations.omp_parallel_loop_trans import (
    OMPParallelLoopTrans)
from psyclone.domain.lfric import (LFRicConstants, LFRicLoop)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class LFRicOMPParallelLoopTrans(OMPParallelLoopTrans):

    ''' LFRic-specific OpenMP loop transformation. Adds LFRic specific
        validity checks. Actual transformation is done by the
        :py:class:`base class <OMPParallelLoopTrans>`.

        :param str omp_directive: choose which OpenMP loop directive to use.
            Defaults to "do".
        :param str omp_schedule: the OpenMP schedule to use. Must be one of
            'runtime', 'static', 'dynamic', 'guided' or 'auto'. Defaults to
            'static'.

    '''
    def __init__(self, omp_directive="do", omp_schedule="static"):
        super().__init__(omp_directive=omp_directive,
                         omp_schedule=omp_schedule)

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to an LFRic loop"

    def validate(self, node, options=None):
        '''
        Perform LFRic-specific loop validity checks then call the `validate`
        method of the base class.

        :param node: the Node in the Schedule to check
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied Node is not an LFRicLoop.
        :raises TransformationError: if the associated loop requires
            colouring.
        '''
        if not isinstance(node, LFRicLoop):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"must be an LFRicLoop but got '{type(node).__name__}'")

        # If the loop is not already coloured then check whether or not
        # it should be. If the field space is discontinuous (including
        # any_discontinuous_space) then we don't need to worry about
        # colouring.
        const = LFRicConstants()
        if node.field_space.orig_name not in const.VALID_DISCONTINUOUS_NAMES:
            if (node.loop_type not in ('cells_in_colour', 'tiles_in_colour')
                    and node.has_inc_arg()):
                raise TransformationError(
                    f"Error in {self.name} transformation. The kernel has an "
                    f"argument with INC access but the loop is of type "
                    f"'{node.loop_type}'. Colouring is required.")
        # As this is a domain-specific loop, we don't perform general
        # dependence analysis because it is too conservative and doesn't
        # account for the special steps taken for such a loop at code-
        # generation time (e.g. the way we ensure variables are given the
        # correct sharing attributes).
        local_options = options.copy() if options else {}
        local_options["force"] = True
        super().validate(node, options=local_options)
