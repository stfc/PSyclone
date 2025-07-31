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
#         A. B. G. Chalk and V. K. Atkinson, STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, J. G. Wallwork, O. Brunt and L. Turner, Met Office
#          S. Valat, Inria / Laboratoire Jean Kuntzmann
#          M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann
#          J. Dendy, Met Office

from psyclone.psyir.transformations.omp_parallel_loop_trans import (
    OMPParallelLoopTrans)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class GOceanOMPParallelLoopTrans(OMPParallelLoopTrans):

    '''GOcean specific OpenMP Do loop transformation. Adds GOcean
       specific validity checks (that supplied Loop is an inner or outer
       loop). Actual transformation is done by
       :py:class:`base class <OMPParallelLoopTrans>`.

        :param str omp_directive: choose which OpenMP loop directive to use. \
            Defaults to "do".
        :param str omp_schedule: the OpenMP schedule to use. Must be one of \
            'runtime', 'static', 'dynamic', 'guided' or 'auto'. Defaults to \
            'static'.

    '''
    def __init__(self, omp_directive="do", omp_schedule="static"):
        super().__init__(omp_directive=omp_directive,
                         omp_schedule=omp_schedule)

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a GOcean loop"

    def apply(self, node, options=None):
        ''' Perform GOcean-specific loop validity checks then call
        :py:meth:`OMPParallelLoopTrans.apply`.

        :param node: a Loop node from an AST.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not an inner or\
            outer loop.

        '''
        self.validate(node, options=options)

        # Check we are either an inner or outer loop
        if node.loop_type not in ["inner", "outer"]:
            raise TransformationError(
                "Error in "+self.name+" transformation.  The requested loop"
                " is not of type inner or outer.")

        OMPParallelLoopTrans.apply(self, node)
