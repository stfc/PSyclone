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
#         A. B. G. Chalk, V. K. Atkinson, STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, J. G. Wallwork, O. Brunt and L. Turner, Met Office
#          S. Valat, Inria / Laboratoire Jean Kuntzmann
#          M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann
#          J. Dendy, Met Office


from psyclone.psyir.transformations.omp_loop_trans import OMPLoopTrans
from psyclone.psyir.nodes import OMPParallelDoDirective


class OMPParallelLoopTrans(OMPLoopTrans):

    ''' Adds an OpenMP PARALLEL DO directive to a loop.

        For example:

        >>> from psyclone.parse.algorithm import parse
        >>> from psyclone.psyGen import PSyFactory
        >>> ast, invokeInfo = parse("lfric.F90")
        >>> psy = PSyFactory("lfric").create(invokeInfo)
        >>> schedule = psy.invokes.get('invoke_v3_kernel_type').schedule
        >>> # Uncomment the following line to see a text view of the schedule
        >>> # print(schedule.view())
        >>>
        >>> from psyclone.transformations import OMPParallelLoopTrans
        >>> trans = OMPParallelLoopTrans()
        >>> trans.apply(schedule.children[0])
        >>> # Uncomment the following line to see a text view of the schedule
        >>> # print(schedule.view())

    '''
    def __str__(self):
        return "Add an 'OpenMP PARALLEL DO' directive"

    def apply(self, node, options=None):
        ''' Apply an OMPParallelLoop Transformation to the supplied node
        (which must be a Loop). In the generated code this corresponds to
        wrapping the Loop with directives:

        .. code-block:: fortran

          !$OMP PARALLEL DO ...
          do ...
            ...
          end do
          !$OMP END PARALLEL DO

        :param node: the node (loop) to which to apply the transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations
            and validation.
        :type options: Optional[Dict[str, Any]]
        '''
        self.validate(node, options=options)

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # add our OpenMP loop directive setting its parent to the node's
        # parent and its children to the node
        directive = OMPParallelDoDirective(children=[node.detach()],
                                           omp_schedule=self.omp_schedule)

        # add the OpenMP loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)
