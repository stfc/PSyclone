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
# Author A. R. Porter, STFC Daresbury Lab

'''Module providing a transformation from a generic PSyIR representation of
   a PSy layer into a NEMO-specific one.

'''

from psyclone.transformations import Transformation, TransformationError
from psyclone.psyir.nodes import Routine, Loop
from psyclone.domain.nemo.transformations import NemoInvokeTrans, \
    NemoKernelTrans, NemoLoopTrans


class NemoPSyTrans(Transformation):
    '''
    Transform a generic PSyIR representation of a PSy layer into a PSyclone
    version with specialised, NEMO-specific nodes.

    '''
    @property
    def name(self):
        '''
        :returns: the name of the transformation.
        :rtype: str

        '''
        return type(self).__name__

    def validate(self, node, options=None):
        '''
        '''

    def apply(self, psyir, options=None):
        '''
        Takes generic PSyIR and replaces recognised structures with
        NEMO-specific PSyIR (in-place). Note that this may mean replacing
        the top-level node itself and therefore this routine returns the
        root of the modified tree.

        :param psyir: the root node of the PSyIR tree to process.
        :type psyir: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        :returns: root of the modified PSyIR tree.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self.validate(psyir, options=options)

        # Deal with Routines first. Have to take care of the case where the
        # supplied top-level node is itself a Routine and must therefore be
        # replaced.
        root = psyir
        invoke_trans = NemoInvokeTrans()
        kern_trans = NemoKernelTrans()
        loop_trans = NemoLoopTrans()

        for routine in psyir.walk(Routine):
            new_node = invoke_trans.apply(routine)
            if routine is root:
                root = new_node

        # Reverse the result of the walk() so that we process loops depth
        # first. This permits the correct identification of NemoKern's.
        for loop in reversed(root.walk(Loop)):
            new_loop = loop_trans.apply(loop)

            try:
                kern_trans.apply(new_loop.loop_body)
            except TransformationError:
                # Not all loop bodies are valid kernels (e.g. if they do IO)
                pass

        return root
