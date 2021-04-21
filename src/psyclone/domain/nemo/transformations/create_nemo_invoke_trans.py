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

'''
Module providing a transformation from a generic PSyIR routine into a
NEMO Invoke.
'''

from psyclone.transformations import Transformation, TransformationError
from psyclone.psyir.nodes import Routine
from psyclone.nemo import NemoInvokeSchedule


class CreateNemoInvokeTrans(Transformation):
    '''
    Transform a generic PSyIR Routine into a NEMO Invoke.

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
        Check that the supplied node is a valid target for this transformation.

        :param node: the target of the transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        :raises TransformationError: if the supplied node is not a Routine.

        '''
        super(CreateNemoInvokeTrans, self).validate(node)

        if not isinstance(node, Routine):
            raise TransformationError(
                "Error in NemoInvokeTrans transformation. The supplied node "
                "should be a PSyIR Routine but found '{0}'".format(
                    type(node).__name__))

    def apply(self, routine, options=None):
        '''
        Takes a generic PSyIR Routine and replaces it with a NEMO Invoke.
        NEMO-specific PSyIR (in-place). Note that this may mean replacing
        the top-level node itself and therefore this routine returns the
        root of the modified tree.

        :param routine: the routine node to be transformed.
        :type routine: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        :returns: the new PSyIR node that replaces the Routine.
        :rtype: :py:class:`psyclone.nemo.NemoInvokeSchedule`

        '''
        self.validate(routine)

        new_node = NemoInvokeSchedule.create(routine.name,
                                             routine.symbol_table,
                                             routine.pop_all_children(),
                                             is_program=routine.is_program)

        # We need to replace the top node in the (possibly sub-) PSyIR
        # tree that we've been passsed.
        if routine.parent:
            routine.replace_with(new_node)

        return new_node
