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
Module providing a transformation from a generic PSyIR Schedule into a
NEMO Kernel.
'''

from psyclone.transformations import Transformation, TransformationError
from psyclone.psyir.nodes import Schedule
from psyclone.nemo import NemoKern


class NemoKernelTrans(Transformation):
    '''
    Transform a generic PSyIR Schedule into a NEMO Kernel.

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

        :raises TransformationError: if the supplied node is not a Schedule \
            or it cannot be represented as a Kernel.

        '''
        super(NemoKernelTrans, self).validate(node)

        if not isinstance(node, Schedule):
            raise TransformationError(
                "Error in NemoKernelTrans transformation. The supplied node "
                "should be a PSyIR Schedule but found '{0}'".format(
                    type(node).__name__))

        # TODO move match() functionality into this routine?
        if not NemoKern.match(node):
            raise TransformationError(
                "Error in NemoKernelTrans transformation. The supplied "
                "Schedule cannot be converted into a NEMO kernel because it "
                "blah blah")

    def apply(self, sched, options=None):
        '''
        Takes a generic PSyIR Schedule and replaces it with a NEMO Kernel.

        :param sched: the Schedule node to be transformed.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        self.validate(sched)

        nemokern = NemoKern(sched.pop_all_children(),
                            None, parent=sched)
        sched.addchild(nemokern)
