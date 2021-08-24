# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#        J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

'''This module contains the GOcean-specific loop-fusion transformation.
'''

from psyclone.psyir.transformations import LoopFuseTrans, TransformationError
import psyclone.gocean1p0
import psyclone.gocean0p1


class GOceanLoopFuseTrans(LoopFuseTrans):
    ''' GOcean API specialisation of the :py:class:`base class <LoopFuseTrans>`
    in order to fuse two GOcean loops after performing validity checks (e.g.
    that the loops are over the same grid-point type). For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> ast, invokeInfo = parse("shallow_alg.f90")
    >>> psy = PSyFactory("gocean1.0").create(invokeInfo)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>>
    >>> from psyclone.transformations import GOceanLoopFuseTrans
    >>> ftrans = GOceanLoopFuseTrans()
    >>> new_schedule, memento = ftrans.apply(schedule[0], schedule[1])
    >>> new_schedule.view()

    '''
    def __str__(self):
        return ("Fuse two adjacent loops together with GOcean-specific "
                "validity checks")

    def validate(self, node1, node2, options=None):
        '''Checks if it is valid to apply the GOceanLoopFuseTrans
        transform. It ensures that the fused loops are over
        the same grid-point types, before calling the normal
        LoopFuseTrans validation function.

        :param node1: the first Node representing a GOLoop.
        :type node1: :py:class:`psyclone.gocean1p0.GOLoop`
        :param node2: the second Node representing a GOLoop.
        :type node2: :py:class:`psyclone.gocean1p0.GOLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied loops are over \
                                     different grid-point types.

        :raises TransformationError: if invalid parameters are passed in.

        '''

        # Either both nodes are gocean1.0 loop nodes, or both
        # nodes are gocean0.1 loop nodes, otherwise raise an exception:
        if not ((isinstance(node1, psyclone.gocean0p1.GOLoop) and
                 isinstance(node2, psyclone.gocean0p1.GOLoop)) or
                (isinstance(node1, psyclone.gocean1p0.GOLoop) and
                 isinstance(node2, psyclone.gocean1p0.GOLoop))):
            raise TransformationError("Error in {0} transformation. "
                                      "Both nodes must be of the same "
                                      "GOLoop class.".format(self.name))

        super(GOceanLoopFuseTrans, self).validate(node1, node2,
                                                  options=options)

        if node1.field_space != node2.field_space:
            raise TransformationError(
                "Error in {0} transformation. Cannot "
                "fuse loops that are over different grid-point types: "
                "{1} {2}".format(self.name, node1.field_space,
                                 node2.field_space))

    def apply(self, node1, node2, options=None):
        ''' Fuses two `psyclone.gocean1p0.GOLoop` loops after performing
        validity checks by calling :py:meth:`LoopFuseTrans.apply` method
        of the base class.

        :param node1: the first Node representing a GOLoop.
        :type node1: :py:class:`psyclone.gocean1p0.GOLoop`
        :param node2: the second Node representing a GOLoop.
        :type node2: :py:class:`psyclone.gocean1p0.GOLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: two-tuple of the modified Schedule and a record of \
                  the transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        :raises TransformationError: if the supplied loops are over \
                                     different grid-point types.
        :raises TransformationError: if there is an unexpected exception.
        '''

        # Validate first
        self.validate(node1, node2, options=options)

        # Now check for GOcean-specific constraints before applying
        # the transformation
        try:
            return LoopFuseTrans.apply(self, node1, node2, options)
        except Exception as err:
            raise TransformationError(
                "Error in {0} transformation. Unexpected exception: {1}".
                format(self.name, err))


# For automatic documentation generation
__all__ = ["GOceanLoopFuseTrans"]
