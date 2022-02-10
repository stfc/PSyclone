# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, STFC Daresbury Lab
#          A. R. Porter, STFC Daresbury Lab

'''Module providing a transformation from an Assignment node
containing an Array Reference node in its left-hand-side which in turn
has at least one PSyIR Range node specifying an access to an array
index (equivalent to an array assignment statement in Fortran) to the
equivalent loop representation using the required number of NemoLoop
nodes.

'''
from __future__ import absolute_import

from psyclone.psyir.nodes import Assignment
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.domain.nemo.transformations.nemo_outerarrayrange2loop_trans \
    import NemoOuterArrayRange2LoopTrans


class NemoAllArrayRange2LoopTrans(Transformation):
    '''Provides a transformation for all PSyIR Array Ranges in an
    assignment to PSyIR NemoLoops. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "nemo"
    >>> filename = "tra_adv.F90" # examples/nemo/code
    >>> ast, invoke_info = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invoke_info)
    >>> schedule = psy.invokes.invoke_list[0].schedule
    >>>
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.domain.nemo.transformations import \
            NemoAllArrayRange2LoopTrans
    >>>
    >>> print(schedule.view())
    >>> trans = NemoAllArrayRange2LoopTrans()
    >>> for assignment in schedule.walk(Assignment):
    >>>     trans.apply(assignment)
    >>> print(schedule.view())

    '''
    def apply(self, node, options=None):
        '''Apply the NemoAllArrayRange2Loop transformation to the specified
        node if the node is an Assignment and the left-hand-side of
        the assignment is an Array Reference containing at least one
        Range node specifying an access to an array index. If this is
        the case then all Range nodes within array references within
        the assignment are replaced with references to the appropriate
        loop indices. The appropriate number of NemoLoop loops are
        also placed around the modified assignment statement and the
        assignment statement is placed within a NemoKern.

        The name of each loop index is taken from the PSyclone
        configuration file if a name exists for the particular array
        index, otherwise a new name is generated. The bounds of each
        loop are taken from the Range node if they are provided. If
        not, the loop bounds are taken from the PSyclone configuration
        file if a bounds value is supplied. If not, the LBOUND or
        UBOUND intrinsics are used as appropriate. The type of the
        NemoLoop is also taken from the configuration file if it is
        supplied for that index, otherwise it is specified as being
        "unknown".

        :param node: an Assignment node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        self.validate(node)

        trans = NemoOuterArrayRange2LoopTrans()
        try:
            while True:
                trans.apply(node)
        except TransformationError:
            pass

    def __str__(self):
        return ("Convert all array ranges in a PSyIR assignment into "
                "PSyIR NemoLoops.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation.
        :rtype: str

        '''
        return type(self).__name__

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        NemoArrayRange2LoopTrans transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        :raises TransformationError: if the supplied node is not an \
            Assignment.

        '''
        # Am I an assignment node?
        if not isinstance(node, Assignment):
            raise TransformationError(
                "Error in NemoAllArrayRange2LoopTrans transformation. The "
                "supplied node argument should be a PSyIR Assignment, but "
                "found '{0}'.".format(type(node).__name__))


# For automatic document generation
__all__ = [
    'NemoAllArrayRange2LoopTrans']
