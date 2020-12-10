# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module providing a transformation from a PSyIR ArrayReference Range
to a PSyIR NemoLoop. This is useful for capturing the contents of
array ranges as kernel regions so they can be optimised.

'''

from __future__ import absolute_import

from psyclone.psyir.nodes import ArrayReference, Assignment
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.transformations import ArrayRange2LoopTrans
from psyclone.domain.nemo.transformations import NemoArrayRange2LoopTrans
from psyclone.domain.nemo.transformations.nemo_arrayrange2loop_trans import \
    _get_outer_index


class NemoOuterArrayRange2LoopTrans(ArrayRange2LoopTrans):
    '''Provides a transformation from the outermost PSyIR ArrayReference
    Range to a PSyIR NemoLoop. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "nemo"
    >>> filename = "tra_adv_compute.F90"
    >>> ast, invoke_info = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invoke_info)
    >>> schedule = psy.invokes.invoke_list[0].schedule
    >>>
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.domain.nemo.transformations import \
    >>>     NemoOuterArrayRange2LoopTrans
    >>> from psyclone.errors import TransformationError
    >>>
    >>> schedule.view()
    >>> trans = NemoOuterArrayRange2LoopTrans()
    >>> for assignment in schedule.walk(Assignment):
    >>>     while True:
    >>>         try:
    >>>             trans.apply(assignment)
    >>>         except TransformationError:
    >>>             break
    >>> schedule.view()

    '''
    def apply(self, node, options=None):
        '''Apply the NemoOuterArrayRange2Loop transformation to the specified
        node. The node must be an assignment. The rightmost range node
        in each array within the assignment is replaced with a loop
        index with the expected name for the particular loop dimension
        within a NemoLoop iterating over that index. The bounds of the
        loop are also determined by the expected bounds for the loop
        dimension.

        :param node: an Assignment node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`

        '''
        self.validate(node)

        # get lhs array
        lhs_array_ref = node.lhs
        index = _get_outer_index(lhs_array_ref)
        nemo_arrayrange2loop = NemoArrayRange2LoopTrans()
        nemo_arrayrange2loop.apply(lhs_array_ref.children[index])

    def __str__(self):
        return ("Convert a PSyIR assignment to the outermost ArrayReference "
                "Range into a PSyIR NemoLoop.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        NemoArrayRange2LoopTrans transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`

        :raises TransformationError: if no conformant loop bounds were \
            found.
        :raises TransformationError: if an associated loop bound \
            specified in the config file is not declared in the symbol \
            table.

        '''
        # Am I an assignment node?
        if not isinstance(node, Assignment):
            raise TransformationError(
                "Error in NemoOuterArrayRange2LoopTrans transformation. The "
                "supplied node argument should be a PSyIR Assignment, but "
                "found '{0}'.".format(type(node).__name__))

        # Is the LHS an array reference?
        if not isinstance(node.lhs, ArrayReference):
            raise TransformationError(
                "Error in NemoOuterArrayRange2LoopTrans transformation. The "
                "supplied assignment node should have an ArrayReference node "
                "on its lhs but found '{0}'."
                "".format(type(node.parent).__name__))
        array_reference = node.lhs
        # Has the array reference got a range?
        try:
            _ = _get_outer_index(array_reference)
        except IndexError:
            raise TransformationError(
                "Error in NemoOuterArrayRange2LoopTrans transformation. The "
                "supplied assignment node should have an ArrayReference node "
                "on its lhs containing at least one Range node but there are "
                "none.")


__all__ = [
    'NemoOuterArrayRange2LoopTrans']
