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
# Author R. W. Ford, STFC Daresbury Lab

'''Module providing a transformation from an Assignment node
containing an Array Reference node in its left-hand-side which in turn
has at least one PSyIR Range node specifying an access to an array
index (equivalent to an array assignment statement in Fortran) to the
equivalent loop representation using a NemoLoop node. The outermost
Range is chosen to be replaced as replacing any other Range node would
result in a reordering of the array accesses.

'''

from psyclone.psyir.nodes import Assignment, Reference, Range
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.transformations import ArrayRange2LoopTrans
from psyclone.domain.nemo.transformations.nemo_arrayrange2loop_trans import \
    NemoArrayRange2LoopTrans


class NemoOuterArrayRange2LoopTrans(ArrayRange2LoopTrans):
    '''Provides a transformation from the outermost PSyIR ArrayReference
    Range to a PSyIR NemoLoop. For example:

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
            NemoOuterArrayRange2LoopTrans
    >>> from psyclone.transformations import TransformationError
    >>>
    >>> print(schedule.view())
    >>> trans = NemoOuterArrayRange2LoopTrans()
    >>> for assignment in schedule.walk(Assignment):
    >>>     while True:
    >>>         try:
    >>>             trans.apply(assignment)
    >>>         except TransformationError:
    >>>             break
    >>> print(schedule.view())

    '''
    def apply(self, node, options=None):
        '''Apply the NemoOuterArrayRange2Loop transformation to the specified
        node if the node is an Assignment and the left-hand-side of
        the assignment is an Array Reference containing at least one
        Range node specifying an access to an array index. If this is
        the case then the outermost Range nodes within array
        references within the assignment are replaced with references
        to a loop index. A NemoLoop loop (with the same loop index) is
        also placed around the modified assignment statement. If the
        array reference on the left-hand-side of the assignment only
        had one range node as an index (so now has none) then the
        assigment is also placed within a NemoKern.

        The name of the loop index is taken from the PSyclone
        configuration file if a name exists for the particular array
        index, otherwise a new name is generated. The bounds of the
        loop are taken from the Range node if they are provided. If
        not, the loop bounds are taken from the PSyclone configuration
        file if bounds values are supplied. If not, the LBOUND or
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

        # Get deepest array in LHS (excluding inside Ranges)
        lhs_array_ref = node.lhs.walk(ArrayMixin, stop_type=Range)[-1]
        index = lhs_array_ref.get_outer_range_index()
        nemo_arrayrange2loop = NemoArrayRange2LoopTrans()
        nemo_arrayrange2loop.apply(lhs_array_ref.children[index])

    def __str__(self):
        return ("Convert a PSyIR assignment to the outermost ArrayReference "
                "Range into a PSyIR NemoLoop.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation.
        :rtype: str

        '''
        return type(self).__name__

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        NemoOuterArrayRange2LoopTrans transformation to the supplied
        PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        :raises TransformationError: if the supplied node is not an \
            Assignment node, if the Assignment node does not have an \
            Array-type Reference node on its left hand side or if the \
            Array-type node does not contain at least one Range \
            node.

        '''
        # Am I an assignment node?
        if not isinstance(node, Assignment):
            raise TransformationError(
                f"Error in NemoOuterArrayRange2LoopTrans transformation. The "
                f"supplied node argument should be a PSyIR Assignment, but "
                f"found '{type(node).__name__}'.")

        # Is the LHS an array reference?
        if not (isinstance(node.lhs, Reference) and node.lhs.walk(ArrayMixin)):
            raise TransformationError(
                f"Error in NemoOuterArrayRange2LoopTrans transformation. The "
                f"LHS of the supplied assignment node should be a Reference "
                f"that contains an array access somewhere in the expression, "
                f"but found '{node.lhs}'.")
        # Has the array reference got a range?
        if not node.lhs.walk(Range):
            raise TransformationError(
                f"Error in NemoOuterArrayRange2LoopTrans transformation. The "
                f"LHS of the supplied assignment node should be an expression "
                f"with an array that has a Range node, but found "
                f"'{node.lhs}'.")


# For automatic document generation
__all__ = [
    'NemoOuterArrayRange2LoopTrans']
