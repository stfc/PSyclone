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
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
#         J. G. Wallwork, University of Cambridge
# -----------------------------------------------------------------------------

''' This module contains the Assignment node implementation.'''

from psyclone.core import VariablesAccessMap
from psyclone.errors import InternalError
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.array_reference import ArrayReference
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.intrinsic_call import (
    IntrinsicCall, REDUCTION_INTRINSICS)
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.structure_reference import StructureReference


class Assignment(Statement):
    '''
    Node representing an Assignment statement. As such it has a LHS and RHS
    as children 0 and 1 respectively.

    :param bool is_pointer: whether this represents a pointer assignment.
    :param kwargs: additional keyword arguments provided to the Node.
    :type kwargs: unwrapped dict.
    '''
    # Textual description of the node.
    _children_valid_format = "DataNode, DataNode"
    _text_name = "Assignment"
    _colour = "blue"

    def __init__(self, is_pointer=False, **kwargs):
        super().__init__(**kwargs)
        self.is_pointer = is_pointer

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position < 2 and isinstance(child, DataNode)

    def __eq__(self, other):
        '''
        :param Any other: the object to check equality to.

        :returns: whether this type is equal to the 'other' type.
        :rtype: bool
        '''
        return super().__eq__(other) and self.is_pointer == other.is_pointer

    @property
    def lhs(self):
        '''
        :returns: the child node representing the Left-Hand Side of the \
            assignment.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises InternalError: Node has fewer children than expected.
        '''
        if not self._children:
            raise InternalError(
                f"Assignment '{repr(self)}' malformed or incomplete. It "
                f"needs at least 1 child to have a lhs.")

        return self._children[0]

    @property
    def rhs(self):
        '''
        :returns: the child node representing the Right-Hand Side of the \
            assignment.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises InternalError: Node has fewer children than expected.
        '''
        if len(self._children) < 2:
            raise InternalError(
                f"Assignment '{repr(self)}' malformed or incomplete. It "
                f"needs at least 2 children to have a rhs.")

        return self._children[1]

    @property
    def is_pointer(self):
        '''
        :returns: whether this represents a pointer assignment.
        :rtype: bool
        '''
        return self._is_pointer

    @is_pointer.setter
    def is_pointer(self, value):
        '''
        :param bool is_pointer: whether this represents a pointer assignment.

        :raises TypeError: if `value` is not a boolean.
        '''
        if not isinstance(value, bool):
            raise TypeError(f"is_pointer must be a boolean "
                            f"but got '{type(value).__name__}'")

        self._is_pointer = value

    @staticmethod
    def create(lhs, rhs, is_pointer=False):
        '''Create an Assignment instance given lhs and rhs child instances.

        :param lhs: the PSyIR node containing the left hand side of
            the assignment.
        :type lhs: :py:class:`psyclone.psyir.nodes.Node`
        :param rhs: the PSyIR node containing the right hand side of
            the assignment.
        :type rhs: :py:class:`psyclone.psyir.nodes.Node`
        :param bool is_pointer: whether this represents a pointer assignment.

        :returns: an Assignment instance.
        :rtype: :py:class:`psyclone.psyir.nodes.Assignment`

        '''
        new_assignment = Assignment(is_pointer)
        new_assignment.children = [lhs, rhs]
        return new_assignment

    def __str__(self):
        pointer_txt = "is_pointer=True" if self._is_pointer else ""
        result = f"Assignment[{pointer_txt}]\n"
        for entity in self._children:
            result += str(entity)
        return result

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        # It is important that a new instance is used to handle the LHS,
        # since a check in 'change_read_to_write' makes sure that there
        # is only one access to the variable!
        lhs_accesses = self.lhs.reference_accesses()
        # Now change the (one) access to the assigned variable to be WRITE.
        # Note that if the LHS is a CodeBlock then reference_accesses() will
        # already have given all Signatures READWRITE access. This is not
        # strictly correct (they should probably be UNKNOWN) and is the
        # subject of #2863.
        if isinstance(self.lhs, Reference):
            sig, _ = self.lhs.get_signature_and_indices()
            var_info = lhs_accesses[sig]
            try:
                var_info.change_read_to_write()
            except InternalError as err:
                # An internal error typically indicates that the same variable
                # is used twice on the LHS, e.g.: g(g(1)) = ... This is not
                # supported in PSyclone.
                raise NotImplementedError(
                    f"The variable '{self.lhs.name}' appears more than once on"
                    f" the left-hand side of an assignment.") from err

        # Merge the data (that shows now WRITE for the variable) with the
        # parameter to this function. It is important that first the
        # RHS is added, so that in statements like 'a=a+1' the read on
        # the RHS comes before the write on the LHS (they have the same
        # location otherwise, but the order is still important)
        rhs_accesses = self.rhs.reference_accesses()
        rhs_accesses.update(lhs_accesses)
        return rhs_accesses

    @property
    def is_array_assignment(self):
        '''
        :returns: True if the lhs of the assignment is an array access with at
            least one of its dimensions being a range and False otherwise.
        :rtype: bool

        '''
        # It's not sufficient simply to check for a Range node as that may be
        # part of an argument to an Operator or function that performs a
        # reduction and thus returns a scalar result, e.g. a(SUM(b(:))) = 1.0
        # TODO #658 this check for reductions needs extending to also support
        # user-implemented functions.
        if isinstance(self.lhs, (ArrayReference, StructureReference)):
            ranges = self.lhs.walk(Range)
            for array_range in ranges:
                opn = array_range.ancestor(IntrinsicCall)
                while opn:
                    if opn.intrinsic in REDUCTION_INTRINSICS:
                        # The current array range is in an argument to a
                        # reduction intrinsic so we assume that the result
                        # is a scalar.
                        # TODO #658 this could still be a reduction
                        # into an array (e.g. SUM(a(:,:), dim=1)) but
                        # we need to be able to interrogate the type
                        # of a PSyIR expression in order to be
                        # sure. e.g. SUM(a(:,:), mask=mask(:,:)) will
                        # return a scalar.
                        break
                    opn = opn.ancestor(IntrinsicCall)
                else:
                    # We didn't find a reduction intrinsic so there is an
                    # array range on the LHS
                    return True
        return False

    @property
    def is_literal_assignment(self):
        '''
        :returns: True if the rhs of the assignment is a literal value and
            False otherwise.
        :rtype: bool

        '''
        return isinstance(self.rhs, Literal)
