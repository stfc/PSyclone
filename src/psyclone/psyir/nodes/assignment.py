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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the Assignment node implementation.'''

from psyclone.core import VariablesAccessInfo
from psyclone.errors import InternalError
from psyclone.f2pygen import PSyIRGen
from psyclone.psyir.nodes.array_reference import ArrayReference
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.structure_reference import StructureReference
from psyclone.psyir.nodes.operation import Operation, REDUCTION_OPERATORS


class Assignment(Statement):
    '''
    Node representing an Assignment statement. As such it has a LHS and RHS
    as children 0 and 1 respectively.
    '''
    # Textual description of the node.
    _children_valid_format = "DataNode, DataNode"
    _text_name = "Assignment"
    _colour = "blue"

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

    @staticmethod
    def create(lhs, rhs):
        '''Create an Assignment instance given lhs and rhs child instances.

        :param lhs: the PSyIR node containing the left hand side of \
            the assignment.
        :type lhs: :py:class:`psyclone.psyir.nodes.Node`
        :param rhs: the PSyIR node containing the right hand side of \
            the assignment.
        :type rhs: :py:class:`psyclone.psyir.nodes.Node`

        :returns: an Assignment instance.
        :rtype: :py:class:`psyclone.psyir.nodes.Assignment`

        '''
        new_assignment = Assignment()
        new_assignment.children = [lhs, rhs]
        return new_assignment

    def __str__(self):
        result = "Assignment[]\n"
        for entity in self._children:
            result += str(entity)
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information from this node. The assigned-to
        variable will be set to 'WRITE'.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # It is important that a new instance is used to handle the LHS,
        # since a check in 'change_read_to_write' makes sure that there
        # is only one access to the variable!
        accesses_left = VariablesAccessInfo()
        self.lhs.reference_accesses(accesses_left)
        # Now change the (one) access to the assigned variable to be WRITE:
        sig, _ = self.lhs.get_signature_and_indices()
        var_info = accesses_left[sig]
        try:
            var_info.change_read_to_write()
        except InternalError as err:
            # An internal error typically indicates that the same variable
            # is used twice on the LHS, e.g.: g(g(1)) = ... This is not
            # supported in PSyclone.
            raise NotImplementedError(f"The variable '{self.lhs.name}' appears"
                                      f" more than once on the left-hand side "
                                      f"of an assignment.") from err

        # Merge the data (that shows now WRITE for the variable) with the
        # parameter to this function. It is important that first the
        # RHS is added, so that in statements like 'a=a+1' the read on
        # the RHS comes before the write on the LHS (they have the same
        # location otherwise, but the order is still important)
        self.rhs.reference_accesses(var_accesses)
        var_accesses.merge(accesses_left)
        var_accesses.next_location()

    @property
    def is_array_range(self):
        '''
        returns: True if the lhs of the assignment is an array access with at \
            least one of its dimensions being a range and False otherwise.
        rtype: bool

        '''
        # It's not sufficient simply to check for a Range node as that may be
        # part of an argument to an Operator or function that performs a
        # reduction and thus returns a scalar result, e.g. a(SUM(b(:))) = 1.0
        # TODO #658 this check for reductions needs extending to also support
        # user-implemented functions.
        if isinstance(self.lhs, (ArrayReference, StructureReference)):
            ranges = self.lhs.walk(Range)
            for array_range in ranges:
                opn = array_range.ancestor(Operation)
                while opn:
                    if opn.operator in REDUCTION_OPERATORS:
                        # The current array range is in an argument to a
                        # reduction operation so we assume that the result
                        # is a scalar.
                        # TODO 658 this could still be a reduction into an
                        # array (e.g. SUM(a(:,:), 1)) but we need to be able
                        # to interrogate the type of a PSyIR expression in
                        # order to be sure. e.g. SUM(a(:,:), mask(:,:)) will
                        # return a scalar.
                        break
                    opn = opn.ancestor(Operation)
                else:
                    # We didn't find a reduction operation so there is an
                    # array range on the LHS
                    return True
        return False

    def gen_code(self, parent):
        '''F2pygen code generation of an Assignment.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        '''
        parent.add(PSyIRGen(parent, self))
