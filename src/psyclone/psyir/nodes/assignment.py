# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the Assignment node implementation.'''

import re
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.core.access_info import VariablesAccessInfo, AccessType
from psyclone.errors import InternalError


class Assignment(Statement):
    '''
    Node representing an Assignment statement. As such it has a LHS and RHS
    as children 0 and 1 respectively.
    '''
    # Textual description of the node.
    _children_valid_format = "DataNode, DataNode"
    _text_name = "Assignment"
    _colour_key = "Assignment"

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
                "Assignment '{0}' malformed or incomplete. It "
                "needs at least 1 child to have a lhs.".format(repr(self)))

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
                "Assignment '{0}' malformed or incomplete. It "
                "needs at least 2 children to have a rhs.".format(repr(self)))

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
        lhs.parent = new_assignment
        rhs.parent = new_assignment
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
        if isinstance(self.lhs, CodeBlock):
            # TODO #363: Assignment to user defined type, not supported yet.
            # Here an absolute hack to get at least some information out
            # from the AST - though indices are just strings, which will
            # likely cause problems later as well.
            name = str(self.lhs.ast)
            # A regular expression that tries to find the last parenthesis
            # pair in the name ("a(i,j)" --> "(i,j)")
            ind = re.search(r"\([^\(]+\)$", name)
            if ind:
                # Remove the index part of the name
                name = name.replace(ind.group(0), "")
                # The index must be added as a list
                accesses_left.add_access(name, AccessType.WRITE, self,
                                         [ind.group(0)])
            else:
                accesses_left.add_access(name, AccessType.WRITE, self)
        else:
            var_info = accesses_left[self.lhs.name]
            try:
                var_info.change_read_to_write()
            except InternalError:
                # An internal error typically indicates that the same variable
                # is used twice on the LHS, e.g.: g(g(1)) = ... This is not
                # supported in PSyclone.
                from psyclone.parse.utils import ParseError
                raise ParseError("The variable '{0}' appears more than once "
                                 "on the left-hand side of an assignment."
                                 .format(self.lhs.name))

        # Merge the data (that shows now WRITE for the variable) with the
        # parameter to this function. It is important that first the
        # RHS is added, so that in statements like 'a=a+1' the read on
        # the RHS comes before the write on the LHS (they have the same
        # location otherwise, but the order is still important)
        self.rhs.reference_accesses(var_accesses)
        var_accesses.merge(accesses_left)
        var_accesses.next_location()
