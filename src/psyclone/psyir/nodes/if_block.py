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
# -----------------------------------------------------------------------------

''' This module contains the IfBlock node implementation.'''

from psyclone.core import VariablesAccessMap
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.statement import Statement


class IfBlock(Statement):
    '''
    Node representing an if-block within the PSyIR. It has two mandatory
    children: the first one represents the if-condition and the second one
    the if-body; and an optional third child representing the else-body.

    '''
    # The valid annotations for this If node:
    # 'was_elseif' to tag nested ifs originally written with the 'else if'
    # languague syntactic construct;
    # 'was_single_stmt' to tag ifs with a 1-statement body which were
    # originally written in a single line;
    # 'was_case' to tag a conditional structure which was originally written
    # with the Fortran 'case' or C 'switch' syntactic constructs;
    # 'was_where' - a conditional structure originally implied by a Fortran
    # WHERE construct.
    # 'was_type_is' to tag a conditional structure originally
    # written with the Fortran 'select type' construct with a 'type
    # is' clause.
    # 'was_class_is' to tag a conditional structure originally
    # written with the Fortran 'select type' construct with a 'class
    # is' clause.
    valid_annotations = ('was_elseif', 'was_single_stmt', 'was_case',
                         'was_where', 'was_type_is', 'was_class_is')
    # Textual description of the node.
    _children_valid_format = "DataNode, Schedule [, Schedule]"
    _text_name = "If"
    _colour = "red"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return (position == 0 and isinstance(child, DataNode)) or (
            position in (1, 2) and isinstance(child, Schedule))

    @property
    def condition(self):
        ''' Return the PSyIR Node representing the conditional expression
        of this IfBlock.

        :returns: IfBlock conditional expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        :raises InternalError: If the IfBlock node does not have the correct \
            number of children.
        '''
        if len(self.children) < 2:
            raise InternalError(
                f"IfBlock malformed or incomplete. It should have at least 2 "
                f"children, but found {len(self.children)}.")
        return self._children[0]

    @property
    def if_body(self):
        ''' Return the Schedule executed when the IfBlock evaluates to True.

        :returns: Schedule to be executed when IfBlock evaluates to True.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`
        :raises InternalError: If the IfBlock node does not have the correct \
            number of children.
        '''
        if len(self.children) < 2:
            raise InternalError(
                f"IfBlock malformed or incomplete. It should have at least 2 "
                f"children, but found {len(self.children)}.")

        return self._children[1]

    @property
    def else_body(self):
        ''' If available return the Schedule executed when the IfBlock
        evaluates to False, otherwise return None.

        :returns: Schedule to be executed when IfBlock evaluates \
            to False, if it doesn't exist returns None.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule` or NoneType
        '''
        if len(self._children) == 3:
            return self._children[2]
        return None

    @staticmethod
    def create(if_condition, if_body, else_body=None):
        '''Create an IfBlock instance given valid instances of an
        if_condition, an if_body and an optional else_body.

        :param if_condition: the PSyIR node containing the if \
            condition of the if block.
        :type if_condition: :py:class:`psyclone.psyir.nodes.Node`
        :param if_body: the PSyIR nodes representing the if body of \
            the if block.
        :type if_body: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param else_body: PSyIR nodes representing the else body of the \
            if block or None if there is no else body (defaults to None).
        :type else_body: Optional[List[:py:class:`psyclone.psyir.nodes.Node`]]

        :returns: an IfBlock instance.
        :rtype: :py:class:`psyclone.psyir.nodes.IfBlock`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(if_body, list):
            raise GenerationError(
                f"if_body argument in create method of IfBlock class should "
                f"be a list but found '{type(if_body).__name__}'.")
        if else_body is not None and not isinstance(else_body, list):
            raise GenerationError(
                f"else_body argument in create method of IfBlock class should "
                f"be a list but found '{type(else_body).__name__}'.")

        if_stmt = IfBlock()
        if_schedule = Schedule(parent=if_stmt, children=if_body)
        if else_body is not None:
            else_schedule = Schedule(parent=if_stmt, children=else_body)
            if_stmt.children = [if_condition, if_schedule, else_schedule]
        else:
            if_stmt.children = [if_condition, if_schedule]
        return if_stmt

    def __str__(self):
        name = self._text_name
        result = name + "[]\n"
        for entity in self._children:
            result += str(entity)
        result += "End " + name
        return result

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are SingleVariableAccessInfo
            (a sequence of AccessTypes).

        '''
        var_accesses = self.condition.reference_accesses()
        var_accesses.update(self.if_body.reference_accesses())

        if self.else_body:
            var_accesses.update(self.else_body.reference_accesses())
        return var_accesses
