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

''' This module contains the IfBlock node implementation.'''

from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.errors import InternalError, GenerationError


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
    valid_annotations = ('was_elseif', 'was_single_stmt', 'was_case',
                         'was_where')
    # Textual description of the node.
    _children_valid_format = "DataNode, Schedule [, Schedule]"
    _text_name = "If"
    _colour_key = "If"

    def __init__(self, parent=None, annotations=None):
        super(IfBlock, self).__init__(parent=parent)
        if annotations:
            for annotation in annotations:
                if annotation in IfBlock.valid_annotations:
                    self._annotations.append(annotation)
                else:
                    raise InternalError(
                        "IfBlock with unrecognized annotation '{0}', valid "
                        "annotations are: {1}.".format(
                            annotation, IfBlock.valid_annotations))

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
                "IfBlock malformed or incomplete. It should have at least 2 "
                "children, but found {0}.".format(len(self.children)))
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
                "IfBlock malformed or incomplete. It should have at least 2 "
                "children, but found {0}.".format(len(self.children)))

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
        :type if_body: list of :py:class:`psyclone.psyir.nodes.Node`
        :param else_body: PSyIR nodes representing the else body of the \
            if block of None if there is no else body (defaults to None).
        :type else_body: list of :py:class:`psyclone.psyir.nodes.Node` or \
            NoneType

        :returns: an IfBlock instance.
        :rtype: :py:class:`psyclone.psyir.nodes.IfBlock`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(if_body, list):
            raise GenerationError(
                "if_body argument in create method of IfBlock class should be "
                "a list.")
        if else_body is not None and not isinstance(else_body, list):
            raise GenerationError(
                "else_body argument in create method of IfBlock class should "
                "be a list.")

        if_stmt = IfBlock()
        if_schedule = Schedule(parent=if_stmt)
        if_schedule.children = if_body
        for node in if_body:
            node.parent = if_schedule
        if else_body is not None:
            else_schedule = Schedule(parent=if_stmt)
            else_schedule.children = else_body
            for node in else_body:
                node.parent = else_schedule
            if_stmt.children = [if_condition, if_schedule, else_schedule]
        else:
            if_stmt.children = [if_condition, if_schedule]
        if_condition.parent = if_stmt
        return if_stmt

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        text = self.coloured_name(colour) + "["
        if self.annotations:
            text += "annotations='" + ','.join(self.annotations) + "'"
        text += "]"
        return text

    def __str__(self):
        result = "If[]\n"
        for entity in self._children:
            result += str(entity)
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. It combines the data from
        the condition, if-body and (if available) else-body. This could
        later be extended to handle cases where a variable is only written
        in one of the two branches.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # The first child is the if condition - all variables are read-only
        self.condition.reference_accesses(var_accesses)
        var_accesses.next_location()
        self.if_body.reference_accesses(var_accesses)
        var_accesses.next_location()

        if self.else_body:
            self.else_body.reference_accesses(var_accesses)
            var_accesses.next_location()
