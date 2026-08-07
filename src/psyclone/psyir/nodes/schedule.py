# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the Schedule node implementation.'''

from psyclone.psyir.nodes.scoping_node import ScopingNode
from psyclone.psyir.nodes.statement import Statement


class Schedule(ScopingNode):
    ''' Stores schedule information for a sequence of statements (supplied
    as a list of children).

    '''
    # Textual description of the node.
    _children_valid_format = "[Statement]*"
    _text_name = "Schedule"
    _colour = "white"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # pylint: disable=unused-argument
        return isinstance(child, Statement)

    def __getitem__(self, index):
        '''
        Overload the subscript notation ([int]) to access specific statements
        in the Schedule.

        :param int index: index of the statement to access.
        :returns: statement in a given position in the Schedule sequence.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''
        return self._children[index]

    def __str__(self):
        result = self.coloured_name(False) + ":\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + self.coloured_name(False)
        return result


# For AutoAPI documentation generation
__all__ = ['Schedule']
