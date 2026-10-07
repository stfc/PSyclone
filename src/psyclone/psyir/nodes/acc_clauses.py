# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the implementations of the various OpenACC Directive
Clause nodes.'''

from typing import Union

from psyclone.psyir.nodes.clause import Clause
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.reference import Reference


class ACCAsyncQueueClause(Clause):
    '''
    OpenACC async clause. Has one child which specifies which queue, if any,
    this node is associated with.

    '''
    _children_valid_format = "DataNode"
    _clause_string = "async"

    @staticmethod
    def _validate_child(position: int, child: Node) -> bool:
        '''
        Decides whether a given child and position are valid for this node.
        Only zero or one child of type DataNode is permitted.

        :param position: the position to be validated.
        :param child: a child to be validated.

        '''
        if position != 0:
            return False
        return isinstance(child, DataNode)

    @property
    def queue(self) -> Union[DataNode, None]:
        '''
        :returns: the queue specified by this clause (if any)
        '''
        if self.children:
            return self.children[0]
        return None


class ACCCopyClause(Clause):
    '''
    OpenACC copy clause. Specifies a list of variables that are to be copied
    to the device at the start of the associated region and back to the host
    at the end.

    '''
    _children_valid_format = "Reference"
    _clause_string = "copy"

    @staticmethod
    def _validate_child(position, child):
        '''
        Decides whether a given child and position are valid for this node.
        Any number of children are allowed, all of type Reference.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)


class ACCCopyInClause(Clause):
    '''
    OpenACC copy clause. Specifies a list of variables that are to be copied
    to the device at the start of a region.

    '''
    _children_valid_format = "Reference"
    _clause_string = "copyin"

    @staticmethod
    def _validate_child(position, child):
        '''
        Decides whether a given child and position are valid for this node.
        Any number of children are allowed, all of type Reference.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)


class ACCCopyOutClause(Clause):
    '''
    OpenACC copy clause. Specifies a list of variables that are to be copied
    from the device to the host at the end of the associated region.

    '''
    _children_valid_format = "Reference"
    _clause_string = "copyout"

    @staticmethod
    def _validate_child(position, child):
        '''
        Decides whether a given child and position are valid for this node.
        Any number of children are allowed, all of type Reference.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)


__all__ = ["ACCAsyncQueueClause", "ACCCopyClause",
           "ACCCopyInClause", "ACCCopyOutClause"]
