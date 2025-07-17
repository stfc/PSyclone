# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Author: S. Valat, Inria / Lab. Jean Kuntzmann
# Modified: M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann
#           A. R. Porter, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

''' This module contains the mixins to apply some ACC features on many
classes.'''

import abc
from typing import Union

from psyclone.psyir.nodes.acc_clauses import ACCAsyncQueueClause
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.symbols import INTEGER_TYPE


class ACCAsyncMixin(metaclass=abc.ABCMeta):
    '''
    Class handling the common code to handle the async keyword on related acc
    directives.

    :param async_queue: Enable async support and attach it to the given queue.
                        Can use False to disable, True to enable on default
                        stream. Use int to attach to the given stream ID or
                        use a PSyIR expression to say at runtime what stream
                        to be used.
    '''
    def __init__(
                self,
                async_queue: Union[bool, int, DataNode] = False
            ):
        clause = self._create_clause(async_queue)
        if clause:
            self.addchild(clause)

    @staticmethod
    def convert_queue(
            async_queue: Union[bool, int, DataNode]) -> Union[bool, DataNode]:
        '''
        Utility to convert the provided queue value to PSyIR when
        applicable.

        :param async_queue: the queue value to convert.

        :returns: PSyIR of queue value or bool specifying whether or not async
                  is enabled.

        :raises TypeError: if the supplied queue value is of unsupported type.

        '''
        if isinstance(async_queue, bool):
            qarg = async_queue
        elif isinstance(async_queue, int):
            qarg = Literal(f"{async_queue}", INTEGER_TYPE)
        elif isinstance(async_queue, DataNode):
            qarg = async_queue
        else:
            raise TypeError(f"Invalid async_queue value, expected DataNode, "
                            f"integer or bool, got : {async_queue}")
        return qarg

    @staticmethod
    def _create_clause(
            async_queue: Union[bool, int, DataNode]
    ) -> Union[ACCAsyncQueueClause, None]:
        '''
        Utility to create a new ACCAsyncQueueClause for the specified queue.

        :param async_queue: the queue value to use for the async clause (or
            True to enable on default queue or False to disable).

        :returns: a new ACCAsyncQueueClause if async is enabled and None
                  otherwise.

        :raises TypeError: if `async_queue` is of the wrong type

        '''
        if async_queue is False:
            # There's no async clause.
            return None
        # Convert async_queue value to PSyIR if necessary and
        # add as child of clause.
        qarg = ACCAsyncMixin.convert_queue(async_queue)
        clause = ACCAsyncQueueClause()
        if qarg and qarg is not True:
            # A specific queue is supplied.
            clause.addchild(qarg)
        # No queue is specified
        return clause

    @property
    def async_clause(self) -> Union[ACCAsyncQueueClause, None]:
        '''
        :returns: the queue clause associated with this node or None.
        '''
        for child in self.clauses:
            if isinstance(child, ACCAsyncQueueClause):
                return child
        return None

    @property
    def async_queue(self) -> Union[bool, int, DataNode]:
        '''
        :returns: whether or not async is enabled and if so, which queue this
                  node is associated with. (True indicates the default stream.)
                  Can use False to disable, True to enable on default stream.
                  Int to attach to the given stream ID or use a PSyIR
                  expression to say at runtime what stream to be used.
        '''
        clause = self.async_clause
        if clause:
            if clause.queue is None:
                # async is enabled on the default stream.
                return True
            return clause.queue
        # No clause => async is not enabled.
        return False

    @async_queue.setter
    def async_queue(self, async_queue: Union[bool, int, DataNode]):
        '''
        Set the asynchronous behaviour associated with this node.

        :param async_queue: Enable async support and attach it to the given
                            queue. Can use False to disable, True to enable on
                            default stream. Int to attach to the given stream
                            ID or use a PSyIR expression to say at runtime
                            which stream to be used.
        '''
        # `clause` will be None if async support is disabled.
        clause = ACCAsyncMixin._create_clause(async_queue)
        existing = self.async_clause
        if existing:
            # This node already had an ACCAsyncQueueClause so we have to either
            # replace or remove it.
            if clause:
                existing.replace_with(clause)
            else:
                existing.detach()
        else:
            if clause:
                # No existing clause but async support is now enabled so add
                # the new clause.
                self.addchild(clause)

    def __eq__(self, other) -> bool:
        '''
        Checks whether two nodes are equal. Two ACCAsyncMixin are
        equal if their async_queue members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        '''
        if type(self) is not type(other):
            return False
        return self.async_queue == other.async_queue
