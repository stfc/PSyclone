# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
# Authors S. Valat, INRIA / LJK
# -----------------------------------------------------------------------------

''' This module contains the mixins to apply some ACC features on many
classes.'''

import abc

from psyclone.psyir.nodes.reference import Reference


class ACCAsyncMixin(metaclass=abc.ABCMeta):
    '''
    Class handling the common code to handle the async keyword on related acc
    directives.

    :param async_queue: Enable async support and attach it to the given queue.
                        Can use False to disable, True to enable on default
                        stream. Use int to attach to the given stream ID or
                        use a variable Reference to say at runtime what stream
                        to be used.
    :type async_queue: bool | int | :py:class:`psyclone.core.Reference`
    '''
    def __init__(self, async_queue=False):
        self.async_queue = async_queue

    @property
    def async_queue(self):
        '''
        :returns: whether or not async is enabled and if so, which queue this
                  node is associated with. (True indicates the default stream.)
                  Can use False to disable, True to enable on default stream.
                  Int to attach to the given stream ID or use a variable
                  Reference to say at runtime what stream to be used.
        :rtype async_queue: bool | int | :py:class:`psyclone.core.Reference`
        '''
        return self._async_queue

    @async_queue.setter
    def async_queue(self, async_queue):
        '''
        :param async_queue: Enable async support and attach it to the given
                            queue. Can use False to disable, True to enable on
                            default stream. Int to attach to the given stream
                            ID or use a variable Reference to say at runtime
                            what stream to be used.
        :type async_queue: bool | int | :py:class:`psyclone.core.Reference`
        :raises TypeError: if `wait_queue` is of the wrong type
        '''
        # check
        if (async_queue is not None and
           not isinstance(async_queue, (bool, Reference, int))):
            raise TypeError(f"Invalid async_queue value, expect Reference or "
                            f"integer or None or bool, got : {async_queue}")

        # assign
        self._async_queue = async_queue

    def _build_async_string(self):
        '''
        Build the async arg to concat to the acc directive when generating the
        code.

        :returns: The `async()` option to add to the directive.
        :rtype: str
        '''
        # default
        result = ""

        # async
        if self.async_queue is not None:
            if isinstance(self.async_queue, bool):
                if self.async_queue:
                    result = " async()"
            elif isinstance(self.async_queue, int):
                result = f" async({self.async_queue})"
            elif isinstance(self.async_queue, Reference):
                from psyclone.psyir.backend.fortran import FortranWriter
                text = FortranWriter()(self.async_queue)
                result = f" async({text})"

        # ok
        return result

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two ACCAsyncMixin are
        equal if their async_queue members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        if type(self) is not type(other):
            return False
        else:
            return self.async_queue == other.async_queue
