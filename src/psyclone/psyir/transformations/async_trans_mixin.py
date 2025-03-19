# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025-2025, Science and Technology Facilities Council.
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
# Author A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------
''' This module contains the AsyncTransMixin.'''

import abc
from typing import Union

from psyclone.core import VariablesAccessInfo
from psyclone.psyir.nodes import (
        Directive, Loop, Node,
)


class AsyncTransMixin(metaclass=abc.ABCMeta):
    '''Abstract class containing the base methods and implementation to
    support asynchronous additions to parallel or region transformations, e.g.
    OpenMP's nowait clause.'''

    def _add_asynchronicity(self, nodes: Union[Loop, list[Node]],
                            instance: Directive):
        ''' Function to enable child classes to handle adding asynchronicity
        (e.g. nowait or dynamic queue choices) as part of the transformation.

        :param node: The Loop
        '''
        pass

    def _find_next_dependency(self, nodes: Union[Loop, list[Node]],
                              instance: Directive) -> Union[Node, bool]:
        ''''''
        if isinstance(nodes, Loop):
            var_accesses = VariablesAccessInfo(nodes=nodes.loop_body)
        else:
            var_accesses = VariablesAccessInfo(nodes=nodes)
        writes = []
        for signature in var_accesses.all_signatures:
            if var_accesses.is_written(signature):
                writes.append(signature)

        if isinstance(nodes, Loop):
            loop_position = nodes.abs_position
        else:
            loop_position = instance.abs_position
        closest = None
        closest_position = None
        # Now we have all the writes we want to find the closest
        # forward dependency.
        for signature in writes:
            accesses = var_accesses[signature].all_accesses
            last_access = accesses[-1].node
            private, firstprivate, need_sync = \
                instance.infer_sharing_attributes()
            sym = last_access.symbol
            # If the symbol is private or firstprivate then we can
            # ignore it.
            if sym in private or sym in firstprivate:
                continue
            next_accesses = last_access.next_accesses()
            # next_accesses always appear in the order of
            # nodes before loop followed by nodes after loop.
            for access in next_accesses:
                # If its inside the directive then we should skip it.
                if access.is_descendent_of(instance):
                    continue
                # Otherwise find the abs_position
                abs_position = access.abs_position
                if not closest:
                    closest = access
                    closest_position = abs_position
                else:
                    # If its closer then its the closest.
                    # Closest here is complex since if we have a parent loop
                    # we need to consider that.
                    if closest_position < loop_position:
                        # If the closest is before the loop we're inside an
                        # ancestor loop. In this case another node is only
                        # closer if its :
                        # 1. before the previous closest and has the same loop
                        # as an ancestor
                        # 2. after the loop but also inside the same ancestor.
                        # 3. inside a loop with lower depth than the previous
                        # closest that is also an ancestor of node and before
                        # loop and after closest.
                        # Find the loop ancestor of access that is an ancestor
                        # of node
                        # N.B. accesses will always appear in abs_position
                        # order so we don't need to consider cases where
                        # abs_position < closest_position since it cannot
                        # happen.
                        anc_loop = access.ancestor(Loop)
                        while anc_loop is not None:
                            if instance.is_descendent_of(anc_loop):
                                break
                            anc_loop = anc_loop.ancestor(Loop)
                        # Find the loop ancestor of closest that is an ancestor
                        # of node.
                        close_loop = closest.ancestor(Loop)
                        while close_loop is not None:
                            if instance.is_descendent_of(close_loop):
                                break
                            close_loop = close_loop.ancestor(Loop)
                        if (abs_position > loop_position and
                                anc_loop is close_loop):
                            closest = access
                            closest_position = abs_position
                            continue
                        if (abs_position < loop_position and
                                abs_position > closest_position and anc_loop
                                and anc_loop.depth > close_loop.depth):
                            closest = access
                            closest_position = abs_position
                    elif (abs_position < closest_position and
                          abs_position > loop_position):
                        closest = access
                        closest_position = abs_position

        # If this directive is contained inside a loop the closest foward
        # dependency might be itself. So if closest is not within the ancestor
        # loop of node then we can't do nowait, so return False.
        node_ancestor = instance.ancestor(Loop)
        if node_ancestor:
            # If we didn't find a closest and we have an ancestor Loop, then
            # the loops next dependency is itself.
            if not closest:
                return False
            if not closest.is_descendent_of(node_ancestor):
                return False
        if not closest:
            return True
        return closest
