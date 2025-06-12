# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
from typing import List, Union

from psyclone.core import VariablesAccessMap
from psyclone.psyir.nodes import (
        Directive, Loop, Node, Schedule, Statement, WhileLoop
)


class AsyncTransMixin(metaclass=abc.ABCMeta):
    '''Abstract class containing the base methods and implementation to
    support asynchronous additions to parallel or region transformations, e.g.
    OpenMP's nowait clause.'''

    def _add_asynchronicity(self, instance: Directive):
        ''' Function to enable child classes to handle adding asynchronicity
        (e.g. nowait or dynamic queue choices) as part of the transformation.

        This must implemented by the child class.

        :param instance: The Directive to execute asynchronously.
        '''
        # TODO #11: If the base function is called then we should log that
        # the user asked asynchronicity to be added to a transformation that
        # doesn't support it.

    def _find_next_dependency(self, nodes: Union[Loop, List[Node]],
                              directive: Directive) -> Union[Statement, bool]:
        '''
        Finds the closest dependency to the loop or set of nodes supplied.
        If the supplied input is contained inside a Loop, then this dependency
        can be before the input in the tree.

        If there is no dependency, this function will return True.
        If the next dependency is itself, the function will return False.

        Otherwise the next dependency node will be returned. The returned
        value will always be the statement containing the dependency, as
        directives will occur the dependency, and this avoids needing special
        cases to handle while and if conditions.

        :param nodes: The Loop or list of nodes to find the next dependency
                      for.
        :param directive: The directive containing nodes that may become
                          asynchronous.

        :returns: The next dependency of nodes. This will either be a Node if
                  a valid dependency is found, False if a unsatisfiable
                  dependency is found, or True if there is no dependency.

        '''
        if isinstance(nodes, (Loop, WhileLoop)):
            var_accesses = nodes.loop_body.reference_accesses()
        else:
            var_accesses = VariablesAccessMap()
            for node in nodes:
                var_accesses.update(node.reference_accesses())
        writes = []
        for signature in var_accesses.all_signatures:
            if var_accesses.is_written(signature):
                writes.append(signature)

        if isinstance(nodes, (Loop, WhileLoop)):
            loop_position = nodes.abs_position
        else:
            loop_position = directive.abs_position
        closest = None
        closest_position = None
        private, firstprivate, need_sync = \
            directive.infer_sharing_attributes()
        # Now we have all the writes we want to find the closest
        # forward dependency.
        for signature in writes:
            accesses = var_accesses[signature].all_accesses
            sym_name = signature.var_name
            last_access = accesses[-1].node
            sym = last_access.scope.symbol_table.lookup(sym_name)
            # If the symbol is private or firstprivate then we can
            # ignore it.
            if sym in private or sym in firstprivate:
                continue
            # TODO: #2982 next_accesses ability to look at Structures is
            # limited, and returns the next access(es) to any structure member
            # and not necessarily to the member of interest which limits
            # the behaviour of this.
            next_accesses = last_access.next_accesses()
            # next_accesses always appear in the order of
            # nodes before loop followed by nodes after loop.
            for access in next_accesses:
                # If it's inside the directive then we should skip it.
                if access.is_descendent_of(directive):
                    continue
                # Otherwise find the abs_position
                abs_position = access.abs_position
                if not closest:
                    closest = access
                    closest_position = abs_position
                else:
                    # If it's closer then it's the closest.
                    # Closest here is complex since if we have a parent loop
                    # we need to consider that.
                    if closest_position < loop_position:
                        # If the closest is before the loop we're inside an
                        # ancestor loop. In this case another node is only
                        # closer if it's :
                        # 1. after the input nodes but also inside the same
                        # ancestor, e.g.:
                        #  do i = 1, n
                        #    arr(x) = i <--- initial closest
                        #    nodes <--- the node or nodes input.
                        #    arr(x) = arr(x) * i <-- new closest, inside same
                        #    ancestor node as the current closest but after
                        #    the input nodes.
                        # 2. inside a loop with lower depth than the previous
                        # closest that is also an ancestor of node and before
                        # loop and after closest, e.g.:
                        #  do i = 1, n
                        #    arr(x) = i <-- initial closest
                        #    do j = 1, k
                        #      arr(j) = arr(j) + j <-- new closest, lower
                        #      depth ancestor of input node(s).
                        #      nodes <-- the node or nodes input
                        # Find the loop ancestor of access that is an ancestor
                        # of the input nodes
                        # N.B. accesses will always appear in abs_position
                        # order so we don't need to consider cases where
                        # abs_position < closest_position since it cannot
                        # happen.
                        anc_loop = access.ancestor((Loop, WhileLoop),
                                                   shared_with=directive)
                        # Find the loop ancestor of closest that is an ancestor
                        # of the directive.
                        close_loop = closest.ancestor(
                            (Loop, WhileLoop), shared_with=directive
                        )
                        # If access and closest are in the same ancestor loop
                        # of directive, then the later node in the tree is
                        # closest.
                        if (abs_position > loop_position and
                                anc_loop is close_loop):
                            closest = access
                            closest_position = abs_position
                            continue
                        # Otherwise if the ancestor loop of the access is
                        # deeper in the tree, and the current closest is
                        # before nodes, then the deeper access is the closest.
                        if (abs_position < loop_position and
                                abs_position > closest_position and anc_loop
                                and anc_loop.depth > close_loop.depth):
                            closest = access
                            closest_position = abs_position
                    # Otherwise if the closest is after the input nodes, then
                    # whichever access is closer is the closest dependency.
                    elif (abs_position < closest_position and
                          abs_position > loop_position):
                        closest = access
                        closest_position = abs_position

        # If this directive is contained inside a loop the closest foward
        # dependency might be itself. So if closest is not within the ancestor
        # loop of node then we can't do nowait, so return False.
        node_ancestor = directive.ancestor((Loop, WhileLoop))
        if node_ancestor:
            # If we didn't find a closest and we have an ancestor Loop, then
            # the loop's next dependency is itself.
            if not closest:
                return False
            # If we have an ancestor loop and the found dependency is not
            # contained within the ancestor loop, then the next dependency
            # is itself.
            if not closest.is_descendent_of(node_ancestor):
                return False
        if not closest:
            return True
        closest = closest.ancestor(Statement, include_self=True)

        # Closest can still be an IntrinsicCall or function call inside a
        # condition, e.g. while loop condition or loop bound etc.
        # We need to find this and return the statement. The easiest way
        # to do this is check that closest.parent is a Schedule.
        while not isinstance(closest.parent, Schedule):
            closest = closest.ancestor(Statement)
        # Now we need to check that closest isn't an ancestor of the
        # directive, since this means that the next dependency is the
        # condition of a while loop, which means we can't satisfy the
        # dependency
        if directive.is_descendent_of(closest):
            return False
        return closest
