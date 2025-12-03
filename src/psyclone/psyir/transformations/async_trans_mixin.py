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

from psyclone.core import VariablesAccessMap, AccessType
from psyclone.psyir.nodes import (
        Directive, IfBlock, Loop, Node, Reference, Schedule, Statement,
        WhileLoop,
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
                              directive: Directive) -> Union[List[Statement],
                                                             bool]:
        '''
        Finds the statement with the closest following dependency of any symbol
        in the supplied nodes. It can be multiple locations due to branching,
        and be before the supplied nodes if they are contained inside a
        looping construct.

        If there is no dependency, this function will return True.
        If the next dependency is itself, the function will return False.

        :param nodes: The Loop or list of nodes to find the next dependency
                      for.
        :param directive: The directive containing nodes that may become
                          asynchronous.

        :returns: The next dependency of nodes. This will either be a List of
                  Nodes if a valid dependency is found, False if a
                  unsatisfiable dependency is found, or True if there is no
                  dependency.

        '''
        if isinstance(nodes, (Loop, WhileLoop)):
            var_accesses = nodes.reference_accesses()
        else:
            var_accesses = VariablesAccessMap()
            for node in nodes:
                var_accesses.update(node.reference_accesses())
        writes = []
        reads = []
        for signature in var_accesses.all_signatures:
            if var_accesses.is_written(signature):
                writes.append(signature)
            elif var_accesses.is_read(signature):
                reads.append(signature)

        closest = None
        closest_position = None
        private, firstprivate, need_sync = \
            directive.infer_sharing_attributes()

        dependencies = []
        # For each of the reads and writes we want to find the closest
        # forward dependency.
        for signature in reads+writes:
            accesses = var_accesses[signature]
            sym_name = signature.var_name
            # TODO #3060: If any of the accesses are CONSTANT then this
            # is a kind parameter, which can currently sometimes appear as
            # a READ. If the only access to a kind parameter is detected as
            # a READ, then we won't skip it.
            if any([x.access_type == AccessType.CONSTANT for x in accesses]):
                continue
            last_access = accesses[-1].node
            sym = last_access.scope.symbol_table.lookup(sym_name)
            # If the symbol is private or firstprivate then we can
            # ignore it.
            if sym in private or sym in firstprivate:
                continue
            # If the symbol is a loop variable of an ancestor loop then we can
            # ignore it, as loop variables are only modifiable by the loop
            # and we already account for loop/self dependencies.
            if isinstance(nodes, list):
                anc_loop = nodes[0].ancestor(Loop)
            else:
                anc_loop = nodes.ancestor(Loop)
            ancestor_var = False
            while anc_loop:
                if sym_name == anc_loop.variable.name:
                    ancestor_var = True
                    break
                anc_loop = anc_loop.ancestor(Loop)
            if ancestor_var:
                continue
            # TODO: #2982 next_accesses ability to look at Structures is
            # limited, and returns the next access(es) to any structure member
            # and not necessarily to the member of interest which limits
            # the behaviour of this.
            next_accesses = last_access.next_accesses()
            # next_accesses always appear in the order of
            # nodes before loop followed by nodes after loop.
            for access in next_accesses:
                # If they're both reads then we should skip it.
                if (signature in reads and isinstance(access, Reference)
                        and access.is_read):
                    continue
                # If it's inside the directive then we should skip it.
                if access.is_descendant_of(directive):
                    continue
                # Otherwise find the abs_position
                abs_position = access.abs_position
                # If the access occurs before, then there will be a dependency
                # between them and this doesn't prevent any dependency after
                # them.
                if access.abs_position < directive.abs_position:
                    dependencies.append(access)
                    continue
                if not closest:
                    closest = access
                    closest_position = abs_position
                    continue
                # If the access is in the same IfBlock as closest, but one
                # is in if_body and the other is in else_body we
                # add the entire IfBlock to dependencies.
                # TODO #2551: This is a simple solution that avoids needing
                # to compute the closest dependency for both sections of
                # an ifblock, which can get very complex.
                shared_if_anc = access.ancestor(IfBlock, shared_with=closest)
                if shared_if_anc and shared_if_anc.else_body:
                    if ((access.is_descendant_of(shared_if_anc.if_body) and
                         closest.is_descendant_of(shared_if_anc.else_body)) or
                        (access.is_descendant_of(shared_if_anc.else_body) and
                         closest.is_descendant_of(shared_if_anc.if_body))):
                        dependencies.append(shared_if_anc)
                # Otherwise if the closest is after the input nodes, then
                # whichever access is closer is the closest dependency.
                if (abs_position < closest_position):
                    closest = access
                    closest_position = abs_position
                    continue
        # If this directive is contained inside a loop the closest foward
        # dependency might be itself. So if closest is not within the ancestor
        # loop of node then we can't do nowait, so return False.
        node_ancestor = directive.ancestor((Loop, WhileLoop))
        if node_ancestor:
            # If we didn't find a closest and we have an ancestor Loop, then
            # the loop's next dependency may be itself.
            if not closest:
                # If none of the accesses in dependencies are a child of
                # the ancestor loop then the dependencies is itself
                for access in dependencies:
                    if access.is_descendant_of(node_ancestor):
                        break
                else:
                    return False
            else:
                # If we have an ancestor loop and the found dependency is not
                # contained within the ancestor loop, then the next dependency
                # is itself unless we have a prior dependency inside the
                # ancestor loop.
                if not closest.is_descendant_of(node_ancestor):
                    for access in dependencies:
                        if access.is_descendant_of(node_ancestor):
                            break
                    else:
                        return False
        if not closest and len(dependencies) == 0:
            return True

        if closest:
            # If there is a dependency after the directive, we can remove any
            # dependencies from before the directive that are contained in the
            # same Loop/WhileLoop AND IfBlock (if there is one) as the
            # dependency after the directive.
            # We loop over in reverse as we are removing elements from the
            # dependencies array.
            for i, depend in reversed(list(enumerate(dependencies[:]))):
                # Check that they are contained within the same body of
                # the same IfBlock if the dependency after the directive is.
                if_anc = closest.ancestor(IfBlock)
                if if_anc:
                    # If they're not both in the same part of the IfBlock
                    # ancestor, then we need to keep both.
                    if not ((closest.is_descendant_of(if_anc.if_body) and
                             depend.is_descendant_of(if_anc.if_body)) or
                            (if_anc.else_body and
                             closest.is_descendant_of(if_anc.else_body) and
                             depend.is_descendant_of(if_anc.else_body))):
                        continue
                # Check that they share the Loop ancestor of closest that is
                # the first loop ancestor of the directive.
                remove = False
                loop_anc = closest.ancestor((Loop, WhileLoop))
                while loop_anc:
                    if directive.is_descendant_of(loop_anc):
                        if depend.is_descendant_of(loop_anc):
                            remove = True
                            break
                    loop_anc = loop_anc.ancestor((Loop, WhileLoop))
                if remove:
                    del dependencies[i]
            dependencies.append(closest)

        final_dependencies = []
        # Find the statement ancestor of all the dependencies.
        for closest in dependencies:
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
            if directive.is_descendant_of(closest):
                return False
            # If closest is in the same IfBlock as the input directive, but
            # one is in the if and the other is in the else, add the
            # entire IfBlock as the dependency.
            shared_if_anc = closest.ancestor(IfBlock, shared_with=directive)
            if shared_if_anc and shared_if_anc.else_body:
                if ((directive.is_descendant_of(shared_if_anc.if_body) and
                     closest.is_descendant_of(shared_if_anc.else_body)) or
                    (directive.is_descendant_of(shared_if_anc.else_body)
                     and closest.is_descendant_of(shared_if_anc.if_body))):
                    closest = shared_if_anc
            # Don't add repeats
            for dep in final_dependencies:
                if dep is closest:
                    break
            else:
                final_dependencies.append(closest)
        return final_dependencies
