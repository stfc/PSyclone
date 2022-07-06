# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

'''
This module contains the InlineTrans transformation.

'''

from psyclone.errors import InternalError
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Call, Routine, Reference, CodeBlock, Return, Literal)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class InlineTrans(Transformation):
    '''
    This transformation takes a Call and replaces it with the body of the
    target routine.

    '''
    def apply(self, node, options=None):
        '''
        Inlines the body of the routine that is the target of the supplied
        call.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        '''
        self.validate(node, options)

        # The table we will copy symbols into.
        table = node.scope.symbol_table
        # Find the routine to be inlined.
        orig_routine = self._find_routine(node)

        if not orig_routine.children:
            # Called routine is empty so just remove the call.
            node.detach()
            return

        if isinstance(orig_routine.children[0], Return):
            # The first statement in the routine is a Return so we can
            # just remove the call.
            node.detach()
            return

        # Ensure we don't modify the original Routine by working with a
        # copy of it.
        routine = orig_routine.copy()
        dummy_args = routine.symbol_table.argument_list

        # Copy each Symbol from the Routine into the symbol table associated
        # with the call site.
        for sym in routine.symbol_table.symbols:
            if sym in dummy_args:
                continue
            try:
                table.add(sym)
            except KeyError:
                # A Symbol with the same name already exists so we rename the
                # one that we are adding.
                new_name = table.next_available_name(
                    sym.name, other_table=routine.symbol_table)
                routine.symbol_table.rename_symbol(sym, new_name)
                table.add(sym)

        # Construct lists of the nodes that will be inserted and all of the
        # References that they contain.
        new_stmts = []
        refs = []
        for child in routine.children:
            new_stmts.append(child.copy())
            refs.extend(new_stmts[-1].walk(Reference))

        if isinstance(new_stmts[-1], Return):
            # If the final statement of the routine is a return then
            # remove it from the list.
            del new_stmts[-1]

        # Update any references to dummy arguments so that they refer to the
        # actual arguments instead.
        for ref in refs:
            if ref.symbol in dummy_args:
                ref.replace_with(
                    node.children[dummy_args.index(ref.symbol)].copy())

        # Copy the nodes from the Routine into the call site.
        parent = node.parent
        idx = node.position

        node.replace_with(new_stmts[0])
        for child in new_stmts[1:]:
            idx += 1
            parent.addchild(child, idx)

    def validate(self, node, options=None):
        '''
        Checks that the supplied node is a valid target for inlining.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        :raises TransformationError: if the supplied node is not a Call.
        :raises TransformationError: if the routine body contains a return \
            that is not the first or last statement.
        :raises TransformationError: if the routine body contains a CodeBlock.

        '''
        super().validate(node, options=options)

        # The node should be a Call.
        if not isinstance(node, Call):
            raise TransformationError(
                f"The target of the InlineTrans transformation "
                f"should be a Call but found '{type(node).__name__}'.")

        name = node.routine.name

        # Check that we can find the source of the routine being inlined.
        routine = self._find_routine(node)

        if not routine.children:
            # An empty routine is fine.
            return

        if routine.children and isinstance(routine.children[0], Return):
            # The first statement in the routine is a Return so we don't
            # care what else it may contain.
            return

        #if not all(isinstance(child, (Reference, Literal)) for child in
        #           node.children):
        #    raise TransformationError(
        #        f"One or more of the arguments in the call to '{name}' is "
        #        f"not a simple Reference or Literal. Cannot inline such "
        #        f"a call.")

        return_stmts = routine.walk(Return)
        if return_stmts:
            if len(return_stmts) > 1 or not isinstance(routine.children[-1],
                                                       Return):
                # Either there is more than one Return statement or there is
                # just one but it isn't the last statement of the Routine.
                raise TransformationError(
                    f"Routine '{name}' contains one or more "
                    f"Return statements and therefore cannot be inlined.")

        if routine.walk(CodeBlock):
            raise TransformationError(
                f"Routine '{name}' contains one or more "
                f"CodeBlocks and therefore cannot be inlined.")

    def _find_routine(self, call_node):
        '''
        Searches for the definition of the routine that is being called by
        the supplied Call.

        Currently only supports routines that are present in the
        same source file - TODO #924.

        :returns: the PSyIR for the target routine.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine`

        :raises TransformationError: if the RoutineSymbol is not local.
        :raises InternalError: if the routine symbol is local but the \
            definition cannot be found.
        '''
        name = call_node.routine.name
        routine_sym = call_node.scope.symbol_table.lookup(name)
        if not routine_sym.is_local:
            raise TransformationError(
                f"Routine '{name}' is imported and therefore cannot currently "
                f"be inlined - TODO #924.")
        table = routine_sym.find_symbol_table(call_node)
        for routine in table.node.walk(Routine):
            if routine.name == name:
                return routine
        else:
            raise InternalError(
                f"Failed to find the source for routine '{name}' and "
                f"therefore cannot inline it.")
