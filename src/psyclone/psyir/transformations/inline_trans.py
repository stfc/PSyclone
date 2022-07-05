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

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Call, Routine, Reference
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class InlineTrans(Transformation):
    '''
    This transformation takes a Call and replaces it with the body of the
    target routine.

    '''
    def apply(self, node, options=None):
        '''

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        '''
        self.validate(node, options)

        # The table we will copy symbols into.
        table = node.scope.symbol_table
        name = node.routine.name
        root = node.root
        for routine in root.walk(Routine):
            if routine.name == name:
                break
        else:
            raise TransformationError("This should probably be in validate()")

        if not routine.children:
            # Called routine is empty so just remove the call.
            node.detach()
            return

        dummy_args = routine.symbol_table.argument_list

        new_stmts = []
        refs = []
        for child in routine.children:
            new_stmts.append(child.copy())
            refs.extend(new_stmts[-1].walk(Reference))

        # Copy each Symbol from the Routine into the symbol table associated
        # with the call site.
        for sym in routine.symbol_table.symbols:
            if sym not in dummy_args:
                table.add(sym)

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

    @staticmethod
    def map_dummy_arg_refs(node, dummy_args, actual_args):
        new_node = node.copy()
        for ref in new_node.walk(Reference):
            if ref.symbol in dummy_args:
                ref.replace_with(
                    actual_args[dummy_args.index(ref.symbol)].copy())
        return new_node

    def validate(self, node, options=None):
        '''
        Checks that the supplied node is a valid target for inlining.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        :raises TransformationError: if the supplied node is not a Call.

        '''
        super().validate(node, options=options)

        # The node should be a Call.
        if not isinstance(node, Call):
            raise TransformationError(
                f"The target of the InlineTrans transformation "
                f"should be a Call but found '{type(node).__name__}'.")
