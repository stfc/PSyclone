# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk STFC Daresbury Lab
# -----------------------------------------------------------------------------
'''This module provides the BlockLoopTrans, which transforms a Loop into a
BlockedLoop'''

from psyclone.core import VariablesAccessInfo, Signature, AccessType
from psyclone.psyir import nodes
from psyclone.psyir.nodes import Assignment, BinaryOperation, Reference, \
        BlockedLoop, Literal
from psyclone.psyir.symbols import DataSymbol
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
        TransformationError


class BlockLoopTrans(LoopTrans):
    '''
    Apply a blocking transformationt to a loop (in order to permit a
    chunked parallelisation). For example:

    TODO
    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyir.transformations import BlockLoopTrans
    >>> bltrans = BlockLoopTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>> for child in schedule.children:
    >>>     bltrans.apply(child)
    >>> schedule.view()
    '''
    def __str__(self):
        return "Split a loop into a blocked loop pair"

    def validate(self, node, options=None):
        '''
        Validates that the Loop represented by :py:obj:`node` can have
        a BlockLoopTrans applied.

        :param node: the loop to validate.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformation.
        :type options: dictionary of string:values or None
        :param int options["blocksize"]: The size to block over for this \
                transformation. If not specified, the value 32 is used.

        :raises TransformationError: if the supplied Loop has a step size \
                which is not a constant value.
        :raises TransformationError: if the supplied Loop has a step size \
                larger than the chosen block size.
        :raises TransformationError: if the supplied Loop has an ancestor \
                blocked loop.
        :raises TransformationError: if the supplied Loop has a step size \
                of 0.
        :raises TransformationError: if the supplied Loop writes to Loop \
                variables inside the Loop body.
        '''
        super(BlockLoopTrans, self).validate(node, options=options)
        # If step is a variable don't support it for now.
        if options is None:
            options = {}
        if isinstance(node.children[2], nodes.Reference):
            raise TransformationError("Cannot apply a BlockLoopTrans to "
                                      "a loop with a non-constant step size")
        block_size = options.get("blocksize", 32)
        if abs(int(node.children[2].value)) > block_size:
            raise TransformationError("Cannot apply a BlockLoopTrans to "
                                      "a loop with larger step size than the "
                                      "chosen block size")
        if node.ancestor(BlockedLoop) is not None:
            raise TransformationError("Cannot apply a BlockLoopTrans to "
                                      "a loop with a parent BlockedLoop node")

        if int(node.children[2].value) == 0:
            raise TransformationError("Cannot apply a BlockLoopTrans to "
                                      "a loop with a step size of 0")
        # Other checks needed for validation
        # Dependency analysis, following rules:
        # No child has a write dependency to the loop variable.
        # Find variable access info for the loop variable and step
        refs = VariablesAccessInfo(node.children[0])
        node_vars = VariablesAccessInfo()
        if refs is not None:
            node_vars.merge(refs)
        refs = VariablesAccessInfo(node.children[1])
        if refs is not None:
            node_vars.merge(refs)
        # The current implementation of BlockedLoopTrans does not allow
        # the step size to be non-constant, so it is ignored.

        # Add the access pattern to the node variable name
        node_vars.add_access(Signature(node.variable.name),
                             AccessType.WRITE, self)
        node_vars.add_access(Signature(node.variable.name),
                             AccessType.READ, self)

        node_sigs = node_vars.all_signatures

        # Find the Loop code's signatures
        child_refs = VariablesAccessInfo(node.children[3])
        child_sigs = child_refs.all_signatures

        for ref1 in node_sigs:
            if ref1 not in child_sigs:
                continue
            access2 = child_refs[ref1]

            # If access2 is a write then we write to a loop variable
            if access2.is_written():
                raise TransformationError("Cannot apply a BlockedLoopTrans "
                                          "to a loop where loop variables are "
                                          "written to inside the loop body.")

    def apply(self, node, options=None):
        '''
        Converts the Loop represented by :py:obj:`node` into a
        nested loop where the outer loop is over blocks and the inner
        loop is over each block.

        :param node: the loop to transform.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param int options["blocksize"]: The size to block over for this \
                transformation. If not specified, the value 32 is used.

        :returns: Tuple of None and None
        :rtype: (None, None)
        '''

        self.validate(node, options)
        if options is None:
            options = {}
        block_size = options.get("blocksize", 32)
        # Create (or find) the symbols we need for the blocking transformation
        routine = node.ancestor(nodes.Routine)
        end_inner_loop = routine.symbol_table.symbol_from_tag(
                "el_inner", symbol_type=DataSymbol,
                datatype=node.variable.datatype)
        outer_loop_variable = routine.symbol_table.symbol_from_tag(
                "out_var", symbol_type=DataSymbol,
                datatype=node.variable.datatype)
        # We currently don't allow BlockedLoops to be ancestors of BlockedLoop
        # so our ancestors cannot use these variables.

        # Store the node's parent for replacing later and the start and end
        # indicies
        child0 = node.children[0]
        child1 = node.children[1]

        # For positive steps we do el_inner = min(out_var+block_size, el_outer)
        # For negative steps we do el_inner = max(out_var-block_size, el_outer)
        if int(node.children[2].value) > 0:
            add = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                         Reference(outer_loop_variable),
                                         Literal("{0}".format(block_size),
                                                 node.variable.datatype))
            minop = BinaryOperation.create(BinaryOperation.Operator.MIN, add,
                                           child1.copy())
            inner_loop_end = Assignment.create(Reference(end_inner_loop),
                                               minop)
        elif int(node.children[2].value) < 0:
            sub = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                         Reference(outer_loop_variable),
                                         Literal("{0}".format(block_size),
                                                 node.variable.datatype))
            maxop = BinaryOperation.create(BinaryOperation.Operator.MAX, sub,
                                           child1.copy())
            inner_loop_end = Assignment.create(Reference(end_inner_loop),
                                               maxop)
            # block_size needs to be negative if we're reducing
            block_size = -block_size
        # step size of 0 is caught by the validate call

        # Replace the inner loop start and end with the blocking ones
        child0.replace_with(Reference(outer_loop_variable))
        child1.replace_with(Reference(end_inner_loop))

        # Create the outerloop
        outerloop = BlockedLoop.create(outer_loop_variable, child0, child1,
                                       Literal("{0}".format(block_size),
                                               outer_loop_variable.datatype),
                                       [inner_loop_end])
        # Replace this loop with the outerloop
        node.replace_with(outerloop)
        # Add the loop to the innerloop's schedule
        outerloop.children[3].addchild(node)

        return None, None
