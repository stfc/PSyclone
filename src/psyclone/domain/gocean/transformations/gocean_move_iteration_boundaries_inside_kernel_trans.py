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
# Author S. Siso, STFC Daresbury Lab

'''This module contains the GOMoveIterationBoundariesInsideKernelTrans.'''

from psyclone.psyir.transformations import TransformationError
from psyclone.psyGen import Transformation, InvokeSchedule
from psyclone.gocean1p0 import GOKern
from psyclone.psyir.nodes import (BinaryOperation, Reference, Loop,
                                  Assignment, IfBlock, Return)
from psyclone.psyir.symbols import (INTEGER_TYPE, ArgumentInterface,
                                    DataSymbol)


class GOMoveIterationBoundariesInsideKernelTrans(Transformation):
    ''' Provides a transformation that moves iteration boundaries that are
    encoded in the Loops lower_bound() and upper_bound() methods to a mask
    inside the kernel with the boundaries passed as kernel arguments.

    For example the following kernel call:

    .. code-block:: fortran

        do i = 2, N - 1
            do j = 2, N - 1
                kernel(i, j, field)
            end do
        end do

    will be transformed to:

    .. code-block:: fortran

        startx = 2
        stopx = N - 1
        starty = 2
        stopy = N - 1
        do i = 1, size(field, 1)
            do j = 1, size(field, 2)
                kernel(i, j, field, startx, stopx, starty, stopy)
            end do
        end do

    additionally a mask like the following one will be introduced in the
    kernel code:

    .. code-block:: fortran

        if (i < startx .or. i > stopx .or. j < starty .or. j > stopy) then
            return
        end if

    '''
    def __str__(self):
        return "Move kernel iteration boundaries inside the kernel code."

    @property
    def name(self):
        '''Returns the name of this transformation as a string.'''
        return "GOMoveIterationBoundariesInsideKernelTrans"

    def validate(self, node, options=None):
        '''Ensure that it is valid to apply this transformation to the
        supplied node.

        :param node: the node to validate.
        :type node: :py:class:`psyclone.gocean1p0.GOKern`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        :raises TransformationError: if the node is not a GOKern.

        '''
        if not isinstance(node, GOKern):
            raise TransformationError(
                "Error in {0} transformation. This transformation can only be "
                "applied to 'GOKern' nodes, but found '{1}'."
                "".format(self.name, type(node).__name__))

    def apply(self, node, options=None):
        '''Apply this transformation to the supplied node.

        :param node: the node to transform.
        :type node: :py:class:`psyclone.gocean1p0.GOKern`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        '''
        self.validate(node, options)

        # Get useful references
        invoke_st = node.ancestor(InvokeSchedule).symbol_table
        inner_loop = node.ancestor(Loop)
        outer_loop = inner_loop.ancestor(Loop)
        cursor = outer_loop.position

        # Make sure the boundary symbols in the PSylayer exist
        inv_xstart = invoke_st.find_or_create_tag(
            "xstart_" + node.name, root_name="xstart", symbol_type=DataSymbol,
            datatype=INTEGER_TYPE)
        inv_xstop = invoke_st.find_or_create_tag(
            "xstop_" + node.name, root_name="xstop", symbol_type=DataSymbol,
            datatype=INTEGER_TYPE)
        inv_ystart = invoke_st.find_or_create_tag(
            "ystart_" + node.name, root_name="ystart", symbol_type=DataSymbol,
            datatype=INTEGER_TYPE)
        inv_ystop = invoke_st.find_or_create_tag(
            "ystop_" + node.name, root_name="ystop", symbol_type=DataSymbol,
            datatype=INTEGER_TYPE)

        # If the kernel acts on the whole iteration space, the boundary values
        # are not needed. This also avoids adding duplicated arguments if this
        # transformation is applied more than once to the same kernel. But the
        # declaration and initialisation above still needs to exist because the
        # boundary variables are expected to exist by the generation code.
        if (inner_loop.field_space == "go_every" and
                outer_loop.field_space == "go_every" and
                inner_loop.iteration_space == "go_all_pts" and
                outer_loop.iteration_space == "go_all_pts"):
            return node.root, None

        # Initialise the boundary values provided by the Loop construct
        assign1 = Assignment.create(Reference(inv_xstart),
                                    inner_loop.start_expr.copy())
        outer_loop.parent.children.insert(cursor, assign1)
        cursor = cursor + 1
        assign2 = Assignment.create(Reference(inv_xstop),
                                    inner_loop.stop_expr.copy())
        outer_loop.parent.children.insert(cursor, assign2)
        cursor = cursor + 1
        assign3 = Assignment.create(Reference(inv_ystart),
                                    outer_loop.start_expr.copy())
        outer_loop.parent.children.insert(cursor, assign3)
        cursor = cursor + 1
        assign4 = Assignment.create(Reference(inv_ystop),
                                    outer_loop.stop_expr.copy())
        outer_loop.parent.children.insert(cursor, assign4)

        # Update Kernel Call argument list
        for symbol in [inv_xstart, inv_xstop, inv_ystart, inv_ystop]:
            node.arguments.append(symbol.name, "go_i_scalar")

        # Now that the boundaries are inside the kernel, the looping should go
        # through all the field points
        inner_loop.field_space = "go_every"
        outer_loop.field_space = "go_every"
        inner_loop.iteration_space = "go_all_pts"
        outer_loop.iteration_space = "go_all_pts"

        # Update Kernel
        kschedule = node.get_kernel_schedule()
        kernel_st = kschedule.symbol_table
        iteration_indices = kernel_st.iteration_indices
        data_arguments = kernel_st.data_arguments

        # Create new symbols and insert them as kernel arguments at the end of
        # the kernel argument list
        xstart_symbol = kernel_st.new_symbol(
            "xstart", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        xstop_symbol = kernel_st.new_symbol(
            "xstop", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        ystart_symbol = kernel_st.new_symbol(
            "ystart", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        ystop_symbol = kernel_st.new_symbol(
            "ystop", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        kernel_st.specify_argument_list(
            iteration_indices + data_arguments +
            [xstart_symbol, xstop_symbol, ystart_symbol, ystop_symbol])

        # Create boundary masking conditions
        condition1 = BinaryOperation.create(
            BinaryOperation.Operator.LT,
            Reference(iteration_indices[0]),
            Reference(xstart_symbol))
        condition2 = BinaryOperation.create(
            BinaryOperation.Operator.GT,
            Reference(iteration_indices[0]),
            Reference(xstop_symbol))
        condition3 = BinaryOperation.create(
            BinaryOperation.Operator.LT,
            Reference(iteration_indices[1]),
            Reference(ystart_symbol))
        condition4 = BinaryOperation.create(
            BinaryOperation.Operator.GT,
            Reference(iteration_indices[1]),
            Reference(ystop_symbol))

        condition = BinaryOperation.create(
            BinaryOperation.Operator.OR,
            BinaryOperation.create(
                BinaryOperation.Operator.OR,
                condition1,
                condition2),
            BinaryOperation.create(
                BinaryOperation.Operator.OR,
                condition3,
                condition4)
            )

        # Insert the conditional mask as the first statement of the kernel
        if_statement = IfBlock.create(condition, [Return()])
        kschedule.children.insert(0, if_statement)


# For Sphinx AutoAPI documentation generation
__all__ = ['GOMoveIterationBoundariesInsideKernelTrans']
