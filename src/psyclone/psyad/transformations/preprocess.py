# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Module containing a PSyAD kernel transformation script that applies
any required tranformations to the tangent linear PSyIR before it is
translated to adjoint PSyIR.

'''
from psyclone.core import SymbolicMaths
from psyclone.psyad.utils import node_is_active, node_is_passive
from psyclone.psyir.nodes import BinaryOperation, Assignment, Range, \
    Reference
from psyclone.psyir.transformations import DotProduct2CodeTrans, \
    Matmul2CodeTrans, ArrayRange2LoopTrans, TransformationError, \
    Reference2ArrayRangeTrans


def preprocess_trans(kernel_psyir, active_variable_names):
    '''PSyclone kernel transformation script which modifies the supplied
    kernel psyir by replacing array-ranges with explicit loops,
    dotproduct and matmul intrinsics with equivalent code and
    performing symbolic expansion (e.g. x(a+b) => x*a+x*b). This is
    called internally by the PSyAD script before transforming the code
    to its adjoint form.

    :param kernel_psyir: PSyIR representation of the tangent linear \
        kernel code.
    :type kernel_psyir: :py:class:`psyclone.psyir.nodes.Node`
    :param active_variable_names: list of active variable names.
    :type active_variable_names: list of str

    '''
    # perform any inlining
    from psyclone.psyir.nodes import Call, ArrayReference, Routine
    from psyclone.psyir.symbols import DataSymbol
    from psyclone.psyir.frontend.fortran import FortranReader
    from psyclone.psyir.backend.fortran import FortranWriter
    from psyclone.psyir.transformations import InlineTrans

    # find the names of active calls
    inline_calls = []
    for call in kernel_psyir.walk(Call):
        print(f"found routine '{call.routine.name}'.")
        if node_is_passive(call, active_variable_names):
            print(f"No need to inline '{call.routine.name}' as it is passive.")
        else:
            print("This is active")
            inline_calls.append(call.routine.name)

    # Functions will soon be captured as calls but they are not yet so
    # this is a hack ...
    func_node = None
    for array_ref in kernel_psyir.walk(ArrayReference):
        if not isinstance(array_ref.symbol, DataSymbol):
            print(f"found function '{array_ref.name}'.")
            if node_is_passive(array_ref, active_variable_names):
                print("No need to modify this as it is passive.")
            else:
                inline_calls.append(array_ref.name)
                print("This is active")

    # Find the file where this call exists
    lookup = {"tl_calc_exner_pointwise": "/home/rupert/proj/lfric_trunk/linear/source/kernel/support/tl_calc_exner_pointwise_mod.F90",
              "coordinate_jacobian": "/home/rupert/proj/lfric_trunk/components/science/source/kernel/support/coordinate_jacobian_mod.F90"}

    for call_name in inline_calls:

        try:
            filepath = lookup[call_name]
        except KeyError:
            print(f"No file found for call '{call_name}'")
            exit(1)

        reader = FortranReader()
        writer = FortranWriter()
        try:
            psyir = reader.psyir_from_file(filepath)
        except FileNotFoundError:
            print(f"File '{filepath}' not found.")
            exit(1)

        # Need to deal wih iterfaces
            
        # Find the routine in the PSyIR
        for routine in psyir.walk(Routine):
            if routine.name == call_name:
                routine_psyir = routine.detach()
                break
        else:
            print(f"{call_name} not found in '{filepath}'")
            exit(1)

        print (f"Got PSyIR of '{call_name}'.")
        #print(psyir.view())
        #print(writer(routine_psyir))
        #exit(1)
        
        # Add this to our PSyIR
        #kernel_psyir.children.append(psyir.children[0].detach())
        # Need to change symbol type of call so it is no longer imported,
        # otherwise the inline transformation complains
        #print(kernel_psyir.view())

        # use inlining transformation - note, not yet working with functions
        # note, precision may need to be specified as routine may have an interface :-(
        #inline_trans = InlineTrans()
        #for call in inline_calls:
        #inline_trans.apply(call)

        #exit(1)

    exit(1)
    dot_product_trans = DotProduct2CodeTrans()
    matmul_trans = Matmul2CodeTrans()
    arrayrange2loop_trans = ArrayRange2LoopTrans()
    reference2arrayrange_trans = Reference2ArrayRangeTrans()

    # Replace references to arrays (array notation) with array-ranges
    for reference in kernel_psyir.walk(Reference):
        try:
            reference2arrayrange_trans.apply(reference)
        except TransformationError:
            pass

    # Replace array-ranges with explicit loops
    for assignment in kernel_psyir.walk(Assignment):
        if node_is_passive(assignment, active_variable_names):
            # No need to modify passive assignments
            continue
        # Repeatedly apply the transformation until there are no more
        # array ranges in this assignment.
        while True:
            try:
                arrayrange2loop_trans.apply(assignment)
            except TransformationError:
                break

    for oper in kernel_psyir.walk(BinaryOperation):
        if oper.operator == BinaryOperation.Operator.DOT_PRODUCT:
            # Apply DOT_PRODUCT transformation
            dot_product_trans.apply(oper)
        elif oper.operator == BinaryOperation.Operator.MATMUL:
            # Apply MATMUL transformation
            matmul_trans.apply(oper)

    # Deal with any associativity issues here as AssignmentTrans
    # is not able to.
    for assignment in kernel_psyir.walk(Assignment):
        if assignment.walk(Range):
            # SymbolicMaths currently does not work if the expression
            # contains Range nodes, see issue #1655.
            associativity(assignment, active_variable_names)
        else:
            sym_maths = SymbolicMaths.get()
            sym_maths.expand(assignment.rhs)


def associativity(assignment, active_variable_names):
    '''Repeatedly look for the patterns x * (a +- b) or (a +- b) */ x on
    the rhs of this assignment where x is an inactive expression and a
    and b are active expressions, replacing these patterns with x*a +-
    x*b and a*/x +- b*/x respectively.

    This function can be removed when support for Range nodes is added
    to the SymbolicMaths expand function, see issue #1655.

    :param assignment: the Assignment Node that we are looking at.
    :type assignment: :py:class:`psyclone.psyir.nodes.Assignment`
    :param active_variable_names: list of active variable names.
    :type active_variable_names: list of str

    '''
    if node_is_active(assignment.rhs, active_variable_names):
        while True:
            for oper in assignment.rhs.walk(BinaryOperation):
                if oper.operator == BinaryOperation.Operator.MUL and \
                       node_is_passive(
                           oper.children[0], active_variable_names) and \
                       isinstance(oper.children[1], BinaryOperation) and \
                       oper.children[1].operator in [
                           BinaryOperation.Operator.ADD,
                           BinaryOperation.Operator.SUB] and \
                       node_is_active(
                           oper.children[1].children[0],
                           active_variable_names) and \
                       node_is_active(
                           oper.children[1].children[1],
                           active_variable_names):
                    # Matched one of the patterns we are looking for
                    # x * (a +- b)
                    inactive = oper.children[0]
                    active0 = oper.children[1].children[0]
                    active1 = oper.children[1].children[1]
                    binary_op = oper.children[1]
                    # Restructure to x*a +- x*b
                    mult0 = BinaryOperation.create(
                        BinaryOperation.Operator.MUL, inactive.detach(),
                        active0.detach())
                    mult1 = BinaryOperation.create(
                        BinaryOperation.Operator.MUL, inactive.copy(),
                        active1.detach())
                    binary_op.children.extend([mult0, mult1])
                    oper.replace_with(binary_op.detach())
                    break

                if oper.operator in [
                        BinaryOperation.Operator.MUL,
                        BinaryOperation.Operator.DIV] and \
                        node_is_passive(
                            oper.children[1], active_variable_names) and \
                        isinstance(oper.children[0], BinaryOperation) and \
                        oper.children[0].operator in [
                            BinaryOperation.Operator.ADD,
                            BinaryOperation.Operator.SUB] and \
                        node_is_active(
                            oper.children[0].children[0],
                            active_variable_names) and \
                        node_is_active(
                            oper.children[0].children[1],
                            active_variable_names):
                    # Matched one of the patterns we are looking
                    # for: (a +- b) */ x
                    inactive = oper.children[1]
                    active0 = oper.children[0].children[0]
                    active1 = oper.children[0].children[1]
                    binary_op = oper.children[0]
                    # Restructure to a */ x +- b */ x
                    op0 = BinaryOperation.create(
                        oper.operator, active0.detach(), inactive.detach())
                    op1 = BinaryOperation.create(
                        oper.operator, active1.detach(), inactive.copy())
                    binary_op.children.extend([op0, op1])
                    oper.replace_with(binary_op.detach())
                    break

            else:
                # No matching pattern so break out of while loop
                break
