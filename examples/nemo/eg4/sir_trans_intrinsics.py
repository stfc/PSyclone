# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab

'''Module providing a transformation script that converts the supplied
PSyIR to the Stencil intermediate representation (SIR), modifying any
PSyIR min, abs and sign intrinsics to PSyIR code beforehand using
transformations, as SIR does not support intrinsics. Translation to
the SIR is limited to the NEMO API. The NEMO API has no algorithm
layer so all of the original code is captured in the invoke
objects. Therefore by translating all of the invoke objects, all of
the original code is translated.

The min, abs and sign transformations are currently maintained within
this script as the NEMO API does not yet support a symbol table and
therefore the transformations are non-standard (requiring a symbol
table object to be provided to each transformation).

'''
from __future__ import print_function
from psyclone.psyir.backend.sir import SIRWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.nemo import NemoKern
from psyclone.psyGen import UnaryOperation, BinaryOperation, NaryOperation, \
    Assignment, Reference, Literal, IfBlock, Schedule
import copy
from psyclone.psyir.symbols import DataType, SymbolTable, DataSymbol


class NemoAbsTrans():
    '''Provides a NEMO-api-specific transformation from an abs operator
    node to equivalent code in the PSyIR of a Schedule. The supplied
    arguments are also checked for validity.

    the translation performed for `R=ABS(X)` is
    `IF (X<0.0) R=X*-1.0 ELSE R=X` 

    '''
    def __str__(self):
        return "Convert the PSyIR abs intrinsic to equivalent PSyIR code"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "NemoAbsTrans"

    def validate(self, node, symbol_table):
        '''Perform various checks to ensure that it is valid to apply
        the NemoAbsTrans transformation to the supplied Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyGen.UnaryOperation`
        :param symbol_table: the symbol table that is being checked.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :raises TransformationError: if the node argument is not a \
            :py:class:`psyclone.psyGen.UnaryOperation` with the ABS \
            operator.
        :raises TransformationError: if the symbol_table argument is not a \
            :py:class:`psyclone.psyir.symbols.SymbolTable`.

        '''
        # Check that the node is a PSyIR abs unary operation
        if not isinstance(node, UnaryOperation) or \
           not node.operator is UnaryOperation.Operator.ABS:
            raise TransformationError(
                "Error in {0} transformation. The supplied node argument is "
                "not an abs operator, found '{1}'."
                "".format(self.name, type(node).__name__))
        # Check that symbol_table is a PSyIR symbol table
        if not isinstance(symbol_table, SymbolTable):
            raise TransformationError(
                "Error in {0} transformation. The supplied symbol_table "
                "argument is not an a SymbolTable, found '{1}'."
                "".format(self.name, type(symbol_table).__name__))

    def apply(self, node, symbol_table):
        ''' xxx
        # R=ABS(X) => IF (X<0.0) R=X*-1.0 ELSE R=X 
        # TODO: There is an assumption that operation is child of assignment
        '''
        self.validate(node, symbol_table)
        
        oper_parent = node.parent
        assignment = node.ancestor(Assignment)
        res_var = symbol_table.new_symbol_name("res_abs")
        symbol_table.add(DataSymbol(res_var, DataType.REAL))
        tmp_var = symbol_table.new_symbol_name("tmp_abs")
        symbol_table.add(DataSymbol(tmp_var, DataType.REAL))

        # Replace operation with a temporary (res_X).
        oper_parent.children[node.position] = Reference(res_var, parent=oper_parent)

        # Assign content of operation to a temporary (tmp_X)
        lhs = Reference(tmp_var)
        rhs = node.children[0]
        new_assignment = Assignment.create(lhs, rhs)
        new_assignment.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, new_assignment)

        # Set res_X to the absolute value of tmp_X
        lhs = Reference(tmp_var)
        rhs = Literal("0.0", DataType.REAL)
        if_condition = BinaryOperation.create(BinaryOperation.Operator.GT, lhs, rhs)

        lhs = Reference(res_var)
        rhs = Reference(tmp_var)
        then_body = [Assignment.create(lhs, rhs)]

        lhs = Reference(res_var)
        lhs_child = Reference(tmp_var)
        rhs_child = Literal("-1.0", DataType.REAL)
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL, lhs_child, rhs_child)
        else_body = [Assignment.create(lhs, rhs)]

        if_stmt = IfBlock.create(if_condition, then_body, else_body)
        if_stmt.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, if_stmt)


class SignTransformation():

    def apply(self, oper, symbol_table):
        ''' xxx '''
        # R=SIGN(A,B) if A<0 then (if B<0 R=B else R=B*-1) else ((if B>0 R=B else R=B*-1))
        # [USE THIS ONE] R=SIGN(A,B) => R=ABS(B); if A<0.0 R=R*-1.0
        oper_parent = oper.parent
        assignment = oper.ancestor(Assignment)

        res_var = symbol_table.new_symbol_name("res_sign")
        symbol_table.add(DataSymbol(res_var, DataType.REAL))
        tmp_var = symbol_table.new_symbol_name("tmp_sign")
        symbol_table.add(DataSymbol(tmp_var, DataType.REAL))

        # Replace operation with a temporary (res_X).
        oper_parent.children[oper.position] = Reference(res_var, parent=oper_parent)

        # Set the result to the ABS value of the second argument of SIGN
        lhs = Reference(res_var)
        rhs = UnaryOperation.create(UnaryOperation.Operator.ABS, oper.children[1])
        new_assignment = Assignment.create(lhs, rhs)
        new_assignment.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, new_assignment)

        # Replace the ABS intrinsic
        abs_trans = NemoAbsTrans()
        abs_trans.apply(rhs, symbol_table)

        # Assign the 1st argument to a temporary in case it is a complex expression.
        lhs = Reference(tmp_var)
        new_assignment = Assignment.create(lhs, oper.children[0])
        new_assignment.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, new_assignment)

        # Negate the result if the first argument is negative, otherwise do nothing
        lhs = Reference(tmp_var)
        rhs = Literal("0.0", DataType.REAL)
        if_condition= BinaryOperation.create(BinaryOperation.Operator.LT, lhs, rhs)

        lhs = Reference(res_var)
        lhs_child = Reference(res_var)
        rhs_child = Literal("-1.0", DataType.REAL)
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL, lhs_child, rhs_child)
        then_body = [Assignment.create(lhs, rhs)]

        if_stmt = IfBlock.create(if_condition, then_body)
        if_stmt.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, if_stmt)


class MinTransformation():

    def apply(self, oper, symbol_table):
        ''' xxx '''
        # R=MIN(A,B,C,..) R=A; if B<R R=B; if C<R R=C; ...
        oper_parent = oper.parent
        assignment = oper.ancestor(Assignment)

        res_var = symbol_table.new_symbol_name("res_min")
        symbol_table.add(DataSymbol(res_var, DataType.REAL))
        tmp_var = symbol_table.new_symbol_name("tmp_min")
        symbol_table.add(DataSymbol(tmp_var, DataType.REAL))

        # Replace operation with a temporary (res_X).
        oper_parent.children[oper.position] = Reference(res_var, parent=oper_parent)

        # Set the result to the first min value
        lhs = Reference(res_var)
        new_assignment = Assignment.create(lhs, oper.children[0])
        new_assignment.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, new_assignment)

        # For each of the remaining min values
        for expression in oper.children[1:]:
            tmp_var = symbol_table.new_symbol_name("tmp_min")
            symbol_table.add(DataSymbol(tmp_var, DataType.REAL))

            lhs = Reference(tmp_var)
            new_assignment = Assignment.create(lhs, expression)
            new_assignment.parent = assignment.parent
            assignment.parent.children.insert(assignment.position, new_assignment)

            lhs = Reference(tmp_var)
            rhs = Reference(res_var)
            if_condition = BinaryOperation.create(BinaryOperation.Operator.LT, lhs, rhs)
            lhs = Reference(res_var)
            rhs = Reference(tmp_var)
            then_body = [Assignment.create(lhs, rhs)]
            if_stmt = IfBlock.create(if_condition, then_body)
            if_stmt.parent = assignment.parent
            assignment.parent.children.insert(assignment.position, if_stmt)


def trans(psy):
    '''Transformation routine for use with PSyclone. Applies the PSyIR2SIR
    transform to the supplied invokes. This transformation is limited
    the NEMO API.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''

    abs_trans = NemoAbsTrans()
    sign_trans = SignTransformation()
    min_trans = MinTransformation()

    sir_writer = SIRWriter(skip_nodes=True)
    fortran_writer = FortranWriter()
    # For each Invoke write out the SIR representation of the
    # schedule. Note, there is no algorithm layer in the NEMO API so
    # the invokes represent all of the original code.
    for invoke in psy.invokes.invoke_list:
        sched = invoke.schedule
        for kernel in sched.walk(NemoKern):

            # The NEMO api currently has no symbol table so create one
            # to allow the generation of new variables. Note, this
            # does not guarantee unique names as we don't know any of
            # the existing names (so they could clash).
            symbol_table = SymbolTable()

            kernel_schedule = kernel.get_kernel_schedule()
            for oper in kernel_schedule.walk(UnaryOperation):
                if oper.operator == UnaryOperation.Operator.ABS:
                    #print ("FOUND ABS")
                    abs_trans.apply(oper, symbol_table)
            for oper in kernel_schedule.walk(BinaryOperation):
                if oper.operator == BinaryOperation.Operator.SIGN:
                    #print ("FOUND SIGN")
                    sign_trans.apply(oper, symbol_table)
            for oper in kernel_schedule.walk(BinaryOperation):
                if oper.operator == BinaryOperation.Operator.MIN:
                    #print ("FOUND BINARY MIN")
                    min_trans.apply(oper, symbol_table)
            for oper in kernel_schedule.walk(NaryOperation):
                if oper.operator == NaryOperation.Operator.MIN:
                    #print ("FOUND NARY MIN")
                    min_trans.apply(oper, symbol_table)
        kern = sir_writer(sched)
        # kern = fortran_writer(sched)
        print(kern)
        exit(1)
    return psy

