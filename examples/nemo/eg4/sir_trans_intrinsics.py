# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
PSyIR to the Stencil intermediate representation (SIR). Translation to
the SIR is limited to the NEMO API. The NEMO API has no algorithm
layer so all of the original code is captured in the invoke
objects. Therefore by translating all of the invoke objects, all of
the original code is translated.

'''
from __future__ import print_function
from psyclone.psyir.backend.sir import SIRWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.nemo import NemoKern
from psyclone.psyGen import UnaryOperation, BinaryOperation, NaryOperation, Assignment, Reference, Literal, IfBlock, Schedule
import copy


def make_assignment(lhs, rhs):
    ''' xxx '''
    new_assignment = Assignment()
    lhs.parent = new_assignment
    rhs.parent = new_assignment
    new_assignment.children = [lhs, rhs]
    return new_assignment

def make_if(if_condition, then_body, else_body=None):
    ''' xxx '''
    if_stmt = IfBlock()
    then_schedule = Schedule(parent=if_stmt)
    then_body.parent = then_schedule
    then_schedule.children = [then_body]
    if else_body:
        else_schedule = Schedule(parent=if_stmt)
        else_body.parent = else_schedule
        else_schedule.children = [else_body]
        if_stmt.children = [if_condition, then_schedule, else_schedule]
    else:
        if_stmt.children = [if_condition, then_schedule]
    return if_stmt

def make_unary_op(oper, child):
    ''' xxx '''
    unary_op = UnaryOperation(oper)
    child.parent=unary_op
    unary_op.children = [child]
    return unary_op

def make_binary_op(oper, lhs, rhs):
    ''' xxx '''
    binary_op = BinaryOperation(oper)
    lhs.parent=binary_op
    rhs.parent=binary_op
    binary_op.children = [lhs, rhs]
    return binary_op

def replace_abs(oper, key):
    ''' xxx '''
    # R=ABS(X) => IF (X<0.0) R=X*-1.0 ELSE R=X 
    # TODO: There is an assumption that operation is child of assignment
    oper_parent = oper.parent
    assignment = oper.ancestor(Assignment)

    # Replace operation with a temporary (res_X).
    oper_parent.children[oper.position] = Reference("res_{0}".format(key), parent=oper_parent)

    # Assign content of operation to a temporary (tmp_X)
    lhs = Reference("tmp_{0}".format(key))
    rhs = oper.children[0]
    new_assignment = make_assignment(lhs, rhs)
    new_assignment.parent = assignment.parent
    assignment.parent.children.insert(assignment.position, new_assignment)

    # Set res_X to the absolute value of tmp_X
    lhs = Reference("tmp_{0}".format(key))
    rhs = Literal("0.0")
    if_condition = make_binary_op(BinaryOperation.Operator.GT, lhs, rhs)

    lhs = Reference("res_{0}".format(key))
    rhs = Reference("tmp_{0}".format(key))
    then_body = make_assignment(lhs, rhs)

    lhs = Reference("res_{0}".format(key))
    lhs_child = Reference("tmp_{0}".format(key))
    rhs_child = Literal("-1.0")
    rhs = make_binary_op(BinaryOperation.Operator.MUL, lhs_child, rhs_child)
    else_body = make_assignment(lhs, rhs)

    if_stmt = make_if(if_condition, then_body, else_body)
    if_stmt.parent = assignment.parent
    assignment.parent.children.insert(assignment.position, if_stmt)


def replace_sign(oper, key):
    ''' xxx '''
    # R=SIGN(A,B) if A<0 then (if B<0 R=B else R=B*-1) else ((if B>0 R=B else R=B*-1))
    # [USE THIS ONE] R=SIGN(A,B) => R=ABS(B); if A<0.0 R=R*-1.0
    oper_parent = oper.parent
    assignment = oper.ancestor(Assignment)

    res_var = "XXXRES_{0}".format(key)
    tmp_var = "XXXTMP_{0}".format(key)

    # Replace operation with a temporary (res_X).
    oper_parent.children[oper.position] = Reference(res_var, parent=oper_parent)

    # Set the result to the ABS value of the second argument of SIGN
    lhs = Reference(res_var)
    rhs = make_unary_op(UnaryOperation.Operator.ABS, oper.children[1])
    new_assignment = make_assignment(lhs, rhs)
    new_assignment.parent = assignment.parent
    assignment.parent.children.insert(assignment.position, new_assignment)

    # Assign the 1st argument to a temporary in case it is a complex expression.
    lhs = Reference(tmp_var)
    new_assignment = make_assignment(lhs, oper.children[0])
    new_assignment.parent = assignment.parent
    assignment.parent.children.insert(assignment.position, new_assignment)

    # Negate the result if the first argument is negative, otherwise do nothing
    lhs = Reference(tmp_var)
    rhs = Literal("0.0")
    if_condition= make_binary_op(BinaryOperation.Operator.LT, lhs, rhs)
    
    lhs = Reference(res_var)
    rhs = Reference("XXXRESTIMESMINUS1_{0}".format(key))
    then_body = make_assignment(lhs, rhs)

    if_stmt = make_if(if_condition, then_body)
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
    #sir_writer = SIRWriter(skip_nodes=True)
    fortran_writer = FortranWriter()
    # For each Invoke write out the SIR representation of the
    # schedule. Note, there is no algorithm layer in the NEMO API so
    # the invokes represent all of the original code.
    for invoke in psy.invokes.invoke_list:
        key = 0
        sched = invoke.schedule
        for kernel in sched.walk(NemoKern):

            kern = fortran_writer(sched)
            print(kern)

            kernel_schedule = kernel.get_kernel_schedule()
            for oper in kernel_schedule.walk(UnaryOperation):
                if oper.operator == UnaryOperation.Operator.ABS:
                    print ("FOUND ABS")
                    replace_abs(oper, key)
                    key += 1
            for oper in kernel_schedule.walk(BinaryOperation):
                if oper.operator == BinaryOperation.Operator.SIGN:
                    print ("FOUND SIGN")
                    replace_sign(oper, key)
                    key += 1
            for oper in kernel_schedule.walk(BinaryOperation):
                if oper.operator == BinaryOperation.Operator.MIN:
                    print ("FOUND BINARY MIN")
                    # R=MIN(A,B) R=A; IF B<A R=B
            for oper in kernel_schedule.walk(NaryOperation):
                if oper.operator == NaryOperation.Operator.MIN:
                    print ("FOUND NARY MIN")
                    # R=MIN(A,B,C,..) R=A; if B<R R=B; if C<R R=C; ...
        # kern = sir_writer(sched)
        kern = fortran_writer(sched)
        print(kern)
        exit(1)
    return psy

