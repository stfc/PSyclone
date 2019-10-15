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
        key=0
        sched = invoke.schedule
        for kernel in sched.walk(NemoKern):

            kern = fortran_writer(sched)
            print(kern)

            kernel_schedule = kernel.get_kernel_schedule()
            for oper in kernel_schedule.walk(UnaryOperation):
                if oper.operator == UnaryOperation.Operator.ABS:
                    print ("FOUND ABS")
                    # R=ABS(X) => IF (X<0.0) R=X*-1.0 ELSE R=X 
                    # TODO: There is an assumption that operation is child of assignment
                    oper_parent = oper.parent
                    assignment = oper.ancestor(Assignment)

                    # Replace operation with a temporary (res_X).
                    oper_parent.children[oper.position] = Reference("res_{0}".format(key), parent=oper_parent)

                    # Assign content of operation to a temporary (tmp_X)
                    new_assignment = Assignment(parent=assignment.parent)
                    lhs = Reference("tmp_{0}".format(key), parent=new_assignment)
                    rhs = oper.children[0]
                    rhs.parent = new_assignment
                    new_assignment.children = [lhs, rhs]
                    assignment.parent.children.insert(assignment.position, new_assignment)

                    # Set res_X to the absolute value of tmp_X
                    if_stmt = IfBlock(parent=assignment.parent)
                    if_condition = BinaryOperation(BinaryOperation.Operator.GT, parent=if_stmt)
                    lhs = Reference("tmp_{0}".format(key), parent=if_condition)
                    rhs = Literal("0.0", parent=if_condition)
                    if_condition.children = [lhs, rhs]
                    then_schedule = Schedule(parent=if_stmt)
                    then_body = Assignment(parent=then_schedule)
                    then_body_lhs = Reference("res_{0}".format(key), parent=then_body)
                    then_body_rhs = Reference("tmp_{0}".format(key), parent=then_body)
                    then_body.children = [then_body_lhs, then_body_rhs]
                    then_schedule.children = [then_body]                    
                    else_schedule = Schedule(parent=if_stmt)
                    else_body = Assignment(parent=else_schedule)
                    else_body_lhs = Reference("res_{0}".format(key), parent=else_body)
                    else_body_rhs = BinaryOperation(BinaryOperation.Operator.MUL, parent=else_body)
                    lhs_child = Reference("tmp_{0}".format(key), parent=else_body_rhs)
                    rhs_child = Literal("-1.0", parent=else_body_rhs)
                    else_body_rhs.children = [lhs_child, rhs_child]
                    else_body.children = [else_body_lhs, else_body_rhs]
                    else_schedule.children = [else_body]
                    if_stmt.children = [if_condition, then_schedule, else_schedule]
                    assignment.parent.children.insert(assignment.position, if_stmt)

                    #new_assignment_mult = Assignment(parent=assignment.parent)
                    #lhs = Reference("tmp1_{0}".format(key), parent=new_assignment_mult)
                    #rhs = BinaryOperation(BinaryOperation.Operator.MUL, parent=new_assignment_mult)
                    #new_assignment_mult.children = [lhs, rhs]
                    #lhs_child = copy.copy(oper.children[0])
                    #rhs_child = Literal("-1.0", parent=rhs)
                    #rhs.children = [lhs_child, rhs_child]
                    #assignment.parent.children.insert(assignment.position, new_assignment_mult)

                    # This is essentially a max so reuse
                    #if_stmt = IfBlock(parent=assignment.parent)
                    #if_condition = BinaryOperation(BinaryOperation.Operator.GT, parent=if_stmt)
                    #lhs = Reference("tmp0_{0}".format(key), parent=if_condition)
                    #rhs = Reference("tmp1_{0}".format(key), parent=if_condition)
                    #if_condition.children = [lhs, rhs]
                    #then_schedule = Schedule(parent=if_stmt)
                    #then_body = Assignment(parent=then_schedule)
                    #then_body_lhs = Reference("tmp2_{0}".format(key), parent=then_body)
                    #then_body_rhs = Reference("tmp0_{0}".format(key), parent=then_body)
                    #then_body.children = [then_body_lhs, then_body_rhs]
                    #then_schedule.children = [then_body]                    
                    #else_schedule = Schedule(parent=if_stmt)
                    #else_body = Assignment(parent=else_schedule)
                    #else_body_lhs = Reference("tmp2_{0}".format(key), parent=else_body)
                    #else_body_rhs = Reference("tmp1_{0}".format(key), parent=else_body)
                    #else_body.children = [else_body_lhs, else_body_rhs]
                    #else_schedule.children = [else_body]
                    #if_stmt.children = [if_condition, then_schedule, else_schedule]
                    #assignment.parent.children.insert(assignment.position, if_stmt)

                    key += 1
            for oper in kernel_schedule.walk(BinaryOperation):
                if oper.operator == BinaryOperation.Operator.SIGN:
                    print ("FOUND SIGN")
                    # R=SIGN(A,B) if A<0 then (if B<0 R=B else R=B*-1) else ((if B>0 R=B else R=B*-1))
                    # R=SIGN(A,B) R=ABS(B); if A<0 R=R*-1
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
