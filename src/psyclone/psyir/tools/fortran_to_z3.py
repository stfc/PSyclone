# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, University of Cambridge, UK.
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
# Author: M. Naylor, University of Cambridge, UK
# -----------------------------------------------------------------------------

'''This module provides a class to translate Fortran to Z3. Currently, it
supports translation of scalar integer and scalar logical expressions.'''

import z3
import random
import threading
from psyclone.psyir.nodes import Loop, DataNode, Literal, Assignment, \
    Reference, UnaryOperation, BinaryOperation, IntrinsicCall, \
    Routine, Node, IfBlock, Schedule, ArrayReference, Range, WhileLoop, \
    CodeBlock
from psyclone.psyir.symbols import DataType, ScalarType, ArrayType, \
    INTEGER_TYPE

class FortranToZ3:
    '''The class provides methods to translate Fortran expressions to Z3
       expressions, as well as some useful wrappers around the Z3 solver.

       :param use_bv: whether to treat Fortran integers as bit vectors or
          arbitrary-precision integers. The default is arbitrary-precision
          integers.

       :param int_width: the bit width of Fortran integers when interpreted
          as bit vectors. This is 32 by default.

       :param smt_timeout_ms: the time limit (in milliseconds) given to
          the SMT solver to find a solution.

       :param prohibit_overflow: if True, the translation functions will
          ignore the possibility of integer overflow by generating constraints
          to prohibit it. Integer overflow is undefined behaviour in Fortran
          so this is safe.

       :param handle_array_intrins: if True, array intrinsic calls
          'size(<arr>,<dim>)', 'lbound(<arr>,<dim>)', and
          'ubound(<arr>,<dim>)' will be translated to Z3 integer variables
          of the form '#size_<arr>_<dim>', '#lbound_<arr>_<dim>', and
          '#ubound_<arr>_<dim>'.

       :param num_sweep_threads: when larger than one, this option enables the
          sweeper, which runs multiple solvers across multiple threads with
          each one using a different constraint ordering. This reduces the
          solver's sensitivity to the order of constraints.

       :param sweep_seed: the seed for the random number generator used
          by the sweeper.
    '''
    def __init__(self,
                 use_bv: bool = False,
                 int_width: int = 32,
                 smt_timeout_ms: int = 5000,
                 prohibit_overflow: bool = False,
                 handle_array_intrins: bool = False,
                 num_sweep_threads: int = 4,
                 sweep_seed: int = 1):
        self.use_bv = use_bv
        self.int_width = int_width
        self.smt_timeout = smt_timeout_ms
        self.prohibit_overflow = prohibit_overflow
        self.handle_array_intrins = handle_array_intrins
        self.num_sweep_threads = num_sweep_threads
        self.sweep_seed = sweep_seed

    def translate_integer_expr(self, expr_root: Node) \
                                -> (z3.ExprRef, list[z3.BoolRef]):
        '''Translate a Fortran scalar integer expression to SMT. 

           :param expr_root: the expression to translate. This is assumed
             to have scalar integer type.
           :return: a pair containing the translated expression and a
             list of Z3 constraints introduced by the translation.
             One use of the constraint list is to prohibit bit-vector
             overflow.
        '''
        constraints = []

        def trans(e: Node) -> z3.ExprRef:
            '''Internal recursive function that has implicit access to a
               a mutable list of 'constraints' being gathered.

               :param e: the expression to translate.
               :return: the translated expression.
            '''
            # Literal
            if isinstance(e, Literal):
                if self.use_bv:
                    return z3.BitVecVal(int(e.value), self.int_width)
                else:
                    return z3.IntVal(int(e.value))

            # Reference
            if (isinstance(e, Reference)
                    and not isinstance(e, ArrayReference)):
                (sig, indices) = e.get_signature_and_indices()
                indices_flat = [i for inds in indices for i in inds]
                if indices_flat == []:
                    if self.use_bv:
                        return z3.BitVec(str(sig), self.int_width)
                    else:
                        return z3.Int(str(sig))

            # UnaryOperation
            if isinstance(e, UnaryOperation):
                arg_smt = trans(e.operand)
                if e.operator == UnaryOperation.Operator.MINUS:
                    if self.use_bv and self.prohibit_overflow:
                        constraints.append(z3.BVSNegNoOverflow(arg_smt))
                    return -arg_smt
                if e.operator == UnaryOperation.Operator.PLUS:
                    return arg_smt

            # BinaryOperation
            if isinstance(e, BinaryOperation):
                (left, right) = e.operands
                left_smt = trans(left)
                right_smt = trans(right)

                if e.operator == BinaryOperation.Operator.ADD:
                    if self.use_bv and self.prohibit_overflow:
                        constraints.append(z3.BVAddNoOverflow(
                          left_smt, right_smt, True))
                        constraints.append(z3.BVAddNoUnderflow(
                          left_smt, right_smt))
                    return left_smt + right_smt
                if e.operator == BinaryOperation.Operator.SUB:
                    if self.use_bv and self.prohibit_overflow:
                        constraints.append(z3.BVSubNoOverflow(
                          left_smt, right_smt))
                        constraints.append(z3.BVSubNoUnderflow(
                          left_smt, right_smt, True))
                    return left_smt - right_smt
                if e.operator == BinaryOperation.Operator.MUL:
                    if self.use_bv and self.prohibit_overflow:
                        constraints.append(z3.BVMulNoOverflow(
                          left_smt, right_smt, True))
                        constraints.append(z3.BVMulNoUnderflow(
                          left_smt, right_smt))
                    return left_smt * right_smt
                if e.operator == BinaryOperation.Operator.DIV:
                    if self.use_bv and self.prohibit_overflow:
                        constraints.append(z3.BVSDivNoOverflow(
                          left_smt, right_smt))
                    return left_smt / right_smt

            # IntrinsicCall
            if isinstance(e, IntrinsicCall):
                # Unary operators
                if e.intrinsic == IntrinsicCall.Intrinsic.ABS:
                    smt_arg = trans(e.children[1])
                    if self.use_bv and self.prohibit_overflow:
                        constraints.append(z3.BVSNegNoOverflow(smt_arg))
                    return z3.Abs(smt_arg)

                # Binary operators
                if e.intrinsic in [IntrinsicCall.Intrinsic.SHIFTL,
                                      IntrinsicCall.Intrinsic.SHIFTR,
                                      IntrinsicCall.Intrinsic.SHIFTA,
                                      IntrinsicCall.Intrinsic.IAND,
                                      IntrinsicCall.Intrinsic.IOR,
                                      IntrinsicCall.Intrinsic.IEOR,
                                      IntrinsicCall.Intrinsic.MODULO,
                                      IntrinsicCall.Intrinsic.MOD]:
                    left_smt = trans(e.children[1])
                    right_smt = trans(e.children[2])

                    if (e.intrinsic == IntrinsicCall.Intrinsic.MODULO):
                        return left_smt % right_smt

                    if (e.intrinsic == IntrinsicCall.Intrinsic.MOD):
                        m = left_smt % right_smt
                        return (z3.If(z3.And(m != 0,
                                        (left_smt < 0) != (right_smt < 0)),
                                      m-right_smt, m))

                    if self.use_bv:
                        # TODO: when fparser supports shift operations (#428),
                        # we can uncomment test and remove the "no cover"
                        # block
                        if True:  # pragma: no cover
                            if e.intrinsic == IntrinsicCall.Intrinsic.SHIFTL:
                                return left_smt << right_smt
                            if e.intrinsic == IntrinsicCall.Intrinsic.SHIFTR:
                                return z3.LShR(left_smt, right_smt)
                            if e.intrinsic == IntrinsicCall.Intrinsic.SHIFTA:
                                return left_smt >> right_smt
                        if e.intrinsic == IntrinsicCall.Intrinsic.IAND:
                            return left_smt & right_smt
                        if e.intrinsic == IntrinsicCall.Intrinsic.IOR:
                            return left_smt | right_smt
                        if e.intrinsic == IntrinsicCall.Intrinsic.IEOR:
                            return left_smt ^ right_smt
                    else:
                        # TODO: when fparser supports shift operations (#428),
                        # we can uncomment tests and remove the "no cover"
                        # block
                        if True:  # pragma: no cover
                            if e.intrinsic == IntrinsicCall.Intrinsic.SHIFTL:
                                return z3.BV2Int(
                                         z3.Int2BV(left_smt, self.int_width) <<
                                         z3.Int2BV(right_smt, self.int_width),
                                         is_signed=True)
                            if e.intrinsic == IntrinsicCall.Intrinsic.SHIFTR:
                                return z3.BV2Int(z3.LShR(
                                         z3.Int2BV(left_smt, self.int_width),
                                         z3.Int2BV(right_smt, self.int_width)),
                                         is_signed=True)
                            if e.intrinsic == IntrinsicCall.Intrinsic.SHIFTA:
                                return z3.BV2Int(
                                         z3.Int2BV(left_smt, self.int_width) >>
                                         z3.Int2BV(right_smt, self.int_width),
                                         is_signed=True)
                        if e.intrinsic == IntrinsicCall.Intrinsic.IAND:
                            return z3.BV2Int(
                                z3.Int2BV(left_smt, self.int_width) &
                                z3.Int2BV(right_smt, self.int_width),
                                is_signed=True)
                        if e.intrinsic == IntrinsicCall.Intrinsic.IOR:
                            return z3.BV2Int(
                                z3.Int2BV(left_smt, self.int_width) |
                                z3.Int2BV(right_smt, self.int_width),
                                is_signed=True)
                        if e.intrinsic == IntrinsicCall.Intrinsic.IEOR:
                            return z3.BV2Int(
                                z3.Int2BV(left_smt, self.int_width) ^
                                z3.Int2BV(right_smt, self.int_width),
                                is_signed=True)

                # N-ary operators
                if e.intrinsic in [IntrinsicCall.Intrinsic.MIN,
                                      IntrinsicCall.Intrinsic.MAX]:
                    smt_args = [trans(arg) for arg in e.children[1:]]
                    reduced = smt_args[0]
                    for arg in smt_args[1:]:
                        if e.intrinsic == IntrinsicCall.Intrinsic.MIN:
                            reduced = z3.If(reduced < arg, reduced, arg)
                        elif e.intrinsic == IntrinsicCall.Intrinsic.MAX:
                            reduced = z3.If(reduced < arg, arg, reduced)
                    return reduced

                # Array intrinsics
                if self.handle_array_intrins:
                    array_intrins_pair = self.translate_array_intrinsic_call(e)
                    if array_intrins_pair:
                        if self.use_bv:
                            return z3.BitVec(
                                array_intrins_pair[1], self.int_width)
                        else:
                            return z3.Int(array_intrins_pair[1])

            # Fall through: return a fresh, unconstrained symbol
            if self.use_bv:
                return z3.FreshConst(z3.BitVecSort(self.int_width))
            else:
                return z3.FreshInt()

        # Incoke the recursive function
        expr_root_smt = trans(expr_root)
        return (expr_root_smt, constraints)

    def translate_logical_expr(self, expr_root: Node) \
                                -> (z3.BoolRef, list[z3.BoolRef]):
        '''Translate a scalar logical Fortran expression to SMT.

           :param expr_root: the expression to translate. This is assumed
             to have scalar logical type.
           :return: a pair containing the translated expression and a
             list of Z3 constraints introduced by the translation.
             One use of the constraint list is to prohibit bit-vector
             overflow.
        '''
        constraints = []

        def trans(expr: Node):
            # Literal
            if isinstance(expr, Literal):
                if expr.value == "true":
                    return z3.BoolVal(True)
                if expr.value == "false":
                    return z3.BoolVal(False)

            # Reference
            if (isinstance(expr, Reference)
                    and not isinstance(expr, ArrayReference)):
                (sig, indices) = expr.get_signature_and_indices()
                indices_flat = [i for inds in indices for i in inds]
                if indices_flat == []:
                    return z3.Bool(str(sig))

            # UnaryOperation
            if isinstance(expr, UnaryOperation):
                arg_smt = trans(expr.operand)
                if expr.operator == UnaryOperation.Operator.NOT:
                    return z3.Not(arg_smt)

            # BinaryOperation
            if isinstance(expr, BinaryOperation):
                # Operands are logicals
                if expr.operator in [BinaryOperation.Operator.AND,
                                     BinaryOperation.Operator.OR,
                                     BinaryOperation.Operator.EQV,
                                     BinaryOperation.Operator.NEQV]:
                    (left, right) = expr.operands
                    left_smt = trans(left)
                    right_smt = trans(right)

                    if expr.operator == BinaryOperation.Operator.AND:
                        return z3.And(left_smt, right_smt)
                    if expr.operator == BinaryOperation.Operator.OR:
                        return z3.Or(left_smt, right_smt)
                    if expr.operator == BinaryOperation.Operator.EQV:
                        return left_smt == right_smt
                    if expr.operator == BinaryOperation.Operator.NEQV:
                        return left_smt != right_smt

                # Operands are numbers
                if expr.operator in [BinaryOperation.Operator.EQ,
                                     BinaryOperation.Operator.NE,
                                     BinaryOperation.Operator.GT,
                                     BinaryOperation.Operator.LT,
                                     BinaryOperation.Operator.GE,
                                     BinaryOperation.Operator.LE]:
                    (left, right) = expr.operands
                    (left_smt, cs) = self.translate_integer_expr(left)
                    constraints.extend(cs)
                    (right_smt, cs) = self.translate_integer_expr(right)
                    constraints.extend(cs)

                    if expr.operator == BinaryOperation.Operator.EQ:
                        return left_smt == right_smt
                    if expr.operator == BinaryOperation.Operator.NE:
                        return left_smt != right_smt
                    if expr.operator == BinaryOperation.Operator.GT:
                        return left_smt > right_smt
                    if expr.operator == BinaryOperation.Operator.LT:
                        return left_smt < right_smt
                    if expr.operator == BinaryOperation.Operator.GE:
                        return left_smt >= right_smt
                    if expr.operator == BinaryOperation.Operator.LE:
                        return left_smt <= right_smt

            # Fall through: return a fresh, unconstrained symbol
            return z3.FreshBool()

        expr_root_smt = trans(expr_root)
        return (expr_root_smt, constraints)

    def translate_array_intrinsic_call(self, call: IntrinsicCall) -> (str, str):
        '''Translate array intrinsic call to an array name and a scalar
           integer variable name. Only a small number of important array
           intrinsics are recognised, such as 'size', 'lbound', and 'ubound'.

           :param call: the intrinsic call being transatled to SMT.
           :return: a pair containing the name of the array on which
              the intrinsic is being applied, and a scalar integer
              variable name representing the result of the intrinsic.
              If the intrinisic call is not recognised then None is returned.
        '''
        if call.intrinsic == IntrinsicCall.Intrinsic.SIZE:
            var = "#size"
        elif call.intrinsic == IntrinsicCall.Intrinsic.LBOUND:
            var = "#lbound"
        elif call.intrinsic == IntrinsicCall.Intrinsic.UBOUND:
            var = "#ubound"
        else:
            return None

        if (len(call.children) != 2 and len(call.children) != 3):
            return None  # pragma: no cover

        array = call.children[1]
        if isinstance(array, Reference):
            (sig, indices) = array.get_signature_and_indices()
            indices_flat = [i for inds in indices for i in inds]
            if indices_flat == [] and len(sig) == 1:
                var = var + "_" + sig.var_name
                if len(call.children) == 3:
                    rank = call.children[2]
                    if isinstance(rank, Literal):
                        var = var + "_" + rank.value
                    else:
                        return None
                return (sig.var_name, var)

        return None

    def solve(self, constraints: list[z3.BoolRef],
                    sum_of_prods: list[list[z3.BoolRef]] = \
                                  [[z3.BoolVal(True)]],
                    exprs_to_eval: list[z3.ExprRef] = []) -> \
            (z3.CheckSatResult, list[z3.ExprRef]):
        '''Invoke the solver on the given constraints. If the constraints
        are satisfiable then the given expressions are evaluated and
        returned.

        The solver is quite sensitive to the order of constraints.
        If the sweeper is enabled, multiple solvers are run in parallel,
        with each one using a different constraint order. As soon as one
        solver completes, the others are cancelled. 

        :param constraints: a set of constraints to solve. These are
           implicitly ANDed together. If the sweeper is enabled,
           this list is randomly shuffled by each solver thread.
        :param sum_of_prods: a sum of products of constraints to solve.
           Elements of the inner lists are ANDed together.
           Elements of the outer lists are ORed together.
           These constraints are implicitly ANDed with the 'constraints'.
           If the sweeper is enabled, elements of the inner lists are
           randomly shuffled by each solver thread.
        :param exprs_to_eval: a list of expressions to evaluate, assuming
           the constraints are satisfiable.
        :return: the result of the solver and a list of evaluated expressions
           (the list is empty if the constraints were not satisifiable)
        '''
        if self.num_sweep_threads <= 1:
            s = z3.Solver()
            if self.smt_timeout is not None:
                s.set("timeout", self.smt_timeout)
            s.add(z3.And(constraints))
            s.add(z3.Or([z3.And(prod) for prod in sum_of_prods]))
            result = s.check()
            result_values = []
            if result == z3.sat:
                m = s.model()
                for expr in exprs_to_eval:
                    result_values.append(m.eval(expr,
                                                model_completion=True))
            return (result, result_values)
        else:
            return self.sweep_solve(constraints, sum_of_prods, exprs_to_eval)

    def sweep_solve(self, constraints: list[z3.BoolRef],
                          sum_of_prods: list[list[z3.BoolRef]] = \
                                        [[z3.BoolVal(True)]],
                          exprs_to_eval: list[z3.ExprRef] = []) -> \
            (z3.CheckSatResult, list[z3.ExprRef]):
        '''The interface to this method is identical to that of the
        'solve()' method. This method implements the sweeper.'''
        result = []
        result_values = []
        result_lock = threading.Lock()
        done_event = threading.Event()

        # Function that runs in each thread
        def wrapper(solver, exprs_to_eval):
            out = solver.check()
            with result_lock:
                if not done_event.is_set():
                    if out == z3.sat:
                        m = solver.model()
                        for expr in exprs_to_eval:
                            result_values.append(str(
                               m.eval(expr, model_completion=True)))
                    result.append(out)
                    done_event.set()

        # Random number generator for shuffling constraints
        rnd = random.Random(self.sweep_seed)

        # Create a solver per thread
        solvers = []
        threads = []
        for i in range(0, self.num_sweep_threads):
            # Create a solver for the problem
            ctx = z3.Context()
            s = z3.Solver(ctx=ctx)
            if self.smt_timeout is not None:
                s.set("timeout", self.smt_timeout)
            s.add(z3.And(constraints).translate(ctx))
            sum_constraint = z3.Or([z3.And(prod) for prod in sum_of_prods])
            s.add(sum_constraint.translate(ctx))
            solvers.append(s)
            exprs_to_eval_ctx = [e.translate(ctx) for e in exprs_to_eval]

            # Create a thread for this solver and start it
            t = threading.Thread(target=wrapper, args=(s,exprs_to_eval_ctx))
            threads.append(t)
            t.start()

            # Shuffle the constraints for the next thread
            rnd.shuffle(constraints)
            for prod in sum_of_prods:
                rnd.shuffle(prod)
            if done_event.is_set():
                break

        # Wait for first thread to complete
        done_event.wait()

        # Interrupt all solvers and wait for all threads to complete.
        # This loop appears more complex than necessary, but we want to
        # handle the possibility that we may interrupt a solver before
        # it has started, in which case the interrupt would have no effect
        # and we'd have to wait for that solver's timeout to expire.
        for (s, t) in zip(solvers, threads):
            while True:
                s.interrupt()
                t.join(timeout=0.1)
                if not t.is_alive():
                    break

        return (result[0], result_values)
