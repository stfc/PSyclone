# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, University of Cambridge, UK.
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

'''This module provides a class to determine whether or not distinct iterations
of a given loop can generate conflicting array accesses (if not, the loop can
potentially be parallelised).  It formulates the problem as a set of SMT
constraints over array indices which are then are passed to the Z3 solver.'''

import z3
from psyclone.psyir.nodes import Loop, DataNode, Literal, Assignment, \
    Reference, UnaryOperation, BinaryOperation, IntrinsicCall, \
    Routine, Node, IfBlock, Schedule, ArrayReference, Range, WhileLoop
from psyclone.psyir.symbols import DataType, ScalarType, ArrayType, \
    INTEGER_TYPE

# Outline
# =======
#
# The analysis class provides a method 'is_loop_conflict_free()' to decide
# whether or not the array accesses in a given loop are conflicting between
# iterations. Two array accesses are conflicting if they access the same
# element of the same array, and at least one of the accesses is a write.  The
# analysis algorithm operates, broadly, as follows.
#
# Given a loop, we find its enclosing routine, and start analysing the routine
# statement-by-statement in a recursive-descent fashion.
#
# As we proceed, we maintain a set of SMT constraints and a substitution that
# maps Fortran variable names to SMT variable names.  For each Fortran
# variable, the substitution points to an SMT variable that is constrained (in
# the set of constraints) such that it captures the value of the Fortran
# variable at the current point in the code. When a Fortran variable is
# mutated, the substitution is be modified to point to a fresh SMT variable,
# with new constraints, without destroying the old constraints.
#
# More concretely, when we encounter an assignment of a scalar integer/logical
# variable, of the form 'x = rhs', we translate 'rhs' to the SMT formula
# 'smt_rhs' with the current substitution applied. We then add a constraint
# 'var = smt_rhs' where 'var' is a fresh SMT variable, and update the
# substition so that 'x' maps to 'var'.
#
# The Fortran-expression-to-SMT translator knows about several Fortran
# operators and intrinsics, but not all of them; when it sees something it
# doesn't know about, it simply translates it to a fresh unconstrained SMT
# variable.
#
# Sometimes we reach a statement that modifies a Fortran variable in an unknown
# way (e.g. calling a subroutine). This can be handled by updating the
# substitution to point to a fresh unconstrained SMT variable; we refer to this
# process as "killing" the variable.
#
# In addition to the current substitution, we maintain a stack of previous
# substitutions. This allows substitutions to be saved and restored before and
# after analysing a block of code that may or may not be executed at run time.
#
# We also maintain a "current condition".  This can be viewed as a constraint
# that has not been comitted to the constraint set because we want to be able
# to grow, contract, and retract it as we enter and exit conditional blocks of
# code. This current condition is passed in recursive calls, so there is an
# implicit stack of them.
#
# More concretely, when we encouter an 'if' statement, we copy the current
# substitution onto the stack, then recurse into the 'then' body, passing in
# the 'if' condition as an argument, and then restore the old substitution. We
# do the same for the 'else' body if there is one (in this case the negated
# condition is passed to the recursive call). Finally, we kill all variables
# written by the 'then' and 'else' bodies, because we don't know which will be
# executed at run time.
#
# As the analysis proceeds, we also maintain a list of array accesses. For each
# access, we record various information including the name of the array,
# whether it is a read or a write, the current condition at the point the
# access is made, and its list of indices (translated to SMT). When we are
# analysing code that is inside the loop of interest, we add all array accesses
# encountered to the list.
#
# When we encounter the loop of interest, we perform a couple of steps before
# recursing into the loop body. First, we kill all variables written by the
# loop body, because we don't know whether we are entering the loop (at
# run time) for the first time or not. Second, we create two SMT variables to
# represent two arbitary but distinct iterations of the loop. Each variable is
# constrained to the start, stop, and step of the loop, and the two variables
# are constrained to be not equal. After that, we analyse the loop body twice,
# each time mapping the loop variable in the substitution to a different SMT
# variable.  After analysing the loop body for the first time, we save the
# array access list and start afresh with a new one. Therefore, once the
# analysis is complete, we have two array access lists, one for each iteration.
#
# When we encounter a loop that is not the loop of interest, we follow a
# similar approach but only consider a single arbitrary iteration of the loop.
#
# When the recursive descent is complete, we are left with two array access
# lists. We are interested in whether any pair of accesses to the same array
# (in which one of the accesses is a write) represents a conflict.  An access
# pair is conflict-free if an equality constraint between each access's
# indices, when combined with the current condition of each access and the
# global constraint set, is unsatisfiable.  In this way, we can check every
# access pair and determine whether or not the loop is conflict free.


class ArrayIndexAnalysis:
    # Class representing analysis options
    class Options:
        def __init__(self,
                     int_width: int = 32,
                     use_bv: int = True,
                     smt_timeout_ms: int = 5000,
                     prohibit_overflow: bool = False):
            # Set SMT solver timeout in milliseconds
            self.smt_timeout = smt_timeout_ms
            # Fortran integer width in bits
            self.int_width = int_width
            # Use fixed-width bit vectors or arbirary precision integers?
            self.use_bv = use_bv
            # Prohibit bit-vector overflow when solving constraints?
            self.prohibit_overflow = prohibit_overflow

    # Class representing an array access
    class ArrayAccess:
        def __init__(self,
                     cond:       z3.BoolRef,
                     is_write:   bool,
                     indices:    list[z3.ExprRef],
                     psyir_node: Node):
            # The condition at the location of the access
            self.cond = cond
            # Whether the access is a read or a write
            self.is_write = is_write
            # SMT expressions representing the indices of the access
            self.indices = indices
            # PSyIR node for the access (useful for error reporting)
            self.psyir_node = psyir_node

    # ArrayIndexAnalysis constructor
    def __init__(self, options=Options()):
        self.smt_timeout = options.smt_timeout
        self.int_width = options.int_width
        self.use_bv = options.use_bv
        self.prohibit_overflow = options.prohibit_overflow

    # Initialise analysis
    def init_analysis(self):
        # The substitution maps integer and logical Fortran variables
        # to SMT symbols
        self.subst = {}
        # We have a stack of these to support save/restore
        self.subst_stack = []
        # The constraint set is represented as a list of boolean SMT formulae
        self.constraints = []
        # The access dict maps each array name to a list of array accesses
        self.access_dict = {}
        # We record two access dicts, representing two arbitrary but distinct
        # iterations of the loop to parallelise
        self.saved_access_dicts = []
        # Are we currently analysing the loop to parallelise?
        self.in_loop_to_parallelise = False
        # Has the analaysis finished?
        self.finished = False

    # Push copy of current substitution to the stack
    def save_subst(self):
        self.subst_stack.append(self.subst.copy())

    # Pop substitution from stack into current substitution
    def restore_subst(self):
        self.subst = self.subst_stack.pop()

    # Create an fresh SMT integer variable
    def fresh_integer_var(self) -> z3.ExprRef:
        if self.use_bv:
            return z3.FreshConst(z3.BitVecSort(self.int_width))
        else:
            return z3.FreshInt()

    # Create an integer SMT variable with the given name
    def integer_var(self, var) -> z3.ExprRef:
        if self.use_bv:
            return z3.BitVec(var, self.int_width)
        else:
            return z3.Int(var)

    # Create an SMT integer value
    def integer_val(self, val: int) -> z3.ExprRef:
        if self.use_bv:
            return z3.BitVecVal(val, self.int_width)
        else:
            return z3.IntVal(val)

    # Clear knowledge of 'var' by mapping it to a fresh, unconstrained symbol
    def kill_integer_var(self, var: str):
        fresh_sym = self.fresh_integer_var()
        smt_var = self.integer_var(var)
        self.subst[smt_var] = fresh_sym

    # Clear knowledge of 'var' by mapping it to a fresh, unconstrained symbol
    def kill_logical_var(self, var: str):
        fresh_sym = z3.FreshBool()
        smt_var = z3.Bool(var)
        self.subst[smt_var] = fresh_sym

    # Kill all scalar integer/logical variables written inside 'node'
    def kill_all_written_vars(self, node: Node):
        var_accesses = node.reference_accesses()
        for sig, access_seq in var_accesses.items():
            for access_info in access_seq.all_write_accesses:
                sym = self.routine.symbol_table.lookup(sig.var_name)
                if sym.is_unresolved:
                    continue  # pragma: no cover
                elif _is_scalar_integer(sym.datatype):
                    self.kill_integer_var(sig.var_name)
                elif _is_scalar_logical(sym.datatype):
                    self.kill_logical_var(sig.var_name)

    # Add the SMT constraint to the constraint set
    def add_constraint(self, smt_expr: z3.BoolRef):
        self.constraints.append(smt_expr)

    # Add an integer assignment constraint to the constraint set
    def add_integer_assignment(self, var: str, smt_expr: z3.ExprRef):
        # Create a fresh symbol
        fresh_sym = self.fresh_integer_var()
        # Assert equality between this symbol and the given SMT expression
        self.add_constraint(fresh_sym == smt_expr)
        # Update the substitution
        smt_var = self.integer_var(var)
        self.subst[smt_var] = fresh_sym

    # Add a logical assignment constraint to the constraint set
    def add_logical_assignment(self, var: str, smt_expr: z3.BoolRef):
        # Create a fresh symbol
        fresh_sym = z3.FreshBool()
        # Assert equality between this symbol and the given SMT expression
        self.add_constraint(fresh_sym == smt_expr)
        # Update the substitution
        smt_var = z3.Bool(var)
        self.subst[smt_var] = fresh_sym

    # Translate integer expresison to SMT, and apply current substitution
    def translate_integer_expr_with_subst(self, expr: z3.ExprRef):
        (smt_expr, prohibit_overflow) = translate_integer_expr(
            expr, self.int_width, self.use_bv)
        subst_pairs = list(self.subst.items())
        if self.prohibit_overflow:
            self.add_constraint(z3.substitute(prohibit_overflow, *subst_pairs))
        return z3.substitute(smt_expr, *subst_pairs)

    # Translate logical expresison to SMT, and apply current substitution
    def translate_logical_expr_with_subst(self, expr: z3.BoolRef):
        (smt_expr, prohibit_overflow) = translate_logical_expr(
            expr, self.int_width, self.use_bv)
        subst_pairs = list(self.subst.items())
        if self.prohibit_overflow:
            self.add_constraint(z3.substitute(prohibit_overflow, *subst_pairs))
        return z3.substitute(smt_expr, *subst_pairs)

    # Translate conditional expresison to SMT, and apply current substitution
    def translate_cond_expr_with_subst(self, expr: z3.BoolRef):
        (smt_expr, prohibit_overflow) = translate_logical_expr(
            expr, self.int_width, self.use_bv)
        if self.prohibit_overflow:
            smt_expr = z3.And(smt_expr, prohibit_overflow)
        subst_pairs = list(self.subst.items())
        return z3.substitute(smt_expr, *subst_pairs)

    # Constrain a loop variable to given start/stop/step
    def constrain_loop_var(self,
                           var:   z3.ExprRef,
                           start: DataNode,
                           stop:  DataNode,
                           step:  DataNode):
        zero = self.integer_val(0)
        var_begin = self.translate_integer_expr_with_subst(start)
        var_end = self.translate_integer_expr_with_subst(stop)
        if step is None:
            step = Literal("1", INTEGER_TYPE)  # pragma: no cover
        var_step = self.translate_integer_expr_with_subst(step)
        self.add_constraint(z3.And(
          ((var - var_begin) % var_step) == zero,
          z3.Implies(var_step > zero, var >= var_begin),
          z3.Implies(var_step < zero, var <= var_begin),
          z3.Implies(var_step > zero, var <= var_end),
          z3.Implies(var_step < zero, var >= var_end)))

    # Add an array access to the current access dict
    def add_array_access(self,
                         array_name: str,
                         is_write:   bool,
                         cond:       z3.BoolRef,
                         indices:    list[z3.ExprRef],
                         psyir_node: Node):
        access = ArrayIndexAnalysis.ArrayAccess(
                   cond, is_write, indices, psyir_node)
        if array_name not in self.access_dict:
            self.access_dict[array_name] = []
        self.access_dict[array_name].append(access)

    # Add all array accesses in the given node to the current access dict
    def add_all_array_accesses(self, node: Node, cond: z3.BoolRef):
        var_accesses = node.reference_accesses()
        for sig, access_seq in var_accesses.items():
            for access_info in access_seq:
                if access_info.is_data_access:

                    # ArrayReference
                    if isinstance(access_info.node, ArrayReference):
                        indices = []
                        for index in access_info.node.indices:
                            if isinstance(index, Range):
                                var = self.fresh_integer_var()
                                self.constrain_loop_var(
                                  var, index.start, index.stop, index.step)
                                indices.append(var)
                            else:
                                indices.append(
                                  self.translate_integer_expr_with_subst(
                                    index))
                        self.add_array_access(
                            sig.var_name,
                            access_info.is_any_write(),
                            cond, indices, access_info.node)

                    # Reference with datatype ArrayType
                    elif (isinstance(access_info.node, Reference) and
                          isinstance(access_info.node.datatype, ArrayType)):
                        indices = []
                        for index in access_info.node.datatype.shape:
                            var = self.fresh_integer_var()
                            indices.append(var)
                        self.add_array_access(
                            sig.var_name, access_info.is_any_write(),
                            cond, indices, access_info.node)

    # Move the current access dict to the stack, and proceed with an empty one
    def save_access_dict(self):
        self.saved_access_dicts.append(self.access_dict)
        self.access_dict = {}

    # Check if the given loop has a conflict
    def is_loop_conflict_free(self, loop: Loop) -> bool:
        # Type checking
        if not isinstance(loop, Loop):
            raise TypeError("ArrayIndexAnalysis: Loop argument expected")
        self.loop = loop

        # Find the enclosing routine
        routine = loop.ancestor(Routine)
        if not routine:
            raise ValueError(
                    "ArrayIndexAnalysis: loop has no enclosing routine")
        self.routine = routine

        # Start with an empty constraint set and substitution
        self.init_analysis()
        self.loop_to_parallelise = loop

        # Step through body of the enclosing routine, statement by statement
        for stmt in routine.children:
            self.step(stmt, z3.BoolVal(True))

        # Check that we have found and analysed the loop to parallelise
        if not (self.finished and len(self.saved_access_dicts) == 2):
            return None

        # Forumlate constraints for solving, considering the two iterations
        iter_i = self.saved_access_dicts[0]
        iter_j = self.saved_access_dicts[1]
        conflicts = []
        for (arr_name, i_accesses) in iter_i.items():
            j_accesses = iter_j[arr_name]
            # For each write access in the i iteration
            for i_access in i_accesses:
                if i_access.is_write:
                    # Check for conflicts against every access in the
                    # j iteration
                    for j_access in j_accesses:
                        assert len(i_access.indices) == len(j_access.indices)
                        indices_equal = []
                        for (i_idx, j_idx) in zip(i_access.indices,
                                                  j_access.indices):
                            indices_equal.append(i_idx == j_idx)
                        conflicts.append(z3.And(
                          *indices_equal,
                          i_access.cond,
                          j_access.cond))

        # Invoke Z3 solver with a timeout
        solver = z3.Solver()
        solver.set("timeout", self.smt_timeout)
        solver.add(z3.And(*self.constraints, z3.Or(*conflicts)))
        result = solver.check()
        if result == z3.unsat:
            return True
        elif result == z3.sat:
            return False
        else:
            return None  # pragma: no cover

    # Analyse a single statement
    def step(self, stmt: Node, cond: z3.BoolRef):
        # Has analysis finished?
        if self.finished:
            return

        # Assignment
        if isinstance(stmt, Assignment):
            if (isinstance(stmt.lhs, Reference)
                    and not isinstance(stmt.lhs, ArrayReference)):
                if _is_scalar_integer(stmt.lhs.datatype):
                    rhs_smt = self.translate_integer_expr_with_subst(stmt.rhs)
                    self.add_integer_assignment(stmt.lhs.name, rhs_smt)
                    if self.in_loop_to_parallelise:
                        self.add_all_array_accesses(stmt.rhs, cond)
                    return
                elif _is_scalar_logical(stmt.lhs.datatype):
                    rhs_smt = self.translate_logical_expr_with_subst(stmt.rhs)
                    self.add_logical_assignment(stmt.lhs.name, rhs_smt)
                    return

        # Schedule
        if isinstance(stmt, Schedule):
            for child in stmt.children:
                self.step(child, cond)
            return

        # IfBlock
        if isinstance(stmt, IfBlock):
            if self.in_loop_to_parallelise:
                self.add_all_array_accesses(stmt.condition, cond)
            # Translate condition to SMT
            smt_condition = self.translate_cond_expr_with_subst(stmt.condition)
            # Recursively step into 'then'
            if stmt.if_body:
                self.save_subst()
                self.step(stmt.if_body, z3.And(cond, smt_condition))
                self.restore_subst()
            # Recursively step into 'else'
            if stmt.else_body:
                self.save_subst()
                self.step(stmt.else_body,
                          z3.And(cond, z3.Not(smt_condition)))
                self.restore_subst()
            # Kill vars written by each branch
            if stmt.if_body:
                self.kill_all_written_vars(stmt.if_body)
            if stmt.else_body:
                self.kill_all_written_vars(stmt.else_body)
            return

        # Loop
        if isinstance(stmt, Loop):
            # Kill variables written by loop body
            self.kill_all_written_vars(stmt.loop_body)
            # Kill loop variable
            self.kill_integer_var(stmt.variable.name)
            # Have we reached the loop we'd like to parallelise?
            if stmt is self.loop_to_parallelise:
                self.in_loop_to_parallelise = True
                # Consider two arbitary but distinct iterations
                i_var = self.fresh_integer_var()
                j_var = self.fresh_integer_var()
                self.add_constraint(i_var != j_var)
                iteration_vars = [i_var, j_var]
            else:
                # Consider a single, arbitrary iteration
                i_var = self.fresh_integer_var()
                iteration_vars = [i_var]
            # Analyse loop body for each iteration variable separately
            for var in iteration_vars:
                self.save_subst()
                smt_loop_var = self.integer_var(stmt.variable.name)
                self.subst[smt_loop_var] = var
                # Introduce constraints on loop variable
                self.constrain_loop_var(
                    var, stmt.start_expr, stmt.stop_expr, stmt.step_expr)
                # Analyse loop body
                self.step(stmt.loop_body, cond)
                if stmt is self.loop_to_parallelise:
                    self.save_access_dict()
                self.restore_subst()
            # Record whether the analysis has finished
            if stmt is self.loop_to_parallelise:
                self.finished = True
            return

        # WhileLoop
        if isinstance(stmt, WhileLoop):
            # Kill variables written by loop body
            self.kill_all_written_vars(stmt.loop_body)
            # Add array accesses in condition
            if self.in_loop_to_parallelise:
                self.add_all_array_accesses(stmt.condition, cond)
            # Translate condition to SMT
            smt_condition = self.translate_cond_expr_with_subst(
              stmt.condition)
            # Recursively step into loop body
            self.save_subst()
            self.step(stmt.loop_body, z3.And(cond, smt_condition))
            self.restore_subst()
            return

        # Fall through
        if self.in_loop_to_parallelise:
            self.add_all_array_accesses(stmt, cond)
        self.kill_all_written_vars(stmt)

# Translating Fortran expressions to SMT formulae
# ===============================================


# Translate a scalar integer Fortran expression to SMT. In addition,
# return a constraint that prohibits bit vector overflow in the expression.
def translate_integer_expr(expr_root: Node,
                           int_width: int,
                           use_bv: bool) -> (z3.ExprRef, z3.BoolRef):
    constraints = []

    def trans(expr: Node) -> z3.ExprRef:
        # Check that type is a scalar integer of unspecified precision
        type_ok = _is_scalar_integer(expr.datatype)

        # Literal
        if isinstance(expr, Literal) and type_ok:
            if use_bv:
                return z3.BitVecVal(int(expr.value), int_width)
            else:
                return z3.IntVal(int(expr.value))

        # Reference
        if (isinstance(expr, Reference)
                and not isinstance(expr, ArrayReference)
                and type_ok):
            if use_bv:
                return z3.BitVec(expr.name, int_width)
            else:
                return z3.Int(expr.name)

        # UnaryOperation
        if isinstance(expr, UnaryOperation):
            arg_smt = trans(expr.operand)
            if expr.operator == UnaryOperation.Operator.MINUS:
                if use_bv:
                    constraints.append(z3.BVSNegNoOverflow(arg_smt))
                return -arg_smt
            if expr.operator == UnaryOperation.Operator.PLUS:
                return arg_smt

        # BinaryOperation
        if isinstance(expr, BinaryOperation):
            (left, right) = expr.operands
            left_smt = trans(left)
            right_smt = trans(right)

            if expr.operator == BinaryOperation.Operator.ADD:
                if use_bv:
                    constraints.append(z3.BVAddNoOverflow(
                      left_smt, right_smt, True))
                    constraints.append(z3.BVAddNoUnderflow(
                      left_smt, right_smt))
                return left_smt + right_smt
            if expr.operator == BinaryOperation.Operator.SUB:
                if use_bv:
                    constraints.append(z3.BVSubNoOverflow(
                      left_smt, right_smt))
                    constraints.append(z3.BVSubNoUnderflow(
                      left_smt, right_smt, True))
                return left_smt - right_smt
            if expr.operator == BinaryOperation.Operator.MUL:
                if use_bv:
                    constraints.append(z3.BVMulNoOverflow(
                      left_smt, right_smt, True))
                    constraints.append(z3.BVMulNoUnderflow(
                      left_smt, right_smt))
                return left_smt * right_smt
            if expr.operator == BinaryOperation.Operator.DIV:
                if use_bv:
                    constraints.append(z3.BVSDivNoOverflow(
                      left_smt, right_smt))
                return left_smt / right_smt

        # IntrinsicCall
        if isinstance(expr, IntrinsicCall):
            # Unary operators
            if expr.intrinsic == IntrinsicCall.Intrinsic.ABS:
                smt_arg = trans(expr.children[1])
                if use_bv:
                    constraints.append(z3.BVSNegNoOverflow(smt_arg))
                return z3.Abs(smt_arg)

            # Binary operators
            if expr.intrinsic in [IntrinsicCall.Intrinsic.SHIFTL,
                                  IntrinsicCall.Intrinsic.SHIFTR,
                                  IntrinsicCall.Intrinsic.SHIFTA,
                                  IntrinsicCall.Intrinsic.IAND,
                                  IntrinsicCall.Intrinsic.IOR,
                                  IntrinsicCall.Intrinsic.IEOR,
                                  IntrinsicCall.Intrinsic.MOD]:
                left_smt = trans(expr.children[1])
                right_smt = trans(expr.children[2])

                if expr.intrinsic == IntrinsicCall.Intrinsic.MOD:
                    return left_smt % right_smt

                if use_bv:
                    if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTL:
                        return left_smt << right_smt
                    if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTR:
                        return z3.LShR(left_smt, right_smt)
                    if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTA:
                        return left_smt >> right_smt
                    if expr.intrinsic == IntrinsicCall.Intrinsic.IAND:
                        return left_smt & right_smt
                    if expr.intrinsic == IntrinsicCall.Intrinsic.IOR:
                        return left_smt | right_smt
                    if expr.intrinsic == IntrinsicCall.Intrinsic.IEOR:
                        return left_smt ^ right_smt
                else:
                    if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTL:
                        return z3.BV2Int(z3.Int2BV(left_smt, int_width) <<
                                         z3.Int2BV(right_smt, int_width),
                                         is_signed=True)
                    if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTR:
                        return z3.BV2Int(z3.LShR(
                                 z3.Int2BV(left_smt, int_width),
                                 z3.Int2BV(right_smt, int_width)),
                                 is_signed=True)
                    if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTA:
                        return z3.BV2Int(z3.Int2BV(left_smt, int_width) >>
                                         z3.Int2BV(right_smt, int_width),
                                         is_signed=True)
                    if expr.intrinsic == IntrinsicCall.Intrinsic.IAND:
                        return z3.BV2Int(z3.Int2BV(left_smt, int_width) &
                                         z3.Int2BV(right_smt, int_width),
                                         is_signed=True)
                    if expr.intrinsic == IntrinsicCall.Intrinsic.IOR:
                        return z3.BV2Int(z3.Int2BV(left_smt, int_width) |
                                         z3.Int2BV(right_smt, int_width),
                                         is_signed=True)
                    if expr.intrinsic == IntrinsicCall.Intrinsic.IEOR:
                        return z3.BV2Int(z3.Int2BV(left_smt, int_width) ^
                                         z3.Int2BV(right_smt, int_width),
                                         is_signed=True)

            # N-ary operators
            if expr.intrinsic in [IntrinsicCall.Intrinsic.MIN,
                                  IntrinsicCall.Intrinsic.MAX]:
                smt_args = [trans(arg) for arg in expr.children[1:]]
                reduced = smt_args[0]
                for arg in smt_args[1:]:
                    if expr.intrinsic == IntrinsicCall.Intrinsic.MIN:
                        reduced = z3.If(reduced < arg, reduced, arg)
                    elif expr.intrinsic == IntrinsicCall.Intrinsic.MAX:
                        reduced = z3.If(reduced < arg, arg, reduced)
                return reduced

        # Fall through: return a fresh, unconstrained symbol
        if use_bv:
            return z3.FreshConst(z3.BitVecSort(int_width))
        else:
            return z3.FreshInt()

    expr_root_smt = trans(expr_root)
    return (expr_root_smt, z3.And(*constraints))


# Translate a scalar logical Fortran expression to SMT. In addition,
# return a constraint that prohibits bit vector overflow in the expression.
def translate_logical_expr(expr_root: Node,
                           int_width: int,
                           use_bv: bool) -> (z3.BoolRef, z3.BoolRef):
    # Constraints to prohibit bit-vector overflow
    overflow = []

    def trans(expr: Node):
        # Check that type is a scalar logical
        type_ok = _is_scalar_logical(expr.datatype)

        # Literal
        if isinstance(expr, Literal) and type_ok:
            if expr.value == "true":
                return z3.BoolVal(True)
            if expr.value == "false":
                return z3.BoolVal(False)

        # Reference
        if (isinstance(expr, Reference)
                and not isinstance(expr, ArrayReference)
                and type_ok):
            return z3.Bool(expr.name)

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
                (left_smt, prohibit_overflow) = translate_integer_expr(
                    left, int_width, use_bv)
                overflow.append(prohibit_overflow)
                (right_smt, prohibit_overflow) = translate_integer_expr(
                    right, int_width, use_bv)
                overflow.append(prohibit_overflow)

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
    return (expr_root_smt, z3.And(*overflow))

# Helper functions
# ================


# Check that type is a scalar integer of unspecified precision
def _is_scalar_integer(dt: DataType) -> bool:
    return (isinstance(dt, ScalarType) and
            dt.intrinsic == ScalarType.Intrinsic.INTEGER and
            dt.precision == ScalarType.Precision.UNDEFINED)


# Check that type is a scalar logical
def _is_scalar_logical(dt: DataType) -> bool:
    return (isinstance(dt, ScalarType) and
            dt.intrinsic == ScalarType.Intrinsic.BOOLEAN)
