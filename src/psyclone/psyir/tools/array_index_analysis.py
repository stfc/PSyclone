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
constraints over array indices which are then are passed to a third-party
solver via pySMT. We currently mandate use of the Z3 solver as it has a useful
timeout option, missing in other solvers.'''

# PySMT imports
import pysmt.shortcuts as smt
import pysmt.fnode as smt_fnode
import pysmt.typing as smt_types
import pysmt.logics as smt_logics
from pysmt.exceptions import SolverReturnedUnknownResultError

# PSyclone imports
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
    # Fortran integer width in bits
    int_width = 32

    def __init__(self, smt_timeout_ms=5000):
        # Set SMT solver timeout in milliseconds
        self.smt_timeout = smt_timeout_ms

    # Class representing an array access
    class ArrayAccess:
        def __init__(self,
                     cond:       smt_fnode.FNode,
                     is_write:   bool,
                     indices:    list[smt_fnode.FNode],
                     psyir_node: Node):
            # The condition at the location of the access
            self.cond = cond
            # Whether the access is a read or a write
            self.is_write = is_write
            # SMT expressions representing the indices of the access
            self.indices = indices
            # PSyIR Node for the access (useful for error reporting)
            self.psyir_node = psyir_node

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

    # Clear knowledge of 'var' by mapping it to a fresh, unconstrained symbol
    def kill_integer_var(self, var: str):
        fresh_sym = smt.FreshSymbol(typename=smt_types.BVType(self.int_width))
        smt_var = smt.Symbol(var, typename=smt_types.BVType(self.int_width))
        self.subst[smt_var] = fresh_sym

    # Clear knowledge of 'var' by mapping it to a fresh, unconstrained symbol
    def kill_logical_var(self, var: str):
        fresh_sym = smt.FreshSymbol(typename=smt_types.BOOL)
        smt_var = smt.Symbol(var, typename=smt_types.BOOL)
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
    def add_constraint(self, smt_expr: smt_fnode.FNode):
        self.constraints.append(smt_expr)

    # Add an integer assignment constraint to the constraint set
    def add_integer_assignment(self, var: str, smt_expr: smt_fnode.FNode):
        # Create a fresh symbol
        fresh_sym = smt.FreshSymbol(typename=smt_types.BVType(self.int_width))
        # Assert equality between this symbol and the given SMT expression
        self.add_constraint(smt.Equals(fresh_sym, smt_expr))
        # Update the substitution
        smt_var = smt.Symbol(var, typename=smt_types.BVType(self.int_width))
        self.subst[smt_var] = fresh_sym

    # Add a logical assignment constraint to the constraint set
    def add_logical_assignment(self, var: str, smt_expr: smt_fnode.FNode):
        # Create a fresh symbol
        fresh_sym = smt.FreshSymbol(typename=smt_types.BOOL)
        # Assert equality between this symbol and the given SMT expression
        self.add_constraint(smt.Iff(fresh_sym, smt_expr))
        # Update the substitution
        smt_var = smt.Symbol(var, typename=smt_types.BOOL)
        self.subst[smt_var] = fresh_sym

    # Translate integer expresison to SMT, and apply current substitution
    def translate_integer_expr_with_subst(self, expr: smt_fnode.FNode):
        smt_expr = translate_integer_expr(expr, self.int_width)
        return smt_expr.substitute(self.subst)

    # Translate logical expresison to SMT, and apply current substitution
    def translate_logical_expr_with_subst(self, expr: smt_fnode.FNode):
        smt_expr = translate_logical_expr(expr, self.int_width)
        return smt_expr.substitute(self.subst)

    # Constrain a loop variable to given start/stop/step
    def constrain_loop_var(self,
                           var:   smt_fnode.FNode,
                           start: DataNode,
                           stop:  DataNode,
                           step:  DataNode):
        zero = smt.SBV(0, self.int_width)
        var_begin = self.translate_integer_expr_with_subst(start)
        var_end = self.translate_integer_expr_with_subst(stop)
        if step is None:
            step = Literal("1", INTEGER_TYPE)  # pragma: no cover
        var_step = self.translate_integer_expr_with_subst(step)
        self.add_constraint(smt.And(
          # (var - var_begin) % var_step == 0
          smt.Equals(smt.BVSRem(smt.BVSub(var, var_begin), var_step), zero),
          # var_step > 0 ==> var >= var_begin
          smt.Implies(smt.BVSGT(var_step, zero),
                      smt.BVSGE(var, var_begin)),
          # var_step < 0 ==> var <= var_begin
          smt.Implies(smt.BVSLT(var_step, zero),
                      smt.BVSLE(var, var_begin)),
          # var_step > 0 ==> var <= var_end
          smt.Implies(smt.BVSGT(var_step, zero),
                      smt.BVSLE(var, var_end)),
          # var_step < 0 ==> var >= var_end
          smt.Implies(smt.BVSLT(var_step, zero),
                      smt.BVSGE(var, var_end))))

    # Add an array access to the current access dict
    def add_array_access(self,
                         array_name: str,
                         is_write:   bool,
                         cond:       smt_fnode.FNode,
                         indices:    list[smt_fnode.FNode],
                         psyir_node: Node):
        access = ArrayIndexAnalysis.ArrayAccess(
                   cond, is_write, indices, psyir_node)
        if array_name not in self.access_dict:
            self.access_dict[array_name] = []
        self.access_dict[array_name].append(access)

    # Add all array accesses in the given node to the current access dict
    def add_all_array_accesses(self, node: Node, cond: smt_fnode.FNode):
        var_accesses = node.reference_accesses()
        for sig, access_seq in var_accesses.items():
            for access_info in access_seq:
                if access_info.is_data_access:

                    # ArrayReference
                    if isinstance(access_info.node, ArrayReference):
                        indices = []
                        for index in access_info.node.indices:
                            if isinstance(index, Range):
                                var = smt.FreshSymbol(
                                        typename=smt_types.BVType(
                                          self.int_width))
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
                            var = smt.FreshSymbol(
                              typename=smt_types.BVType(self.int_width))
                            indices.append(var)
                        self.add_array_access(
                            sig.var_name, access_info.is_any_write(),
                            cond, indices, access_info.node)

    # Move the current access dict to the stack, and proceed with an empty one
    def save_access_dict(self):
        self.saved_access_dicts.append(self.access_dict)
        self.access_dict = {}

    # Check if the given loop has a conflict
    def is_loop_conflict_free(self, loop: Loop) -> tuple[Node, Node]:
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
        smt.reset_env()

        # Step through body of the enclosing routine, statement by statement
        for stmt in routine.children:
            self.step(stmt, smt.TRUE())

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
                            indices_equal.append(smt.Equals(i_idx, j_idx))
                        conflicts.append(smt.And(
                          *indices_equal,
                          i_access.cond,
                          j_access.cond))

        # Invoke Z3 solver with a timeout
        solver = smt.Solver(name='z3',
                            logic=smt_logics.QF_BV,
                            generate_models=False,
                            incremental=False,
                            solver_options={'timeout': self.smt_timeout})
        try:
            return not solver.is_sat(smt.And(*self.constraints,
                                             smt.Or(conflicts)))
        except SolverReturnedUnknownResultError:  # pragma: no cover
            return None                           # pragma: no cover

    # Analyse a single statement
    def step(self, stmt: Node, cond: smt_fnode.FNode):
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
            smt_condition = self.translate_logical_expr_with_subst(
                              stmt.condition)
            # Recursively step into 'then'
            if stmt.if_body:
                self.save_subst()
                self.step(stmt.if_body, smt.And(cond, smt_condition))
                self.restore_subst()
            # Recursively step into 'else'
            if stmt.else_body:
                self.save_subst()
                self.step(stmt.else_body,
                          smt.And(cond, smt.Not(smt_condition)))
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
                i_var = smt.FreshSymbol(
                          typename=smt_types.BVType(self.int_width))
                j_var = smt.FreshSymbol(
                          typename=smt_types.BVType(self.int_width))
                self.add_constraint(smt.NotEquals(i_var, j_var))
                iteration_vars = [i_var, j_var]
            else:
                # Consider a single, arbitrary iteration
                i_var = smt.FreshSymbol(
                          typename=smt_types.BVType(self.int_width))
                iteration_vars = [i_var]
            # Analyse loop body for each iteration variable separately
            for var in iteration_vars:
                self.save_subst()
                smt_loop_var = smt.Symbol(
                    stmt.variable.name,
                    typename=smt_types.BVType(self.int_width))
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
          smt_condition = self.translate_logical_expr_with_subst(
            stmt.condition)
          # Recursively step into loop body
          self.save_subst()
          self.step(stmt.loop_body, smt.And(cond, smt_condition))
          self.restore_subst()
          return

        # Fall through
        if self.in_loop_to_parallelise:
            self.add_all_array_accesses(stmt, cond)
        self.kill_all_written_vars(stmt)

# Translating Fortran expressions to SMT formulae
# ===============================================


# Translate a scalar integer Fortran expression to SMT
def translate_integer_expr(expr_root: Node, int_width: int):
    # SMT type to represent Fortran integers
    bv_int_t = smt_types.BVType(int_width)

    def trans(expr: Node):
        # Check that type is a scalar integer of unspecified precision
        type_ok = _is_scalar_integer(expr.datatype)

        # Literal
        if isinstance(expr, Literal) and type_ok:
            return smt.SBV(int(expr.value), int_width)

        # Reference
        if (isinstance(expr, Reference)
                and not isinstance(expr, ArrayReference)
                and type_ok):
            return smt.Symbol(expr.name, typename=bv_int_t)

        # UnaryOperation
        if isinstance(expr, UnaryOperation):
            arg_smt = trans(expr.operand)
            if expr.operator == UnaryOperation.Operator.MINUS:
                return smt.BVNeg(arg_smt)
            if expr.operator == UnaryOperation.Operator.PLUS:
                return arg_smt

        # BinaryOperation
        if isinstance(expr, BinaryOperation):
            (left, right) = expr.operands
            left_smt = trans(left)
            right_smt = trans(right)

            if expr.operator == BinaryOperation.Operator.ADD:
                return smt.BVAdd(left_smt, right_smt)
            if expr.operator == BinaryOperation.Operator.SUB:
                return smt.BVSub(left_smt, right_smt)
            if expr.operator == BinaryOperation.Operator.MUL:
                return smt.BVMul(left_smt, right_smt)
            if expr.operator == BinaryOperation.Operator.DIV:
                return smt.BVSDiv(left_smt, right_smt)

        # IntrinsicCall
        if isinstance(expr, IntrinsicCall):
            # Unary operators
            if expr.intrinsic == IntrinsicCall.Intrinsic.ABS:
                zero = smt.BVZero(int_width)
                smt_arg = trans(expr.children[1])
                return smt.Ite(smt.BVSLT(smt_arg, zero),
                               smt.BVNeg(smt_arg),
                               smt_arg)

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

                if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTL:
                    return smt.BVLShl(left_smt, right_smt)
                if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTR:
                    return smt.BVLShr(left_smt, right_smt)
                if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTA:
                    return smt.BVAShr(left_smt, right_smt)
                if expr.intrinsic == IntrinsicCall.Intrinsic.IAND:
                    return smt.BVAnd(left_smt, right_smt)
                if expr.intrinsic == IntrinsicCall.Intrinsic.IOR:
                    return smt.BVOr(left_smt, right_smt)
                if expr.intrinsic == IntrinsicCall.Intrinsic.IEOR:
                    return smt.BVXor(left_smt, right_smt)
                # TODO: does BVSRem match the semantics of Fortran MOD?
                if expr.intrinsic == IntrinsicCall.Intrinsic.MOD:
                    return smt.BVSRem(left_smt, right_smt)

            # N-ary operators
            if expr.intrinsic in [IntrinsicCall.Intrinsic.MIN,
                                  IntrinsicCall.Intrinsic.MAX]:
                smt_args = [trans(arg) for arg in expr.children[1:]]
                reduced = smt_args[0]
                for arg in smt_args[1:]:
                    if expr.intrinsic == IntrinsicCall.Intrinsic.MIN:
                        reduced = smt.Ite(smt.BVSLT(reduced, arg),
                                          reduced, arg)
                    elif expr.intrinsic == IntrinsicCall.Intrinsic.MAX:
                        reduced = smt.Ite(smt.BVSLT(reduced, arg),
                                          arg, reduced)
                return reduced

        # Fall through: return a fresh, unconstrained symbol
        return smt.FreshSymbol(typename=bv_int_t)

    return trans(expr_root)


# Translate a scalar logical Fortran expression to SMT
def translate_logical_expr(expr_root: Node, int_width: int):
    def trans(expr: Node):
        # Check that type is a scalar logical
        type_ok = _is_scalar_logical(expr.datatype)

        # Literal
        if isinstance(expr, Literal) and type_ok:
            if expr.value == "true":
                return smt.TRUE()
            if expr.value == "false":
                return smt.FALSE()

        # Reference
        if (isinstance(expr, Reference)
                and not isinstance(expr, ArrayReference)
                and type_ok):
            return smt.Symbol(expr.name, typename=smt_types.BOOL)

        # UnaryOperation
        if isinstance(expr, UnaryOperation):
            arg_smt = trans(expr.operand)
            if expr.operator == UnaryOperation.Operator.NOT:
                return smt.Not(arg_smt)

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
                    return smt.And(left_smt, right_smt)
                if expr.operator == BinaryOperation.Operator.OR:
                    return smt.Or(left_smt, right_smt)
                if expr.operator == BinaryOperation.Operator.EQV:
                    return smt.Iff(left_smt, right_smt)
                if expr.operator == BinaryOperation.Operator.NEQV:
                    return smt.Not(smt.Iff(left_smt, right_smt))

            # Operands are numbers
            if expr.operator in [BinaryOperation.Operator.EQ,
                                 BinaryOperation.Operator.NE,
                                 BinaryOperation.Operator.GT,
                                 BinaryOperation.Operator.LT,
                                 BinaryOperation.Operator.GE,
                                 BinaryOperation.Operator.LE]:
                (left, right) = expr.operands
                left_smt = translate_integer_expr(left, int_width)
                right_smt = translate_integer_expr(right, int_width)

                if expr.operator == BinaryOperation.Operator.EQ:
                    return smt.Equals(left_smt, right_smt)
                if expr.operator == BinaryOperation.Operator.NE:
                    return smt.NotEquals(left_smt, right_smt)
                if expr.operator == BinaryOperation.Operator.GT:
                    return smt.BVSGT(left_smt, right_smt)
                if expr.operator == BinaryOperation.Operator.LT:
                    return smt.BVSLT(left_smt, right_smt)
                if expr.operator == BinaryOperation.Operator.GE:
                    return smt.BVSGE(left_smt, right_smt)
                if expr.operator == BinaryOperation.Operator.LE:
                    return smt.BVSLE(left_smt, right_smt)

        # Fall through: return a fresh, unconstrained symbol
        return smt.FreshSymbol(typename=smt_types.BOOL)

    return trans(expr_root)

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
