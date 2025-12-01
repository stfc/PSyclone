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

'''This module provides a class to determine whether or not distinct
iterations of a given loop can generate conflicting array accesses (if
not, the loop can potentially be parallelised).  It formulates the
problem as a set of SMT constraints over array indices which are then
are passed to the Z3 solver.'''

import z3
import random
import threading
from psyclone.psyir.nodes import Loop, DataNode, Literal, Assignment, \
    Reference, UnaryOperation, BinaryOperation, IntrinsicCall, \
    Routine, Node, IfBlock, Schedule, ArrayReference, Range, WhileLoop
from psyclone.psyir.symbols import DataType, ScalarType, ArrayType, \
    INTEGER_TYPE

# Outline
# =======
#
# The analysis class provides a method 'is_loop_conflict_free()' to
# decide whether or not the array accesses in a given loop are
# conflicting between iterations. Two array accesses are conflicting
# if they access the same element of the same array, and at least one
# of the accesses is a write.
#
# The analysis assumes that any scalar integer or scalar logical
# variables written by the loop can safely be considered as private
# to each iteration. This should be validated by the callee.
#
# The analysis algorithm operates, broadly, as follows.
#
# Given a loop, we find its enclosing routine, and start analysing the
# routine statement-by-statement in a recursive-descent fashion.
#
# As we proceed, we maintain a set of SMT constraints and a
# substitution that maps Fortran variable names to SMT variable names.
# For each Fortran variable, the substitution points to an SMT
# variable that is constrained (in the set of constraints) such that
# it captures the value of the Fortran variable at the current point
# in the code. When a Fortran variable is mutated, the substitution is
# modified to point to a fresh SMT variable, with new constraints,
# without destroying the old constraints.
#
# More concretely, when we encounter an assignment of a scalar
# integer/logical variable, of the form 'x = rhs', we translate 'rhs'
# to the SMT formula 'smt_rhs' with the current substitution applied.
# We then add a constraint 'var = smt_rhs' where 'var' is a fresh SMT
# variable, and update the substition so that 'x' maps to 'var'.
#
# The Fortran-expression-to-SMT translator knows about several Fortran
# operators and intrinsics, but not all of them; when it sees
# something it doesn't know about, it simply translates it to a fresh
# unconstrained SMT variable.
#
# Sometimes we reach a statement that modifies a Fortran variable in
# an unknown way (e.g. calling a subroutine). This can be handled by
# updating the substitution to point to a fresh unconstrained SMT
# variable; we refer to this process as "killing" the variable.
#
# In addition to the current substitution, we maintain a stack of
# previous substitutions. This allows substitutions to be saved and
# restored before and after analysing a block of code that may or may
# not be executed at run time.
#
# We also maintain a "current condition".  This can be viewed as a
# constraint that has not been comitted to the constraint set because
# we want to be able to grow, contract, and retract it as we enter and
# exit conditional blocks of code. This current condition is passed in
# recursive calls, so there is an implicit stack of them.
#
# More concretely, when we encouter an 'if' statement, we copy the
# current substitution onto the stack, then recurse into the 'then'
# body, passing in the 'if' condition as an argument, and then restore
# the old substitution. We do the same for the 'else' body if there is
# one (in this case the negated condition is passed to the recursive
# call). Finally, we kill all variables written by the 'then' and
# 'else' bodies, because we don't know which will be executed at run
# time. (In future, we could do better here by introducing OR
# constraints, e.g. each variable written is either equal to the value
# written in the 'then' OR the 'else' depending on the condition.)
#
# As the analysis proceeds, we also maintain a list of array accesses.
# For each access, we record various information including the name of
# the array, whether it is a read or a write, the current condition at
# the point the access is made, and its list of indices (translated to
# SMT). When we are analysing code that is inside the loop of
# interest, we add all array accesses encountered to the list.
#
# When we encounter the loop of interest, we perform a couple of steps
# before recursing into the loop body. First, we kill all variables
# written by the loop body, because we don't know whether we are
# entering the loop (at run time) for the first time or not. Second,
# we create two SMT variables to represent two arbitary but distinct
# iterations of the loop. Each variable is constrained to the start,
# stop, and step of the loop, and the two variables are constrained to
# be not equal. After that, we analyse the loop body twice, each time
# mapping the loop variable in the substitution to each of the SMT
# loop variables.  After analysing the loop body for the first time,
# we save the array access list and start afresh with a new one.
# Therefore, once the analysis is complete, we have two array access
# lists, one for each iteration.
#
# When we encounter a loop that is not the loop of interest, we follow
# a similar approach but only consider a single arbitrary iteration of
# the loop.
#
# When the recursive descent is complete, we are left with two array
# access lists. We are interested in whether any pair of accesses to
# the same array (in which one of the accesses is a write) represents
# a conflict.  An access pair is conflict-free if an equality
# constraint between each access's indices, when combined with the
# current condition of each access and the global constraint set, is
# unsatisfiable.  In this way, we can check every access pair and
# determine whether or not the loop is conflict free.


# Analysis Options
# ================

class ArrayIndexAnalysisOptions:
    '''The analysis supports a range of different options, which are all
    captured together in this class.

    :param int_width: the bit width of Fortran integers. This is 32 by
       default but it can be useful to reduce it to (say) 8 in particular
       cases to improve the ability of solver to find a timely solution,
       provided the user considers it safe to do so.  (Note that the analysis
       currently only gathers information about Fortran integer values of
       unspecified width.)

    :param use_bv: whether to treat Fortran integers as bit vectors or
       arbitrary-precision integers. If None is specified then the
       analysis will use a simple heuristic to decide.

    :param smt_timeout_ms: the time limit (in milliseconds) given to
       the SMT solver to find a solution. If the solver does not
       return within this time, the analysis will conservatively return
       that a conflict exists even though it has not yet found one.

    :param prohibit_overflow: if True, the analysis will tell the solver
       to ignore the possibility of integer overflow. Integer overflow is
       undefined behaviour in Fortran so this is safe.

    :param handle_array_intrins: handle array intrinsics 'size()',
       'lbound()', and 'ubound()' specially. For example, multiple
       occurences of 'size(arr)' will be assumed to return the same value,
       provided that those occurrences are not separated by a statement
       that may modify the size/bounds of 'arr'.

    :param num_sweep_threads: when larger than one, this option enables the
       sweeper, which runs multiple solvers across multiple threads with each
       one using a different constraint ordering (and potentially different
       solver parameters in future). This reduces the solver's sensitivity
       to the order of constraints.

    :param sweep_seed: the seed for the random number generator used
      by the sweeper.
    '''
    def __init__(self,
                 int_width: int = 32,
                 use_bv: bool = None,
                 smt_timeout_ms: int = 5000,
                 prohibit_overflow: bool = False,
                 handle_array_intrins: bool = True,
                 num_sweep_threads: int = 4,
                 sweep_seed: int = 1):
        self.smt_timeout = smt_timeout_ms
        self.int_width = int_width
        self.use_bv = use_bv
        self.prohibit_overflow = prohibit_overflow
        self.handle_array_intrins = handle_array_intrins
        self.num_sweep_threads = num_sweep_threads
        self.sweep_seed = sweep_seed


# Analysis
# ========

class ArrayIndexAnalysis:
    class ArrayAccess:
        '''This class is used to record details of each array access
        encountered during the analysis.

        :param cond: a boolean SMT expression representing the current
           condition at the point the array access is made.

        :param is_write: whether the access is a read or a write.

        :param indices: SMT integer expressions representing the
          indices of the array access.

        :param psyir_node: PSyIR node for the access (useful for reporting
           conflict messages / errors).
        '''
        def __init__(self,
                     cond:       z3.BoolRef,
                     is_write:   bool,
                     indices:    list[list[z3.ExprRef]],
                     psyir_node: Node):
            self.cond = cond
            self.is_write = is_write
            self.indices = indices
            self.psyir_node = psyir_node

    def __init__(self, options=ArrayIndexAnalysisOptions()):
        '''This class provides a method 'is_loop_conflict_free()' to
        determine whether or not distinct iterations of a given loop
        can generate conflicting array accesses.

        :param options: these options allow user control over features
           provided by, and choices made by, the analysis.
        '''
        self.opts = options

    def _init_analysis(self):
        '''Intialise the analysis by setting all the internal state
        varibles accordingly.'''

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
        # We map array intrinsic calls (e.g. size, lbound, ubound) to SMT
        # integer variables. The following dict maps array names to a
        # set of integer variable names holding the results of intrinsic
        # calls on that array.
        self.array_intrins_vars = {}

    def _init_array_intrins_vars(self, routine: Routine):
        '''Initialise the 'array_intrins_vars' dict so that, for each
        array accessed, it holds a set of integer variables
        representing the results of intrinsics (such as size,
        lbound, ubound) applied to that array.

        :param routine: the Routine holding the Loop that we are
           analysing for conflicts.
        '''
        if self.opts.handle_array_intrins:
            for stmt in routine.children:
                for call in stmt.walk(IntrinsicCall):
                    intrins_pair = translate_array_intrinsic_call(call)
                    if intrins_pair:
                        (arr_name, var_name) = intrins_pair
                        if arr_name not in self.array_intrins_vars:
                            self.array_intrins_vars[arr_name] = set()
                        self.array_intrins_vars[arr_name].add(var_name)

    def _save_subst(self):
        '''Push copy of current substitution to the stack.'''
        self.subst_stack.append(self.subst.copy())

    def _restore_subst(self):
        '''Pop substitution from stack into current substitution.'''
        self.subst = self.subst_stack.pop()

    def _fresh_integer_var(self) -> z3.ExprRef:
        '''Create an fresh SMT integer variable.'''
        if self.opts.use_bv:
            return z3.FreshConst(z3.BitVecSort(self.opts.int_width))
        else:
            return z3.FreshInt()

    def _integer_var(self, var: str) -> z3.ExprRef:
        '''Create an integer SMT variable with the given name.'''
        if self.opts.use_bv:
            return z3.BitVec(var, self.opts.int_width)
        else:
            return z3.Int(var)

    def _integer_val(self, val: int) -> z3.ExprRef:
        '''Create an SMT integer value.'''
        if self.opts.use_bv:
            return z3.BitVecVal(val, self.opts.int_width)
        else:
            return z3.IntVal(val)

    def _kill_integer_var(self, var: str):
        '''Clear knowledge of integer 'var' by mapping it to a fresh,
        unconstrained symbol.'''
        fresh_sym = self._fresh_integer_var()
        smt_var = self._integer_var(var)
        self.subst[smt_var] = fresh_sym

    def _kill_logical_var(self, var: str):
        '''Clear knowledge of logical 'var' by mapping it to a fresh,
        unconstrained symbol'''
        fresh_sym = z3.FreshBool()
        smt_var = z3.Bool(var)
        self.subst[smt_var] = fresh_sym

    def _kill_all_written_vars(self, node: Node):
        '''Kill all scalar integer/logical variables written inside 'node'.'''
        var_accesses = node.reference_accesses()
        for sig, access_seq in var_accesses.items():
            for access_info in access_seq.all_write_accesses:
                if isinstance(access_info.node, Loop):
                    self._kill_integer_var(sig.var_name)
                    break
                elif isinstance(access_info.node, Reference):
                    if _is_scalar_integer(access_info.node.datatype):
                        self._kill_integer_var(sig.var_name)
                        break
                    elif _is_scalar_logical(access_info.node.datatype):
                        self._kill_logical_var(sig.var_name)
                        break
                    elif isinstance(access_info.node.datatype, ArrayType):
                        # If an array variable is modified we kill intrinsic
                        # vars associated with it. This is overly safe:
                        # we probably only need to kill these vars if the
                        # array is passed to a mutating routine/intrinsic.
                        if sig.var_name in self.array_intrins_vars:
                            for v in self.array_intrins_vars[sig.var_name]:
                                self._kill_integer_var(v)
                        break

    def _add_constraint(self, smt_expr: z3.BoolRef):
        '''Add the SMT constraint to the constraint set.'''
        self.constraints.append(smt_expr)

    def _add_integer_assignment(self, var: str, smt_expr: z3.ExprRef):
        '''Add an integer assignment constraint to the constraint set.'''
        # Create a fresh symbol
        fresh_sym = self._fresh_integer_var()
        # Assert equality between this symbol and the given SMT expression
        self._add_constraint(fresh_sym == smt_expr)
        # Update the substitution
        smt_var = self._integer_var(var)
        self.subst[smt_var] = fresh_sym

    def _add_logical_assignment(self, var: str, smt_expr: z3.BoolRef):
        '''Add a logical assignment constraint to the constraint set.'''
        # Create a fresh symbol
        fresh_sym = z3.FreshBool()
        # Assert equality between this symbol and the given SMT expression
        self._add_constraint(fresh_sym == smt_expr)
        # Update the substitution
        smt_var = z3.Bool(var)
        self.subst[smt_var] = fresh_sym

    def _translate_integer_expr_with_subst(self, expr: Node):
        '''Translate the given integer expresison to SMT, and apply the
        current substitution.'''
        (smt_expr, prohibit_overflow) = translate_integer_expr(
            expr, self.opts)
        subst_pairs = list(self.subst.items())
        if self.opts.prohibit_overflow:
            self._add_constraint(
              z3.substitute(prohibit_overflow, *subst_pairs))
        return z3.substitute(smt_expr, *subst_pairs)

    def _translate_logical_expr_with_subst(self, expr: Node):
        '''Translate the given logical expresison to SMT, and apply the
        current substitution.'''
        (smt_expr, prohibit_overflow) = translate_logical_expr(
            expr, self.opts)
        subst_pairs = list(self.subst.items())
        if self.opts.prohibit_overflow:
            self._add_constraint(
              z3.substitute(prohibit_overflow, *subst_pairs))
        return z3.substitute(smt_expr, *subst_pairs)

    def _translate_cond_expr_with_subst(self, expr: Node):
        '''Translate the given conditional expresison to SMT, and apply
        the current substitution. Instead of adding constraints to
        the constraint set, this function ANDs constraints with the
        translated expression.'''
        (smt_expr, prohibit_overflow) = translate_logical_expr(
            expr, self.opts)
        if self.opts.prohibit_overflow:
            smt_expr = z3.And(smt_expr, prohibit_overflow)
        subst_pairs = list(self.subst.items())
        return z3.substitute(smt_expr, *subst_pairs)

    def _constrain_loop_var(self,
                            var:   z3.ExprRef,
                            start: DataNode,
                            stop:  DataNode,
                            step:  DataNode):
        '''Constrain a loop variable to given start/stop/step.'''
        zero = self._integer_val(0)
        var_begin = self._translate_integer_expr_with_subst(start)
        var_end = self._translate_integer_expr_with_subst(stop)
        if step is None:
            step = Literal("1", INTEGER_TYPE)  # pragma: no cover
        var_step = self._translate_integer_expr_with_subst(step)
        i = self._fresh_integer_var()
        self._add_constraint(var_step != zero)
        self._add_constraint(
          z3.Implies(var_step > zero,
                     z3.And(var >= var_begin, var <= var_end)))
        self._add_constraint(
          z3.Implies(var_step < zero,
                     z3.And(var <= var_begin, var >= var_end)))
        self._add_constraint(var == var_begin + i * var_step)
        self._add_constraint(i >= zero)
        # Prohibit overflow/underflow of "i * var_step"
        if self.opts.use_bv and self.opts.prohibit_overflow:
            self._add_constraint(z3.BVMulNoOverflow(i, var_step, True))
            self._add_constraint(z3.BVMulNoUnderflow(i, var_step))

    def _add_array_access(self, array_name: str, access: ArrayAccess):
        '''Add an array access to the current access dict.'''
        if array_name not in self.access_dict:
            self.access_dict[array_name] = []
        self.access_dict[array_name].append(access)

    def _add_all_array_accesses(self, node: Node, cond: z3.BoolRef):
        '''Add all array accesses in the given node to the current
        access dict.'''
        var_accesses = node.reference_accesses()
        for sig, access_seq in var_accesses.items():
            for access_info in access_seq:
                if isinstance(access_info.node, Reference):
                    (_, indices) = access_info.node.get_signature_and_indices()
                    indices_flat = [i for inds in indices for i in inds]
                    is_array_access = (
                      access_info.is_data_access and
                      (indices_flat != [] or
                       isinstance(access_info.node.datatype, ArrayType)))
                    if is_array_access:
                        smt_indices = []
                        for inds in indices:
                            smt_inds = []
                            for ind in inds:
                                if isinstance(ind, Range):
                                    var = self._fresh_integer_var()
                                    self._constrain_loop_var(
                                      var, ind.start, ind.stop, ind.step)
                                    smt_inds.append(var)
                                else:
                                    smt_inds.append(
                                      self._translate_integer_expr_with_subst(
                                        ind))
                            smt_indices.append(smt_inds)
                        self._add_array_access(
                            str(sig),
                            ArrayIndexAnalysis.ArrayAccess(
                              cond, access_info.is_any_write(),
                              smt_indices, access_info.node))

    def _save_access_dict(self):
        '''Move the current access dict to the stack, and proceed with
        an empty one.'''
        self.saved_access_dicts.append(self.access_dict)
        self.access_dict = {}

    def is_loop_conflict_free(self, loop: Loop) -> bool:
        '''Determine whether or not distinct iterations of the given loop
        can generate conflicting array accesses.'''

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
        self._init_analysis()
        self.loop_to_parallelise = loop
        self._init_array_intrins_vars(routine)

        # Resolve choice of integers v. bit vectors
        if self.opts.use_bv is None:
            for call in routine.walk(IntrinsicCall):
                i = call.intrinsic
                if i in [IntrinsicCall.Intrinsic.SHIFTL,
                         IntrinsicCall.Intrinsic.SHIFTR,
                         IntrinsicCall.Intrinsic.SHIFTA,
                         IntrinsicCall.Intrinsic.IAND,
                         IntrinsicCall.Intrinsic.IOR,
                         IntrinsicCall.Intrinsic.IEOR]:
                    self.opts.use_bv = True
                    break

        # Step through body of the enclosing routine, statement by statement
        for stmt in routine.children:
            self._step(stmt, z3.BoolVal(True))

        # Check that we have found and analysed the loop to parallelise
        if not (self.finished and len(self.saved_access_dicts) == 2):
            return None  # pragma: no cover

        # A list of lists holding a sum (OR) of products (AND) of constraints
        sum_of_prods = []

        # Forumlate constraints for solving, considering the two iterations
        iter_i = self.saved_access_dicts[0]
        iter_j = self.saved_access_dicts[1]
        for (i_arr_name, i_accesses) in iter_i.items():
            for (j_arr_name, j_accesses) in iter_j.items():
                if (i_arr_name == j_arr_name or
                        i_arr_name.startswith(j_arr_name + "%") or
                        j_arr_name.startswith(i_arr_name + "%")):
                    # For each write access in the i iteration
                    for i_access in i_accesses:
                        if i_access.is_write:
                            # Check for conflicts against every access in the
                            # j iteration
                            for j_access in j_accesses:
                                indices_equal = []
                                for (i_idxs, j_idxs) in zip(i_access.indices,
                                                            j_access.indices):
                                    for (i_idx, j_idx) in zip(i_idxs, j_idxs):
                                        indices_equal.append(i_idx == j_idx)
                                sum_of_prods.append(indices_equal +
                                                    [i_access.cond] +
                                                    [j_access.cond])

        # Solve the constraints
        if self.opts.num_sweep_threads <= 1:
            s = z3.Solver()
            s.set("timeout", self.opts.smt_timeout)
            s.add(z3.And(self.constraints))
            s.add(z3.Or([z3.And(prod) for prod in sum_of_prods]))
            result = s.check()
        else:
            result = self._sweep_solve(sum_of_prods)
        return result == z3.unsat

    def _step(self, stmt: Node, cond: z3.BoolRef):
        '''Analyse the given statement in recursive-descent fashion.'''

        # Has analysis finished?
        if self.finished:
            return

        # Assignment
        if isinstance(stmt, Assignment):
            if isinstance(stmt.lhs, Reference):
                (sig, indices) = stmt.lhs.get_signature_and_indices()
                indices_flat = [i for inds in indices for i in inds]
                if indices_flat == [] and len(sig) == 1:
                    if _is_scalar_integer(stmt.lhs.datatype):
                        rhs_smt = self._translate_integer_expr_with_subst(
                                    stmt.rhs)
                        self._add_integer_assignment(sig.var_name, rhs_smt)
                        if self.in_loop_to_parallelise:
                            self._add_all_array_accesses(stmt.rhs, cond)
                        return
                    elif _is_scalar_logical(stmt.lhs.datatype):
                        rhs_smt = self._translate_logical_expr_with_subst(
                                    stmt.rhs)
                        self._add_logical_assignment(sig.var_name, rhs_smt)
                        if self.in_loop_to_parallelise:
                            self._add_all_array_accesses(stmt.rhs, cond)
                        return

        # Schedule
        if isinstance(stmt, Schedule):
            for child in stmt.children:
                self._step(child, cond)
            return

        # IfBlock
        if isinstance(stmt, IfBlock):
            if self.in_loop_to_parallelise:
                self._add_all_array_accesses(stmt.condition, cond)
            # Translate condition to SMT
            smt_cond = self._translate_cond_expr_with_subst(stmt.condition)
            # Recursively step into 'then'
            if stmt.if_body:
                self._save_subst()
                self._step(stmt.if_body, z3.And(cond, smt_cond))
                self._restore_subst()
            # Recursively step into 'else'
            if stmt.else_body:
                self._save_subst()
                self._step(stmt.else_body,
                           z3.And(cond, z3.Not(smt_cond)))
                self._restore_subst()
            # Kill vars written by each branch
            if stmt.if_body:
                self._kill_all_written_vars(stmt.if_body)
            if stmt.else_body:
                self._kill_all_written_vars(stmt.else_body)
            return

        # Loop
        if isinstance(stmt, Loop):
            # Kill variables written by loop body
            self._kill_all_written_vars(stmt.loop_body)
            # Kill loop variable
            self._kill_integer_var(stmt.variable.name)
            # Have we reached the loop we'd like to parallelise?
            if stmt is self.loop_to_parallelise:
                self.in_loop_to_parallelise = True
                # Consider two arbitary but distinct iterations
                i_var = self._fresh_integer_var()
                j_var = self._fresh_integer_var()
                self._add_constraint(i_var != j_var)
                iteration_vars = [i_var, j_var]
            else:
                # Consider a single, arbitrary iteration
                i_var = self._fresh_integer_var()
                iteration_vars = [i_var]
            # Analyse loop body for each iteration variable separately
            for var in iteration_vars:
                self._save_subst()
                smt_loop_var = self._integer_var(stmt.variable.name)
                self.subst[smt_loop_var] = var
                # Introduce constraints on loop variable
                self._constrain_loop_var(
                    var, stmt.start_expr, stmt.stop_expr, stmt.step_expr)
                # Analyse loop body
                self._step(stmt.loop_body, cond)
                if stmt is self.loop_to_parallelise:
                    self._save_access_dict()
                self._restore_subst()
            # Record whether the analysis has finished
            if stmt is self.loop_to_parallelise:
                self.finished = True
            return

        # WhileLoop
        if isinstance(stmt, WhileLoop):
            # Kill variables written by loop body
            self._kill_all_written_vars(stmt.loop_body)
            # Add array accesses in condition
            if self.in_loop_to_parallelise:
                self._add_all_array_accesses(stmt.condition, cond)
            # Translate condition to SMT
            smt_condition = self._translate_cond_expr_with_subst(
              stmt.condition)
            # Recursively step into loop body
            self._save_subst()
            self._step(stmt.loop_body, z3.And(cond, smt_condition))
            self._restore_subst()
            return

        # Fall through
        if self.in_loop_to_parallelise:
            self._add_all_array_accesses(stmt, cond)
        self._kill_all_written_vars(stmt)

    def _sweep_solve(self, sum_of_prods: list[list[z3.BoolRef]]):
        '''The solver is quite sensitive to the order of constraints.
        This sweeper runs multiple solvers in parallel, with each one
        using a different constraint order. As soon as one solver completes,
        the others are cancelled. This reduces the aforementioned sensitivity.
        In future, the sweeper could also run solvers with different
        paramters in parallel, or solve each product list in
        'sum_of_prods' in parallel (OR parallelism).

        :param sum_of_prods: a sum of products of constraints to solve.
           These constraints are implicitly ANDed with the global
           constraint set.
        '''
        result = []
        result_lock = threading.Lock()
        done_event = threading.Event()

        # Function that runs in each thread
        def wrapper(solver):
            out = solver.check()
            with result_lock:
                if not done_event.is_set():
                    result.append(out)
                    done_event.set()

        # Random number generator for shuffling constraints
        rnd = random.Random(self.opts.sweep_seed)

        # Create a solver per thread
        solvers = []
        threads = []
        for i in range(0, self.opts.num_sweep_threads):
            # Create a solver for the problem
            ctx = z3.Context()
            s = z3.Solver(ctx=ctx)
            s.set("timeout", self.opts.smt_timeout)
            s.add(z3.And(self.constraints).translate(ctx))
            sum_constraint = z3.Or([z3.And(prod) for prod in sum_of_prods])
            s.add(sum_constraint.translate(ctx))
            solvers.append(s)

            # Create a thread for this solver and start it
            t = threading.Thread(target=wrapper, args=(s,))
            threads.append(t)
            t.start()

            # Shuffle the constraints for the next thread
            rnd.shuffle(self.constraints)
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

        return result[0]

# Translating Fortran expressions to SMT formulae
# ===============================================


def translate_integer_expr(expr_root: Node,
                           opts: ArrayIndexAnalysisOptions
                           ) -> (z3.ExprRef, z3.BoolRef):
    '''Translate a scalar integer Fortran expression to SMT. In addition,
    return a constraint that prohibits/ignores bit-vector overflow in the
    expression.'''

    constraints = []

    def trans(expr: Node) -> z3.ExprRef:
        # Check that type is a scalar integer of unspecified precision
        type_ok = _is_scalar_integer(expr.datatype)

        # Literal
        if isinstance(expr, Literal) and type_ok:
            if opts.use_bv:
                return z3.BitVecVal(int(expr.value), opts.int_width)
            else:
                return z3.IntVal(int(expr.value))

        # Reference
        if (isinstance(expr, Reference)
                and not isinstance(expr, ArrayReference)
                and type_ok):
            (sig, indices) = expr.get_signature_and_indices()
            indices_flat = [i for inds in indices for i in inds]
            if indices_flat == []:
                if opts.use_bv:
                    return z3.BitVec(str(sig), opts.int_width)
                else:
                    return z3.Int(str(sig))

        # UnaryOperation
        if isinstance(expr, UnaryOperation):
            arg_smt = trans(expr.operand)
            if expr.operator == UnaryOperation.Operator.MINUS:
                if opts.use_bv:
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
                if opts.use_bv:
                    constraints.append(z3.BVAddNoOverflow(
                      left_smt, right_smt, True))
                    constraints.append(z3.BVAddNoUnderflow(
                      left_smt, right_smt))
                return left_smt + right_smt
            if expr.operator == BinaryOperation.Operator.SUB:
                if opts.use_bv:
                    constraints.append(z3.BVSubNoOverflow(
                      left_smt, right_smt))
                    constraints.append(z3.BVSubNoUnderflow(
                      left_smt, right_smt, True))
                return left_smt - right_smt
            if expr.operator == BinaryOperation.Operator.MUL:
                if opts.use_bv:
                    constraints.append(z3.BVMulNoOverflow(
                      left_smt, right_smt, True))
                    constraints.append(z3.BVMulNoUnderflow(
                      left_smt, right_smt))
                return left_smt * right_smt
            if expr.operator == BinaryOperation.Operator.DIV:
                if opts.use_bv:
                    constraints.append(z3.BVSDivNoOverflow(
                      left_smt, right_smt))
                return left_smt / right_smt

        # IntrinsicCall
        if isinstance(expr, IntrinsicCall):
            # Unary operators
            if expr.intrinsic == IntrinsicCall.Intrinsic.ABS:
                smt_arg = trans(expr.children[1])
                if opts.use_bv:
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

                if opts.use_bv:
                    # TODO: when fparser supports shift operations (#428),
                    # we can remove the "no cover" block
                    if True:  # pragma: no cover
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
                    # TODO: when fparser supports shift operations (#428),
                    # we can remove the "no cover" block
                    if True:  # pragma: no cover
                        if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTL:
                            return z3.BV2Int(
                                     z3.Int2BV(left_smt, opts.int_width) <<
                                     z3.Int2BV(right_smt, opts.int_width),
                                     is_signed=True)
                        if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTR:
                            return z3.BV2Int(z3.LShR(
                                     z3.Int2BV(left_smt, opts.int_width),
                                     z3.Int2BV(right_smt, opts.int_width)),
                                     is_signed=True)
                        if expr.intrinsic == IntrinsicCall.Intrinsic.SHIFTA:
                            return z3.BV2Int(
                                     z3.Int2BV(left_smt, opts.int_width) >>
                                     z3.Int2BV(right_smt, opts.int_width),
                                     is_signed=True)
                    if expr.intrinsic == IntrinsicCall.Intrinsic.IAND:
                        return z3.BV2Int(z3.Int2BV(left_smt, opts.int_width) &
                                         z3.Int2BV(right_smt, opts.int_width),
                                         is_signed=True)
                    if expr.intrinsic == IntrinsicCall.Intrinsic.IOR:
                        return z3.BV2Int(z3.Int2BV(left_smt, opts.int_width) |
                                         z3.Int2BV(right_smt, opts.int_width),
                                         is_signed=True)
                    if expr.intrinsic == IntrinsicCall.Intrinsic.IEOR:
                        return z3.BV2Int(z3.Int2BV(left_smt, opts.int_width) ^
                                         z3.Int2BV(right_smt, opts.int_width),
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

            # Array intrinsics
            if opts.handle_array_intrins:
                array_intrins_pair = translate_array_intrinsic_call(expr)
                if array_intrins_pair:
                    if opts.use_bv:
                        return z3.BitVec(array_intrins_pair[1], opts.int_width)
                    else:
                        return z3.Int(array_intrins_pair[1])

        # Fall through: return a fresh, unconstrained symbol
        if opts.use_bv:
            return z3.FreshConst(z3.BitVecSort(opts.int_width))
        else:
            return z3.FreshInt()

    expr_root_smt = trans(expr_root)
    return (expr_root_smt, z3.And(*constraints))


def translate_logical_expr(expr_root: Node,
                           opts: ArrayIndexAnalysisOptions
                           ) -> (z3.BoolRef, z3.BoolRef):
    '''Translate a scalar logical Fortran expression to SMT. In addition,
    return a constraint that prohibits/ignores bit-vector overflow in the
    expression.'''

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
                (left_smt, prohibit_overflow) = translate_integer_expr(
                    left, opts)
                overflow.append(prohibit_overflow)
                (right_smt, prohibit_overflow) = translate_integer_expr(
                    right, opts)
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


def translate_array_intrinsic_call(call: IntrinsicCall) -> (str, str):
    '''Translate array intrinsic call to an array name and a scalar
    integer variable name.'''

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

# Helper functions
# ================


def _is_scalar_integer(dt: DataType) -> bool:
    '''Check that type is a scalar integer of unspecified precision.'''
    return (isinstance(dt, ScalarType) and
            dt.intrinsic == ScalarType.Intrinsic.INTEGER and
            dt.precision == ScalarType.Precision.UNDEFINED)


def _is_scalar_logical(dt: DataType) -> bool:
    '''Check that type is a scalar logical.'''
    return (isinstance(dt, ScalarType) and
            dt.intrinsic == ScalarType.Intrinsic.BOOLEAN)
