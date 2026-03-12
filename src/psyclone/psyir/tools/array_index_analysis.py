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

'''This module provides a class to determine whether or not distinct
iterations of a given loop can generate conflicting array accesses (if
not, the loop can potentially be parallelised). It formulates the
problem as a set of SMT constraints over array indices which are then
are passed to the Z3 solver.'''

import z3
from psyclone.psyir.nodes import Loop, DataNode, Literal, Assignment, \
    Reference, IntrinsicCall, \
    Routine, Node, IfBlock, Schedule, Range, WhileLoop, \
    CodeBlock
from psyclone.core import Signature
from psyclone.psyir.symbols import DataType, ScalarType, ArrayType, \
    INTEGER_TYPE
from psyclone.psyir.tools.fortran_to_z3 import FortranToZ3
from typing import Optional, Tuple
from fparser.two import Fortran2003, Fortran2008

# Outline
# =======
#
# The analysis class provides a method 'get_loop_conflicts()' to
# determine whether or not the array accesses in a given loop are
# conflicting between iterations. Two array accesses are conflicting
# if they access the same element of the same array in different loop
# iterations, and at least one is a write.
#
# The analysis assumes that any scalar integer or scalar logical
# variables written by the loop can safely be considered as private
# to each iteration. This should be validated by the callee and is
# typically done by DependencyTools.
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
# in the code. When a Fortran variable is updated, the substitution is
# modified to point to a fresh SMT variable, with new constraints,
# without destroying the old constraints.
#
# More concretely, when we encounter an assignment of a scalar
# integer/logical variable, of the form 'x = rhs', we translate 'rhs'
# to the SMT formula 'smt_rhs' with the current substitution applied.
# We then add a constraint 'var = smt_rhs' where 'var' is a fresh SMT
# variable, and update the substitution so that 'x' maps to 'var'.
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
# We also maintain a "current condition". This can be viewed as a
# constraint that has not been committed to the constraint set because
# we want to be able to grow, contract, and retract it as we enter and
# exit conditional blocks of code. This current condition is passed in
# recursive calls, so there is an implicit stack of them.
#
# More concretely, when we encounter an 'if' statement, we copy the
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
# we create two SMT variables to represent the loop variables of two
# arbitary but distinct iterations of the loop. Each of these two
# variables is constrained to the start, stop, and step of the loop,
# and the two variables are constrained to be not equal. After that,
# we analyse the loop body twice, each time mapping the loop variable
# in the substitution to each of the SMT loop variables. After
# analysing the loop body for the first time, we save the array access
# list and start afresh with a new one. Therefore, once the analysis
# is complete, we have two array access lists, one for each iteration.
#
# When we encounter a loop that is not the loop of interest, we follow
# a similar approach but only consider a single arbitrary iteration of
# the loop.
#
# When the recursive descent is complete, we are left with two array
# access lists representing two different iterations of the same loop.
# A conflict occurs if there is an access to an array in the first
# list that can have the same loop indices as an access to the same
# array in the second list, and one of which is a write.  This is
# determined by asserting an equality constraint between each access's
# indices which, when combined with the current condition of each
# access and the global constraint set, will be satisfiable if and
# only if there is a conflict.  In this way, we check every access
# pair and determine whether or not the loop contains conflicts.

# Analysis Options
# ================


class ArrayIndexAnalysisOptions:
    '''The analysis supports a range of different options, which are all
    captured together in this class.

    :param use_bv: whether to treat Fortran integers as bit vectors or
       arbitrary-precision integers. If None is specified then the
       analysis will use a simple heuristic to decide.

    :param int_width: the bit width of Fortran integers. This is 32 by
       default but it can be useful to reduce it to (say) 8 in particular
       cases to improve the ability of solver to find a timely solution,
       provided the user considers it safe to do so. (Note that the analysis
       currently only gathers information about Fortran integer values of
       unspecified width.)

    :param smt_timeout_ms: the time limit (in milliseconds) given to
       the SMT solver to find a solution. If the solver does not
       return within this time, the analysis will conservatively return
       that a conflict exists even though it has not yet found one.
       This can be set to 'None' to disable the timeout.

    :param prohibit_overflow: if True, the analysis will tell the solver
       to ignore the possibility of integer overflow. Integer overflow is
       undefined behaviour in Fortran so this is safe.

    :param handle_array_intrins: handle array intrinsics 'size()',
       'lbound()', and 'ubound()' specially. For example, multiple
       occurrences of 'size(arr)' will be assumed to return the same value,
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
            self._cond = cond
            self._is_write = is_write
            self._indices = indices
            self._psyir_node = psyir_node

        @property
        def cond(self):
            return self._cond

        @property
        def is_write(self):
            return self._is_write

        @property
        def indices(self):
            return self._indices

        @property
        def psyir_node(self):
            return self._psyir_node

    def __init__(self, options=ArrayIndexAnalysisOptions()):
        '''This class provides a method 'get_loop_conflicts()' to
        determine whether or not distinct iterations of a given loop
        can generate conflicting array accesses.

        :param options: these options allow user control over features
           provided by, and choices made by, the analysis.
        '''
        self.opts = options

    def _init_analysis(self):
        '''Initialise the analysis by setting all the internal state
        variables accordingly.'''

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
        # The SMT variables representing each loop iteration variable
        self.smt_loop_var_i = None
        self.smt_loop_var_j = None
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
                    intrins_pair = \
                        self.trans.translate_array_intrinsic_call(call)
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

    def _apply_subst(self, expr: z3.ExprRef) -> z3.ExprRef:
        '''Apply the current substitution to the given expression.'''
        # The Z3 substitute() function takes a list of pairs rather
        # than a dict and, as the substitution can get quite large,
        # this can be inefficient. Therefore, we first narrow down
        # the substitution to cover only the free variables present
        # in the expression, and then apply it.
        subst_pairs = []
        for fv in _free_vars(expr):
            if fv in self.subst:
                subst_pairs.append((fv, self.subst[fv]))
        return z3.substitute(expr, *subst_pairs)

    def _translate_integer_expr_with_subst(self, expr: Node):
        '''Translate the given integer expression to SMT, and apply the
        current substitution.'''
        (smt_expr, cs) = self.trans.translate_integer_expr(expr)
        for c in cs:
            self._add_constraint(self._apply_subst(c))
        return self._apply_subst(smt_expr)

    def _translate_logical_expr_with_subst(self, expr: Node):
        '''Translate the given logical expression to SMT, and apply the
        current substitution.'''
        (smt_expr, cs) = self.trans.translate_logical_expr(expr)
        for c in cs:
            self._add_constraint(self._apply_subst(c))
        return self._apply_subst(smt_expr)

    def _translate_cond_expr_with_subst(self, expr: Node):
        '''Translate the given conditional expression to SMT, and apply
        the current substitution. Instead of adding constraints to
        the constraint set, this function ANDs constraints with the
        translated expression.'''
        (smt_expr, cs) = self.trans.translate_logical_expr(expr)
        smt_expr = z3.And([smt_expr] + cs)
        return self._apply_subst(smt_expr)

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
        if array_name in self.access_dict:
            self.access_dict[array_name].append(access)
        else:
            self.access_dict[array_name] = [access]

    def _add_all_array_accesses(self, node: Node, cond: z3.BoolRef):
        '''Add all array accesses in the given node to the current
        access dict.'''
        var_accesses = node.reference_accesses()
        for sig, access_seq in var_accesses.items():
            for access_info in access_seq:
                if isinstance(access_info.node, Reference):
                    (s, indices) = access_info.node.get_signature_and_indices()
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
                            str(s),
                            ArrayIndexAnalysis.ArrayAccess(
                              cond, access_info.is_any_write(),
                              smt_indices, access_info.node))

    def _save_access_dict(self):
        '''Move the current access dict to the stack, and proceed with
        an empty one.'''
        self.saved_access_dicts.append(self.access_dict)
        self.access_dict = {}

    def get_loop_conflicts(self, loop: Loop, all_conflicts: bool = False) -> \
            list[Tuple[Signature, Optional[str]]]:
        '''Determine whether or not distinct iterations of the given loop
           can generate conflicting array accesses.

           :param loop: loop to be analysed.
           :param all_conflicts: if True, enumerate all conflicts, otherwise
              stop after the first conflict. Defaults to False.
           :return: a list of pairs array-name/message pairs. If the list
              is empty, the loop is conflict free. If the solver times out,
              the message is None.
        '''

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

        # Create Fortran-to-Z3 translator
        self.trans = FortranToZ3(
                         use_bv=self.opts.use_bv,
                         int_width=self.opts.int_width,
                         smt_timeout_ms=self.opts.smt_timeout,
                         prohibit_overflow=self.opts.prohibit_overflow,
                         handle_array_intrins=self.opts.handle_array_intrins,
                         num_sweep_threads=self.opts.num_sweep_threads,
                         sweep_seed=self.opts.sweep_seed)

        # Initialise array intrinsic variables
        self._init_array_intrins_vars(routine)

        # Step through body of the enclosing routine, statement by statement
        for stmt in routine.children:
            self._step(stmt, z3.BoolVal(True))

        # Check that we have found and analysed the loop to parallelise
        if not (self.finished and len(self.saved_access_dicts) == 2):
            return None  # pragma: no cover

        # A list of conflicts to return
        conflicts = []

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
                            conflict = self._get_conflict(i_access, j_accesses)
                            if conflict:
                                conflicts.append(conflict)
                                if not all_conflicts:
                                    return conflicts

        return conflicts

    def _get_conflict(self, write: ArrayAccess, accs: list[ArrayAccess]) -> \
            Optional[Tuple[Signature, Optional[str]]]:
        '''Get the conflict between the write access 'write' and
           any access in 'accs', if there is one.

           :param write: a write access from one iteration.
           :param accs: a list of accesses from another iteration.
           :return: a pair containing an array name and a message string,
              if a conflict exists, and None otherwise. If the solver
              times out, the message is None.
        '''
        sum_of_prods = []
        for acc in accs:
            indices_equal = []
            for (i_idxs, j_idxs) in zip(write.indices, acc.indices):
                for (i_idx, j_idx) in zip(i_idxs, j_idxs):
                    indices_equal.append(i_idx == j_idx)
            sum_of_prods.append(indices_equal + [write.cond, acc.cond])

        # Invoke solver
        (result, result_values) = self.trans.solve(
            self.constraints,
            sum_of_prods,
            [self.smt_loop_var_i, self.smt_loop_var_j] +
            [ind for inds in write.indices for ind in inds])

        # Determine return value
        (sig, sig_inds) = write.psyir_node.get_signature_and_indices()
        if result == z3.sat:
            # Produce message
            i_val = str(result_values.pop(0))
            j_val = str(result_values.pop(0))
            components = []
            sig_fields = [sig[i] for i in range(len(sig))]
            for (field, inds) in zip(sig_fields, sig_inds):
                vals = []
                for ind in inds:
                    if result_values:
                        vals.append(str(result_values.pop(0)))
                if vals:
                    components.append(field + '(' + ','.join(vals) + ')')
            access_str = '%'.join(components)
            msg = (f"Iterations {i_val} and {j_val} have conflicting "
                   f"accesses to {access_str}")
            return (sig, msg)
        elif result == z3.unknown:  # pragma: no cover
            return (sig, None)
        else:
            return None

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
            # Loop over each condition/body pair in the list of branches
            for (if_cond, if_body) in stmt.flat():
                # Translate condition to SMT
                if if_cond is None:
                    smt_cond = z3.BoolVal(True)
                else:
                    smt_cond = self._translate_cond_expr_with_subst(if_cond)
                    if self.in_loop_to_parallelise:
                        self._add_all_array_accesses(if_cond, cond)
                # Recursively step into body
                self._save_subst()
                self._step(if_body, z3.And(cond, smt_cond))
                self._restore_subst()
                # Accumulate the condition for the next branch
                cond = z3.And(cond, z3.Not(smt_cond))
            # Kill vars written by each branch
            for (_, if_body) in stmt.flat():
                self._kill_all_written_vars(if_body)
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
                self.smt_loop_var_i = i_var
                self.smt_loop_var_j = j_var
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

        # Stop statement
        if _is_stop(stmt):
            # We can assume that the current condition doesn't hold anywhere
            # beyond this point
            self._add_constraint(z3.Not(cond))
            return

        # Fall through
        if self.in_loop_to_parallelise:
            self._add_all_array_accesses(stmt, cond)
        self._kill_all_written_vars(stmt)

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


def _free_vars(expr: z3.ExprRef) -> list[z3.ExprRef]:
    '''Return all the free variables (uninterpreted constants) in the
    given expression.'''
    if z3.is_const(expr):
        if expr.decl().kind() == z3.Z3_OP_UNINTERPRETED:
            return {expr}
        else:
            return {}
    else:
        return {fv for child in expr.children() for fv in _free_vars(child)}


def _is_stop(node: Node) -> bool:
    '''Determines whether or not the given PSyIR node represents a
    Fortran "stop" or "error stop" statement.'''
    if isinstance(node, CodeBlock) and len(node.get_ast_nodes) == 1:
        stmt = node.get_ast_nodes[0]
        if (isinstance(stmt, Fortran2003.Stop_Stmt) or
                isinstance(stmt, Fortran2008.Error_Stop_Stmt)):
            return True
    return False
