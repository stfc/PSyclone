# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# Modified: A. R. Porter, R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module provides tools that are based on the code
    dependency analysis.'''

from __future__ import absolute_import, print_function
from enum import IntEnum

import sympy

from psyclone.configuration import Config
from psyclone.core import (AccessType, SymbolicMaths,
                           VariablesAccessInfo)
from psyclone.errors import InternalError, LazyString
from psyclone.psyir.nodes import Loop
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.sympy_writer import SymPyWriter
from psyclone.psyir.backend.visitor import VisitorError


class DTCode(IntEnum):
    '''A simple enum to store the various info, warning and error
    codes used in the dependency analysis. It is based in IntEnum
    so the codes can be compared with the ..._MIN and ..._MAX values.

    '''
    INFO_MIN = 1
    INFO_NOT_NESTED_LOOP = 2
    INFO_WRONG_LOOP_TYPE = 3
    INFO_MAX = 99

    WARN_MIN = 100
    WARN_SCALAR_WRITTEN_ONCE = 101
    WARN_SCALAR_REDUCTION = 102
    WARN_MAX = 199

    ERROR_MIN = 200
    ERROR_WRITE_WRITE_RACE = 201
    ERROR_DEPENDENCY = 202
    ERROR_MAX = 299


# ============================================================================
class Message:
    '''This class stores an (error or warning) message, a numerical code
    for the message, and a list of variable names used. The DependencyTools
    use instance of this class to provide feedback to the caller: a user
    can be given the error message as it is, a script can analyse the
    numerical code and/or additional information.

    :param str message: the message.
    :param int code: error or warning code.
    :param var_names: list of variable names (defaults to []).
    :type var_names: List[str]

    '''
    def __init__(self, message, code, var_names=None):
        self._message = message
        self._code = code
        if var_names:
            self._var_names = var_names
        else:
            self._var_names = []

    # ------------------------------------------------------------------------
    def __str__(self):
        return self._message

    # ------------------------------------------------------------------------
    @property
    def code(self):
        ''':returns: the numerical code of this message.
        :rtype: int

        '''
        return self._code

    # ------------------------------------------------------------------------
    @property
    def var_names(self):
        ''':returns: the list of variable names to which the message applies.
        :rtype: List[str]

        '''
        # We convert each expression into a string to support LazyStrings
        # inside of 'var_names'
        return [str(i) for i in self._var_names]


# ============================================================================
class DependencyTools():
    '''This class provides some useful dependency tools, allowing a user to
    overwrite/modify functions depending on the application. It includes
    a messaging system where functions can store messages that might be
    useful for the user to see.

    :param loop_types_to_parallelise: A list of loop types that will be \
        considered for parallelisation. An example loop type might be\
        'lat', indicating that only loops over latitudes should be\
        parallelised. The actually supported list of loop types is\
        specified in the PSyclone config file. This can be used to\
        exclude for example 1-dimensional loops.
    :type loop_types_to_parallelise: Optional[List[str]]
    :param language_writer: a backend visitor to convert PSyIR expressions \
        to a representation in the selected language. This is used for \
        creating error and warning messages.
    :type language_writer: \
        Optional[:py:class:`psyclone.psyir.backend.visitor.PSyIRVisitor`]

    :raises TypeError: if an invalid loop type is specified.

    '''
    def __init__(self, loop_types_to_parallelise=None,
                 language_writer=None):
        if loop_types_to_parallelise:
            # Verify that all loop types specified are valid:
            config = Config.get()
            constants = config.get_constants()
            for loop_type in loop_types_to_parallelise:
                if loop_type not in constants.VALID_LOOP_TYPES:
                    out_list = constants.VALID_LOOP_TYPES
                    raise TypeError(f"Invalid loop type '{loop_type}' "
                                    f"specified in DependencyTools. Valid "
                                    f"values for API '{config.api}' are "
                                    f"{out_list}.")

            self._loop_types_to_parallelise = loop_types_to_parallelise[:]
        else:
            self._loop_types_to_parallelise = []
        if not language_writer:
            self._language_writer = FortranWriter()
        else:
            self._language_writer = language_writer
        self._clear_messages()

    # -------------------------------------------------------------------------
    def _clear_messages(self):
        '''Removes all currently stored messages for the user.'''
        self._messages = []

    # -------------------------------------------------------------------------
    def _add_message(self, message, code, var_names=None):
        '''Adds an informational message to the internal message
        handling system.

        :param str message: the message for the user.
        :param int code: error or warning code.
        :param var_names: list of variable names (defaults to []).
        :type var_names: List[str]

        '''
        if DTCode.INFO_MIN <= code <= DTCode.INFO_MAX:
            message_type = "Info"
        elif DTCode.WARN_MIN <= code <= DTCode.WARN_MAX:
            message_type = "Warning"
        elif DTCode.ERROR_MIN <= code <= DTCode.ERROR_MAX:
            message_type = "Error"
        else:
            raise InternalError(f"Unknown message code {code}.")

        self._messages.append(Message(f"{message_type}: {message}", code,
                                      var_names))

    # -------------------------------------------------------------------------
    def get_all_messages(self):
        '''Returns all messages that have been stored by the
        last function the user has called.

        :return: a list of all messages.
        :rtype: List[str]
        '''
        return self._messages

    # -------------------------------------------------------------------------
    def _is_loop_suitable_for_parallel(self, loop, only_nested_loops=True):
        '''Simple first test to see if a loop should even be considered to
        be parallelised. The default implementation tests whether the loop
        has a certain type (e.g. 'latitude'), and optional tests for
        nested loops (to avoid parallelising single loops which will likely
        result in a slowdown due to thread synchronisation costs). This
        function is used by can_loop_be_parallelised() and can of course be
        overwritten by the user to implement different suitability criteria.

        :param loop: the loop to test.
        :type loop: :py:class:`psyclone.psyir.nodes.Loop`
        :param bool only_nested_loops: true (default) if only nested loops\
                                        should be considered.

        :returns: true if the loop fulfills the requirements.
        :rtype: bool
        '''
        if only_nested_loops:
            all_loops = loop.walk(Loop)
            if len(all_loops) == 1:
                self._add_message("Not a nested loop.",
                                  DTCode.INFO_NOT_NESTED_LOOP)
                return False

        if self._loop_types_to_parallelise:
            if loop.loop_type not in self._loop_types_to_parallelise:
                self._add_message(f"Loop has wrong loop type '"
                                  f"{loop.loop_type}'.",
                                  DTCode.INFO_WRONG_LOOP_TYPE)
                return False
        return True

    # -------------------------------------------------------------------------
    @staticmethod
    def _partition(comp_ind1, comp_ind2, loop_variables):
        '''This method partitions the subscripts of the component indices
        into sets of minimal coupled groups. For example:
        `a(i)` and `a(i+3)` results in one partition with the variable `i`,
        `a(i)` and `a(j)` results in one partition with the variable `i`, `j`,
        `a(i,j,k)` and `a(i,k,j)` results in two partitions,
            one for subscript 0 (variable `i`),
            one for subscripts 1 and 2 (variables `j` and `k`)
        It returns a list of 2-tuples, each 2-tuple contains the set of
        variables used, and the list of subscript indices.

        :param comp_ind1: component_indices of the first array access.
        :type comp_ind1:  \
            :py:class:`psyclone.core.component_indices.ComponentIndices`
        :param comp_ind2: component_indices of the first array access.
        :type comp_ind2:  \
            :py:class:`psyclone.core.component_indices.ComponentIndices`
        :param loop_variables: list with name of all loop variables.
        :type loop_variables: List[str]

        :return: partition information.
        :rtype: List[Tuple[Set[str], List[int]]]

        '''
        # Get the (string) name of all variables used in each subscript
        # of the two accesses. E.g. `a(i,j+k)` --> [ {"i"}, {"j","k"}]
        set_of_loop_vars = set(loop_variables)
        indices_1 = comp_ind1.get_subscripts_of(set_of_loop_vars)
        indices_2 = comp_ind2.get_subscripts_of(set_of_loop_vars)
        # This list stores the partition information, which
        # is a pair consisting of:
        # - a set of all loop variables used in the subscript of
        #   both accesses
        # - list of all subscripts. Initially these lists contain
        #   only one subscript, but they will be modified later
        # Example: `a(i,j)` and `a(i,k)` -->
        #          [ ({"i"}, [(0,0)]), ({"j","k"}, [(0,1)])]
        partition_infos = []
        for i, indx in enumerate(comp_ind1.iterate()):
            partition_infos.append((indices_1[i].union(indices_2[i]), [indx]))

        # Check each loop variable to find subscripts in which they are used:
        for loop_var in loop_variables:
            first_use = None
            # The partition_infos list will get modified inside the loop.
            # So we use a while loop to accommodate this.
            k = 0
            while k < len(partition_infos):
                part_info = partition_infos[k]
                if loop_var not in part_info[0]:
                    k += 1
                    continue
                # Current loop variable is used in subscript k.
                # If this is the first usage of this loop variable,
                # just remember this index and continue with next partition.
                if first_use is None:
                    first_use = k
                    k += 1
                    continue
                # Partition `k` and `first_use` share a variable.
                # Add partition info from `k` to `first_use`, and
                # then discard partition `k`.
                partition_infos[first_use] = \
                    (partition_infos[first_use][0].union(part_info[0]),
                     partition_infos[first_use][1] + part_info[1])
                del partition_infos[k]

        return partition_infos

    # -------------------------------------------------------------------------
    @staticmethod
    def _independent_0_var(index_exp1, index_exp2):
        '''Checks if the two index expressions, that are not dependent on any
        loop variable, are independent or not. E.g. `a(3)` and `a(5)`
        are independent of each other, `a(n)` and `a(n)` are not.
        Even `a(n)` and `a(3)` are considered dependent, since no
        restriction on the variable `n` is made, so `n` could be 3.

        :param index_exp1: the first index expression to compare.
        :type index_exp1: :py:class:`psyclone.psyir.nodes.Node`
        :param index_exp2: the second index expression to compare.
        :type index_exp2: :py:class:`psyclone.psyir.nodes.Node`

        '''
        sym_maths = SymbolicMaths.get()

        # If the indices can be shown to be never equal, the accesses
        # to the given subscript are always independent.
        if sym_maths.never_equal(index_exp1, index_exp2):
            return True

        # Otherwise we have to conservatively assume that the accesses
        # are dependent on each other. Additional tests could be added,
        # e.g. `a(n)` and `a(5)` ... if it should be known that n != 5.
        return False

    # -------------------------------------------------------------------------
    @staticmethod
    def _get_dependency_distance(var_name, index_read, index_written):
        '''Computes the dependency distance between two accesses to the
        same variable. The distance specifies in how many loop iterations
        the same memory location would be accessed. E.g. `a(i)=a(i-1)`
        would have a distance of 1: solving i=(i+di)-1 gives di=1
        Similarly, `a(i) = a(i) + 1` has a distance of 0, and
        `a(2*i)=a(2*i+1)` will have a distance of -0.5 (which indicates that
        with an index that's an integer the accesses are independent). More
        complex examples can have several solutions, e.g. `a(i*i) = a(i*i)+1`.
        This will have the distance 0 and `-2*i`. The latter would result
        in a negative loop index (i+(-2*i)<0), indicating that actually no
        dependency exists.
        This function will return the distance, if it is an integer value
        independent of loop iterations, and None otherwise.
        Multiple solution are not handled yet, since they are unlikely to
        be observed in code handled with PSyclone, so they will also
        return None.

        :param str var_name: name of the one variable used in the two \
            index expressions.
        :param index_read: the index expression of the variable read that \
            is to be compare.
        :type index_read: :py:class:`psyclone.psyir.nodes.Node`
        :param index_written: the index expression of the variable written \
            that is to be compare.
        :type index_written: :py:class:`psyclone.psyir.nodes.Node`

        :returns: the dependency distance in loop iterations if it is a
            independent integer value, and None otherwise.
        :rtype: Union[int, None]

        '''
        # pylint: disable=too-many-return-statements
        sym_maths = SymbolicMaths.get()
        try:
            sympy_expressions, symbol_map = SymPyWriter.\
                get_sympy_expressions_and_symbol_map([index_read,
                                                     index_written])
        except VisitorError:
            return None
        # If the subscripts do not even depend on the specified variable,
        # any dependency distance is possible (e.g. `do i ... a(j)=a(j)+1`)
        if var_name not in symbol_map:
            return None

        var = symbol_map[var_name]
        # Create a unique 'dx' variable name if 'x' is the variable.
        d_var_name = "d_"+var_name
        idx = 1
        while d_var_name in symbol_map:
            d_var_name = f"d{idx}_{var_name}"

        # Create a sympy symbol for this new variable
        d_var = sympy.Symbol(d_var_name)

        # Replace 'var' with 'var+d_var' in the read expression in order
        # to determine distance between the two accesses:
        sympy_expressions[1] = sympy_expressions[1].subs({var: (var+d_var)})

        # Now solve for `d_var` to identify the distance
        solutions = sym_maths.solve_equal_for(sympy_expressions[0],
                                              sympy_expressions[1],
                                              d_var)
        if solutions == "independent":
            # The solution is independent of the variable, i.e. every value
            # of `d_var` is a solution. So there are certainly
            # dependencies.
            return None

        if len(solutions) == 1:
            # solutions is a FiniteSet, which can't be accessed directly,
            # so convert this one element to a one element list:
            sol = list(solutions)[0]
            if var in sol.free_symbols:
                # If the loop variable is used in the solution, in general
                # we have a dependency. Loop start/end/step values could be
                # evaluated here. We then also need to check if `i+di` (i.e.
                # the iteration to which the dependency is) is a valid
                # iteration. E.g. in case of a(i^2)=a(i^2) --> di=0 or di=-2*i
                # --> # i+di = -i < 0 for i>0. Since this is not a valid loop
                # iteration that means no dependencies.
                return None

            if not isinstance(sol, sympy.Integer):
                # This likely indicates several (or an infinite) number of
                # iterations.
                return None

            # Otherwise return the distance of the dependency (i.e. how many
            # loop iterations apart the same memory location will be accessed).
            # If this should be 0, it means no dependency. Though even here
            # loop boundaries could be used for further checks - e.g. if
            # the distance is N with N loop iterations, e.g.:
            # do i=1, N: a(i)=a(i+N)
            # Then there would still be no dependency.
            return sol

        return None

    # -------------------------------------------------------------------------
    @staticmethod
    def _independent_multi_subscript(var_name, write_access, other_access,
                                     subscripts):
        '''Test multiple subscripts that share variables. This includes cases
        like `a(i,i) = a(i,i+1)` or `a(i, indx(i)) = a(i,5)` etc.
        At this stage only a minimal test is done: if there is one subscript
        that is independent, the whole access can be parallelised, e.g.
        `a(i,i+j) = a(i,j*j-i*i)` if the 'i' loop is parallelised. Even though
        the second subscript might be identical for different i and j values,
        the fact that the first subscript is independent makes the access
        parallelisable.

        :param str var_name: the name of the loop variable of the loop to be \
            parallelised.
        :param write_access: access information a single write access.
        :type write_access: :py:class:`psyclone.core.access_info.AccessInfo`
        :param other_access: access information the other (read or write) \
            access.
        :type other_access: :py:class:`psyclone.core.access_info.AccessInfo`
        :param subscripts: the subscript indices (as a tuple, see \
            ComponentIndices class) which are all handled together because \
            of shared loop variables.
        :type subscripts: List[Tuple(int,int)]

        :returns: whether the two accesses can be parallelised or not.
        :type: bool

        '''
        # If we find one subscript that is independent, the loop can be
        # parallelised. E.g. `a(i, index(i)) = a(i, 5)`. The fact that
        # the first subscript is i, means that each different iteration
        # will access a different column, even if index(i) is 5.
        for ind in subscripts:
            index_written = write_access.component_indices[ind]
            index_other = other_access.component_indices[ind]
            distance = DependencyTools._get_dependency_distance(var_name,
                                                                index_written,
                                                                index_other)
            if distance == 0:
                # Notice that distance 0 will only be returned if the
                # subscript actually depends on the loop variable.
                return True
        # Additional tests could be added here (e.g. to see that
        # `a(i,i)=a(i,i+1)+1` can be parallelised).

        return False

    # -------------------------------------------------------------------------
    def _is_loop_carried_dependency(self, loop_variables, write_access,
                                    other_access):
        '''Checks if there is any write access that is dependent with
        another (read or write) access in a different iteration. If there
        is a dependency, then the access to this array cannot be parallelised,
        it would create a race condition.

        :param loop_variables: the list of all loop variables in the code to \
            be parallelised. The first one must be the loop to be \
            parallelised (a possible outer loop does not matter, the value of \
            the loop variable is a constant within the loop to be parallelised.
        :type loop_variables: List[str]
        :param write_access: access information of a single array write access.
        :type write_access: :py:class:`psyclone.core.access_info.AccessInfo`
        :param other_access: access information of the second single array \
            access (for the same variable).
        :type other_access: :py:class:`psyclone.core.access_info.AccessInfo`

        :returns: whether there is a loop carried dependency between the \
            pair of accesses, which prevents parallelisation.
        :rtype: bool

        '''
        # Partition all subscripts to create sets of subscripts that can
        # be analysed independently of others. For example, `a(i,j+k,k,l)`
        # and `a(i,j,k,l)` would create three partitions:
        # 1) subscript 0: only variable i is used.
        # 2) subscript 1+2: uses the variables j and k
        # 3) subscript 3: only uses l
        partitions = self._partition(write_access.component_indices,
                                     other_access.component_indices,
                                     loop_variables)
        # Get the name of the loop variable that is to be parallelised:
        loop_var = loop_variables[0]

        # Analyse each subscript partition individually. If we find even
        # one partition that guarantees that the accesses cannot interfere
        # with each other, the accesses can be parallelised and we do not
        # need any further test.
        for set_of_vars, subscripts in partitions:
            # First only test independent subscripts - i.e. subscripts that
            # consistently only use one variable
            if len(subscripts) == 1:
                # There is only one subscript involved in this test.
                # Get its index of its component_index:
                subscript = subscripts[0]
                index_write = write_access.component_indices[subscript]
                index_other = other_access.component_indices[subscript]
                if len(set_of_vars) == 0:
                    # No loop variable used, constant access (which might
                    # still be using unknown non-loop variables).
                    # E.g. `a(5) = a(n)`
                    indep = self._independent_0_var(index_write, index_other)
                    # If we can show that there is at least one subscript
                    # that is independent (`a(5, i)` and `a(3, i)`), we know
                    # that the accesses are independent.
                    if indep:
                        return True
                elif len(set_of_vars) == 1:
                    # One loop variable used in both accesses.
                    # E.g. `a(2*i+3) = a(i*i)`
                    distance = self._get_dependency_distance(loop_var,
                                                             index_write,
                                                             index_other)
                    # If the dependency distance is 0, it means that in each
                    # iteration a different index is accessed, so the loop
                    # can be parallelised.
                    if distance == 0:
                        return True
                else:
                    # One subscript with several loop variables, e.g.
                    # a(i+j) in a nest of i and j loops. Assume that there
                    # will be dependencies.
                    return False
            else:
                # This is reached only if there is more than one subscript in
                # which one or several variables are used
                indep = self._independent_multi_subscript(loop_var,
                                                          write_access,
                                                          other_access,
                                                          subscripts)
                if indep:
                    return True

        return False

    # -------------------------------------------------------------------------
    def _array_access_parallelisable(self, loop_variables, var_info):
        '''Tries to determine if the access pattern for an array
        given in `var_info` allows parallelisation along the first variable
        in `loop_variables`. The other elements of `loop_variables` specify
        the loop variables of any inner loops. This implementation follows:

        "Optimizing compilers for modern architectures -
        a dependence-based approach
        Ken Kennedy and John R. Allen. 2001.
        Morgan Kaufmann Publishers Inc., San Francisco, CA, USA."

        Chapter 3.6 "Dependency Testing - Putting it all Together"
        But many of the more advanced tests (e.g. multi-variable ones)
        are not (yet) implemented, since it appears unlikely that they
        will occur in 'real' code. But the outline of this testing follows
        the full structure  When we use the same function for other tests
        (loop fusion etc), the subroutines here can be generalised to use
        the dependency direction (i.e. "<", "=", ">") and instead of
        checking for distance 0 then check for direction being only "="").

        De-facto all that is happening atm is to try to find one subscript
        that is guaranteed to be independent, which is all that is needed
        for parallelisation.

        :param loop_variables: the list of all loop variables in the code to \
            be parallelised. The first one must be the loop to be \
            parallelised (a possible outer loop does not matter, the value of \
            the loop variable is a constant within the loop to be parallelised.
        :type loop_variables: List[str]
        :param var_info: access information for this variable.
        :type var_info: \
            :py:class:`psyclone.core.access_info.SingleVariableAccessInfo`

        :return: whether the variable can be used in parallel.
        :rtype: bool

        '''
        # pylint: disable=too-many-locals
        # If a variable is read-only, it can be parallelised
        if var_info.is_read_only():
            return True

        all_write_accesses = var_info.all_write_accesses

        for write_access in all_write_accesses:
            # We need to compare each write access with any other access,
            # including itself (to detect write-write race conditions:
            # a((i-2)**2) = b(i): i=1 and i=3 would write to a(1))
            for other_access in var_info:
                if not self._is_loop_carried_dependency(loop_variables,
                                                        write_access,
                                                        other_access):
                    # There is a dependency. Try to give precise error
                    # messages:
                    # We need to use default parameters, since otherwise
                    # the value of a variable might be different when
                    # the message is actually evaluated.
                    # Some pylint version complain here (because of the
                    # above). The code is correct, so disable this
                    # message:
                    # pylint: disable=cell-var-from-loop
                    if write_access is other_access:
                        # The write access has a dependency on itself, e.g.
                        # a(3) = ...    or a((i-2)**2) = ...
                        # Both would result in a write-write conflict
                        node = write_access.node
                        self._add_message(LazyString(
                            lambda node=write_access.node:
                                (f"The write access to '"
                                 f"{self._language_writer(node)}' causes "
                                 f"a write-write race condition.")),
                            DTCode.ERROR_WRITE_WRITE_RACE,
                            [LazyString(lambda node=node:
                                        f"{self._language_writer(node)}")])
                    else:
                        self._add_message(LazyString(
                            lambda wnode=write_access.node,
                            onode=other_access.node:
                                (f"The write access to "
                                 f"'{self._language_writer(wnode)}' "
                                 f"and to '{self._language_writer(onode)}"
                                 f"' are dependent and cannot be "
                                 f"parallelised.")),
                            DTCode.ERROR_DEPENDENCY,
                            [LazyString(lambda wnode=write_access.node:
                                        f"{self._language_writer(wnode)}"
                                        ),
                             LazyString(lambda onode=other_access.node:
                                        f"{self._language_writer(onode)}")])

                    return False
        return True

    # -------------------------------------------------------------------------
    def _is_scalar_parallelisable(self, var_info):
        '''Checks if the accesses to the given scalar variable can be
        parallelised, i.e. it is not a reduction.

        :param var_info: the access information for the variable to test.
        :type var_info: :py:class:`psyclone.core.var_info.VariableInfo`
        :return: True if the scalar variable is not a reduction, i.e. it \
            can be parallelised.
        :rtype: bool
        '''

        # Read only scalar variables can be parallelised
        if var_info.is_read_only():
            return True

        all_accesses = var_info.all_accesses
        if len(all_accesses) == 1:
            # The variable is used only once. Either it is a read-only
            # variable, or it is supposed to store the result from the loop to
            # be used outside of the loop (or it is bad code). Read-only access
            # has already been tested above, so it must be a write access here,
            # which prohibits parallelisation.
            # We could potentially use lastprivate here?
            self._add_message(f"Scalar variable '{var_info.var_name}' is "
                              f"only written once.",
                              DTCode.WARN_SCALAR_WRITTEN_ONCE,
                              [f"{var_info.var_name}"])
            return False

        # Now we have at least two accesses. If the first access is a WRITE,
        # then the variable is not used in a reduction. This relies on sorting
        # the accesses by location. Note that an argument to a kernel can have
        # a 'READWRITE' access because, in that case, all we know is what the
        # kernel metadata tells us. However, we do know that such an access is
        # *not* a reduction because that would have 'SUM' access.
        if all_accesses[0].access_type in (AccessType.WRITE,
                                           AccessType.READWRITE):
            return True

        # Otherwise there is a read first, which would indicate that this loop
        # is a reduction, which is not supported atm.
        self._add_message(f"Variable '{var_info.var_name}' is read first, "
                          f"which indicates a reduction.",
                          DTCode.WARN_SCALAR_REDUCTION,
                          [var_info.var_name])
        return False

    # -------------------------------------------------------------------------
    def can_loop_be_parallelised(self, loop,
                                 only_nested_loops=True,
                                 test_all_variables=False,
                                 signatures_to_ignore=None):
        # pylint: disable=too-many-branches,too-many-locals
        '''This function analyses a loop in the PsyIR to see if
        it can be safely parallelised over the specified variable.

        :param loop: the loop node to be analysed.
        :type loop: :py:class:`psyclone.psyir.nodes.Loop`
        :param bool only_nested_loops: if True, a loop must have an inner\
                                       loop in order to be considered\
                                       parallelisable (default: True).
        :param bool test_all_variables: if True, it will test if all variable\
                                        accesses can be parallelised,\
                                        otherwise it will stop after the first\
                                        variable is found that can not be\
                                        parallelised.
        :param signatures_to_ignore: list of signatures for which to skip \
                                     the access checks.
        :type signatures_to_ignore: list of :py:class:`psyclone.core.Signature`

        :returns: True if the loop can be parallelised.
        :rtype: bool

        :raises TypeError: if the supplied node is not a Loop.

        '''
        self._clear_messages()

        if not isinstance(loop, Loop):
            raise TypeError(f"can_loop_be_parallelised: node must be an "
                            f"instance of class Loop but got "
                            f"'{type(loop).__name__}'")

        # Check if the loop type should be parallelised, e.g. to avoid
        # parallelising inner loops which might not have enough work. This
        # is supposed to be a fast first check to avoid collecting variable
        # accesses in some unsuitable loops.
        if not self._is_loop_suitable_for_parallel(loop, only_nested_loops):
            # Appropriate messages will have been added already, so just exit
            return False

        var_accesses = VariablesAccessInfo(loop)
        if not signatures_to_ignore:
            signatures_to_ignore = []

        # Collect all variables used as loop variable:
        loop_vars = [loop.variable.name for loop in loop.walk(Loop)]

        result = True
        # Now check all variables used in the loop
        for signature in var_accesses.all_signatures:
            # This string contains derived type information, e.g.
            # "a%b"
            var_string = str(signature)
            # Ignore all loop variables - they look like reductions because of
            # the write-read access in the loop:
            if var_string in loop_vars:
                continue
            if signature in signatures_to_ignore:
                continue

            # This returns the first component of the signature,
            # i.e. in case of "a%b" it will only return "a"
            var_name = signature.var_name
            var_info = var_accesses[signature]
            symbol_table = loop.scope.symbol_table
            symbol = symbol_table.lookup(var_name)
            # TODO #1270 - the is_array_access function might be moved
            is_array = symbol.is_array_access(access_info=var_info)
            if is_array:
                # Handle arrays
                par_able = self._array_access_parallelisable(loop_vars,
                                                             var_info)
            else:
                # Handle scalar variable
                par_able = self._is_scalar_parallelisable(var_info)
            if not par_able:
                if not test_all_variables:
                    return False
                # The user might have requested to continue in order to get
                # all messages for all variables preventing parallelisation,
                # not just the first one
                result = False

        return result

    # -------------------------------------------------------------------------
    def get_input_parameters(self, node_list, variables_info=None):
        # pylint: disable=no-self-use
        '''Return all variables that are input parameters, i.e. are
        read (before potentially being written).

        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param variables_info: optional variable usage information, \
            can be used to avoid repeatedly collecting this information.
        :type variables_info: \
            :py:class:`psyclone.core.variables_info.VariablesAccessInfo`

        :returns: a list of all variable signatures that are read.
        :rtype: List[:py:class:`psyclone.core.Signature`]

        '''
        # Collect the information about all variables used:
        if not variables_info:
            variables_info = VariablesAccessInfo(node_list)

        input_list = []
        for signature in variables_info.all_signatures:
            # Take the first access (index 0) of this variable. Note that
            # loop variables have a WRITE before a READ access, so they
            # will be ignored
            first_access = variables_info[signature][0]
            # If the first access is a write, the variable is not an input
            # parameter and does not need to be saved.
            if first_access.access_type != AccessType.WRITE:
                input_list.append(signature)

        return input_list

    # -------------------------------------------------------------------------
    def get_output_parameters(self, node_list, variables_info=None):
        # pylint: disable=no-self-use
        '''Return all variables that are output parameters, i.e. are
        written.

        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param variables_info: optional variable usage information, \
            can be used to avoid repeatedly collecting this information.
        :type variables_info: \
        Optional[:py:class:`psyclone.core.variables_info.VariablesAccessInfo`]

        :returns: a list of all variable signatures that are written.
        :rtype: List[:py:class:`psyclone.core.Signature`]

        '''
        # Collect the information about all variables used:
        if not variables_info:
            variables_info = VariablesAccessInfo(node_list)

        return [signature for signature in variables_info.all_signatures
                if variables_info.is_written(signature)]

    # -------------------------------------------------------------------------
    def get_in_out_parameters(self, node_list):
        '''Return a 2-tuple of lists that contains all variables that are input
        parameters (first entry) and output parameters (second entry).
        This function calls get_input_parameter and get_output_parameter,
        but avoids the repeated computation of the variable usage.

        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: List[:py:class:`psyclone.psyir.nodes.Node`]

        :returns: a 2-tuple of two lists, the first one containing \
            the input parameters, the second the output parameters.
        :rtype: Tuple[List[:py:class:`psyclone.core.Signature`],
                      List[:py:class:`psyclone.core.Signature`])

        '''
        variables_info = VariablesAccessInfo(node_list)
        return (self.get_input_parameters(node_list, variables_info),
                self.get_output_parameters(node_list, variables_info))
