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
# Author J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module provides tools that are based on the code
    dependency analysis.'''

from __future__ import absolute_import, print_function

from psyclone.core.access_info import VariablesAccessInfo
from psyclone.core.access_type import AccessType
from psyclone.psyGen import InternalError, Loop
from psyclone.psyir.backend.fortran import FortranWriter


class DependencyTools(object):
    '''This class wraps all dependency tools, allowing a user to
    overwrite/modify functions depending on the application. It includes
    a messaging system where functions can store messages that might be
    useful for the user to see.
    '''

    def __init__(self, loop_types_to_parallelise=None):
        if loop_types_to_parallelise:
            self._loop_types_to_parallelise = loop_types_to_parallelise[:]
        else:
            self._loop_types_to_parallelise = []
        self._clear_messages()

    # -------------------------------------------------------------------------
    def _clear_messages(self):
        '''Removes all currently stored messages for the user.'''
        self._messages = []

    # -------------------------------------------------------------------------
    def _add_info(self, message):
        '''Adds a informational message to the internal message
        handling system.

        :param str message: the message for the user.
        '''
        self._messages.append("Info: "+message)

    # -------------------------------------------------------------------------
    def _add_warning(self, message):
        '''Adds a warning message to the internal message handling system.

        :param str message: the message for the user.
        '''
        self._messages.append("Warning: "+message)

    # -------------------------------------------------------------------------
    def _add_error(self, message):
        '''Adds an error message to the internal message handling system.

        :param str message: the message for the user.
        '''
        self._messages.append("Error: "+message)

    # -------------------------------------------------------------------------
    def get_all_messages(self):
        '''Returns all messages that have been stored by the
        last function the user has called.

        :return: a list of all messages.
        :rtype: list of str
        '''
        return self._messages

    # -------------------------------------------------------------------------
    def _is_loop_suitable_for_parallel(self, loop, only_nested_loops=True):
        '''Simple first test to see if a loop should even be considered to
        be parallelised. The default implementation test if the loop
        has a certain type (e.g. 'latitude'), and optional tests for
        nested loops (to avoid parallelising single loops which will likely
        result in a slowdown due to thread synchronisation costs). This
        function is used by can_loop_be_parallelised() and can of course be
        overwritten by the user to implement different suitability criteria.

        :param loop: the loop to test.
        :type loop: :py:class:`psyclone.psyGen.Loop`
        :param bool only_nested_loops: true (default) if only nested loops\
                                        should be considered.

        :returns: true if the loop fulfills the requirements.
        :rtype: bool
        '''
        if only_nested_loops:
            all_loops = loop.walk(Loop)
            if len(all_loops) == 1:
                self._add_info("Not a nested loop.")
                return False

        if loop.loop_type not in self._loop_types_to_parallelise:
            self._add_info("Loop has wrong loop type '{0}'.".
                           format(loop.loop_type))
            return False
        return True

    # -------------------------------------------------------------------------
    def is_array_parallelisable(self, loop_variable, var_info):
        '''Tries to determine if the access pattern for a variable
        given in var_info allows parallelisation along the variable
        loop_variable. Additional messages might be provided to the
        user using the message API.

        :param str loop_variable: name of the variable that is parallelised.
        :param var_info: access information for this variable.
        :type var_info: \
            :py:class:`psyclone.core.access_info.VariableAccessInfo`

        :return: if the variable can be used in parallel.
        :rtype: bool
        '''

        # If a variable is read-only, it can be parallelised
        if var_info.is_read_only():
            return True

        # Now detect which dimension(s) is/are parallelised, i.e.
        # which dimension depends on loop_variable. Also collect all
        # indices that are actually used
        dimension_index = -1
        all_indices = []
        for access in var_info.all_accesses:
            list_of_indices = access.indices
            # Now determine all dimensions that depend on the
            # parallel variable:
            for i, index in enumerate(list_of_indices):
                accesses = VariablesAccessInfo()

                # TODO #363: derived types are not supported, which
                # atm have an index of type str
                if isinstance(index, str):
                    visitor = FortranWriter()
                    var_string = var_info.var_name + index
                    self._add_warning("Assignment to derived type '{0}' is "
                                      "not supported yet.".format(var_string))
                    return False
                index.reference_accesses(accesses)
                try:
                    _ = accesses[loop_variable]
                    # This array would be parallelised across different
                    # indices (e.g. a(i,j), and a(j,i) ):
                    if dimension_index > -1 and dimension_index != i:
                        self._add_warning("Variable '{0}' is using loop "
                                          "variable {1} in index {2} and {3}."
                                          .format(var_info.var_name,
                                                  loop_variable,
                                                  dimension_index, i))
                        return False
                    dimension_index = i
                    all_indices.append(index)
                except KeyError:
                    # Raised when the loop variable is not at all used in the
                    # current dimension --> keep on looking
                    continue

        # Array variable does not depend on parallel loop variable:
        if not all_indices:
            # Variable accesses an array that is not dependend on the parallel'
            # loop variable, e.g.:
            # do j=1, n
            #    a(1) = b(j)+1
            #    c(j) = a(1) * 2
            # enddo
            # Either the variable should be declard to be a scalar, or
            # it is a shared variable with constant access, which is not
            # clear if it can be parallelised.
            self._add_warning("Variable '{0}' is written to, and "
                              "does not depend on the loop "
                              "variable '{1}'"
                              .format(var_info.var_name,
                                      loop_variable))
            return False

        # Now we have confirmed that all parallel accesses to the variable
        # are using the same dimension.
        # If all accesses use the same index, then the loop can be
        # parallelised: (this could be tested above, but this way it is a bit
        # clearer for now)
        first_index = all_indices[0]
        for index in all_indices[1:]:
            if not first_index.math_equal(index):
                visitor = FortranWriter()
                self._add_warning("Variable {0} is using index {1} and {2} "
                                  "and can therefore not be parallelised"
                                  .format(var_info.var_name,
                                          visitor(first_index),
                                          visitor(index)))
                return False
        return True

    # -------------------------------------------------------------------------
    def is_scalar_parallelisable(self, var_info):
        '''Checks if the accesses to the given scalar variable can be
        parallelised, i.e. it is not a reduction.

        :param var_info: the access information for the variable to test.
        :type var_info: :py:class:`psyclone.core.var_info.VariableInfo`
        :return: True if the scalar variable is not a reduction, i.e. it \
            can be parallelised.
        '''

        # Read only scalar variables can be parallelised
        if var_info.is_read_only():
            return True

        all_accesses = var_info.all_accesses
        # The variable is used only once. Either it is a read-only variable,
        # or it is supposed to store the result from the loop to be used
        # outside of the loop (or it is bad code). Read-only access has already
        # been tested above, so it must be a write access here, which prohibits
        # parallelisation.
        # We could potentially use lastprivate here?
        if len(all_accesses) == 1:
            self._add_warning("Variable '{0}' is only written once"
                              .format(var_info.var_name))
            return False

        # Now we have at least two accesses. If the first access is a WRITE,
        # then the variable is not used in a reduction. This relies on sorting
        # the accesses by location.
        if all_accesses[0].access_type == AccessType.WRITE:
            return True

        # Otherwise there is a read first, which would indicate that this loop
        # is a reduction, which is not supported atm.
        self._add_warning("Variable '{0}' is read first, which indicates a"
                          " reduction."
                          .format(var_info.var_name))
        return False

    # -------------------------------------------------------------------------
    def can_loop_be_parallelised(self, loop, loop_variable,
                                 only_nested_loops=True,
                                 test_all_variables=False,
                                 variables_to_ignore=None,
                                 var_accesses=None):
        # pylint: disable=too-many-arguments
        '''This function analyses the loop in PsyIR to see if
        it can be safely parallelised over the specified variable.

        :param loop: the loop node to be analysed.
        :type loop: :py:class:`psyclone.psyGen.Node`
        :param str loop_variable: name of the variable that is parallelised.
        :param bool only_nested_loops: if true, a loop must have an inner \
                                       loop in order to be considered \
                                       parallelisable (default: True).
        :param bool test_all_variables: if true, it will test if all variables\
                                        can be parallelised, otherwise it will\
                                        stop after the first variable is found\
                                        that can not be parallelised.
        :param variables_to_ignore: List of variables that are not checked if\
                                    they are parallelisable.
        :type variables_to_ignore: list of str
        :param var_accesses: optional argument containing the variable access\
                           pattern of the loop (default: None).
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :returns: true if the loop can be parallelised.
        :rtype: bool.
        '''
        self._clear_messages()

        if not isinstance(loop, Loop):
            raise InternalError("can_loop_be_parallelised: Loop must be an "
                                "instance of class Loop")

        # Check if the loop type should be parallelised, e.g. to avoid
        # parallelising inner loops which might not have enough work. This
        # is supposed to be a fast first check to avoid collecting variable
        # accesses in some unsuitable loops.
        if not self._is_loop_suitable_for_parallel(loop, only_nested_loops):
            # Appropriate messages will have been added already, so just exit
            return False

        if not var_accesses:
            var_accesses = VariablesAccessInfo()
            loop.reference_accesses(var_accesses)
        if not variables_to_ignore:
            variables_to_ignore = []

        # Collect all variables used as loop variable:
        loop_vars = [l.variable_name for l in loop.walk(Loop)]

        result = True
        # Now check all variables used in the loop
        for var_name in var_accesses.all_vars:
            # Ignore all loop variables - they look like reductions because of
            # the write-read access in the loop:
            if var_name in loop_vars:
                continue
            if var_name in variables_to_ignore:
                continue
            var_info = var_accesses[var_name]
            if var_info.is_array():
                # Handle arrays
                par_able = self.is_array_parallelisable(loop_variable,
                                                        var_info)
            else:
                # Handle scalar variable
                par_able = self.is_scalar_parallelisable(var_info)
            if not par_able:
                result = False
                if not test_all_variables:
                    return False

        return result
