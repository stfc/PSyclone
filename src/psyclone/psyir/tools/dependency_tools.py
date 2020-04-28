# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Modified: A. R. Porter, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

''' This module provides tools that are based on the code
    dependency analysis.'''

from __future__ import absolute_import, print_function

from psyclone.core.access_info import VariablesAccessInfo
from psyclone.core.access_type import AccessType
from psyclone.psyir.nodes import Loop
from psyclone.psyir.backend.fortran import FortranWriter


class DependencyTools(object):
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
    :type loop_types_to_parallelise: list of str
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
        '''Adds an informational message to the internal message
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

        :return: whether the variable can be used in parallel.
        :rtype: bool
        '''

        # If a variable is read-only, it can be parallelised
        if var_info.is_read_only():
            return True

        # Now detect which dimension(s) is/are parallelised, i.e.
        # which dimension depends on the loop_variable.  For example
        # if a "do j..." loop is parallelised, consider expressions like
        # a(i,j) and a(j+2, i-1) in one loop:
        # In this case the dimensions 1 (a(i,j)) and 0 (a(j+2,i-1)) would
        # be accessed. Since the variable is written somewhere (read-only
        # was tested above), the variable can not be used in parallel.
        # Additionally, collect all indices that are actually used, since
        # they are needed in a test further down.
        found_dimension_index = -1
        all_indices = []

        # Loop over all the accesses of this variable
        for access in var_info.all_accesses:
            list_of_indices = access.indices
            # Now determine all dimensions that depend
            # on the parallel variable:
            for dimension_index, index_expression in \
                    enumerate(list_of_indices):
                accesses = VariablesAccessInfo()

                # TODO #363: derived types are not supported, which
                # atm have an index_expression of type str
                if isinstance(index_expression, str):
                    var_string = var_info.var_name + index_expression
                    self._add_warning("Assignment to derived type '{0}' is "
                                      "not supported yet.".format(var_string))
                    return False
                index_expression.reference_accesses(accesses)
                if loop_variable not in accesses:
                    continue

                # if a previously identified index location does not match
                # the current index location (e.g. a(i,j), and a(j,i) ), then
                # the loop can not be parallelised
                if found_dimension_index > -1 and \
                        found_dimension_index != dimension_index:
                    self._add_warning("Variable '{0}' is using loop "
                                      "variable {1} in index {2} and {3}."
                                      .format(var_info.var_name,
                                              loop_variable,
                                              found_dimension_index,
                                              dimension_index))
                    return False
                found_dimension_index = dimension_index
                all_indices.append(index_expression)

        if not all_indices:
            # An array is used that is not actually dependent on the parallel
            # loop variable. This means the variable can not always be safely
            # parallelised. Example 1:
            # do j=1, n
            #    a(1) = b(j)+1
            #    c(j) = a(1) * 2
            # enddo
            # In this case a(1) should be a thread-private scalar.
            # Example2:
            # do j=1, n
            #    if(some_cond)
            #       a(1) = b(j)
            #    endif
            #  enddo
            # In this case it is not clear if the loop can be parallelised.
            # So in any case we add the information for the user to decide.
            self._add_warning("Variable '{0}' is written to, and "
                              "does not depend on the loop "
                              "variable '{1}'."
                              .format(var_info.var_name,
                                      loop_variable))
            return False

        # Now we have confirmed that all parallel accesses to the variable
        # are using the same dimension. If there is a case of stencil
        # access with write operations (read-only has already been tested)
        # the loop can not be parallelised. E.g. in one j loop:
        # b(j) = a(j-1) + a(j+1)
        # a(j) = c(j)

        first_index = all_indices[0]
        for index in all_indices[1:]:
            if not first_index.math_equal(index):
                visitor = FortranWriter()
                self._add_warning("Variable {0} is written and is accessed "
                                  "using indices {1} and {2} and can "
                                  "therefore not be parallelised."
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
        if len(all_accesses) == 1:
            # The variable is used only once. Either it is a read-only
            # variable, or it is supposed to store the result from the loop to
            # be used outside of the loop (or it is bad code). Read-only access
            # has already been tested above, so it must be a write access here,
            # which prohibits parallelisation.
            # We could potentially use lastprivate here?
            self._add_warning("Scalar variable '{0}' is only written once."
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
    def can_loop_be_parallelised(self, loop, loop_variable=None,
                                 only_nested_loops=True,
                                 test_all_variables=False,
                                 variables_to_ignore=None,
                                 var_accesses=None):
        # pylint: disable=too-many-arguments
        '''This function analyses a loop in the PsyIR to see if
        it can be safely parallelised over the specified variable.

        :param loop: the loop node to be analysed.
        :type loop: :py:class:`psyclone.psyir.nodes.Loop`
        :param str loop_variable: Optional name of the variable that is\
                                  parallelised. If not specified, the loop\
                                  variable of the loop is used.
        :param bool only_nested_loops: if True, a loop must have an inner\
                                       loop in order to be considered\
                                       parallelisable (default: True).
        :param bool test_all_variables: if True, it will test if all variable\
                                        accesses can be parallelised,\
                                        otherwise it will stop after the first\
                                        variable is found that can not be\
                                        parallelised.
        :param variables_to_ignore: list of variables for which to skip the\
                                    checks on how they are accessed.
        :type variables_to_ignore: list of str
        :param var_accesses: optional argument containing the variable access\
                           pattern of the loop (default: None).
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :returns: True if the loop can be parallelised.
        :rtype: bool

        :raises TypeError: if the supplied node is not a Loop.

        '''
        self._clear_messages()

        if not isinstance(loop, Loop):
            raise TypeError("can_loop_be_parallelised: node must be an "
                            "instance of class Loop but got '{0}'".
                            format(type(loop).__name__))
        if not loop_variable:
            loop_variable = loop.variable_name

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
            # Find the symbol for this variable
            symbol = loop.find_or_create_symbol(var_name)
            var_info = var_accesses[var_name]
            if symbol.is_array:
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

    # -------------------------------------------------------------------------
    def get_input_parameters(self, node_list, variables_info=None):
        # pylint: disable=no-self-use
        '''Return all variables that are input parameters, i.e. are
        read (before potentially being written).

        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param variables_info: optional variable usage information, \
            can be used to avoid repeatedly collecting this information.
        :type variables_info: \
            :py:class:`psyclone.core.variables_info.VariablesAccessInfo`

        :returns: a list of all variable names that are read.
        :rtype: list of str

        '''
        # Collect the information about all variables used:
        if not variables_info:
            variables_info = VariablesAccessInfo(node_list)

        input_list = []
        for var_name in variables_info.all_vars:
            # Take the first access (index 0) of this variable. Note that
            # loop variables have a WRITE before a READ access, so they
            # will be ignored
            first_access = variables_info[var_name][0]
            # If the first access is a write, the variable is not an input
            # parameter and does not need to be saved.
            if first_access.access_type != AccessType.WRITE:
                input_list.append(var_name)

        return input_list

    # -------------------------------------------------------------------------
    def get_output_parameters(self, node_list, variables_info=None):
        # pylint: disable=no-self-use
        '''Return all variables that are output parameters, i.e. are
        written.

        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param variables_info: optional variable usage information, \
            can be used to avoid repeatedly collecting this information.
        :type variables_info: \
            :py:class:`psyclone.core.variables_info.VariablesAccessInfo`

        :returns: a list of all variable names that are written.
        :rtype: list of str

        '''
        # Collect the information about all variables used:
        if not variables_info:
            variables_info = VariablesAccessInfo(node_list)

        return [var_name for var_name in variables_info.all_vars
                if variables_info.is_written(var_name)]

    # -------------------------------------------------------------------------
    def get_in_out_parameters(self, node_list):
        '''Return a 2-tuple of lists that contains all variables that are input
        parameters (first entry) and output parameters (second entry).
        This function calls get_input_parameter and get_output_parameter,
        but avoids the repeated computation of the variable usage.

        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a 2-tuple of two lists, the first one containing \
            the input parameters, the second the output paramters.
        :rtype: 2-tuple of list of str

        '''
        variables_info = VariablesAccessInfo(node_list)
        return (self.get_input_parameters(node_list, variables_info),
                self.get_output_parameters(node_list, variables_info))
