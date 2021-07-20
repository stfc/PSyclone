# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
# Modified: A. R. Porter, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module provides tools that are based on the code
    dependency analysis.'''

from __future__ import absolute_import, print_function

from psyclone.core import AccessType, Signature, VariablesAccessInfo
from psyclone.errors import InternalError
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
    :param language_writer: a backend visitor to convert PSyIR expressions \
        to a representation in the selected language. This is used for \
        creating error and warning messages.
    :type language_writer: None (default is Fortran), or an instance \
        of :py:class:`psyclone.psyir.backend.visitor.PSyIRVisitor`
    '''
    def __init__(self, loop_types_to_parallelise=None,
                 language_writer=FortranWriter()):
        if loop_types_to_parallelise:
            self._loop_types_to_parallelise = loop_types_to_parallelise[:]
        else:
            self._loop_types_to_parallelise = []

        self._language_writer = language_writer
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
    def array_accesses_consistent(self, loop_variable, var_infos,
                                  all_indices=None):
        '''Check whether all accesses to an array, whose accesses are
        specified in the `var_infos` parameter, have consistent usage of
        the loop variable. This can be used e.g. to verify whether two loops
        may be fused by providing the access information of each loop in
        the `var_infos` parameter as a list. For example, `a(i,j)` and
        `a(j,i)` would be inconsistent. It does not test for index values
        (e.g. `a(i,j)` and `a(i+3,j)`).

        If the optional argument `all_indices` is given, it will store the
        list of all accesses that use the loop variable. This is an additional
        convenient result that can simplify further tests.

        :param loop_variable: symbol of the variable associated with the \
            loops being fused.
        :type loop_variable: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param var_infos: access information for an array. Can be either a \
            single object, or a list of access objects.
        :type var_infos: a list or a single instance of \
            :py:class:`psyclone.core.var_info.SingleVariableAccessInfo`
        :param all_indices: optional list argument which will store all \
            indices that use the specified loop variable.
        :type all_indices: None or a list to which all PSyIR expressions \
            of indices that use the loop variable are added.

        :returns: True if all array accesses are consistent.
        :rtype: bool

        :raises InternalError: if more than one one SingleVariableAccessInfo
            object is given and the information is for different variables.
        '''

        # pylint: disable=too-many-locals
        consistent = True

        if not isinstance(var_infos, list):
            all_accesses = var_infos.all_accesses
            signature = var_infos.signature
        else:
            # Verify that all var_info objects are indeed for
            # the same variable.
            signature = var_infos[0].signature
            different = [vi.signature for vi in var_infos
                         if vi.signature != signature]
            if different:
                diff_string = [str(sig) for sig in different]
                raise InternalError("Inconsistent signature provided in "
                                    "'array_accesses_consistent'. Expected "
                                    "all accesses to be for '{0}', but also "
                                    "got '{1}'."
                                    .format(signature, ",".join(diff_string)))
            all_accesses = []
            for var_info in var_infos:
                all_accesses = all_accesses + var_info.all_accesses

        # The variable 'first_index' will store the index of the component
        # and the dimension used when accessing it (i.e. it is a 2-tuple),
        # which is what `ComponentIndices.iterate` returns.
        # For example, `a(i)%b(j,k)` would have `(1, 0)` as the `first_index`
        # when checking for the loop variable `j`. This 2-tuple can be
        # used to get the corresponding PSyIR node from the component_indices
        # object. In 'first_component_indices' it will also store the
        # ComponentIndices of the first access. This is used for more
        # informative error messages if required.
        first_index = None
        first_component_indices = None

        # Test all access to the array. Consider the following code (enclosed
        # in nested j, i loops), when analysing the 'j' loop:
        #       a(i,j) = a(i,j) + 1    ! (1)
        #       b(i,j) = sin(a(i,j))   ! (2)
        #       c(i,j) = b(j,i)        ! (3)
        # When testing the accesses to 'a', three access will be reported,
        # a read and a write access in (1), and another read access in (2).
        # These accesses are consistent, they all have 'j' as the second
        # dimension. When testing 'b' on the other hand, there will be
        # two accesses (write in (2) and read in (3)), but the accesses are
        # not consistent - 'j' is used in dimension 2 when writing, and
        # in dimension 1 when reading.

        # Loop over all the accesses to the array:
        for access in all_accesses:
            component_indices = access.component_indices
            # Now verify that the index variable is always used
            # at the same place:
            for indx in component_indices.iterate():
                index_expression = component_indices[indx]
                accesses = VariablesAccessInfo(index_expression)

                # If the loop variable is not used at all, no need to
                # check indices
                if Signature(loop_variable.name) not in accesses:
                    continue
                # Store the first access information:
                if not first_index:
                    first_index = indx
                    first_component_indices = component_indices
                elif first_index != indx:
                    # If a previously identified index location does not match
                    # the current index location (e.g. a(i,j), and a(j,i) ),
                    # then add an error message:
                    consistent = False
                    self._add_error(
                        "Variable '{0}' is written to and the loop variable "
                        "'{1}' is used in different index locations: "
                        "{2} and {3}."
                        .format(signature.var_name,
                                loop_variable.name,
                                signature.to_language(first_component_indices),
                                signature.to_language(component_indices)))
                if all_indices is not None:
                    # If requested, collect all indices that are actually
                    # used as a convenience additional result for the user
                    all_indices.append(index_expression)

        return consistent

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
    def array_access_parallelisable(self, loop_variable, var_info):
        '''Tries to determine if the access pattern for a variable
        given in `var_info` allows parallelisation along the variable
        `loop_variable`. The following messages might be provided
        to the user using the message API:

        * if the array access does not depend on the loop variable, a
          warning is added (e.g. for the variable `a` in `a(1,2) = b(i,j)`).
        * if the array variable is accessed inconsistently, e.g.
          `a(i,j) = a(j,i) + 1`.

        :param loop_variable: symbol of the variable that is parallelised.
        :type loop_variable: :py:class:`psyclone.psyir.symbol.DataSymbol`
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

        # Now detect which dimension(s) is/are parallelised, i.e.
        # which dimension depends on the loop_variable.  For example
        # if a "do j..." loop is parallelised, consider expressions like
        # a(i,j) and a(j+2, i-1) in one loop:
        # In this case the dimensions 1 (a(i,j)) and 0 (a(j+2,i-1)) would
        # be accessed. Since the variable is written somewhere (read-only
        # was tested above), the variable cannot be used in parallel.
        # Additionally, collect all indices that are actually used, since
        # they are needed in a test further down.

        # Detect if this variable adds a new message, and if so, abort early
        all_indices = []
        consistent = self.array_accesses_consistent(loop_variable, var_info,
                                                    all_indices)
        if not consistent:
            return False

        if not all_indices:
            # An array is used that is not actually dependent on the parallel
            # loop variable, but is written to (which was checked earlier
            # in this function). This means the variable can not always be
            # safely parallelised. Example 1:
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
                                      loop_variable.name))
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
                self._add_warning("Variable '{0}' is written and is accessed "
                                  "using indices '{1}' and '{2}' and can "
                                  "therefore not be parallelised."
                                  .format(var_info.var_name,
                                          self._language_writer(first_index),
                                          self._language_writer(index)))
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
                                 signatures_to_ignore=None,
                                 var_accesses=None):
        # pylint: disable=too-many-arguments, too-many-branches
        # pylint: disable=too-many-locals
        '''This function analyses a loop in the PsyIR to see if
        it can be safely parallelised over the specified variable.

        :param loop: the loop node to be analysed.
        :type loop: :py:class:`psyclone.psyir.nodes.Loop`
        :param loop_variable: Optional symbol of the variable that is \
            parallelised. If not specified, the loop variable of the loop \
            is used.
        :type loop_variable: :py:class:`psyclone.psyir.symbol.DataSymbol`
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
            loop_variable = loop.variable

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
                par_able = self.array_access_parallelisable(loop_variable,
                                                            var_info)
            else:
                # Handle scalar variable
                par_able = self.is_scalar_parallelisable(var_info)
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
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param variables_info: optional variable usage information, \
            can be used to avoid repeatedly collecting this information.
        :type variables_info: \
            :py:class:`psyclone.core.variables_info.VariablesAccessInfo`

        :returns: a list of all variable signatures that are read.
        :rtype: list of :py:class:`psyclone.core.Signature`

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
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param variables_info: optional variable usage information, \
            can be used to avoid repeatedly collecting this information.
        :type variables_info: \
            :py:class:`psyclone.core.variables_info.VariablesAccessInfo`

        :returns: a list of all variable signatures that are written.
        :rtype: list of :py:class:`psyclone.core.Signature`

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
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a 2-tuple of two lists, the first one containing \
            the input parameters, the second the output paramters.
        :rtype: 2-tuple of list of :py:class:`psyclone.core.Signature`

        '''
        variables_info = VariablesAccessInfo(node_list)
        return (self.get_input_parameters(node_list, variables_info),
                self.get_output_parameters(node_list, variables_info))
