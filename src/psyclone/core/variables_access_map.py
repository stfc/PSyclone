# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council.
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
# Modified by: S. Siso, STFC Daresbury Laboratory
#              A. R. Porter, STFC Daresbury Laboratory
#              A. B. G. Chalk, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

'''This module provides management of variable access information.'''


from typing import List

from psyclone.core.component_indices import ComponentIndices
from psyclone.core.signature import Signature
from psyclone.core.access_sequence import AccessSequence
from psyclone.errors import InternalError


class VariablesAccessMap(dict):
    ''' This dictionary stores `AccessSequence` instances indexed by
    their signature.

    '''

    def __str__(self):
        '''Gives a shortened visual representation of all variables
        and their access mode '''

        output_list = []
        for key, value in self.items():
            output_list.append(f"{key}: {value.str_access_summary()}")
        return ", ".join(sorted(output_list))

    def add_access(self, signature, access_type, node, component_indices=None):
        '''Adds access information for the variable with the given signature.
        If the `component_indices` parameter is not an instance of
        `ComponentIndices`, it is used to construct an instance. Therefore it
        can be None, a list or a list of lists of PSyIR nodes. In the case of
        a list of lists, this will be used unmodified to construct the
        ComponentIndices structures. If it is a simple list, it is assumed
        that it contains the indices used in accessing the last component
        of the signature. For example, for `a%b` with
        `component_indices=[i,j]`, it will create `[[], [i,j]` as component
        indices, indicating that no index is used in the first component `a`.
        If the access is supposed to be for `a(i)%b(j)`, then the
        `component_indices` argument must be specified as a list of lists,
        i.e. `[[i], [j]]`.

        :param signature: the signature of the variable.
        :type signature: :py:class:`psyclone.core.Signature`
        :param access_type: the type of access (READ, WRITE, ...)
        :type access_type: :py:class:`psyclone.core.access_type.AccessType`
        :param node: Node in PSyIR in which the access happens.
        :type node: :py:class:`psyclone.psyir.nodes.Node` instance
        :param component_indices: index information for the access.
        :type component_indices: \
            :py:class:`psyclone.core.component_indices.ComponentIndices`, or \
            any other type that can be used to construct a ComponentIndices \
            instance (None, List[:py:class:`psyclone.psyir.nodes.Node`] \
             or List[List[:py:class:`psyclone.psyir.nodes.Node`]])

        '''
        if not isinstance(signature, Signature):
            raise InternalError(f"Got '{signature}' of type "
                                f"'{type(signature).__name__}' but expected "
                                f"it to be of type psyclone.core.Signature.")

        # To make it easier for the user, we allow to implicitly create the
        # component indices instance here:
        if not isinstance(component_indices, ComponentIndices):
            # Handle some convenient cases:
            # 1. Add the right number of [] if component_indices is None:
            if component_indices is None:
                component_indices = [[]] * len(signature)
            elif isinstance(component_indices, list):
                # 2. If the argument is a simple list (not a list of lists),
                # assume that the indices are for the last component, and
                # add enough [] to give the right number of entries in the
                # list that is used to create the ComponentIndices instance:
                is_list_of_lists = all(isinstance(indx, list)
                                       for indx in component_indices)
                if not is_list_of_lists:
                    component_indices = [[]] * (len(signature)-1) \
                                      + [component_indices]

            component_indices = ComponentIndices(component_indices)

        if len(signature) != len(component_indices):
            raise InternalError(f"Cannot add '{component_indices}' with "
                                f"length {len(component_indices)} as "
                                f"indices for '{signature}' which "
                                f"requires {len(signature)} elements.")

        if signature in self:
            self[signature].add_access(access_type, node, component_indices)
        else:
            var_info = AccessSequence(signature)
            var_info.add_access(access_type, node, component_indices)
            self[signature] = var_info

    @property
    def all_signatures(self):
        ''':returns: all signatures contained in this instance, sorted (in \
                     order to make test results reproducible).
        :rtype: List[:py:class:`psyclone.core.Signature`]
        '''
        list_of_vars = list(self.keys())
        list_of_vars.sort()
        return list_of_vars

    @property
    def all_data_accesses(self) -> List[Signature]:
        '''
        :returns: all Signatures in this instance that have a data access (i.e.
                  the data associated with them is read or written).
        '''
        result = []
        for sig in self.all_signatures:
            if self[sig].has_data_access():
                result.append(sig)
        return result

    def update(self, other_access_map):
        ''' Updates this dictionary with the entries in the provided
        VariablesAccessMap. If there are repeated signatures, the provided
        values are appended to the existing sequence of accesses.

        :param other_access_map: the other VariablesAccessMap instance.
        :type other_access_map: :py:class:`psyclone.core.VariablesAccessMap`

        '''
        for signature in other_access_map.all_signatures:
            access_sequence = other_access_map[signature]
            for access_info in access_sequence:
                if signature in self:
                    var_info = self[signature]
                else:
                    var_info = AccessSequence(signature)
                    self[signature] = var_info

                var_info.add_access(access_info.access_type,
                                    access_info.node,
                                    access_info.component_indices)

    def is_called(self, signature: Signature) -> bool:
        '''
        :param signature: signature of the variable.

        :returns: True if the specified variable is called at least once.
        '''
        return self[signature].is_called()

    def is_written(self, signature):
        '''Checks if the specified variable signature is at least
        written once.

        :param signature: signature of the variable.
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: True if the specified variable is written (at least \
            once).
        :rtype: bool

        :raises: KeyError if the signature name cannot be found.

        '''
        var_access_info = self[signature]
        return var_access_info.is_written()

    def is_read(self, signature) -> bool:
        '''Checks if the specified variable signature is at least read once.

        :param signature: signature of the variable
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: True if the specified variable name is read (at least
            once).

        :raises: KeyError if the signature cannot be found.'''

        var_access_info = self[signature]
        return var_access_info.is_read()

    def has_read_write(self, signature):
        '''Checks if the specified variable signature has at least one
        READWRITE access (which is typically only used in a function call).

        :param signature: signature of the variable
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: True if the specified variable name has (at least one) \
            READWRITE access.
        :rtype: bool

        :raises: KeyError if the signature cannot be found.'''

        var_access_info = self[signature]
        return var_access_info.has_read_write()


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ["VariablesAccessMap"]
