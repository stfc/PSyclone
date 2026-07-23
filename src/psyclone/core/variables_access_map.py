# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2026, Science and Technology Facilities Council.
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


from typing import List, TYPE_CHECKING

from psyclone.core.signature import Signature
from psyclone.core.access_sequence import AccessSequence
from psyclone.core.access_type import AccessType
from psyclone.errors import InternalError
if TYPE_CHECKING:
    from psyclone.psyir.nodes import Node


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

    def add_access(
            self,
            signature: Signature,
            access_type: AccessType,
            node: "Node") -> None:
        '''Adds access information for the variable with the given signature.

        :param signature: the signature of the variable.
        :param access_type: the type of access (READ, WRITE, ...)
        :param node: Node in PSyIR in which the access happens.

        '''
        if not isinstance(signature, Signature):
            raise InternalError(f"Got '{signature}' of type "
                                f"'{type(signature).__name__}' but expected "
                                f"it to be of type psyclone.core.Signature.")

        if signature in self:
            self[signature].add_access(access_type, node)
        else:
            var_info = AccessSequence(signature)
            var_info.add_access(access_type, node)
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

    def update(self,   # type: ignore[override]
               other_access_map: "VariablesAccessMap") -> None:
        ''' Updates this dictionary with the entries in the provided
        VariablesAccessMap. If there are repeated signatures, the provided
        values are appended to the existing sequence of accesses.

        :param other_access_map: the other VariablesAccessMap instance.

        '''
        for signature in other_access_map.all_signatures:
            access_sequence = other_access_map[signature]
            if signature in self:
                var_info = self[signature]
            else:
                var_info = AccessSequence(signature)
                self[signature] = var_info
            var_info.update(access_sequence)

    def is_called(self, signature: Signature) -> bool:
        '''
        :param signature: signature of the variable.

        :returns: True if the specified variable is called at least once.
        '''
        return self[signature].is_called()

    def is_written(self, signature: Signature) -> bool:
        '''Checks if the specified variable signature is at least
        written once.

        :param signature: signature of the variable.

        :returns: True if the specified variable is written (at least \
            once).

        :raises: KeyError if the signature name cannot be found.

        '''
        var_access_info = self[signature]
        return var_access_info.is_written()

    def is_read(self, signature: Signature) -> bool:
        '''Checks if the specified variable signature is at least read once.

        :param signature: signature of the variable
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: True if the specified variable name is read (at least
            once).

        :raises: KeyError if the signature cannot be found.'''

        var_access_info = self[signature]
        return var_access_info.is_read()

    def has_read_write(self, signature: Signature) -> bool:
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
