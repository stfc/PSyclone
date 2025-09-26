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


from __future__ import annotations
from typing import TYPE_CHECKING, Optional

from psyclone.core.access_type import AccessType
from psyclone.core.component_indices import ComponentIndices
from psyclone.errors import InternalError
if TYPE_CHECKING:  # pragma: no cover
    from psyclone.psyir.nodes import Node


class AccessInfo():
    ''' This class stores information about an access to a variable (the node
    where it happens and the type of access, and the index accessed if
    available).

    :param access: the access type.
    :param node: Node in PSyIR in which the access happens.
    :param component_indices: indices used in the access, defaults to None.

    '''
    def __init__(
        self, access_type: AccessType, node: 'Node',
        component_indices: Optional[list[list['Node']] |
                                    ComponentIndices] = None
    ):
        self._access_type = access_type
        self._node = node
        if not isinstance(component_indices, ComponentIndices):
            self.component_indices = ComponentIndices(component_indices)
        else:
            self.component_indices = component_indices

    def __str__(self):
        return f"{self._access_type}"

    def change_read_to_write(self):
        '''This changes the access mode from READ to WRITE.
        This is used for processing assignment statements,
        where the LHS is first considered to be READ,
        and which is then changed to be WRITE.

        :raises InternalError: if the variable originally does not have\
            READ access.

        '''
        if self._access_type != AccessType.READ:
            raise InternalError("Trying to change variable to 'WRITE' "
                                "which does not have 'READ' access.")
        self._access_type = AccessType.WRITE

    @property
    def component_indices(self):
        '''
        This function returns the list of accesses used for each component
        as an instance of ComponentIndices. For example, `a(i)%b(j,k)%c`
        will return an instance of ComponentIndices representing
        `[ [i], [j, k], [] ]`. In the case of a simple scalar variable
        such as `a`, the `component_indices` will represent `[ [] ]`.

        :returns: the indices used in this access for each component.
        :rtype: :py:class:`psyclone.core.component_indices.ComponentIndices`
        '''
        return self._component_indices

    @component_indices.setter
    def component_indices(self, component_indices: ComponentIndices):
        '''Sets the indices for this AccessInfo instance. The component_indices
        contains a list of indices for each component of the signature,
        e.g. for `a(i)%b(j,k)%c` the component_indices will be
        `[ [i], [j, k], [] ]` (with each element being the PSyIR of the
        index expression).

        :param component_indices: indices used in the access.

        :raises InternalError: if component_indices is not an instance \
            of :py:class:`psyclone.core.component_indices.ComponentIndices`.

        '''

        if not isinstance(component_indices, ComponentIndices):
            raise InternalError(f"The component_indices object in the setter "
                                f"of AccessInfo must be an instance of "
                                f"ComponentIndices, got '{component_indices}'")
        self._component_indices = component_indices

    def has_indices(self) -> bool:
        '''
        :returns: whether any of the access components uses an index.
        '''
        return self._component_indices.has_indices()

    @property
    def access_type(self) -> AccessType:
        '''
        :returns: the access type.
        '''
        return self._access_type

    def is_any_write(self) -> bool:
        '''
        :returns: whether this access represents a write of any kind.
        '''
        return self._access_type in AccessType.all_write_accesses()

    def is_any_read(self) -> bool:
        '''
        :returns: whether this access represents a write of any kind.
        '''
        return self._access_type in AccessType.all_read_accesses()

    @property
    def is_data_access(self) -> bool:
        '''
        :returns: whether or not this access is to the data associated with
                  a signature (i.e. is not just an inquiry-type access).
        '''
        return self._access_type not in AccessType.non_data_accesses()

    @property
    def node(self):
        ''':returns: the PSyIR node at which this access happens.
        :rtype: :py:class:`psyclone.psyir.nodes.Node` '''
        return self._node

    @property
    def description(self) -> str:
        '''
        :returns: a textual description of this access for use in error
                  messages.
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Statement
        from psyclone.psyir.symbols import Symbol
        if isinstance(self.node, Symbol):
            text = f"the definition of Symbol '{self.node}'"
        else:
            stmt = self.node.ancestor(Statement, include_self=True)
            if stmt:
                text = f"'{stmt.debug_string()}'"
            else:
                text = f"'{self.node.debug_string()}'"
        return text


# =============================================================================
class AccessSequence(list):
    ''' This class stores a list with all accesses to one variable.

    :param signature: signature of the variable.
    :type signature: :py:class:`psyclone.core.Signature`

    '''
    def __init__(self, signature):
        self._signature = signature

    def __str__(self) -> str:
        '''
        :returns: a string representation of this object with the format:
            var_name:[WRITE,WRITE,READ]
        '''
        all_accesses = ",".join([str(access) for access in self])

        return f"{self._signature}:[{all_accesses}]"

    def str_access_summary(self) -> str:
        '''
        :returns: a string of the accesstypes but removing duplicates.
        '''
        # Use a dict comprehension to get non-duplicated ordered results
        access_set = "+".join({str(access): None for access in self}.keys())
        return f"{access_set}"

    @property
    def signature(self):
        ''':returns: the signature for which the accesses are stored.
        :rtype: :py:class:`psyclone.core.Signature`
        '''
        return self._signature

    @property
    def var_name(self):
        ''':returns: the name of the variable whose access info is managed.
        :rtype: str
        '''
        return str(self._signature)

    def is_called(self) -> bool:
        '''
        :returns: whether or not any accesses of this variable
                  represent a call.
        '''
        return any(access.access_type == AccessType.CALL for access in self)

    def is_written(self) -> bool:
        '''
        :returns: True if this variable is written (at least once).
        '''
        return any(access_info.access_type in
                   AccessType.all_write_accesses()
                   for access_info in self)

    def is_written_first(self) -> bool:
        '''
        :returns: True if this variable is written in the first data access
            (which indicates that this variable is not an input variable
            for a kernel).
        '''
        for acc in self:
            if acc.access_type in AccessType.non_data_accesses():
                continue
            if acc.access_type != AccessType.WRITE:
                return False
            return True
        return False

    def is_read_only(self) -> bool:
        '''Checks if this variable is always read, and never written.

        :returns: True if this variable is read only.
        '''
        access_types = AccessType.non_data_accesses() + [AccessType.READ]
        return all(access_info.access_type in access_types
                   for access_info in self)

    def is_read(self) -> bool:
        '''
        :returns: True if this variable is read (at least once).
        '''
        read_accesses = AccessType.all_read_accesses()
        return any(access_info.access_type in read_accesses
                   for access_info in self)

    def has_read_write(self):
        '''Checks if this variable has at least one READWRITE access.

        :returns: True if this variable is read (at least once).
        :rtype: bool
        '''
        return any(access_info.access_type == AccessType.READWRITE
                   for access_info in self)

    def has_data_access(self) -> bool:
        '''
        :returns: True if there is an access of the data associated with this
            signature (as opposed to a call or an inquiry), False otherwise.
        '''
        for info in self:
            if info.access_type not in AccessType.non_data_accesses():
                return True
        return False

    @property
    def all_read_accesses(self):
        ''':returns: a list with all AccessInfo data for this variable
            that involve reading this variable.
        :rtype: List[:py:class:`psyclone.core.AccessInfo`]
        '''
        return [access for access in self
                if access.access_type in AccessType.all_read_accesses()]

    @property
    def all_write_accesses(self):
        ''':returns: a list with all AccessInfo data for this variable
            that involve writing this variable.
        :rtype: List[:py:class:`psyclone.core.AccessInfo`]
        '''
        return [access for access in self
                if access.access_type in AccessType.all_write_accesses()]

    def add_access(
        self, access_type: AccessType, node: 'Node',
        component_indices: Optional[list[list['Node']] |
                                    ComponentIndices] = None
    ):
        '''Adds access information to this variable.

        :param access_type: the type of access (READ, WRITE, ....)
        :param node: Node in PSyIR in which the access happens.
        :param component_indices: indices used for each component of the \
            access.
        '''
        self.append(AccessInfo(access_type, node, component_indices))

    def change_read_to_write(self):
        '''This function is only used when analysing an assignment statement.
        The LHS has first all variables identified, which will be READ.
        This function is then called to change the assigned-to variable
        on the LHS to from READ to WRITE. Since the LHS is stored in a separate
        AccessSequence class, it is guaranteed that there is only
        one READ entry for the variable (although there maybe INQUIRY accesses
        for array bounds).

        :raises InternalError: if there is an access that is not READ or
                               INQUIRY or there is > 1 READ access.
        '''
        read_access = None
        for acc in self:

            if acc.access_type == AccessType.READ:
                if read_access:
                    raise InternalError(
                        f"Trying to change variable '{self._signature}' to "
                        f"'WRITE' but it has more than one 'READ' access.")
                read_access = acc

            elif acc.access_type not in AccessType.non_data_accesses():
                raise InternalError(
                    f"Variable '{self._signature}' has a '{acc.access_type}' "
                    f"access. change_read_to_write() expects only inquiry "
                    f"accesses and a single 'READ' access.")

        if not read_access:
            raise InternalError(
                f"Trying to change variable '{self._signature}' to 'WRITE' but"
                f" it does not have a 'READ' access.")
        read_access.change_read_to_write()

    def has_indices(self, index_variable: str = None) -> bool:
        ''' Checks whether this variable accesses has any index. If the
        optional `index_variable` is provided, only indices involving the given
        variable are considered.

        :param index_variable: only consider index expressions that involve
            this variable.

        :returns: true if any of the accesses has an index.

        '''
        has_indices = any(access.has_indices() for access in self)

        # If there is no access information using an index, or there is no
        # index variable specified, return the current result:
        if not has_indices or index_variable is None:
            return has_indices

        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Reference

        lowered_name = index_variable.lower()
        for access_info in self:
            if any(ref.symbol.name.lower() == lowered_name
                   for ref in access_info.node.walk(Reference)):
                return True

        # The index variable is not used in any index in any access:
        return False


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ["AccessInfo",
           "AccessSequence"
           ]
