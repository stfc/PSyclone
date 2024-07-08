# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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


from psyclone.core.access_type import AccessType
from psyclone.core.component_indices import ComponentIndices
from psyclone.errors import InternalError


class AccessInfo():
    '''This class stores information about a single access
    pattern of one variable (e.g. variable is read at a certain location).
    A location is a number which can be used to compare different accesses
    (i.e. if one access happens before another). Each consecutive
    location will have an increasing location number, but read and write
    accesses in the same statement will have the same location number.
    If the variable accessed is an array, this class will also store
    the indices used in the access.
    Note that the name of the variable is not stored in this class.
    It is a helper class used in the `SingleVariableAccessInfo` class,
    which stores all `AccessInfo` objects for a variable, and it stores
    the name of the variable.

    :param access: the access type.
    :type access_type: :py:class:`psyclone.core.access_type.AccessType`
    :param int location: a number used in ordering the accesses.
    :param node: Node in PSyIR in which the access happens.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    :param component_indices: indices used in the access, defaults to None.
    :type component_indices: None, [], a list or a list of lists of \
        :py:class:`psyclone.psyir.nodes.Node` objects, or an object of type \
        :py:class:`psyclone.core.component_indices.ComponentIndices`

    '''
    def __init__(self, access_type, location, node, component_indices=None):
        self._location = location
        self._access_type = access_type
        self._node = node
        if not isinstance(component_indices, ComponentIndices):
            self.component_indices = ComponentIndices(component_indices)
        else:
            self.component_indices = component_indices

    def __str__(self):
        '''Returns a string representation showing the access mode
        and location, e.g.: WRITE(5).'''
        return f"{self._access_type}({self._location})"

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
    def component_indices(self, component_indices):
        '''Sets the indices for this AccessInfo instance. The component_indices
        contains a list of indices for each component of the signature,
        e.g. for `a(i)%b(j,k)%c` the component_indices will be
        `[ [i], [j, k], [] ]` (with each element being the PSyIR of the
        index expression).

        :param component_indices: indices used in the access.
        :type component_indices: \
            :py:class:`psyclone.core.component_indices.ComponentIndices`

        :raises InternalError: if component_indices is not an instance \
            of :py:class:`psyclone.core.component_indices.ComponentIndices`.

        '''

        if not isinstance(component_indices, ComponentIndices):
            raise InternalError(f"The component_indices object in the setter "
                                f"of AccessInfo must be an instance of "
                                f"ComponentIndices, got '{component_indices}'")
        self._component_indices = component_indices

    def is_array(self):
        '''Test if any of the components has an index. E.g. an access like
        a(i)%b would still be considered an array.

        :returns: if any of the variable components uses an index, i.e.\
            the variable is an array.
        :rtype: bool
        '''
        return self._component_indices.is_array()

    @property
    def access_type(self):
        ''':returns: the access type.
        :rtype: :py:class:`psyclone.core.access_type.AccessType`'''
        return self._access_type

    @property
    def location(self):
        ''':returns: the location information for this access.\
        Please see the Developers' Guide for more information.
        :rtype: int
        '''
        return self._location

    @property
    def node(self):
        ''':returns: the PSyIR node at which this access happens.
        :rtype: :py:class:`psyclone.psyir.nodes.Node` '''
        return self._node


# =============================================================================
class SingleVariableAccessInfo():
    '''This class stores a list with all accesses to one variable.

    :param signature: signature of the variable.
    :type signature: :py:class:`psyclone.core.Signature`

    '''
    def __init__(self, signature):
        self._signature = signature
        # This is the list of AccessInfo instances for this variable.

        self._accesses = []

    def __str__(self):
        '''Returns a string representation of this object with the format:
        var_name:WRITE(2),WRITE(3),READ(5) where the numbers indicate
        the 'location' of the corresponding access. The location is an
        integer number that enumerates each statement in a program unit,
        and can be used to compare if an access is earlier, later or in
        the same statement as another access.

        '''
        all_accesses = ",".join([str(access) for access in self._accesses])

        return f"{self._signature}:{all_accesses}"

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

    def is_written(self):
        ''':returns: True if this variable is written (at least once).
        :rtype: bool
        '''
        return any(access_info.access_type in
                   AccessType.all_write_accesses()
                   for access_info in self._accesses)

    def is_written_first(self):
        ''':returns: True if this variable is written in the first access \
            (which indicates that this variable is not an input variable \
            for a kernel).

        :rtype: bool
        '''
        return len(self._accesses) > 0 and \
            (self._accesses[0].access_type == AccessType.WRITE)

    def is_read_only(self):
        '''Checks if this variable is always read, and never
        written.

        :returns: True if this variable is read only.
        :rtype: bool
        '''
        return all(access_info.access_type == AccessType.READ
                   for access_info in self._accesses)

    def is_read(self):
        ''':returns: True if this variable is read (at least once).
        :rtype: bool
        '''
        return any(access_info.access_type in AccessType.all_read_accesses()
                   for access_info in self._accesses)

    def has_read_write(self):
        '''Checks if this variable has at least one READWRITE access.

        :returns: True if this variable is read (at least once).
        :rtype: bool
        '''
        return any(access_info.access_type == AccessType.READWRITE
                   for access_info in self._accesses)

    def __getitem__(self, index):
        ''':return: the access information for the specified index.
        :rtype: py:class:`psyclone.core.AccessInfo`

        :raises IndexError: If there is no access with the specified index.
        '''
        return self._accesses[index]

    @property
    def all_accesses(self):
        ''':returns: a list with all AccessInfo data for this variable.
        :rtype: List[:py:class:`psyclone.core.AccessInfo`]
        '''
        return self._accesses

    @property
    def all_read_accesses(self):
        ''':returns: a list with all AccessInfo data for this variable
            that involve reading this variable.
        :rtype: List[:py:class:`psyclone.core.AccessInfo`]
        '''
        return [access for access in self._accesses
                if access.access_type in AccessType.all_read_accesses()]

    @property
    def all_write_accesses(self):
        ''':returns: a list with all AccessInfo data for this variable
            that involve writing this variable.
        :rtype: List[:py:class:`psyclone.core.AccessInfo`]
        '''
        return [access for access in self._accesses
                if access.access_type in AccessType.all_write_accesses()]

    def add_access_with_location(self, access_type, location, node,
                                 component_indices):
        '''Adds access information to this variable.

        :param access_type: the type of access (READ, WRITE, ....)
        :type access_type: \
            :py:class:`psyclone.core.access_type.AccessType`
        :param location: location information
        :type location: int
        :param node: Node in PSyIR in which the access happens.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param component_indices: indices used for each component of the \
            access.
        :type component_indices:  \
            :py:class:`psyclone.core.component_indices.ComponentIndices`
        '''
        self._accesses.append(AccessInfo(access_type, location, node,
                                         component_indices))

    def change_read_to_write(self):
        '''This function is only used when analysing an assignment statement.
        The LHS has first all variables identified, which will be READ.
        This function is then called to change the assigned-to variable
        on the LHS to from READ to WRITE. Since the LHS is stored in a separate
        SingleVariableAccessInfo class, it is guaranteed that there is only
        one entry for the variable.
        '''
        if len(self._accesses) != 1:
            raise InternalError(f"Variable '{self._signature}' had "
                                f"{len(self._accesses)} accesses listed, "
                                "not one in change_read_to_write.")

        if self._accesses[0].access_type != AccessType.READ:
            raise InternalError(f"Trying to change variable "
                                f"'{self._signature}' to 'WRITE' "
                                "which does not have 'READ' access.")

        self._accesses[0].change_read_to_write()

    def is_array(self, index_variable=None):
        '''Checks if the variable is used as an array, i.e. if it has
        an index expression. If the optional `index_variable` is specified,
        this variable must be used in (at least one) index access in order
        for this variable to be considered as an array.

        :param str index_variable: only considers this variable to be used \
            as array if there is at least one access using this \
            index_variable.

        :returns: true if there is at least one access to this variable \
            that uses an index.
        :rtype: bool

        '''
        is_array = any(access_info.is_array() for
                       access_info in self._accesses)

        # If there is no access information using an index, or there is no
        # index variable specified, return the current result:
        if not is_array or index_variable is None:
            return is_array

        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Reference

        for access_info in self._accesses:
            if any(ref.symbol.name == index_variable
                   for ref in access_info.node.walk(Reference)):
                return True

        # The index variable is not used in any index in any access:
        return False

    def is_written_before(self, reference):
        '''Returns True if this variable is written before the specified
        reference, and False if not.

        :param reference: the reference at which to stop for access checks.
        :type reference: :py:class:`psyclone.psyir.nodes.Reference`

        :returns: True if this variable is written before the specified \
            reference, and False if not.
        :rtype: bool

        :raises ValueError: if the specified reference is not in the list of \
            all accesses.

        '''
        result = False

        for access in self._accesses:
            if access.node is reference:
                return result
            if access.access_type == AccessType.WRITE:
                result = True
        raise ValueError(f"Reference not found in 'is_written_before' for "
                         f"variable '{self.var_name}'.")

    def is_read_before(self, reference):
        '''Returns True if this variable is read before the specified
        reference, and False if not.

        :param reference: the reference at which to stop for access checks.
        :type reference: :py:class:`psyclone.psyir.nodes.Reference`

        :returns: True if this variable is read before the specified \
            reference, and False if not.
        :rtype: bool

        :raises ValueError: if the specified reference is not in the list of \
            all accesses.

        '''
        result = False

        for access in self._accesses:
            if access.node is reference:
                return result
            if access.access_type == AccessType.READ:
                result = True
        raise ValueError(f"Reference not found in 'is_read_before' for "
                         f"variable '{self.var_name}'.")

    def is_accessed_before(self, reference):
        '''Returns True if this variable is accessed before the specified
        reference, and False if not. This is equivalent to testing that
        'reference' is the very first access, but this function will also
        verify that 'reference' is indeed in the list of accesses.

        :param reference: the reference at which to stop for access checks.
        :type reference: :py:class:`psyclone.psyir.nodes.Reference`

        :returns: True if this variable is read before the specified \
            reference, and False if not.
        :rtype: bool

        :raises ValueError: if the specified reference is not in the list of \
            all accesses.

        '''

        result = False

        for access in self._accesses:
            if access.node is reference:
                return result
            result = True
        raise ValueError(f"Reference not found in 'is_accessed_before' for "
                         f"variable '{self.var_name}'.")


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ["AccessInfo",
           "SingleVariableAccessInfo"
           ]
