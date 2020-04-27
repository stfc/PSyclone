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
# Modified by: S. Siso, STFC Daresbury Laboratory
#              A. R. Porter, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

'''This module provides management of variable access information.'''

from __future__ import print_function, absolute_import

from psyclone.core.access_type import AccessType
from psyclone.errors import InternalError


class AccessInfo(object):
    '''This class stores information about a single access
    pattern of one variable (e.g. variable is read at a certain location).
    A location is a number which can be used to compare different accesses
    (i.e. if one access happens before another). Each consecutive
    location will have an increasing location number, but read and write
    accesses in the same statement will have the same location number.
    If the variable accessed is an array, this class will also store
    the indices used in the access.
    Note that the name of the variable is not stored in this instance, it
    is typically available using VariableAccessInfo class (which stores all
    AccessInfo objects for a variable).

    :param access: The access type.
    :type access_type: :py:class:`psyclone.core.access_type.AccessType`
    :param int location: A number used in ordering the accesses.
    :param indices: Indices used in the access, defaults to None
    :type indices: list of :py:class:`psyclone.psyir.nodes.Node` instances \
        (e.g. Reference, ...)
    :param node: Node in PSyIR in which the access happens, defaults to None.
    :type node: :py:class:`psyclone.psyir.nodes.Node` instance.

    '''
    def __init__(self, access_type, location, node, indices=None):
        self._location = location
        self._access_type = access_type
        self._node = node
        # Create a copy of the list of indices
        if indices:
            self._indices = indices[:]
        else:
            self._indices = None

    def __str__(self):
        '''Returns a string representating showing the access mode
        and location, e.g.: WRITE(5).'''
        return "{0}({1})".format(self._access_type, self._location)

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
    def indices(self):
        '''
        :returns: the indices used in this access. Can be None.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node` instances, \
                or None.
        '''
        return self._indices

    @indices.setter
    def indices(self, indices):
        '''Sets the indices for this AccessInfo instance.

        :param indices: List of indices used in the access.
        :type indices: list of :py:class:`psyclone.psyir.nodes.Node` instances.
        '''
        self._indices = indices[:]

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
class VariableAccessInfo(object):
    '''This class stores a list with all accesses to one variable.

    :param str var_name: Name of the variable.

    '''
    def __init__(self, var_name):
        self._var_name = var_name
        # This is the list of AccessInfo instances for this variable.

        self._accesses = []

    def __str__(self):
        '''Returns a string representation of this object with the format:
        var_name:WRITE(2),WRITE(3),READ(5).'''
        return "{0}:{1}".format(self._var_name,
                                ",".join([str(access)
                                          for access in self._accesses]))

    @property
    def var_name(self):
        ''':returns: the name of the variable whose access info is managed.
        :rtype: str
        '''
        return self._var_name

    def is_written(self):
        ''':returns: True if this variable is written (at least once).
        :rtype: bool
        '''
        return any(access_info.access_type in
                   AccessType.all_write_accesses()
                   for access_info in self._accesses)

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
        :rtype: py:class:`psyclone.core.access_info.AccessInfo`

        :raises IndexError: If there is no access with the specified index.
        '''
        return self._accesses[index]

    @property
    def all_accesses(self):
        ''':returns: a list with all AccessInfo data for this variable.
        :rtype: List of :py:class:`psyclone.core.access_info.AccessInfo` \
            instances.
        '''
        return self._accesses

    def add_access(self, access_type, location, node, indices=None):
        '''Adds access information to this variable.

        :param access_type: The type of access (READ, WRITE, ....)
        :type access_type: \
            :py:class:`psyclone.core.access_type.AccessType`
        :param location: Location information
        :type location: int
        :param indicies: Indices used in the access (None if the variable \
            is not an array). Defaults to None
        :type indices: list of :py:class:`psyclone.psyir.nodes.Node` instances.
        :param node: Node in PSyIR in which the access happens.
        :type node: :py:class:`psyclone.psyir.nodes.Node` instance.
        '''
        self._accesses.append(AccessInfo(access_type, location, node, indices))

    def change_read_to_write(self):
        '''This function is only used when analysing an assignment statement.
        The LHS has first all variables identified, which will be READ.
        This function is then called to change the assigned-to variable
        on the LHS to from READ to WRITE. Since the LHS is stored in a separate
        VariableAccessInfo class, it is guaranteed that there is only
        one entry for the variable.
         '''
        if len(self._accesses) != 1:
            raise InternalError("Variable '{0}' had {1} accesses listed, "
                                "not one in change_read_to_write.".
                                format(self._var_name,
                                       len(self._accesses)))

        if self._accesses[0].access_type != AccessType.READ:
            raise InternalError("Trying to change variable '{0}' to 'WRITE' "
                                "which does not have 'READ' access."
                                .format(self.var_name))

        self._accesses[0].change_read_to_write()


# =============================================================================
class VariablesAccessInfo(dict):
    '''This class stores all VariableAccessInfo instances for all variables
    in the corresponding code section. It maintains a 'location' information,
    which is an integer number that is increased for each new statement. It
    can be used to easily determine if one access is before another.

    :param nodes: optional, a single PSyIR node or list of nodes from \
                  which to initialise this object.
    :type nodes: None, :py:class:`psyclone.psyir.nodes.Node` or a list of \
                 :py:class:`psyclone.psyir.nodes.Node`.

    '''
    def __init__(self, nodes=None):
        # This dictionary stores the mapping of variable names to the
        # corresponding VariableAccessInfo instance.
        dict.__init__(self)

        # Stores the current location information
        self._location = 0
        if nodes:
            # Import here to avoid circular dependency
            from psyclone.psyir.nodes import Node
            if isinstance(nodes, list):
                for node in nodes:
                    if not isinstance(node, Node):
                        raise InternalError("Error in VariablesAccessInfo. "
                                            "One element in the node list is "
                                            "not a Node, but of type {0}"
                                            .format(type(node)))

                    node.reference_accesses(self)
            elif isinstance(nodes, Node):
                nodes.reference_accesses(self)
            else:
                arg_type = str(type(nodes))
                raise InternalError("Error in VariablesAccessInfo. "
                                    "Argument must be a single Node in a "
                                    "schedule or a list of Nodes in a "
                                    "schedule but have been passed an "
                                    "object of type: {0}".
                                    format(arg_type))

    def __str__(self):
        '''Gives a shortened visual representation of all variables
        and their access mode. The output is one of: READ, WRITE, READ+WRITE,
        or READWRITE for each variable accessed.
        READ+WRITE is used if the statement (or set of statements)
        contain individual read and write accesses, e.g. 'a=a+1'. In this
        case two accesses to `a` will be recorded, but the summary displayed
        using this function will be 'READ+WRITE'. Same applies if this object
        stores variable access information about more than one statement, e.g.
        'a=b; b=1'. There would be two different accesses to 'b' with two
        different locations, but the string representation would show this as
        READ+WRITE. If a variable is is passed to a kernel for which no
        individual variable information is available, and the metadata for
        this kernel indicates a READWRITE access, this is marked as READWRITE
        in the string output.'''

        all_vars = list(self.keys())
        all_vars.sort()
        output_list = []
        for var_name in all_vars:
            mode = ""
            if self.has_read_write(var_name):
                mode = "READWRITE"
            else:
                if self.is_read(var_name):
                    if self.is_written(var_name):
                        mode = "READ+WRITE"
                    else:
                        mode = "READ"
                elif self.is_written(var_name):
                    mode = "WRITE"
            output_list.append("{0}: {1}".format(var_name, mode))
        return ", ".join(output_list)

    @property
    def location(self):
        '''Returns the current location of this instance, which is
        the location at which the next accesses will be stored.
        See the Developers' Guide for more information.

        :returns: the current location of this object.
        :rtype: int'''
        return self._location

    def next_location(self):
        '''Increases the location number.'''
        self._location = self._location + 1

    def add_access(self, var_name, access_type, node, indices=None):
        '''Adds access information for the specified variable.

        :param str var_name: Name of the variable for which an access is\
            added.
        :param access_type: The type of access (READ, WRITE, ...)
        :type access_type: :py:class:`psyclone.core.access_type.AccessType`
        :param node: Node in PSyIR in which the access happens.
        :type node: :py:class:`psyclone.psyir.nodes.Node` instance.
        :param indicies: Indices used in the access (None if the variable \
            is not an array). Defaults to None.
        :type indices: list of :py:class:`psyclone.psyir.nodes.Node` instances.

        '''
        if var_name in self:
            self[var_name].add_access(access_type, self._location,
                                      node, indices)
        else:
            var_info = VariableAccessInfo(var_name)
            var_info.add_access(access_type, self._location, node, indices)
            self[var_name] = var_info

    @property
    def all_vars(self):
        ''':returns: all variables contained in this instance, sorted (in \
                     order to make test results reproducible).
        :rtype: list of str.
        '''
        list_of_vars = list(self.keys())
        list_of_vars.sort()
        return list_of_vars

    def merge(self, other_access_info):
        '''Merges data from a VariablesAccessInfo instance to the
        information in this instance.

        :param other_access_info: The other VariablesAccessInfo instance.
        :type other_access_info: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # For each variable add all accesses. After merging the new data,
        # we need to increase the location so that all further added data
        # will have a location number that is larger.
        max_new_location = 0
        for var_name in other_access_info.all_vars:
            var_info = other_access_info[var_name]
            for access_info in var_info.all_accesses:
                # Keep track of how much we need to update the next location
                # in this object:
                if access_info.location > max_new_location:
                    max_new_location = access_info.location
                new_location = access_info.location + self._location
                if var_name in self:
                    var_info = self[var_name]
                else:
                    var_info = VariableAccessInfo(var_name)
                    self[var_name] = var_info

                var_info.add_access(access_info.access_type, new_location,
                                    access_info.node, access_info.indices)
        # Increase the current location of this instance by the amount of
        # locations just merged in
        self._location = self._location + max_new_location

    def is_written(self, var_name):
        '''Checks if the specified variable name is at least written once.

        :param str var_name: Name of the variable

        :returns: True if the specified variable name is written (at least \
            once).
        :rtype: bool

        :raises: KeyError if the variable name cannot be found.

        '''
        var_access_info = self[var_name]
        return var_access_info.is_written()

    def is_read(self, var_name):
        '''Checks if the specified variable name is at least read once.

        :param str var_name: Name of the variable
        :returns: True if the specified variable name is read (at least \
            once).
        :rtype: bool
        :raises: KeyError if the variable names can not be found.'''

        var_access_info = self[var_name]
        return var_access_info.is_read()

    def has_read_write(self, var_name):
        '''Checks if the specified variable name has at least one READWRITE
        access (which is typically only used in a function call)

        :param str var_name: Name of the variable
        :returns: True if the specified variable name has (at least one) \
            READWRITE access.
        :rtype: bool
        :raises: KeyError if the variable names can not be found.'''

        var_access_info = self[var_name]
        return var_access_info.has_read_write()
