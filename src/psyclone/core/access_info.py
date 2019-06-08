# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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

'''This module management of variable access information.'''

from __future__ import print_function, absolute_import

from psyclone.core.access_type import AccessType


class AccessInfo(object):
    '''This class stores information about a single access
    pattern of one variable (e.g. variable is read at a certain location).
    A location is a number which can be used to compare different access
    (i.e. if one access is happening before another). Each consecutive
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
    :type indices: list of :py:class:`psyclone.psyGen.Node` instances \
        (e.g. Reference, ...)
    '''

    def __init__(self, access_type, location=None, indices=None):
        self._location = location
        self._access_type = access_type
        # Create a copy of the list of indices
        if indices:
            self._indices = indices[:]
        else:
            self._indices = None

    def change_read_to_write(self):
        '''This changes the access mode from READ to WRITE.
        This is used for processing assignment statements,
        where the LHS is first considered to be 'read',
        and which is then changed to be write.'''
        assert self._access_type == AccessType.READ
        self._access_type = AccessType.WRITE

    def set_indices(self, indices):
        '''Sets the indices for this AccessInfo instance.
        :param indices: List of indices used in the access.
        :type indices: list of :py:class:`psyclone.psyGen.Node` instances.
        '''
        self._indices = indices[:]

    def get_indices(self):
        ''':returns: The indices used in this access. Can be None.
        :rtype: List of :py:class:`psyclone.psyGen.Node` instances, or None.
        '''
        return self._indices

    def get_access_type(self):
        ''':returns: the access type.
        :rtype: :py:class:`psyclone.core.access_type.AccessType`'''
        return self._access_type

    def get_location(self):
        ''':returns: the location information for this access.
        :rtype: int'''
        return self._location


# =============================================================================
class VariableAccessInfo(object):
    '''This class stores a list with all accesses to one variable.
    :param str var_name: Name of the variable.
    '''

    def __init__(self, var_name):
        self._var_name = var_name
        # This is the list of AccessInfo instances for this variable.
        self._accesses = []

    def get_var_name(self):
        ''':returns: the name of the variable whose access info is managed.
        :rtype: str
        '''
        return self._var_name

    def is_written(self):
        '''Checks if the specified variable name is at least written once.
        :returns: true if the specified variable name is written (at least \
            once).
        :rtype: bool
        '''
        for access_info in self._accesses:
            if access_info.get_access_type() == AccessType.WRITE:
                return True
        return False

    def is_read(self):
        '''Checks if the specified variable name is at least read once.
        :returns: true if the specified variable name is read (at least once).
        :rtype: bool
        '''
        for access_info in self._accesses:
            if access_info.get_access_type() == AccessType.READ:
                return True
        return False

    def get_all_accesses(self):
        ''':returns: a list with all AccessInfo data for this variable.
        :rtype: List of :py:class:`psyclone.core.access_info.AccessInfo` \
            instances.
        '''
        return self._accesses

    def add_access(self, access_type, location, indices=None):
        '''Adds access information to this variable.
        :param access_type: The type of access (READ, WRITE, ....)
        :type access_type
            :py:class:`psyclone.core.access_type.AccessType`
        :param location: Location information
        :type location: int
        :param indicies: Indices used in the access (None if the variable \
            is not an array). Defaults to None
        :type indices: list of :py:class:`psyclone.psyGen.Node` instances.
        '''
        self._accesses.append(AccessInfo(access_type, location, indices))

    def change_read_to_write(self):
        '''This function is only used when analysing an assignment statement.
        The LHS has first all variables identified, which will be READ.
        This function is then called to change the assigned-to variable
        on the LHS to from READ to WRITE. Since the LHS is stored in a separate
        VariableAccessInfo class, it is guaranteed that there is only
        one entry for the variable.
         '''
        assert len(self._accesses) == 1
        self._accesses[0].change_read_to_write()


# =============================================================================
class VariablesAccessInfo(object):
    '''This class stores all VariableAccessInfo instances for all variables
    in the corresponding code section.
    '''

    def __init__(self):
        self._var_to_varinfo = {}

    def __str__(self):
        '''Gives a shortened visual representation of all variables
        and their access mode.
        :returns: One line string listing all variables and their access mode.
        :rtype: str'''
        all_vars = list(self._var_to_varinfo.keys())
        all_vars.sort()
        output_list = []
        for var_name in all_vars:
            mode = ""
            if self.is_read(var_name):
                mode = mode+"READ"
            if self.is_written(var_name):
                mode = mode+"WRITE"
            output_list.append("{0}: {1}".format(var_name, mode))
        return ", ".join(output_list)

    def add_access(self, var_name, access_type, location=None, indices=None):
        '''Adds access information to this variable.
        :param access_type: The type of access (READ, WRITE, ....)
        :type access_type :py:class:`psyclone.core.access_type.AccessType`
        :param location: Location information.
        :type location: int
        :param indicies: Indices used in the access (None if the variable \
            is not an array). Defaults to None.
        :type indices: list of :py:class:`psyclone.psyGen.Node` instances.
        '''
        if var_name in self._var_to_varinfo:
            self._var_to_varinfo[var_name].add_access(access_type,
                                                      location, indices)
        else:
            var_info = VariableAccessInfo(var_name)
            var_info.add_access(access_type, location, indices)
            self._var_to_varinfo[var_name] = var_info

    def get_all_vars(self):
        ''':returns: all variables contained in this instance.
        :type: List of str.
        '''
        return list(self._var_to_varinfo.keys())

    def get_varinfo(self, name):
        '''
        :param str name: The variable name to get the access info for.
        :returns: The VariableAccessInfo for the variable
        :rtype: :py:class:`psyclone.core.access_info.VariableAccessInfo`
        :raises: KeyError if there is no information for the specified \
            variable.
        '''
        return self._var_to_varinfo[name]

    def merge(self, other_access_info):
        '''Merges data from a var_info instance to the information
        in this class.
        '''
        for var_name in other_access_info.get_all_vars():
            var_info = other_access_info.get_varinfo(var_name)
            for access_info in var_info.get_all_accesses():
                self.add_access(var_name, access_info.get_access_type(),
                                access_info.get_location(),
                                access_info.get_indices())

    def is_written(self, var_name):
        '''Checks if the specified variable name is at least written once.
        :param str var_name: Name of the variable
        :returns: true if the specified variable name is written (at least \
            once).
        :rtype: bool
        :raises: KeyError if the variable names can not be found.'''

        var_access_info = self.get_varinfo(var_name)
        return var_access_info.is_written()

    def is_read(self, var_name):
        '''Checks if the specified variable name is at least read once.
        :param str var_name: Name of the variable
        :returns: true if the specified variable name is read (at least \
            once).
        :rtype: bool
        :raises: KeyError if the variable names can not be found.'''

        var_access_info = self.get_varinfo(var_name)
        return var_access_info.is_read()
