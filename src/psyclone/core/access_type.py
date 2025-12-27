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
# Modified by R. W. Ford and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''This module implements the AccessType used throughout PSyclone.'''

from enum import Enum
from psyclone.configuration import Config


class AccessType(Enum):
    '''A simple enum-class for the various valid access types.
    '''
    #: Data associated with the symbol is read.
    READ = 1
    #: Data associated with the symbols is written.
    WRITE = 2
    #: Data associated with the symbol is both read and written (e.g. is passed
    #: to a routine with intent(inout)).
    READWRITE = 3
    #: Incremented from more than one cell column (see the LFRic API section
    #: of the User Guide).
    INC = 4
    #: Read before incrementing. Requires that the outermost halo be clean (see
    #: the LFRic API section of the User Guide).
    READINC = 5
    #: Is the output of a SUM reduction.
    SUM = 6
    #: This is used internally to indicate unknown access type of
    #: a variable, e.g. when a variable is passed to a subroutine
    #: and the access type of this variable in the subroutine
    #: is unknown.
    #: TODO #2863 - VariablesAccessMap does not currently consider
    #: UNKNOWN accesses and it should!
    UNKNOWN = 7
    #: A symbol representing a routine is called.
    CALL = 8
    #: The property/ies of a symbol is/are queried but the data it
    #: represents is not accessed (e.g. 'var' in SIZE(var, dim=1)).
    INQUIRY = 9
    #: Access data that cannot be redefined during execution, therefore, it
    #: is available at compile-time and can be used for type properties such
    #: as kinds or dimensions.
    CONSTANT = 10

    def __str__(self) -> str:
        '''Convert to a string representation, returning just the
        enum (e.g. 'WRITE').
        '''
        # pylint complains without str() that the return type is not a str
        return str(self.name)

    def api_specific_name(self) -> str:
        '''This convenience function returns the name of the type in the
        current API. E.g. in the lfric API, WRITE --> "gh_write". If no
        mapping is available then the generic name is returned.

        :returns: The API specific name.
        '''
        api_config = Config.get().api_conf()
        rev_access_mapping = api_config.get_reverse_access_mapping()
        return rev_access_mapping.get(self, str(self).lower())

    @staticmethod
    def from_string(access_string: str):
        '''Convert a string (e.g. "read") into the corresponding
        AccessType enum value (AccessType.READ).

        :param access_string: Access type as a string.

        :returns: Corresponding AccessType enum.
        :rtype: :py:class:`psyclone.core.access_type.AccessType`

        :raises ValueError: if access_string is not a valid access type.
        '''
        for access in AccessType:
            if access.name == access_string.upper():
                return access
        valid = [str(access).lower() for access in AccessType]
        raise ValueError(f"Unknown access type '{access_string}'. "
                         f"Valid values are {valid}.")

    @staticmethod
    def all_write_accesses():
        ''':returns: A list of all access types that involve writing to an
                     argument in some form.
        :rtype: List of py:class:`psyclone.core.access_type.AccessType`.
        '''
        return [AccessType.WRITE, AccessType.READWRITE, AccessType.INC,
                AccessType.READINC] + AccessType.get_valid_reduction_modes()

    @staticmethod
    def all_read_accesses():
        ''':returns: A list of all access types that involve reading an
                     argument in some form.
        :rtype: List of py:class:`psyclone.core.access_type.AccessType`.
        '''
        return [AccessType.READ, AccessType.READWRITE, AccessType.INC,
                AccessType.READINC]

    @staticmethod
    def get_valid_reduction_modes():
        '''
        :returns: A list of valid reduction access modes.
        :rtype: List of py:class:`psyclone.core.access_type.AccessType`.
        '''
        return [AccessType.SUM]

    @staticmethod
    def get_valid_reduction_names():
        '''
        :returns: A list of valid reduction access names.
        :rtype: List of strings.
        '''
        return [access.api_specific_name() for access in
                AccessType.get_valid_reduction_modes()]

    @staticmethod
    def non_data_accesses():
        '''
        :returns: all access types that do not touch any data associated with
                  a symbol.
        :rtype: list[:py:class:`psyclone.core.AccessType`]
        '''
        return [AccessType.CALL, AccessType.CONSTANT, AccessType.INQUIRY]


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ["AccessType"]
