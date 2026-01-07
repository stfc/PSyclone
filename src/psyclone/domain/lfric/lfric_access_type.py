from __future__ import annotations
from enum import Enum

from psyclone.configuration import Config
from psyclone.core.access_type import AccessType


class LFRicAccessType(Enum):
    '''
    '''
    #: Incremented from more than one cell column (see the LFRic API section
    #: of the User Guide).
    INC = 21
    #: Read before incrementing. Requires that the outermost halo be clean (see
    #: the LFRic API section of the User Guide).
    READINC = 22
    #: Is the output of a SUM reduction.
    SUM = 23
    #: Is the output of a MIN reduction (i.e. global minimum value).
    MIN = 24
    #: Is the output of a MAX reduction (i.e. global maximum value).
    MAX = 25

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
    def from_string(access_string: str) -> AccessType:
        '''Convert a string (e.g. "read") into the corresponding
        AccessType enum value (AccessType.READ).

        :param access_string: Access type as a string.

        :returns: Corresponding AccessType enum.

        :raises ValueError: if access_string is not a valid access type.
        '''
        for access in LFRicAccessType:
            if access.name == access_string.upper():
                return access
        return AccessType.from_string(access_string)

    @classmethod
    def all_write_accesses(cls) -> list[Union[AccessType, LFRicAccessType]]:
        ''':returns: A list of all access types that involve writing to an
                     argument in some form.
        :rtype: List of py:class:`psyclone.core.access_type.AccessType`.
        '''
        return (AccessType.all_write_accesses() + [cls.INC, cls.READINC] +
                cls.get_valid_reduction_modes())

    @classmethod
    def all_read_accesses(cls):
        ''':returns: A list of all access types that involve reading an
                     argument in some form.
        :rtype: List of py:class:`psyclone.core.access_type.AccessType`.
        '''
        return AccessType.all_read_accesses() + [cls.INC, cls.READINC]

    @classmethod
    def get_valid_reduction_modes(cls) -> list[AccessType]:
        '''
        :returns: A list of valid reduction access modes.
        '''
        return [cls.SUM, cls.MIN, cls.MAX]

    @classmethod
    def get_valid_reduction_names(cls):
        '''
        :returns: A list of valid reduction access names.
        :rtype: List of strings.
        '''
        return [access.api_specific_name() for access in
                cls.get_valid_reduction_modes()]


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ["LFRicAccessType"]

