# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module creates an LFRic-specific Invokes object which controls all
    the required invocation calls. It also overrides the PSy gen method so
    that LFRic-specific PSy module code is generated.
    '''


from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicInvokes
from psyclone.psyGen import PSy


class LFRicPSy(PSy):
    '''
    The LFRic-specific PSy class. This creates an LFRic-specific
    Invokes object (which controls all the required invocation calls).
    It also overrides the PSy gen method so that we generate
    LFRic-specific PSy module code.

    :param invoke_info: object containing the required invocation information
                        for code optimisation and generation.
    :type invoke_info: :py:class:`psyclone.parse.algorithm.FileInfo`

    '''
    def __init__(self, invoke_info):
        Config.get().api = "lfric"
        super().__init__(invoke_info)

        # Then initialise the Invokes
        self._invokes = LFRicInvokes(invoke_info.calls, self)

    @property
    def name(self):
        '''
        :returns: a name for the PSy layer. This is used as the PSy module
                  name. We override the default value as the Met Office
                  prefer "_psy" to be appended, rather than prepended.
        :rtype: str

        '''
        return self._name + "_psy"

    @property
    def orig_name(self):
        '''
        :returns: the unmodified PSy-layer name.
        :rtype: str

        '''
        return self._name


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicPSy']
