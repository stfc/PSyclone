# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the LFRicInvokes class which passes the
LFRicInvoke class to the base class.'''

# Imports
from psyclone.domain.lfric import LFRicInvoke
from psyclone.psyGen import Invokes


class LFRicInvokes(Invokes):
    '''The LFRic-specific invokes class. This passes the LFRic-specific
    LFRicInvoke class to the base class so it creates the one we
    require.

    :param alg_calls: A list of objects containing the parsed invoke
                      information.
    :type alg_calls: List[:py:class:`psyclone.parse.algorithm.InvokeCall`]
    :param psy: The PSy object containing this LFRicInvokes object.
    :type psy: :py:class:`psyclone.domain.lfric.LFRicPSy`

    '''
    def __init__(self, alg_calls, psy):
        Invokes.__init__(self, alg_calls, LFRicInvoke, psy)


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicInvokes']
