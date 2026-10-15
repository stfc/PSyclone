# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to the 'psyclone' command
via its -s option. This script demonstrates the use of the
optional trans_alg() function which gives access to PSyclone's
intermediate representation of the algorithm layer.

'''
from psyclone.domain.common.algorithm.psyir import AlgorithmInvokeCall


def trans(psyir):
    '''The trans function is required for the script to be valid.'''


def trans_alg(psyir):
    '''Output algorithm layer invoke information.'''
    for invoke in psyir.walk(AlgorithmInvokeCall):
        print(invoke.view())
