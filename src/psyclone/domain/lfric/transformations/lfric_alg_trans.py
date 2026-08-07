# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Specialise generic PSyIR representing an algorithm layer to an
LFRic algorithm-layer-specific PSyIR which uses specialised classes.

'''
from psyclone.domain.common.transformations import AlgTrans
from psyclone.domain.lfric.transformations.raise_psyir_2_lfric_alg_trans \
    import RaisePSyIR2LFRicAlgTrans


class LFRicAlgTrans(AlgTrans):
    '''Transform a generic PSyIR representation of the Algorithm layer to
    an LFRic version with specialised domain-specific nodes.

    '''
    def __init__(self):
        super().__init__()
        self._invoke_trans = RaisePSyIR2LFRicAlgTrans()
