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
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class LFRicAlgTrans(AlgTrans):
    '''Transform a generic PSyIR representation of the Algorithm layer to
    an LFRic version with specialised domain-specific nodes.

    '''
    def __init__(self):
        super().__init__()
        self._invoke_trans = RaisePSyIR2LFRicAlgTrans()

    def apply(self, node, options=None, **kwargs):
        '''Apply the transformation to the supplied algorithm PSyIR.

        :param node: the root of an algorithm-layer PSyIR tree.
        :param options: a dictionary with options for transformations.

        '''
        super().apply(node, options=options, **kwargs)
