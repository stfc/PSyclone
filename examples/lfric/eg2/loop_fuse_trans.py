# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module implementing a `trans` method for use as a PSyclone transformation
    script. This example performs loop fusion.
'''

from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans


def trans(psyir):
    '''
    PSyclone transformation routine. This is an example which performs loop
    fusion for the Built-in 'setval_c' kernels in the first 'invoke'. For the
    sake of this example we use the 'same_space' option to tell the
    transformation that this is safe to do.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # Get first subroutine of the first module
    schedule = psyir.children[0].children[0]

    lftrans = LFRicLoopFuseTrans()

    # Since the arguments to the 'setval_c' built-in are on 'ANY_SPACE', we
    # assert that the various loops over degrees of freedom are of
    # the same extent and may safely be fused. (This is not actually true
    # for this particular example but we do this for the purposes of
    # illustration.)
    lftrans.apply(schedule[0], schedule[1], {"same_space": True})
    lftrans.apply(schedule[0], schedule[1], {"same_space": True})
    lftrans.apply(schedule[0], schedule[1], {"same_space": True})
