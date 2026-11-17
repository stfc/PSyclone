# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Example transformation script showing the use of the module-inline
    transformation for the LFRic domain. '''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyGen import Kern, InvokeSchedule


def trans(psyir):
    '''
    PSyclone transformation routine. This is an example which module-inlines
    the kernel used in the second 'invoke' in the supplied PSy-layer.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    for schedule in psyir.walk(InvokeSchedule):
        if schedule.name == "invoke_1":
            # Find the kernel we want to inline.
            kern = schedule.walk(Kern)[0]
            # Setting module inline via a transformation.
            KernelModuleInlineTrans().apply(kern)
