# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. Transforms the invoke with the addition of
OpenACC directives and then encloses the whole in a profiling region. '''

from acc_transform import trans as acc_trans
from psyclone.psyir.transformations import ProfileTrans


def trans(psyir):
    '''
    Take the supplied psy object, add OpenACC directives and then enclose
    the whole schedule within a profiling region.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    proftrans = ProfileTrans()

    # Use the trans() routine in acc_transform.py to add the OpenACC directives
    acc_trans(psyir)

    schedule = next(x for x in psyir.children[0].children
                    if x.name == 'invoke_0_inc_field')

    # Enclose everything in a profiling region
    proftrans.apply(schedule.children)
