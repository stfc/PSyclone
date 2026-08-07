# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It adds kernel extraction to the all kernels.
'''

from psyclone.domain.gocean.transformations import GOceanExtractTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object and apply kernel extraction.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    extract = GOceanExtractTrans()

    # Now loop over all Invoke schedules to apply the transformation.
    # You have to provide the option `'create_driver': True` to
    # create a driver
    for inv_schedule in psyir.walk(InvokeSchedule):
        extract.apply(inv_schedule,
                      # INSERT OPTIONS HERE to create the driver
                      )
    print(psyir.view())
