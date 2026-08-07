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

    for inv_schedule in psyir.walk(InvokeSchedule):
        # You can also add "region_name": ("timestep", "combine")
        # as additional optional parameter to create nicer names
        # Though changing the name would require an update of the Makefile
        extract.apply(inv_schedule, {"create_driver": True})
