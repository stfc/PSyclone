# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds kernel extraction code to
the invokes. When the transformed program is compiled and run, it
will create one NetCDF file for each of the two invokes. A separate
driver program is also created for each invoke which can read the
created NetCDF files, execute the invokes and then compare the results.
'''

from psyclone.domain.gocean.transformations import GOceanExtractTrans
from psyclone.psyGen import InvokeSchedule


def trans(psyir):
    '''
    Take the supplied psy object, and add kernel extraction code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    extract = GOceanExtractTrans()

    for schedule in psyir.walk(InvokeSchedule):
        if schedule.name == "invoke_0":
            extract.apply(schedule.children, {"create_driver": True,
                                              "region_name": ("main", "init")})

        if schedule.name == "invoke_1_update_field":
            # Enclose everything in a extract region
            extract.apply(schedule.children,
                          {"create_driver": True,
                           "region_name": ("main", "update")})
