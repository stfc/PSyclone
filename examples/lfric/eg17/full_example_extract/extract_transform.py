# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone via the -s option.
It adds kernel extraction code to
the invokes. When the transformed program is compiled and run, it
will create one NetCDF file for each of the two invokes. A separate
driver program is also created for each invoke which can read the
created NetCDF files, execute the invokes and then compare the results.
'''

from psyclone.domain.lfric.transformations import LFRicExtractTrans


def trans(psyir):
    '''
    Add kernel extraction code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    extract = LFRicExtractTrans()

    for subroutine in psyir.children[0].children:
        # Show that it works on a builtin:
        if subroutine.name == "invoke_initialise_fields":
            extract.apply(subroutine.children,
                          {"create_driver": True,
                           "region_name": ("main", "init")})

        # Enclose everything in a extract region
        if subroutine.name == "invoke_testkern_w0":
            extract.apply(subroutine.children,
                          {"create_driver": True,
                           "region_name": ("main", "update")})
