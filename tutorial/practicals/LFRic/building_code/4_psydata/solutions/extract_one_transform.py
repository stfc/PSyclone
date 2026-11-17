# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds kernel extraction code to
the 'invoke_propagate_perturbation' invoke.
'''

from psyclone.domain.lfric.transformations import LFRicExtractTrans


def trans(psyir):
    '''
    Take the supplied psy object, and add kernel extraction code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    extract = LFRicExtractTrans()

    name = "invoke_propagate_perturbation"
    subroutine = [x for x in psyir.children[0].children if x.name == name][0]

    # Enclose everything in a extract region
    extract.apply(subroutine, {"region_name": ("time_evolution", "propagate"),
                               "create_driver": True})

    print(subroutine.view())
