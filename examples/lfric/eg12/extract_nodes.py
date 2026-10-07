# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
An example of PSyclone transformation script to extract a list of Nodes from
"invoke_1" of the algorithm gw_mixed_schur_preconditioner_alg_mod.x90.

This script can be applied via the '-s' option when running PSyclone:

$ psyclone -api lfric -nodm -s ./extract_nodes.py \
    ../code/gw_mixed_schur_preconditioner_alg_mod.x90

Please note that distributed memory is not supported for code extraction
(hence the '-nodm' option above).

The user-specified settings are:
INVOKE_NAME - name of the Invoke containing the Nodes to extract,
LBOUND - lower index in the list of Nodes to extract,
UBOUND - upper index in the list of Nodes to extract.

Please note that ExtractTrans works for consecutive Nodes in an
Invoke Schedule (the Nodes also need to be children of the same parent).
'''

from psyclone.domain.lfric.transformations import LFRicExtractTrans


# Specify the name of the Invoke containing the Nodes to extract.
# If the Invoke name does not correspond to PSy Invoke names in
# the Algorithm file no Nodes will be extracted.
INVOKE_NAME = "invoke_1"
# Specify the lower index in the list of Nodes to extract
LBOUND = 0
# Specify the upper index in the list of Nodes to extract (please note
# that the corresponding Node index is UBOUND - 1)
UBOUND = 3


def trans(psyir):
    ''' PSyclone transformation script for the LFRic API to extract
    the specified Nodes in an Invoke.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # Get instance of the ExtractTrans transformation
    etrans = LFRicExtractTrans()

    # Get the Invoke Schedule
    schedule = next(x for x in psyir.children[0].children
                    if x.name == INVOKE_NAME)

    # Apply extract transformation to selected Nodes
    print("\nExtracting Nodes '[" + str(LBOUND) + ":" + str(UBOUND) +
          "]' from Invoke '" + schedule.name + "'\n")
    etrans.apply(schedule.children[LBOUND:UBOUND])
