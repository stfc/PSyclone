# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author I. Kavcic, Met Office


'''
An example of PSyclone transformation script to extract a list of Nodes from
"invoke_1" of the algorithm gw_mixed_schur_preconditioner_alg_mod.x90.

This script can be applied via the '-s' option when running PSyclone:

$ psyclone -nodm -s extract_nodes.py \
    gw_mixed_schur_preconditioner_alg_mod.x90

Please note that distributed memory is not supported for code extraction
(hence the '-nodm' option above).

The user-specified settings are:
INVOKE_NAME - name of the Invoke containing the Nodes to extract,
LBOUND - lower index in the list of Nodes to extract,
UBOUND - upper index in the list of Nodes to extract.

Please note that ExtractTrans works for consecutive Nodes in an
Invoke Schedule (the Nodes also need to be children of the same parent).
'''

from __future__ import print_function
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


def trans(psy):
    ''' PSyclone transformation script for the Dynamo0.3 API to extract
    the specified Nodes in an Invoke. '''

    # Get instance of the ExtractTrans transformation
    etrans = LFRicExtractTrans()

    # Get Invoke and its Schedule
    invoke = psy.invokes.get(INVOKE_NAME)
    schedule = invoke.schedule

    # Apply extract transformation to selected Nodes
    print("\nExtracting Nodes '[" + str(LBOUND) + ":" + str(UBOUND) +
          "]' from Invoke '" + invoke.name + "'\n")
    schedule, _ = etrans.apply(schedule.children[LBOUND:UBOUND])

    # Take a look at the transformed Schedule
    schedule.view()

    return psy
