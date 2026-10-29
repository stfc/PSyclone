# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
PSyclone transformation script for the LFRic API to apply
colouring, OpenMP and redundant computation to the level-1 halo for
the initialisation built-ins generically.

'''

from psyclone_tools import redundant_computation_setval
from psyclone.domain.lfric import LFRicLoop
from psyclone.domain.lfric.transformations import LFRicExtractTrans


def trans(psy):
    '''
    Applies PSyclone redundant computation and then instruments all
    kernel calls for extraction, including the creation of appropriate
    drivers.

    '''
    extract = LFRicExtractTrans()
    redundant_computation_setval(psy)
    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule
        for kern in schedule.walk(LFRicLoop):
            try:
                extract.apply(kern, {"create_driver": True})
            except NotImplementedError as err:
                # Print the error details, but ignore otherwise:
                print(f"Error creating the extraction code or driver in "
                      f"kernel '{kern.name}' - error: {err}")
