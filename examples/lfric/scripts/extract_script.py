##############################################################################
# (c) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with the LFRic code
# (https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE),
# contains details of the terms under which the code may be used.
##############################################################################
# Author: J. Henrichs, Bureau of Meteorology

'''
PSyclone transformation script for the LFRic (Dynamo0p3) API to apply
redundant computation and then extract all kernels.

'''

from psyclone.domain.lfric import LFRicLoop
from psyclone.domain.lfric.transformations import LFRicExtractTrans

from psyclone.transformations import LFRicRedundantComputationTrans

SETVAL_BUILTINS = ["setval_c"]


def trans(psy):
    '''
    Applies PSyclone redundant computation and then instruments all
    kernel calls for extraction, including the creation of appropriate
    drivers.

    '''
    extract = LFRicExtractTrans()

    rtrans = LFRicRedundantComputationTrans()

    # Loop over all the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        schedule = invoke.schedule

        # Make setval_* built-ins compute redundantly to the level-1 halo
        # if they are in their own loop
        for loop in schedule.loops():
            if loop.iteration_space == "dof":
                if len(loop.kernels()) != 1:
                    raise Exception(
                        f"Expecting loop to contain 1 call but found "
                        f"'{len(loop.kernels())}'")
                if loop.kernels()[0].name in SETVAL_BUILTINS:
                    rtrans.apply(loop, options={"depth": 1})

    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule
        for kern in schedule.walk(LFRicLoop):
            try:
                extract.apply(kern, {"create_driver": True})
            except NotImplementedError as err:
                # Print the error details, but ignore otherwise:
                print(f"Error creating the extraction code or driver in "
                      f"kernel '{kern.name}' - error: {err}")
