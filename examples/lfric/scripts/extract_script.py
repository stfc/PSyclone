##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################


'''
PSyclone transformation script for the LFRic (Dynamo0p3) API to apply
colouring, OpenMP and redundant computation to the level-1 halo for
the initialisation built-ins generically.

'''

from psyclone.domain.lfric import LFRicLoop
from psyclone.domain.lfric.transformations import LFRicExtractTrans

from psyclone.transformations import LFRicRedundantComputationTrans

SETVAL_BUILTINS = ["setval_c"]


def trans(psy):
    '''
    Applies PSyclone colouring, OpenMP and redundant computation
    transformations.

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
            except NotImplementedError:
                pass
