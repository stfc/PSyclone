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

from psyclone_tools import (redundant_computation_setval,
                            view_transformed_schedule)
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
