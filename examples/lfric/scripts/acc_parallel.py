##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################


'''PSyclone transformation script for the Dynamo0p3 API to apply
colouring, OpenMP and redundant computation to the level1 halo for
setval_* generically.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.psyir.nodes import ACCDirective, Loop
from psyclone.psyir.transformations import (
    ACCKernelsTrans, TransformationError)
from psyclone.transformations import (
    Dynamo0p3ColourTrans, Dynamo0p3OMPLoopTrans, OMPParallelTrans,
    ACCParallelTrans, ACCLoopTrans, ACCRoutineTrans)
from psyclone_tools import redundant_computation_setval


ACC_EXCLUSIONS = [
]


def trans(psy):
    '''Applies PSyclone colouring, OpenMP transformations.

    '''
    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    const = LFRicConstants()
    loop_trans = ACCLoopTrans()
    ktrans = ACCKernelsTrans()
    parallel_trans = ACCParallelTrans(default_present=False)
    artrans = ACCRoutineTrans()
    oregtrans = OMPParallelTrans()

    print(f"PSy name = '{psy.name}'")

    redundant_computation_setval(psy)

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        print("Transforming invoke '{0}' ...".format(invoke.name))
        schedule = invoke.schedule

        if psy.name.lower() in ACC_EXCLUSIONS:
            print(f"Not adding ACC to invoke in '{psy.name}'")
            apply_acc = False
        else:
            apply_acc = True

        # Keep a record of any kernels we fail to module inline as we can't
        # then add ACC ROUTINE to them.
        failed_inline = set()

        # Colour loops over cells unless they are on discontinuous
        # spaces or over dofs
        for loop in schedule.loops():
            if loop.iteration_space == "cell_column":
                if apply_acc:
                    for kern in loop.kernels():
                        try:
                            artrans.apply(kern)
                        except TransformationError as err:
                            failed_inline.add(kern.name.lower())
                            print(f"Adding ACC Routine to kernel '{kern.name}'"
                                  f" failed:\n{err.value}")
                if (loop.field_space.orig_name not in
                        const.VALID_DISCONTINUOUS_NAMES):
                    ctrans.apply(loop)

        # Add OpenACC to loops unless they are over colours or are null.
        schedule = invoke.schedule
        for loop in schedule.walk(Loop):
            if not apply_acc or any(kern.name.lower() in failed_inline for
                                    kern in loop.kernels()):
                print(f"Not adding OpenACC for kernels: "
                      f"{[kern.name for kern in loop.kernels()]}")
                continue
            try:
                if loop.loop_type == "colours":
                    pass
                if loop.loop_type == "colour":
                    loop_trans.apply(loop, options={"independent": True})
                    parallel_trans.apply(loop.ancestor(ACCDirective))
                if loop.loop_type == "":
                    loop_trans.apply(loop, options={"independent": True})
                    parallel_trans.apply(loop.ancestor(ACCDirective))
                if loop.loop_type == "dof":
                    # We use ACC KERNELS for dof loops since they can contain
                    # reductions.
                    ktrans.apply(loop)
            except TransformationError as err:
                print(str(err))
                pass

        for loop in schedule.walk(Loop):
            if not apply_acc or any(kern.name.lower() in failed_inline for
                                    kern in loop.kernels()):
                if loop.loop_type not in ["colours", "null"]:
                    oregtrans.apply(loop)
                    otrans.apply(loop, options={"reprod": True})

    return psy
