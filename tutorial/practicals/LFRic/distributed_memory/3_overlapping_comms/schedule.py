'''A PSyclone transformation script that outputs a textual
representation of the PSyIR representing the PSy-layer for the first
invoke found in the algorithm layer code.

'''


def trans(psy):
    '''Output a textual view of the PSyIR representing the PSy-layer for
    the first invoke found in the algorithm layer code.

    :param psy: a PSyclone PSy object which captures the algorithm and \
    kernel information required by PSyclone.
    :type psy: subclass of :py:class:`psyclone.psyGen.PSy`

    '''
    # Get the object representing the first PSy-layer invoke
    invoke = psy.invokes.invoke_list[0]
    # Get the schedule (the PSyIR representation of the PSy-layer)
    schedule = invoke.schedule
    # Take a look at the PSy-layer PSyIR
    print(schedule.view())
