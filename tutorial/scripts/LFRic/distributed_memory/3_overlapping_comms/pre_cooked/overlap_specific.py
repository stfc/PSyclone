'''A PSyclone transformation script that transforms a specific
synchronous halo exchanges into an asynchronous halo exchange and
moves the halo exchange start part as early as possible in the
schedule in order to maximise the overlap of communication and
computation. Also outputs a textual view of the transformed PSyIR
representing the PSy-layer.

This is a kernel-specific implementation that will only work for
schedules with a halo exchange in a hard-coded location and a
resultant halo exchange start that can be moved to a hard-coded
location.

'''
from psyclone.transformations import Dynamo0p3AsyncHaloExchangeTrans, MoveTrans
from psyclone.psyGen import HaloExchange

def trans(psy):
    '''Transforms a specific synchronous halo exchanges into an
    asynchronous halo exchange and moves the halo exchange start part
    as early as possible in the schedule in order to maximise the
    overlap of communication and computation. Also outputs a textual
    view of the transformed PSyIR representing the PSy-layer.

    :param psy: a PSyclone PSy object which captures the algorithm and \
    kernel information required by PSyclone.
    :type psy: subclass of :py:class:`psyclone.psyGen.PSy`

    '''
    # Create the required transformations
    async_hex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    move_trans = MoveTrans()

    # Get a specific invoke
    invoke = psy.invokes.invoke_list[0]
    # Get the schedule (the PSyIR representation of the PSy-layer)
    schedule = invoke.schedule
    # Reference a specific node
    hex_node = schedule[1]
    async_hex_trans.apply(hex_node)

    # Move the (specific) halo exchange start node to the start of the
    # schedule
    move_trans.apply(schedule[1], schedule[0])

    # Take a look at the modified PSy-layer PSyIR
    schedule.view()
