'''A PSyclone transformation script that transforms all synchronous
halo exchanges into asynchronous halo exchanges and moves the halo
exchange start part of each asynchronous halo exchange as early as
possible in the schedule in order to maximise the overlap of
communication and computation. Also outputs a textual view of the
transformed PSyIR representing the PSy-layer.

This is a generic implementation that will work for all LFRic
schedules and for algorithms containing multiple invoke calls.

'''
from psyclone.transformations import Dynamo0p3AsyncHaloExchangeTrans, \
    MoveTrans, TransformationError
from psyclone.dynamo0p3 import DynHaloExchange, DynHaloExchangeStart

def trans(psy):
    '''Transforms all synchronous halo exchanges into asynchronous halo
    exchanges and moves the halo exchange start part of each
    asynchronous halo exchange as early as possible in the schedule in
    order to maximise the overlap of communication and
    computation. Also outputs a textual view of the transformed PSyIR
    representing the PSy-layer.

    :param psy: a PSyclone PSy object which captures the algorithm and \
    kernel information required by PSyclone.
    :type psy: subclass of :py:class:`psyclone.psyGen.PSy`

    '''
    # Create the required transformations
    async_hex = Dynamo0p3AsyncHaloExchangeTrans()
    move_trans = MoveTrans()

    # Iterate over the invokes in this algorithm file
    invokes = psy.invokes.invoke_list
    for invoke in invokes:

        # Get the schedule (the PSyIR representation of the PSy-layer)
        schedule = invoke.schedule
        # Split any synchronous halo exchanges into asynchronous halo exchanges
        for hex_node in schedule.walk(DynHaloExchange):
            async_hex.apply(hex_node)

        # Move any halo exchange starts as early as possible in the
        # schedule to maximise overlap of compute and comms within the
        # invoke.
        for hex_start_node in reversed(schedule.walk(DynHaloExchangeStart)):
            idx = hex_start_node.position
            parent = hex_start_node.parent
            # Move halo exchange start node up one node at a time
            # until there is an exception (which indicates the move is
            # invalid). No need to check for idx == 0 as a negative
            # index wraps to the end of the list which will be
            # invalid.
            try:
                while True:
                    move_trans.apply(parent[idx], parent[idx-1])
                    idx -= 1
            except TransformationError:
                pass

        # Take a look at the modified PSy-layer PSyIR
        schedule.view()
