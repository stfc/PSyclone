'''File containing a PSyclone transformation script for the dynamo0p3
api to apply colouring and OpenMP generically. This can be applied via
the -s option in the generator.py script. '''
from transformations import Dynamo0p3ColourTrans, DynamoOMPParallelLoopTrans
from psyGen import Loop


def trans(psy):
    ''' PSyclone transformation script for the dynamo0p3 api to apply
    colouring and OpenMP generically.'''
    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        print "Transforming invoke '"+invoke.name+"'..."
        schedule = invoke.schedule

        # Colour all of the loops unless they are on W3
        cschedule = schedule
        for child in schedule.children:
            if isinstance(child, Loop) and child.field_space != "w3":
                cschedule, _ = ctrans.apply(child)

        # Then apply OpenMP to each of the colour loops
        schedule = cschedule
        for child in schedule.children:
            print "child is of type ", type(child)
            if isinstance(child, Loop):
                if child.loop_type == "colours":
                    schedule, _ = otrans.apply(child.children[0])
                else:
                    schedule, _ = otrans.apply(child)

        schedule.view()
        invoke.schedule = schedule

    return psy
