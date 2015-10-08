from transformations import Dynamo0p3ColourTrans, DynamoOMPParallelLoopTrans
from psyGen import Loop

def trans(psy):
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
            print "child is of type ",type(child)
            if isinstance(child, Loop):
                if child.loop_type == "colours":
                    newsched, _ = otrans.apply(child.children[0])
                else:
                    newsched, _ = otrans.apply(child)
            schedule = newsched

        schedule.view()
        invoke.schedule = schedule

    return psy

