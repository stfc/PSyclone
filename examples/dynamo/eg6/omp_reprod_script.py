'''File containing a PSyclone transformation script for the dynamo0p3
api to apply loop fusion and then OpenMP parallelisation to an invoke
with two Kernels. This can be applied via the -s option in the
generator.py script.'''
from transformations import OMPParallelTrans, DynamoLoopFuseTrans, \
    Dynamo0p3OMPLoopTrans

def trans(psy):
    ''' PSyclone transformation script for the dynamo0p3 api to apply
    loop fusion and OpenMP for a particular example.'''
    otrans = OMPParallelTrans()
    ltrans = Dynamo0p3OMPLoopTrans()
    ftrans = DynamoLoopFuseTrans()

    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # loop fuse the two builtin kernels
    schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                               same_space=True)

    # Add an OpenMP do directive to the resultant loop-fused loop,
    # specifying that we want reproducible reductions
    schedule, _ = ltrans.apply(schedule.children[0], reprod=True)

    # Add an OpenMP parallel directive around the OpenMP do directive
    schedule, _ = otrans.apply(schedule.children[0])

    # take a look at what we've done
    schedule.view()

    return psy
