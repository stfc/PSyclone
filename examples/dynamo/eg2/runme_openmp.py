# ------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# ------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

'''Example script showing how to apply OpenMP transformations to
dynamo code'''

from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
API = "dynamo0.1"
_, INVOKEINFO = parse("dynamo_algorithm_mod.F90", api=API)
PSY = PSyFactory(API).create(INVOKEINFO)
print PSY.gen

print PSY.invokes.names

from psyclone.psyGen import TransInfo
TRANS = TransInfo()
print TRANS.list

LOOP_FUSE = TRANS.get_trans_name('LoopFuse')
OMP_PAR = TRANS.get_trans_name('OMPParallelLoopTrans')

SCHEDULE = PSY.invokes.get('invoke_0').schedule
SCHEDULE.view()

FUSE_SCHEDULE, _ = LOOP_FUSE.apply(SCHEDULE.children[0], SCHEDULE.children[1])
FUSE_SCHEDULE.view()
OMP_SCHEDULE, _ = OMP_PAR.apply(FUSE_SCHEDULE.children[0])
OMP_SCHEDULE.view()

PSY.invokes.get('invoke_0').schedule = OMP_SCHEDULE

SCHEDULE = PSY.invokes.get('invoke_1_v2_kernel_type').schedule
SCHEDULE.view()

OMP_SCHEDULE, _ = OMP_PAR.apply(SCHEDULE.children[0])
OMP_SCHEDULE.view()

PSY.invokes.get('invoke_1_v2_kernel_type').schedule = OMP_SCHEDULE

SCHEDULE = PSY.invokes.get('invoke_2_v1_kernel_type').schedule
SCHEDULE.view()

OMP_SCHEDULE, _ = OMP_PAR.apply(SCHEDULE.children[0])
OMP_SCHEDULE.view()

PSY.invokes.get('invoke_2_v1_kernel_type').schedule = OMP_SCHEDULE

print PSY.gen
