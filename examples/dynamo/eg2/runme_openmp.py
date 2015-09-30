#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from parse import parse
from psyGen import PSyFactory
api="dynamo0.1"
ast,invokeInfo=parse("dynamo_algorithm_mod.F90",api=api)
psy=PSyFactory(api).create(invokeInfo)
print psy.gen

print psy.invokes.names

schedule=psy.invokes.get('invoke_0').schedule
schedule.view()

from psyGen import TransInfo
t=TransInfo()
print t.list

lf=t.get_trans_name('LoopFuse')
ol=t.get_trans_name('OMPParallelLoopTrans')
lc=t.get_trans_name('LoopColour')

schedule.view()
fuse_schedule,memento=lf.apply(schedule.children[0],schedule.children[1])
fuse_schedule.view()
omp_schedule,memento=ol.apply(fuse_schedule.children[0])
omp_schedule.view()

psy.invokes.get('invoke_0').schedule=omp_schedule

schedule=psy.invokes.get('invoke_1_v2_kernel_type').schedule
schedule.view()
lc_schedule,memento=lc.apply(schedule.children[0])
lc_schedule.view()

lc_omp_schedule,memento=ol.apply(lc_schedule.children[0].children[0])
lc_omp_schedule.view()

psy.invokes.get('invoke_1_v2_kernel_type').schedule=lc_omp_schedule

schedule=psy.invokes.get('invoke_2_v1_kernel_type').schedule
schedule.view()
lc_schedule,memento=lc.apply(schedule.children[0])
lc_schedule.view()

lc_omp_schedule,memento=ol.apply(lc_schedule.children[0].children[0])
lc_omp_schedule.view()

psy.invokes.get('invoke_2_v1_kernel_type').schedule=lc_omp_schedule

print psy.gen
