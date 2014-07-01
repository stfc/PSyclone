from parse import parse
from psyGen import PSyFactory
api="dynamo0.1"
ast,invokeInfo=parse("dynamo.F90",api=api)
psy=PSyFactory(api).create(invokeInfo)
print psy.gen

print psy.invokes.names
schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
schedule.view()

from psyGen import TransInfo
t=TransInfo()
print t.list
ol=t.get_trans_name('OpenMPLoop')
new_schedule,memento=ol.apply(schedule.children[0])
new_schedule.view()

psy.invokes.get('invoke_v3_kernel_type')._schedule=new_schedule
print psy.gen

schedule=psy.invokes.get('invoke_v3_solver_kernel_type').schedule
schedule.view()

new_schedule,memento=ol.apply(schedule.children[0])
new_schedule.view()

psy.invokes.get('invoke_v3_solver_kernel_type')._schedule=new_schedule
print psy.gen
