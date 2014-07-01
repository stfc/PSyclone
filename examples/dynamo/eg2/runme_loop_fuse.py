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

schedule.view()
new_schedule,memento=lf.apply(schedule.children[0],schedule.children[1])
new_schedule.view()

psy.invokes.get('invoke_0')._schedule=new_schedule
print psy.gen
