#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
api="dynamo0.1"
ast,invokeInfo=parse("dynamo_algorithm_mod.F90",api=api)
psy=PSyFactory(api).create(invokeInfo)
print psy.gen

print psy.invokes.names

schedule=psy.invokes.get('invoke_0').schedule
schedule.view()

from psyclone.psyGen import TransInfo
t=TransInfo()
print t.list

lf=t.get_trans_name('LoopFuse')

schedule.view()
new_schedule,memento=lf.apply(schedule.children[0],schedule.children[1])
new_schedule.view()

psy.invokes.get('invoke_0').schedule=new_schedule
print psy.gen
