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

schedule=psy.invokes.get('invoke_1_v2_kernel_type').schedule
schedule.view()

schedule=psy.invokes.get('invoke_2_v1_kernel_type').schedule
schedule.view()
