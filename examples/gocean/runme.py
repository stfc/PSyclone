#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab
# Funded by the GOcean project

from parse import parse
from psyGen import PSyFactory

api="gocean1.0"
ast,invokeInfo=parse("shallow_alg.f90",api=api)
psy=PSyFactory(api).create(invokeInfo)
print psy.gen

print psy.invokes.names

schedule=psy.invokes.get('invoke_0').schedule
schedule.view()

schedule=psy.invokes.get('invoke_1').schedule
schedule.view()

schedule=psy.invokes.get('invoke_2').schedule
schedule.view()

