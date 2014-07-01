from parse import parse
from psyGen import PSyFactory

api="gocean"
ast,invokeInfo=parse("shallow_gocean.f90",api=api)
psy=PSyFactory(api).create(invokeInfo)
print psy.gen

print psy.invokes.names

schedule=psy.invokes.get('invoke_0').schedule
schedule.view()

schedule=psy.invokes.get('invoke_1').schedule
schedule.view()

schedule=psy.invokes.get('invoke_2').schedule
schedule.view()

