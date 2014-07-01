from parse import parse
from psyGen import PSyFactory
api="dynamo0.1"
ast,invokeInfo=parse("dynamo.F90",api=api)
psy=PSyFactory(api).create(invokeInfo)
print psy.gen

print psy.invokes.names
schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
schedule.view()

schedule=psy.invokes.get('invoke_v3_solver_kernel_type').schedule
schedule.view()

