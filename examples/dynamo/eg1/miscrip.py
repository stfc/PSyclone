from parse import parse
from psyGen import PSyFactory

api="dynamo0.1"

ast,invokeInfo=parse("dynamo.F90",api=api)

psy=PSyFactory(api).create(invokeInfo)

schedule=psy.invokes.get('invoke_0_v3_kernel_type').schedule
schedule.view()

from psyGen import TransInfo, Kern
t=TransInfo()
#print t.list
dp=t.get_trans_name('KernelDumpTrans')
for kernel in schedule.walk(schedule.children, Kern):
    new_sched,momento = dp.apply(kernel)
    psy.invokes.get('invoke_0_v3_kernel_type')._schedule=new_sched

schedule=psy.invokes.get('invoke_0_v3_kernel_type').schedule
schedule.view()
