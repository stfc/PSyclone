from parse import parse
from psyGen import PSyFactory

api="dynamo0.3"

ast,invokeInfo=parse("solver_mod.x90",api=api)

psy=PSyFactory(api).create(invokeInfo)

schedule=psy.invokes.get('invoke_2_matrix_vector_kernel_mm_type').schedule
schedule.view()

from psyGen import TransInfo, Kern
t=TransInfo()
#print t.list

dp=t.get_trans_name('KernelDumpTrans')
for kernel in schedule.walk(schedule.children, Kern):
    new_sched,momento = dp.apply(kernel)
    psy.invokes.get('invoke_2_matrix_vector_kernel_mm_type')._schedule=new_sched


schedule=psy.invokes.get('invoke_2_matrix_vector_kernel_mm_type').schedule
schedule.view()

#print psy.gen
