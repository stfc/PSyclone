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

# Parse the algorithm specification and return the Abstract Syntax Tree and
# invokeInfo objects
ast,invokeInfo=parse("dynamo.F90",api=api)

# Create the PSy-layer object using the invokeInfo
psy=PSyFactory(api).create(invokeInfo)
# Generate the Fortran code for the PSy layer
print psy.gen

# List the various invokes that the PSy layer contains
print psy.invokes.names

# Get the loop schedule associated with one of these
# invokes
schedule=psy.invokes.get('invoke_0_v3_kernel_type').schedule
schedule.view()

# Get the list of possible loop transformations
from psyclone.psyGen import TransInfo
t=TransInfo()
print t.list

# Create an OpenMPLoop-transformation object
ol=t.get_trans_name('OMPParallelLoopTrans')

# Apply it to the loop schedule of the selected invoke
new_schedule,memento=ol.apply(schedule.children[0])
new_schedule.view()

# Replace the original loop schedule of the selected invoke
# with the new, transformed schedule 
psy.invokes.get('invoke_0_v3_kernel_type').schedule=new_schedule
# Generate the Fortran code for the new PSy layer
print psy.gen

schedule=psy.invokes.get('invoke_1_v3_solver_kernel_type').schedule
schedule.view()

new_schedule,memento=ol.apply(schedule.children[0])
new_schedule.view()

psy.invokes.get('invoke_1_v3_solver_kernel_type').schedule=new_schedule
print psy.gen
