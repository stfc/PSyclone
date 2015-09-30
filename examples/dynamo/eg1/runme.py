#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from parse import parse
from psyGen import PSyFactory

# This example uses version 0.1 of the Dynamo API
api="dynamo0.1"

# Parse the file containing the algorithm specification and
# return the Abstract Syntax Tree and invokeInfo objects
ast,invokeInfo=parse("dynamo.F90",api=api)

# Create the PSy-layer object using the invokeInfo
psy=PSyFactory(api).create(invokeInfo)
# Generate the Fortran code for the PSy layer
print psy.gen

# List the invokes that the PSy layer has
print psy.invokes.names

# Examine the 'schedule' (e.g. loop structure) that each
# invoke has
schedule=psy.invokes.get('invoke_0_v3_kernel_type').schedule
schedule.view()

schedule=psy.invokes.get('invoke_1_v3_solver_kernel_type').schedule
schedule.view()

