# -------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab
# Funded by the GOcean project

'''A simple test script showing basic usage of the PSyclone API.
In order to use it you must first configure your PYTHONPATH like so:

 >>> cd <blah>/PSyclone
 >>> export PYTHONPATH=`pwd`/src:`pwd`/f2py_93

Once your PYTHONPATH has been set-up, this script may be run by doing:

 >>> python runme.py

This should generate a lot of output, ending with a view of the
Schedules:

 >>> ...
 >>> Schedule[invoke='invoke_0']
 >>>    Loop[type='outer',field_space='cu',it_space='internal_pts']
 >>>        Loop[type='inner',field_space='cu',it_space='internal_pts']
 >>>            Call compute_cu_code(cu_fld,p_fld,u_fld)
 >>>    Loop[type='outer',field_space='cv',it_space='internal_pts']
 >>>        Loop[type='inner',field_space='cv',it_space='internal_pts']
 >>>            Call compute_cv_code(cv_fld,p_fld,v_fld)
 >>>...

'''

from psyclone.parse import parse
from psyclone.psyGen import PSyFactory

API = "gocean1.0"
_, INVOKEINFO = parse("shallow_alg.f90", api=API)
PSY = PSyFactory(API).create(INVOKEINFO)

# Print the 'vanilla' generated Fortran
print PSY.gen

# Print a list of all of the invokes found
print PSY.invokes.names

# Print the Schedule of each of these Invokes
SCHEDULE = PSY.invokes.get('invoke_0').schedule
SCHEDULE.view()

SCHEDULE = PSY.invokes.get('invoke_1').schedule
SCHEDULE.view()

SCHEDULE = PSY.invokes.get('invoke_2').schedule
SCHEDULE.view()
