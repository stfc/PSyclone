# -------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab
# Funded by the GOcean project

'''A simple test script showing loop-fusion with PSyclone.
In order to use it you must first configure your PYTHONPATH like so:

 >>> cd <blah>/PSyclone
 >>> export PYTHONPATH=`pwd`/src:`pwd`/f2py_93

Once your PYTHONPATH has been set-up, this script may be run by doing:

 >>> python runme_loop_fuse.py

This should generate a lot of output, ending with generated
Fortran. In subroutine invoke_0 you will see the loop-fused code:

 >>>    SUBROUTINE invoke_0(cu_fld, p_fld, u_fld, cv_fld, v_fld, z_fld, h_fld)
 >>>    ...
 >>>      DO j=2,jstop
 >>>        DO i=2,istop+1
 >>>          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
 >>>          CALL compute_cv_code(i, j, cv_fld%data, p_fld%data, v_fld%data)
 >>>          CALL compute_z_code(i, j, z_fld%data, p_fld%data, u_fld%data, \
 >>>                              v_fld%data, p_fld%grid%dx, p_fld%grid%dy)
 >>>          CALL compute_h_code(i, j, h_fld%data, p_fld%data, u_fld%data, \
 >>>                              v_fld%data)
 >>>        END DO
 >>>      END DO
 >>>    END SUBROUTINE invoke_0

'''

from parse import parse
from psyGen import PSyFactory, TransInfo

API = "gocean1.0"
ast, invokeInfo = parse("shallow_alg.f90", api=API)
psy = PSyFactory(API).create(invokeInfo)
# Print the vanilla, generated Fortran
print psy.gen

print psy.invokes.names
schedule = psy.invokes.get('invoke_0').schedule
schedule.view()

t = TransInfo()
print t.list
lf = t.get_trans_name('LoopFuse')

# fuse all outer loops
lf1_schedule, _ = lf.apply(schedule.children[0],
                           schedule.children[1])
lf2_schedule, _ = lf.apply(lf1_schedule.children[0],
                           lf1_schedule.children[1])
lf3_schedule, _ = lf.apply(lf2_schedule.children[0],
                           lf2_schedule.children[1])
lf3_schedule.view()

# fuse all inner loops
lf4_schedule, _ = lf.apply(lf3_schedule.children[0].children[0],
                           lf3_schedule.children[0].children[1])
lf5_schedule, _ = lf.apply(lf4_schedule.children[0].children[0],
                           lf4_schedule.children[0].children[1])
lf6_schedule, _ = lf.apply(lf5_schedule.children[0].children[0],
                           lf5_schedule.children[0].children[1])
lf6_schedule.view()

psy.invokes.get('invoke_0').schedule = lf6_schedule
print psy.gen
