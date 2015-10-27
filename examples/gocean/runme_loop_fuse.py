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
_, INVOKEINFO = parse("shallow_alg.f90", api=API)
PSY = PSyFactory(API).create(INVOKEINFO)
# Print the vanilla, generated Fortran
print PSY.gen

print PSY.invokes.names
SCHEDULE = PSY.invokes.get('invoke_0').schedule
SCHEDULE.view()

TRANS_INFO = TransInfo()
print TRANS_INFO.list
FUSE_TRANS = TRANS_INFO.get_trans_name('LoopFuse')

# fuse all outer loops
LF1_SCHED, _ = FUSE_TRANS.apply(SCHEDULE.children[0],
                                SCHEDULE.children[1])
LF2_SCHED, _ = FUSE_TRANS.apply(LF1_SCHED.children[0],
                                LF1_SCHED.children[1])
LF3_SCHED, _ = FUSE_TRANS.apply(LF2_SCHED.children[0],
                                LF2_SCHED.children[1])
LF3_SCHED.view()

# fuse all inner loops
LF4_SCHED, _ = FUSE_TRANS.apply(LF3_SCHED.children[0].children[0],
                                LF3_SCHED.children[0].children[1])
LF5_SCHED, _ = FUSE_TRANS.apply(LF4_SCHED.children[0].children[0],
                                LF4_SCHED.children[0].children[1])
LF6_SCHED, _ = FUSE_TRANS.apply(LF5_SCHED.children[0].children[0],
                                LF5_SCHED.children[0].children[1])
LF6_SCHED.view()

PSY.invokes.get('invoke_0').schedule = LF6_SCHED
print PSY.gen
