# -------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 20145
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab
# Funded by the GOcean project

'''A simple test script showing the introduction of OpenMP with PSyclone.
In order to use it you must first install PSyclone. See README.md in the
top-level psyclone directory.

Once you have psyclone installed, this script may be run by doing:

 >>> python runme_openmp.py

This should generate a lot of output, ending with generated
Fortran.
'''

from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, TransInfo

API = "nemo0.1"
_, INVOKEINFO = parse("tra_adv.F90", api=API)
PSY = PSyFactory(API).create(INVOKEINFO)
print PSY.gen

print PSY.invokes.names
SCHEDULE = PSY.invokes.get('tra_adv').schedule
SCHEDULE.view()

TRANS_INFO = TransInfo()
print TRANS_INFO.list
FUSE_TRANS = TRANS_INFO.get_trans_name('LoopFuse')
OMP_TRANS = TRANS_INFO.get_trans_name('OMPParallelLoopTrans')

OL_SCHEDULE, _ = OMP_TRANS.apply(SCHEDULE.children[1])
OL_SCHEDULE.view()

PSY.invokes.get('invoke_0').schedule = OL_SCHEDULE
print PSY.gen
