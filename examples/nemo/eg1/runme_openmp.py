#!/usr/bin/env python
## -------------------------------------------------------------------------
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

from __future__ import print_function
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, TransInfo

if __name__ == "__main__":
    api = "nemo"
    _, invokeinfo = parse("tra_adv.F90", api=api)
    psy = PSyFactory(api).create(invokeinfo)
    print(psy.gen)

    print("Invokes found:")
    print(psy.invokes.names)
    
    sched = psy.invokes.get('tra_adv').schedule
    sched.view()

    TRANS_INFO = TransInfo()
    print(TRANS_INFO.list)
    FUSE_TRANS = TRANS_INFO.get_trans_name('LoopFuse')
    OMP_TRANS = TRANS_INFO.get_trans_name('OMPParallelLoopTrans')

    for loop in sched.loops():
        kernel = loop.kernel
        if kernel:
            if loop.loop_type == "levels":
                sched, _ = OMP_TRANS.apply(loop)

    sched.view()

    psy.invokes.get('tra_adv').schedule = sched
    print(psy.gen)
