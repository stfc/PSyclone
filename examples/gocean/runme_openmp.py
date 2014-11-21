#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab
# Funded by the GOcean project

from parse import parse
from psyGen import PSyFactory

api="gocean"
ast,invokeInfo=parse("shallow_gocean.f90",api=api)
psy=PSyFactory(api).create(invokeInfo)
print psy.gen

print psy.invokes.names
schedule=psy.invokes.get('invoke_0').schedule
schedule.view()

from psyGen import TransInfo
t=TransInfo()
print t.list
lf=t.get_trans_name('LoopFuse')
ol=t.get_trans_name('GOceanOpenMPLoop')

# invoke0
# fuse all outer loops
lf1_schedule,memento = lf.apply(schedule.children[0],schedule.children[1])
lf2_schedule,memento = lf.apply(lf1_schedule.children[0],
                                lf1_schedule.children[1])
lf3_schedule,memento = lf.apply(lf2_schedule.children[0],
                                lf2_schedule.children[1])
lf3_schedule.view()

# fuse all inner loops
lf4_schedule,memento = lf.apply(lf3_schedule.children[0].children[0],
                                lf3_schedule.children[0].children[1])
lf5_schedule,memento = lf.apply(lf4_schedule.children[0].children[0],
                                lf4_schedule.children[0].children[1])
lf6_schedule,memento = lf.apply(lf5_schedule.children[0].children[0],
                                lf5_schedule.children[0].children[1])
lf6_schedule.view()

ol_schedule,memento = ol.apply(lf6_schedule.children[0])
ol_schedule.view()

psy.invokes.get('invoke_0')._schedule=ol_schedule
print psy.gen
