#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' Tests of transformations with the Dynamo 0.3 API '''

from parse import parse
from psyGen import PSyFactory
from transformations import Dynamo0p3ColourTrans
import os
import pytest

TEST_API = "dynamo0.3"


def test_colour_trans():
    ''' test of the colouring transformation of a single loop '''
    _,info=parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "test_files", "dynamo0p3",
                              "1_single_invoke.f90"),
                 api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invokes = psy.invokes
    invoke = invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = cschedule

    # Store the results of applying this code transformation as
    # a string
    gen=str(psy.gen)

    print gen

    # Check that we're calling the API to get the no. of colours
    assert "%get_colours(" in gen
