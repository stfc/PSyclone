# ----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# ----------------------------------------------------------------------------
# Authors R. Ford and A. Porter, STFC Daresbury Lab

''' Tests specific to the GungHo prototype API '''

from __future__ import absolute_import
import os
from psyclone.parse import parse, ParseError
import pytest


def test_single_invoke_undeclared():
    ''' Check that an invoke of an undeclared function raises a
    ParseError '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gunghoproto",
                           "2_undeclared_function.f90"),
              api="gunghoproto")
