#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from parse import parse, ParseError
import pytest
import os

class TestParserGungHoProto:

    def test_single_invoke_undeclared(self):
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gunghoproto","2_undeclared_function.f90"),api="gunghoproto")
