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

class TestParserGOcean1p0:

    def test_kernels_different_grid_offsets_one_invoke(self):
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gocean1p0","test01_different_grid_offsets_one_invoke.f90"),api="gocean1.0")

    def test_kernels_different_grid_offsets_two_invokes(self):
        ''' Check that we raise an error if the kernels in different invokes 
            specify different grid offsets. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gocean1p0","test02_different_grid_offsets_two_invokes.f90"),api="gocean1.0")

    def test_kernel_missing_index_offset(self):
        ''' Check that we raise an error if a kernel's meta-data is 
            missing the index_offset field. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gocean1p0","test03_invoke_kernel_missing_offset.f90"),api="gocean1.0")
