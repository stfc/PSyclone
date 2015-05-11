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

    def test01_kernels_different_grid_offsets_one_invoke(self):
        ''' Check that the parser raises an error if two kernels in a 
            single invoke specify different index offsets '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gocean1p0","test01_different_grid_offsets_one_invoke.f90"),api="gocean1.0")

    def test02_kernels_different_grid_offsets_two_invokes(self):
        ''' Check that the parser raises an error if the two kernels
            in different invokes specify different index offsets. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gocean1p0","test02_different_grid_offsets_two_invokes.f90"),api="gocean1.0")

    def test03_kernel_missing_index_offset(self):
        ''' Check that we raise an error if a kernel's meta-data is 
            missing the INDEX_OFFSET field. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gocean1p0","test03_invoke_kernel_missing_offset.f90"),api="gocean1.0")

    def test04_kernel_invalid_index_offset(self):
        ''' Check that we raise an error if a kernel's meta-data is 
            contains an invalid value for the INDEX_OFFSET field. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gocean1p0","test04_invoke_kernel_invalid_offset.f90"),api="gocean1.0")

    def test05_kernel_invalid_index_offset(self):
        ''' Check that we raise an error if a kernel's meta-data is 
            missing the ITERATES_OVER field. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gocean1p0","test05_invoke_kernel_missing_iterates_over.f90"),api="gocean1.0")

    def test06_kernel_invalid_access(self):
        ''' Check that we raise an error if a kernel's meta-data specifies
            an unrecognised access type for a kernel argument (i.e. something
            other than READ,WRITE,READWRITE) '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gocean1p0","test06_invoke_kernel_wrong_access.f90"),api="gocean1.0")
            
