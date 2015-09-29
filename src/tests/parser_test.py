#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Authors R. Ford and A. Porter, STFC Daresbury Lab

from parse import parse, ParseError
import pytest
import os

class TestParserGungHoProto:

    def test_single_invoke_undeclared(self):
        ''' Check that an invoke of an undeclared function raises a
            ParseError '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gunghoproto",
                               "2_undeclared_function.f90"),
                  api="gunghoproto")

class TestParserGOcean1p0:

    def test03_kernel_missing_index_offset(self):
        ''' Check that we raise an error if a kernel's meta-data is
            missing the INDEX_OFFSET field. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test03_invoke_kernel_missing_offset.f90"),
                  api="gocean1.0")

    def test04_kernel_invalid_index_offset(self):
        ''' Check that we raise an error if a kernel's meta-data is
            contains an invalid value for the INDEX_OFFSET field. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test04_invoke_kernel_invalid_offset.f90"),
                  api="gocean1.0")

    def test05_kernel_missing_iterates_over(self):
        ''' Check that we raise an error if a kernel's meta-data is
            missing the ITERATES_OVER field. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test05_invoke_kernel_missing_iterates_over.f90"),
                  api="gocean1.0")

    def test05p1_kernel_invalid_iterates_over(self):
        ''' Check that we raise an error if a kernel's meta-data has
            an invalid ITERATES_OVER field. '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test05.1_invoke_kernel_invalid_iterates_over.f90"),
                  api="gocean1.0")

    def test06_kernel_invalid_access(self):
        ''' Check that we raise an error if a kernel's meta-data specifies
            an unrecognised access type for a kernel argument (i.e. something
            other than READ,WRITE,READWRITE) '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test06_invoke_kernel_wrong_access.f90"),
                  api="gocean1.0")
            
    def test07_kernel_wrong_gridpt_type(self):
        ''' Check that we raise an error if a kernel's meta-data specifies
            an unrecognised grid-point type for a field argument (i.e.
            something other than C{U,V,F,T}, I_SCALAR or R_SCALAR) '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test07_invoke_kernel_wrong_gridpt_type.f90"),
                  api="gocean1.0")

    def test08_kernel_invalid_grid_property(self):
        ''' Check that the parser raises an error if a kernel's meta-data
            specifies an unrecognised grid property '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test08_invoke_kernel_invalid_grid_property.f90"),
                  api="gocean1.0")

    def test09_kernel_missing_stencil_property(self):
        ''' Check that the parser raises an error if there is no stencil specified
            in the meta-data of a kernel '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test09_invoke_kernel_missing_stencil.f90"),
                  api="gocean1.0")

    def test10_kernel_invalid_stencil_property(self):
        ''' Check that the parser raises an error if there is no stencil specified
            in the meta-data of a kernel '''
        with pytest.raises(ParseError):
            parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test10_invoke_kernel_invalid_stencil.f90"),
                  api="gocean1.0")

    def test13_kernel_invalid_fortran(self):
        ''' Check that the parser raises an error if the specified kernel
            code is not valid fortran '''
        with pytest.raises(ParseError):
             parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test13_invoke_kernel_invalid_fortran.f90"),
                  api="gocean1.0")

