# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: R. W. Ford, STFC Daresbury Lab
# Modified: A. R. Porter and S. Siso, STFC Daresbury Lab

'''Module containing tests for the KernelMetadataSymbol
kernel-layer-specific symbol. The tests include translation of
language-level PSyIR to PSyclone GOcean Kernel PSyIR and PSyclone
GOcean Kernel PSyIR to language-level PSyIR.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.configuration import Config
from psyclone.domain.gocean.kernel import GOceanKernelMetadata, \
    GOceanContainer
from psyclone.domain.gocean.transformations import RaisePSyIR2GOceanKernTrans
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import SymbolTable, REAL_TYPE

METADATA = ("TYPE, EXTENDS(kernel_type) :: compute_cu\n"
            "  TYPE(go_arg), DIMENSION(4) :: meta_args = (/ &\n"
            "go_arg(GO_WRITE, GO_CU, GO_POINTWISE), &\n"
            "go_arg(GO_READ, GO_CT, GO_STENCIL(000, 011, 000)), &\n"
            "go_arg(GO_READ, GO_GRID_AREA_T), &\n"
            "go_arg(GO_READ, GO_R_SCALAR, GO_POINTWISE)/)\n"
            "  INTEGER :: ITERATES_OVER = GO_ALL_PTS\n"
            "  INTEGER :: INDEX_OFFSET = GO_OFFSET_SW\n"
            "  CONTAINS\n"
            "    PROCEDURE, NOPASS :: code => compute_cu_code\n"
            "END TYPE compute_cu\n")

PROGRAM = (
    f"module dummy\n"
    f"{METADATA}"
    f"contains\n"
    f"  subroutine compute_cu_code()\n"
    f"  end subroutine compute_cu_code\n"
    f"end module dummy\n")


# Class GOceanContainer

def test_goceancontainer_init():
    '''Test that an instance of GOceanContainer can be created. '''
    container = GOceanContainer("name", None)
    assert container.name == "name"
    assert container._metadata is None


def test_goceancontainer_create():
    '''Test that the GOceanContainer create method works as
    expected. Includes raising any exceptions. Also make use of the
    metadata property.

    '''
    metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    container = GOceanContainer.create("name", metadata, SymbolTable(), [])
    assert container.name == "name"
    assert container.metadata.fortran_string() == METADATA
    assert container.children == []
    with pytest.raises(ValueError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string("Not valid")
    assert ("Expected kernel metadata to be a Fortran derived type, but "
            "found 'Not valid'." in str(str(info.value)))


def test_goceancontainer_lower(fortran_reader):
    '''Test that the GOceanContainer lower_to_language_level method works
    as expected.

    '''
    # First load program and perform checks
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    assert isinstance(kernel_psyir.children[0], Container)
    assert not isinstance(kernel_psyir.children[0], GOceanContainer)
    assert kernel_psyir.children[0].symbol_table.lookup("compute_cu")

    # Now raise to GOcean PSyIR and perform checks
    kern_trans = RaisePSyIR2GOceanKernTrans("compute_cu")
    kern_trans.apply(kernel_psyir)
    assert isinstance(kernel_psyir.children[0], GOceanContainer)
    with pytest.raises(KeyError):
        kernel_psyir.children[0].symbol_table.lookup("compute_cu")

    # Now use lower_to_language_level and perform checks
    container = kernel_psyir.children[0]
    lowered = container.lower_to_language_level()
    assert lowered is kernel_psyir.children[0]
    assert isinstance(kernel_psyir.children[0], Container)
    assert not isinstance(kernel_psyir.children[0], GOceanContainer)
    assert kernel_psyir.children[0].symbol_table.lookup("compute_cu")


# Class GOceanKernelMetadata

def test_goceankernelmetadata_init():
    ''' Test that an instance of GOceanKernelMetadata can be created. '''
    metadata = GOceanKernelMetadata()
    assert isinstance(metadata, GOceanKernelMetadata)
    assert metadata._iterates_over is None
    assert metadata._index_offset is None
    assert metadata._meta_args == []
    assert metadata._procedure_name is None
    assert metadata._name is None


def test_goceankernelmetadata_init2():
    '''Test that initial values for GOceanKernelMetadata can be checked
    and set as expected.

    '''
    # split the assert into two as we can get pollution from other
    # tests which increase the expected argument list.
    with pytest.raises(ValueError) as info:
        _ = GOceanKernelMetadata(iterates_over="hello")
    assert ("Expected one of ['go_all_pts', 'go_internal_pts', "
            "'go_external_pts'" in str(info.value))
    assert ("for 'iterates_over' metadata, but found "
            "'hello'." in str(info.value))

    # split the assert into two as we can get pollution from other
    # tests which increase the expected argument list.
    with pytest.raises(ValueError) as info:
        _ = GOceanKernelMetadata(index_offset="hello")
    assert ("Expected one of ['go_offset_ne', 'go_offset_sw', "
            "'go_offset_any'" in str(info.value))
    assert ("for 'index_offset' metadata, but found "
            "'hello'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = GOceanKernelMetadata(meta_args="hello")
    assert "meta_args should be a list but found str." in str(info.value)

    with pytest.raises(TypeError) as info:
        _ = GOceanKernelMetadata(meta_args=["hello"])
    assert ("meta_args should be a list of FieldArg, GridArg or ScalarArg "
            "objects, but found str." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = GOceanKernelMetadata(procedure_name="1error")
    assert ("Expected procedure_name to be a valid value but found "
            "'1error'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = GOceanKernelMetadata(name="1error")
    assert "Invalid Fortran name '1error' found." in str(info.value)

    metadata = GOceanKernelMetadata(
        iterates_over="go_all_pts", index_offset="go_offset_ne", meta_args=[],
        procedure_name="example_code", name="example_type")
    assert metadata.iterates_over == "go_all_pts"
    assert metadata.index_offset == "go_offset_ne"
    assert metadata.meta_args == []
    assert metadata.procedure_name == "example_code"
    assert metadata.name == "example_type"


# create_from_psyir
def test_goceankernelmetadata_create1(fortran_reader):
    '''Test the create_from_psyir method works as expected including any
    exceptions. Also tests the fortran string method.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    symbol = kernel_psyir.children[0].symbol_table.lookup("compute_cu")
    with pytest.raises(TypeError) as info:
        _ = GOceanKernelMetadata.create_from_psyir("symbol")
    assert "Expected a DataTypeSymbol but found a str." in str(info.value)
    metadata = GOceanKernelMetadata.create_from_psyir(symbol)
    assert METADATA in metadata.fortran_string()
    symbol._datatype = REAL_TYPE
    with pytest.raises(InternalError) as info:
        _ = GOceanKernelMetadata.create_from_psyir(symbol)
    assert ("Expected kernel metadata to be stored in the PSyIR as an "
            "UnsupportedFortranType, but found ScalarType." in str(info.value))


# create_from_fortran_string
def test_goceankernelmetadata_create2():
    '''Test that the create_from_fortran_string method works as
    expected. Also tests the fortran_string method.

    '''
    metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    assert METADATA in metadata.fortran_string()


def test_create_iteratesover():
    '''Test that the create_from_fortran_string method raises an exception
    if iterates_over does not exist or has an invalid value.

    '''
    # Does not exist
    modified_metadata = METADATA.replace("ITERATES_OVER", "ignored")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("'iterates_over' was not found in TYPE, EXTENDS(kernel_type) "
            ":: compute_cu" in str(info.value))
    # invalid value
    modified_metadata = METADATA.replace("GO_ALL_PTS", "invalid")
    with pytest.raises(ValueError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    valid_iterates_over = constants.VALID_ITERATES_OVER
    assert (f"Expected one of {valid_iterates_over} for 'iterates_over' "
            f"metadata, but found 'invalid'." in str(info.value))


def test_create_indexoffset():
    '''Test that the create_from_string method raises an exception if
    index_offset does not exist or has an invalid value.

    '''
    # Does not exist
    modified_metadata = METADATA.replace("INDEX_OFFSET", "ignored")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("'index_offset' was not found in TYPE, EXTENDS(kernel_type) "
            ":: compute_cu" in str(info.value))
    # invalid value
    modified_metadata = METADATA.replace("GO_OFFSET_SW", "invalid")
    with pytest.raises(ValueError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    supported_offsets = constants.SUPPORTED_OFFSETS
    assert (f"Expected one of {supported_offsets} for 'index_offset' "
            f"metadata, but found 'invalid'." in str(info.value))


def test_create_procedure():
    '''Test that the create_from_fortran_string method raises an exception
    if the required type bound procedure does not exist or has an
    invalid value. Also test that it works as expected if the value is
    valid. Also test that it works when the procedure name is
    specified with and without 'code =>'.

    '''
    # no contains
    modified_metadata = METADATA.replace(
        "  CONTAINS\n    PROCEDURE, NOPASS :: code => compute_cu_code\n", "")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("No type-bound procedure found within a 'contains' section in "
            "'TYPE, EXTENDS(kernel_type) :: compute_cu" in str(info.value))

    # no type-bound procedure
    modified_metadata = METADATA.replace(
        "  PROCEDURE, NOPASS :: code => compute_cu_code\n", "")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("Expecting a type-bound procedure, but found 'TYPE, "
            "EXTENDS(kernel_type) :: compute_cu" in str(info.value))

    # not specific binding
    modified_metadata = METADATA.replace(
        "  PROCEDURE, NOPASS :: code => compute_cu_code\n",
        "    generic :: code => compute_cu_code\n")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("Expecting a specific binding for the type-bound procedure, but "
            "found 'GENERIC :: code => compute_cu_code' in 'TYPE, "
            "EXTENDS(kernel_type) :: compute_cu" in str(info.value))

    # binding name not 'code'
    modified_metadata = METADATA.replace(
        "  PROCEDURE, NOPASS :: code => compute_cu_code\n",
        "  PROCEDURE, NOPASS :: ignore => compute_cu_code\n")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("Expecting the type-bound procedure binding-name to be 'code' "
            "if there is a procedure name, but found 'ignore' in 'TYPE, "
            "EXTENDS(kernel_type) :: compute_cu" in str(info.value))

    # OK, no procedure name
    modified_metadata = METADATA.replace(
        "  PROCEDURE, NOPASS :: code => compute_cu_code\n",
        "  PROCEDURE, NOPASS :: code\n")
    metadata = GOceanKernelMetadata.create_from_fortran_string(
        modified_metadata)
    assert metadata.procedure_name == "code"


# metaargs does not exist, len different to nargs, wrong num args,
# type go_arg, each entry go_arg.
def test_create_metaargs():
    '''Test that the setup method raises an exception if the required
    meta_args information does not exist or contains inconsistent
    information. Also test that it works as expected if the values are
    valid.

    '''
    # does not exist
    modified_metadata = METADATA.replace("meta_args", "ignore")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("'meta_args' was not found in TYPE, EXTENDS(kernel_type) :: "
            "compute_cu" in str(info.value))

    # not an array
    modified_metadata = METADATA.replace("meta_args", "ignore")
    modified_metadata = modified_metadata.replace(
        "  CONTAINS", "  integer :: meta_args = hello\n  contains")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("meta_args should be a list, but found 'hello' in 'TYPE, "
            "EXTENDS(kernel_type) :: compute_cu" in str(info.value))

    # nargs is 3 but not field or scalar
    modified_metadata = METADATA.replace("GO_R_SCALAR", "INVALID")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    field_grid_types = constants.VALID_FIELD_GRID_TYPES
    scalar_types = constants.VALID_SCALAR_TYPES
    assert (f"Expected a 'meta_arg' entry with 3 arguments to either be a "
            f"field or a scalar, but found 'invalid' as the second argument "
            f"instead of '{field_grid_types}' (fields) or '{scalar_types}' "
            f"(scalars)." in str(info.value))

    # nargs not 2 or 3
    modified_metadata = METADATA.replace(", GO_GRID_AREA_T", "")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("'meta_args' should have either 2 or 3 arguments, but found 1 in "
            "go_arg(GO_READ)." in str(info.value))


def test_getproperty_error():
    '''Test two exceptions in the getproperty utility function. This is
    called indirectly in this test.

    '''
    # no declarations
    metadata = (
        "  type, extends(kernel_type) :: compute_cu\n"
        "  contains\n"
        "    procedure, nopass :: code => compute_cu_code\n"
        "  end type compute_cu\n")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(metadata)
    assert ("No declarations were found in the kernel metadata: "
            "'TYPE, EXTENDS(kernel_type) :: compute_cu\n  CONTAINS\n  "
            "PROCEDURE, NOPASS :: code => compute_cu_code\nEND TYPE "
            "compute_cu'." in str(info.value))

    # property without a value
    modified_metadata = METADATA.replace(" = GO_ALL_PTS", "")
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(modified_metadata)
    assert ("No value for property iterates_over was found in 'TYPE, "
            "EXTENDS(kernel_type)" in str(info.value))


def test_getproperty(fortran_reader):
    '''Test utility function that takes metadata in an fparser2 tree and
    returns the value associated with the supplied property name.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    datatype = kernel_psyir.children[0].symbol_table.lookup(
        "compute_cu").datatype
    metadata = GOceanKernelMetadata()
    reader = FortranStringReader(datatype.declaration)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    assert metadata._get_property(spec_part, "code").string == \
        "compute_cu_code"
    assert metadata._get_property(spec_part, "iterates_over").string == \
        "GO_ALL_PTS"
    with pytest.raises(ParseError) as info:
        metadata._get_property(spec_part, "not_found")
    assert ("'not_found' was not found in TYPE, EXTENDS(kernel_type) :: "
            "compute_cu" in str(info.value))


def test_iteratesover():
    '''Test that get, set and validate work for iterates_over metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    assert kernel_metadata.iterates_over == "GO_ALL_PTS"
    with pytest.raises(ValueError) as info:
        kernel_metadata.iterates_over = "hello"
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    iterates_over_types = constants.VALID_ITERATES_OVER
    assert (f"Expected one of {iterates_over_types} for 'iterates_over' "
            f"metadata, but found 'hello'." in str(info.value))
    kernel_metadata.iterates_over = "GO_INTERNAL_PTS"
    assert kernel_metadata.iterates_over == "GO_INTERNAL_PTS"


def test_indexoffset():
    '''Test that get, set and validate work for index_offset metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    assert kernel_metadata.index_offset == "GO_OFFSET_SW"
    with pytest.raises(ValueError) as info:
        kernel_metadata.index_offset = "hello"
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    offset_types = constants.SUPPORTED_OFFSETS
    assert (f"Expected one of {offset_types} for 'index_offset' metadata, "
            f"but found 'hello'." in str(info.value))
    kernel_metadata.index_offset = "GO_OFFSET_NE"
    assert kernel_metadata.index_offset == "GO_OFFSET_NE"


def test_meta_args():
    '''Test that get works for args metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    assert len(kernel_metadata.meta_args) == 4
    assert isinstance(
        kernel_metadata.meta_args[0], GOceanKernelMetadata.FieldArg)
    assert isinstance(
        kernel_metadata.meta_args[1], GOceanKernelMetadata.FieldArg)
    assert isinstance(
        kernel_metadata.meta_args[2], GOceanKernelMetadata.GridArg)
    assert isinstance(
        kernel_metadata.meta_args[3], GOceanKernelMetadata.ScalarArg)


def test_procedure_name():
    '''Test that get and set work for procedure metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    assert kernel_metadata.procedure_name == "compute_cu_code"
    kernel_metadata.procedure_name = "new_code"
    assert kernel_metadata.procedure_name == "new_code"
    with pytest.raises(ValueError) as info:
        kernel_metadata.procedure_name = "1invalid"
    assert ("Expected procedure_name to be a valid value but found "
            "'1invalid'." in str(info.value))


def test_metadata_name():
    '''Test that get and set work for name metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    assert kernel_metadata.name == "compute_cu"
    kernel_metadata.name = "new_name"
    assert kernel_metadata.name == "new_name"
    with pytest.raises(ValueError) as info:
        kernel_metadata.name = "1invalid"
    assert "Invalid Fortran name '1invalid' found." in str(info.value)


# internal GridArg class

def test_gridarg_init():
    '''Test that an instance of the GridArg class can be created
    succesfully.

    '''
    reader = FortranStringReader(METADATA)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    arg_list = walk(spec_part, Fortran2003.Ac_Value_List)[0]
    grid_arg = GOceanKernelMetadata.GridArg(arg_list.children[2], None)
    assert isinstance(grid_arg, GOceanKernelMetadata.GridArg)
    assert grid_arg.access == "GO_READ"


def test_gridarg_error():
    '''Test that the expected exception is raised if the number of
    metadata arguments passed into the constructor is incorrect.

    '''
    reader = FortranStringReader(METADATA)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    arg_list = walk(spec_part, Fortran2003.Ac_Value_List)[0]
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.GridArg(arg_list.children[0], None)
    assert ("There should be 2 kernel metadata arguments for a grid property "
            "but found 3 in go_arg(GO_WRITE, GO_CU, GO_POINTWISE)"
            in str(info.value))


def test_gridarg_fortranstring():
    '''Test that the fortran_string method in a GridArg instance
    works as expected.

    '''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    grid_arg = kernel_metadata.meta_args[2]
    result = grid_arg.fortran_string()
    assert result == "go_arg(GO_READ, GO_GRID_AREA_T)"


def test_gridarg_access():
    '''Test that get, set and validate work for access metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    grid_arg = kernel_metadata.meta_args[2]
    assert grid_arg.access == "GO_READ"
    with pytest.raises(ValueError) as info:
        grid_arg.access = "hello"
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    access_types = constants.get_valid_access_types()
    assert (f"The first metadata entry for a grid property argument should "
            f"be a valid access descriptor (one of {access_types}), but "
            f"found 'hello'." in str(info.value))
    grid_arg.access = "GO_WRITE"
    assert grid_arg.access == "GO_WRITE"


def test_gridarg_name():
    '''Test that get, set and validate work for name metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    grid_arg = kernel_metadata.meta_args[2]
    assert grid_arg.name == "GO_GRID_AREA_T"
    with pytest.raises(ValueError) as info:
        grid_arg.name = "hello"
    config = Config.get()
    api_config = config.api_conf("gocean1.0")
    grid_property_names = list(api_config.grid_properties.keys())
    assert (f"The second metadata entry for a grid property argument should "
            f"have a valid name (one of {grid_property_names}), but found "
            f"'hello'." in str(info.value))
    grid_arg.name = "GO_GRID_XSTOP"
    assert grid_arg.name == "GO_GRID_XSTOP"


# internal FieldArg class

def test_fieldarg_init():
    '''Test that a instance of the FieldArg class can be created
    succesfully.

    '''
    reader = FortranStringReader(METADATA)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    arg_list = walk(spec_part, Fortran2003.Ac_Value_List)[0]
    field_arg = GOceanKernelMetadata.FieldArg(arg_list.children[0], None)
    assert isinstance(field_arg, GOceanKernelMetadata.FieldArg)
    assert field_arg.access == "GO_WRITE"


def test_fieldarg_error():
    '''Test that the expected exception is raised if the number of
    metadata arguments passed into the constructor is incorrect.

    '''
    reader = FortranStringReader(METADATA)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    arg_list = walk(spec_part, Fortran2003.Ac_Value_List)[0]
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.FieldArg(arg_list.children[2], None)
    assert ("There should be 3 kernel metadata entries for a field argument, "
            "but found 2 in go_arg(GO_READ, GO_GRID_AREA_T)."
            in str(info.value))


def test_fieldarg_fortranstring():
    '''Test that the fortran_string method in a FieldArg instance
    works as expected. Test when there is and there is not a stencil.

    '''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    # no stencil
    field_arg = kernel_metadata.meta_args[0]
    result = field_arg.fortran_string()
    assert result == "go_arg(GO_WRITE, GO_CU, GO_POINTWISE)"
    # stencil
    field_arg = kernel_metadata.meta_args[1]
    result = field_arg.fortran_string()
    assert result == "go_arg(GO_READ, GO_CT, GO_STENCIL(000, 011, 000))"


def test_fieldarg_access():
    '''Test that get, set and validate work for access metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    field_arg = kernel_metadata.meta_args[0]
    assert field_arg.access == "GO_WRITE"
    with pytest.raises(ValueError) as info:
        field_arg.access = "hello"
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    access_types = constants.get_valid_access_types()
    assert (f"The first metadata entry for a field argument should be a "
            f"recognised access descriptor (one of {access_types}), but "
            f"found 'hello'." in str(info.value))
    field_arg.access = "GO_READ"
    assert field_arg.access == "GO_READ"


def test_fieldarg_grid_point_type():
    '''Test that get, set and validate work for grid_point_type
    metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    field_arg = kernel_metadata.meta_args[0]
    assert field_arg.grid_point_type == "GO_CU"
    with pytest.raises(ValueError) as info:
        field_arg.grid_point_type = "hello"
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    field_grid_types = constants.VALID_FIELD_GRID_TYPES
    assert (f"The second metadata entry for a field argument should be a "
            f"recognised grid-point type descriptor (one of "
            f"{field_grid_types}), but found 'hello'." in str(info.value))
    field_arg.grid_point_type = "GO_CF"
    assert field_arg.grid_point_type == "GO_CF"


def test_fieldarg_form():
    '''Test that get, set and validate work for form metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    field_arg = kernel_metadata.meta_args[0]
    assert field_arg.form == "GO_POINTWISE"
    with pytest.raises(ValueError) as info:
        field_arg.form = "hello"
    assert ("The third metadata entry for a field should be a recognised "
            "stencil descriptor (one of ['go_pointwise'] or 'go_stencil'), "
            "but found 'hello'." in str(info.value))
    field_arg.form = "go_pointwise"
    assert field_arg.form == "go_pointwise"


def test_fieldarg_stencil():
    '''Test that get, set and validate work for stencil metadata.'''
    kernel_metadata = METADATA.replace("GO_STENCIL", "GO_PENCIL")
    with pytest.raises(ValueError) as info:
        _ = GOceanKernelMetadata.create_from_fortran_string(kernel_metadata)
    assert ("The third metadata entry for a field should be go_stencil(...) "
            "if it contains arguments, but found 'GO_PENCIL'."
            in str(info.value))
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    field_arg = kernel_metadata.meta_args[1]
    assert field_arg.form == "GO_STENCIL"
    assert len(field_arg.stencil) == 3
    assert field_arg.stencil == ["000", "011", "000"]
    with pytest.raises(TypeError) as info:
        field_arg.stencil = "hello"
    assert ("Stencil entries should be provided as a list, but found 'str'."
            in str(info.value))
    with pytest.raises(ValueError) as info:
        field_arg.stencil = ["hello"]
    assert ("If the third metadata entry is a stencil, it should contain 3 "
            "arguments, but found 1 in ['hello']." in str(info.value))
    with pytest.raises(TypeError) as info:
        field_arg.stencil = [0, "hello", "hello"]
    assert ("Stencil entries should be strings, but found 'int'."
            in str(info.value))
    with pytest.raises(ValueError) as info:
        field_arg.stencil = ["000", "011", "00"]
    assert ("Stencil entries should follow the regular expression "
            "[01]{3,3}, but found '00'." in str(info.value))

    field_arg = kernel_metadata.meta_args[0]
    assert field_arg.form == "GO_POINTWISE"
    assert field_arg.stencil is None
    field_arg.stencil = ["000", "111", "000"]
    assert field_arg.form == "GO_STENCIL"
    assert field_arg.stencil == ["000", "111", "000"]


# internal ScalarArg class

def test_scalararg_init():
    '''Test that a instance of the ScalarArg class can be created
    succesfully.

    '''
    reader = FortranStringReader(METADATA)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    arg_list = walk(spec_part, Fortran2003.Ac_Value_List)[0]
    scalar_arg = GOceanKernelMetadata.ScalarArg(arg_list.children[3], None)
    assert isinstance(scalar_arg, GOceanKernelMetadata.ScalarArg)
    assert scalar_arg.access == "GO_READ"


def test_scalararg_error():
    '''Test that the expected exception is raised if the number of
    metadata arguments passed into the constructor is incorrect.

    '''
    reader = FortranStringReader(METADATA)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    arg_list = walk(spec_part, Fortran2003.Ac_Value_List)[0]
    with pytest.raises(ParseError) as info:
        _ = GOceanKernelMetadata.ScalarArg(arg_list.children[2], None)
    assert ("There should be 3 kernel metadata entries for a scalar argument, "
            "but found 2 in go_arg(GO_READ, GO_GRID_AREA_T)."
            in str(info.value))


def test_scalararg_fortranstring():
    '''Test that the fortran_string method in a ScalarArg instance
    works as expected.

    '''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    scalar_arg = kernel_metadata.meta_args[3]
    result = scalar_arg.fortran_string()
    assert result == "go_arg(GO_READ, GO_R_SCALAR, GO_POINTWISE)"


def test_scalararg_access():
    '''Test that get, set and validate work for access metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    scalar_arg = kernel_metadata.meta_args[3]
    assert scalar_arg.access == "GO_READ"
    with pytest.raises(ValueError) as info:
        scalar_arg.access = "hello"
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    access_types = constants.get_valid_access_types()
    assert (f"The first metadata entry for a scalar argument should be a "
            f"recognised access descriptor (one of {access_types}), but "
            f"found 'hello'." in str(info.value))
    scalar_arg.access = "GO_WRITE"
    assert scalar_arg.access == "GO_WRITE"


def test_scalararg_datatype():
    '''Test that get, set and validate work for datatype metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    scalar_arg = kernel_metadata.meta_args[3]
    assert scalar_arg.datatype == "GO_R_SCALAR"
    with pytest.raises(ValueError) as info:
        scalar_arg.datatype = "hello"
    config = Config.get()
    constants = config.api_conf("gocean1.0").get_constants()
    scalar_types = constants.VALID_SCALAR_TYPES
    assert (f"The second metadata entry for a scalar argument should be a "
            f"recognised name (one of {scalar_types}), but found 'hello'."
            in str(info.value))
    scalar_arg.datatype = "GO_I_SCALAR"
    assert scalar_arg.datatype == "GO_I_SCALAR"


def test_scalararg_form():
    '''Test that get, set and validate work for form metadata.'''
    kernel_metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    scalar_arg = kernel_metadata.meta_args[3]
    assert scalar_arg.form == "GO_POINTWISE"
    with pytest.raises(ValueError) as info:
        scalar_arg.form = "hello"
    assert ("The third metadata entry for a scalar should be a recognised "
            "name (one of ['go_pointwise']), but found 'hello'."
            in str(info.value))
    scalar_arg.form = "go_pointwise"
    assert scalar_arg.form == "go_pointwise"
