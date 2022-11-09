# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the LFRicKernelMetadata class.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.columnwise_operator_arg_metadata import \
    ColumnwiseOperatorArgMetadata
from psyclone.domain.lfric.kernel.evaluator_targets_metadata import \
    EvaluatorTargetsMetadata
from psyclone.domain.lfric.kernel.field_arg_metadata import FieldArgMetadata
from psyclone.domain.lfric.kernel.field_vector_arg_metadata import \
    FieldVectorArgMetadata
from psyclone.domain.lfric.kernel.inter_grid_arg_metadata import \
    InterGridArgMetadata
from psyclone.domain.lfric.kernel.inter_grid_vector_arg_metadata import \
    InterGridVectorArgMetadata
from psyclone.domain.lfric.kernel.lfric_kernel_metadata import \
    LFRicKernelMetadata
from psyclone.domain.lfric.kernel.meta_mesh_arg_metadata import \
    MetaMeshArgMetadata
from psyclone.domain.lfric.kernel.meta_funcs_arg_metadata import \
    MetaFuncsArgMetadata
from psyclone.domain.lfric.kernel.operates_on_metadata import \
    OperatesOnMetadata
from psyclone.domain.lfric.kernel.operator_arg_metadata import \
    OperatorArgMetadata
from psyclone.domain.lfric.kernel.meta_ref_element_arg_metadata import \
    MetaRefElementArgMetadata
from psyclone.domain.lfric.kernel.scalar_arg_metadata import ScalarArgMetadata
from psyclone.domain.lfric.kernel.shapes_metadata import ShapesMetadata
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.symbols import DataTypeSymbol, REAL_TYPE, \
    UnknownFortranType


def test_init_noargs():
    '''Test that a LFRicKernelMetadata instance can be created
    successfully when no arguments are provided.

    '''
    meta = LFRicKernelMetadata()
    assert isinstance(meta, LFRicKernelMetadata)
    assert meta._operates_on is None
    assert meta._shapes is None
    assert meta._evaluator_targets is None
    assert meta._meta_args is None
    assert meta._meta_funcs is None
    assert meta._meta_ref_element is None
    assert meta._meta_mesh is None
    assert meta._procedure_name is None
    assert meta._name is None


def test_init_args():
    '''Test that a LFRicKernelMetadata instance can be created
    successfully when valid arguments are provided.

    '''
    scalar_arg = ScalarArgMetadata("GH_REAL", "GH_READ")
    meta_funcs_arg = MetaFuncsArgMetadata("w0", basis_function=True)
    meta_ref_element_arg = MetaRefElementArgMetadata("normals_to_faces")
    meta_mesh_arg = MetaMeshArgMetadata("adjacent_face")
    meta = LFRicKernelMetadata(
        operates_on="DOMAIN", shapes=["GH_EVALUATOR"],
        evaluator_targets=["W0"], meta_args=[scalar_arg],
        meta_funcs=[meta_funcs_arg], meta_ref_element=[meta_ref_element_arg],
        meta_mesh=[meta_mesh_arg], procedure_name="KERN_CODE",
        name="kern_type")

    assert isinstance(meta._operates_on, OperatesOnMetadata)
    assert meta._operates_on.operates_on == "domain"
    assert isinstance(meta._shapes, ShapesMetadata)
    assert meta._shapes.shapes == ["gh_evaluator"]
    assert isinstance(meta._evaluator_targets, EvaluatorTargetsMetadata)
    assert meta._evaluator_targets.evaluator_targets == ["w0"]
    assert meta.meta_args == [scalar_arg]
    # For some reason the equality test does not work for meta_funcs,
    # so use the fortran output instead to check for validity.
    assert (meta._meta_funcs.fortran_string() ==
            "type(FUNC_TYPE) :: META_FUNCS(1) = (/ &\n"
            "    func_type(w0, gh_basis)/)\n")
    assert meta.meta_ref_element == [meta_ref_element_arg]
    assert meta.meta_mesh == [meta_mesh_arg]
    assert meta.procedure_name == "KERN_CODE"
    assert meta.name == "kern_type"


def test_init_args_error():
    '''Test that a LFRicKernelMetadata instance raises the expected
    exceptions when invalid argument values are provided.

    '''
    with pytest.raises(ValueError) as info:
        _ = LFRicKernelMetadata(operates_on="invalid")
    print(str(info.value))
    assert ("The OPERATES_ON metadata should be a recognised value "
            "(one of ['cell_column', 'domain', 'dof']) but found 'invalid'."
            in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(shapes="invalid")
    assert ("ShapesMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(evaluator_targets="invalid")
    assert ("EvaluatorTargetsMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(meta_args="error")
    assert ("MetaArgsMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(meta_funcs="invalid")
    assert ("MetaFuncsMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(meta_ref_element="invalid")
    assert ("MetaRefElementMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(meta_mesh="invalid")
    assert ("MetaMeshMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = LFRicKernelMetadata(procedure_name="1_invalid")
    assert ("Expected procedure_name to be a valid Fortran name but found "
            "'1_invalid'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = LFRicKernelMetadata(name="1_invalid")
    assert ("Expected name to be a valid Fortran name but found "
            "'1_invalid'." in str(info.value))


METADATA = (
    "type, extends(kernel_type) :: testkern_type\n"
    "   type(arg_type), dimension(7) :: meta_args =                       &\n"
    "        (/ arg_type(gh_scalar,   gh_real, gh_read),                  &\n"
    "           arg_type(gh_field,    gh_real, gh_inc,  w1),              &\n"
    "           arg_type(gh_field*3,  gh_real, gh_read, w2),              &\n"
    "           arg_type(gh_field, gh_real, gh_read, w2, "
    "mesh_arg=gh_coarse),                                                 &\n"
    "           arg_type(gh_field*3, gh_real, gh_read, w2, "
    "mesh_arg=gh_fine),                                                   &\n"
    "           arg_type(gh_operator, gh_real, gh_read, w2, w3),          &\n"
    "           arg_type(gh_columnwise_operator, gh_real, gh_read, w3, "
    "w0)                                                                  &\n"
    "         /)\n"
    "   type(func_type), dimension(2) :: meta_funcs =                     &\n"
    "        (/ func_type(w1, gh_basis),                                  &\n"
    "           func_type(w2, gh_basis, gh_diff_basis)                    &\n"
    "        /)\n"
    "   type(reference_element_data_type), dimension(2) ::                &\n"
    "     meta_reference_element =                                        &\n"
    "        (/ reference_element_data_type(normals_to_horizontal_faces), &\n"
    "           reference_element_data_type(normals_to_vertical_faces)    &\n"
    "        /)\n"
    "   type(mesh_data_type) :: meta_mesh(1) =                            &\n"
    "        (/ mesh_data_type(adjacent_face) /)\n"
    "   integer :: gh_shape = gh_quadrature_XYoZ\n"
    "   integer :: gh_evaluator_targets(2) = (/ w0, w3 /)\n"
    "   integer :: operates_on = cell_column\n"
    " contains\n"
    "   procedure, nopass :: code => testkern_code\n"
    "end type testkern_type\n")

PROGRAM = (
    f"module dummy\n"
    f"{METADATA}"
    f"contains\n"
    f"  subroutine kern()\n"
    f"  end subroutine kern\n"
    f"end module dummy\n")


def test_create_from_psyir(fortran_reader):
    '''Test that an instance of the LFRicKernelMetadata class can be
    created from PSyIR.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    symbol = kernel_psyir.children[0].symbol_table.lookup("testkern_type")
    metadata = LFRicKernelMetadata.create_from_psyir(symbol)

    assert metadata.operates_on == "cell_column"
    assert metadata.shapes == ["gh_quadrature_xyoz"]
    assert metadata.evaluator_targets == ["w0", "w3"]

    assert isinstance(metadata.meta_args, list)
    assert len(metadata.meta_args) == 7
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert isinstance(metadata.meta_args[1], FieldArgMetadata)
    assert isinstance(metadata.meta_args[2], FieldVectorArgMetadata)
    assert isinstance(metadata.meta_args[3], InterGridArgMetadata)
    assert isinstance(metadata.meta_args[4], InterGridVectorArgMetadata)
    assert isinstance(metadata.meta_args[5], OperatorArgMetadata)
    assert isinstance(metadata.meta_args[6], ColumnwiseOperatorArgMetadata)

    assert isinstance(metadata.meta_funcs, list)
    assert isinstance(metadata.meta_funcs[0], MetaFuncsArgMetadata)
    assert isinstance(metadata.meta_funcs[1], MetaFuncsArgMetadata)

    assert isinstance(metadata.meta_ref_element, list)
    assert len(metadata.meta_ref_element) == 2
    assert isinstance(metadata.meta_ref_element[0], MetaRefElementArgMetadata)
    assert isinstance(metadata.meta_ref_element[1], MetaRefElementArgMetadata)

    assert isinstance(metadata.meta_mesh, list)
    assert len(metadata.meta_mesh) == 1
    assert isinstance(metadata.meta_mesh[0], MetaMeshArgMetadata)

    assert metadata.procedure_name == "testkern_code"
    assert metadata.name == "testkern_type"


def test_create_from_psyir_error():
    '''Test that create_from_psyir raises the expected exceptions.'''

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata.create_from_psyir(None)
    assert ("Expected a DataTypeSymbol but found a NoneType."
            in str(info.value))

    with pytest.raises(InternalError) as info:
        _ = LFRicKernelMetadata.create_from_psyir(
            DataTypeSymbol("x", REAL_TYPE))
    assert ("Expected kernel metadata to be stored in the PSyIR as an "
            "UnknownFortranType, but found ScalarType." in str(info.value))


@pytest.mark.parametrize("procedure_format", ["", "code =>"])
def test_create_from_fparser2(procedure_format):
    '''Test that an instance of the LFRicKernelMetadata class can be
    created from Fortran encoded as an fparser2 tree. Test with all
    optional metadata. Test using standard and alternative (no 'code
    =>') format for procedure metadata.

    '''
    fortran_metadata = METADATA.replace(procedure_format, "")
    fparser2_tree = LFRicKernelMetadata.create_fparser2(
        fortran_metadata, Fortran2003.Derived_Type_Def)
    metadata = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    assert isinstance(metadata, LFRicKernelMetadata)
    assert metadata.operates_on == "cell_column"
    assert metadata.shapes == ["gh_quadrature_xyoz"]
    assert metadata.evaluator_targets == ["w0", "w3"]

    assert isinstance(metadata.meta_args, list)
    assert len(metadata.meta_args) == 7
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert isinstance(metadata.meta_args[1], FieldArgMetadata)
    assert isinstance(metadata.meta_args[2], FieldVectorArgMetadata)
    assert isinstance(metadata.meta_args[3], InterGridArgMetadata)
    assert isinstance(metadata.meta_args[4], InterGridVectorArgMetadata)
    assert isinstance(metadata.meta_args[5], OperatorArgMetadata)
    assert isinstance(metadata.meta_args[6], ColumnwiseOperatorArgMetadata)
    assert isinstance(metadata.meta_funcs, list)
    assert isinstance(metadata.meta_funcs[0], MetaFuncsArgMetadata)
    assert isinstance(metadata.meta_funcs[1], MetaFuncsArgMetadata)

    assert isinstance(metadata.meta_ref_element, list)
    assert len(metadata.meta_ref_element) == 2
    assert isinstance(metadata.meta_ref_element[0], MetaRefElementArgMetadata)
    assert isinstance(metadata.meta_ref_element[1], MetaRefElementArgMetadata)

    assert isinstance(metadata.meta_mesh, list)
    assert len(metadata.meta_mesh) == 1
    assert isinstance(metadata.meta_mesh[0], MetaMeshArgMetadata)

    assert metadata.procedure_name == "testkern_code"
    assert metadata.name == "testkern_type"


def test_create_from_fparser2_no_optional():
    '''Test that an instance of the LFRicKernelMetadata class can be
    created from Fortran encoded as an fparser2 tree. Test with no
    optional metadata.

    '''
    metadata = (
        "type, extends(kernel_type) :: testkern_type\n"
        "   type(arg_type), dimension(1) :: meta_args =       &\n"
        "        (/ arg_type(gh_scalar,   gh_real, gh_read) /)\n"
        "   integer :: operates_on = cell_column\n"
        " contains\n"
        "   procedure, nopass :: code => testkern_code\n"
        "end type testkern_type\n")

    fparser2_tree = LFRicKernelMetadata.create_fparser2(
        metadata, Fortran2003.Derived_Type_Def)
    metadata = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    assert metadata.shapes is None
    assert metadata.evaluator_targets is None
    assert metadata.meta_funcs is None


def test_create_from_fparser2_error():
    '''Test that the expected exceptions are raised when invalid input is
    provided to the create_from_fparser2 method.

    '''
    # from check_fparser2 method
    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Derived_Type_Def object but found type 'str' with value "
            "'hello'." in str(info.value))

    # invalid metadata declaration
    metadata = (
        "type, extends(kernel_type) :: testkern_type\n"
        "   integer :: invalid_var = invalid_value\n"
        "end type testkern_type\n")
    fparser2_tree = LFRicKernelMetadata.create_fparser2(
        metadata, Fortran2003.Derived_Type_Def)
    with pytest.raises(ParseError) as info:
        _ = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    assert ("Found unexpected metadata declaration 'INTEGER :: invalid_var = "
            "invalid_value' in 'TYPE, EXTENDS(kernel_type) :: testkern_type\n"
            "  INTEGER :: invalid_var = invalid_value\nEND TYPE "
            "testkern_type'." in str(info.value))

    # no 'extends(kernel_type)'
    fparser2_tree = LFRicKernelMetadata.create_fparser2(METADATA.replace(
        ", extends(kernel_type)", ""), Fortran2003.Derived_Type_Def)
    with pytest.raises(ParseError) as info:
        _ = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    assert ("The metadata type declaration should extend kernel_type, but "
            "found 'TYPE :: testkern_type' in TYPE :: "
            "testkern_type\n  TYPE(arg_type), DIMENSION(7)" in str(info.value))

    # metadata type extends incorrect type
    fparser2_tree = LFRicKernelMetadata.create_fparser2(METADATA.replace(
        "kernel_type", "invalid_type"), Fortran2003.Derived_Type_Def)
    with pytest.raises(ParseError) as info:
        _ = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    print(str(info.value))
    assert ("The metadata type declaration should extend kernel_type, but "
            "found 'TYPE, EXTENDS(invalid_type) :: testkern_type' in TYPE, "
            "EXTENDS(invalid_type) :: testkern_type\n  TYPE(arg_type), "
            "DIMENSION(7)" in str(info.value))


def test_lower_to_psyir():
    '''Test that the metadata can be lowered to an UnknownFortranType
    symbol.

    '''
    metadata = LFRicKernelMetadata.create_from_fortran_string(METADATA)
    symbol = metadata.lower_to_psyir()
    assert isinstance(symbol, DataTypeSymbol)
    assert symbol.name == metadata.name
    assert isinstance(symbol.datatype, UnknownFortranType)
    assert symbol.datatype.declaration == metadata.fortran_string()


def test_get_procedure_name_error(fortran_reader):
    '''Test that all the exceptions are raised as expected in the
    _get_procedure_name method.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM.replace(
        "procedure, nopass :: code => testkern_code", ""))
    datatype = kernel_psyir.children[0].symbol_table.lookup(
        "testkern_type").datatype
    metadata = LFRicKernelMetadata()
    reader = FortranStringReader(datatype.declaration)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    with pytest.raises(ParseError) as info:
        metadata._get_procedure_name(spec_part)
    assert "Expecting a type-bound procedure, but found" in str(info.value)

    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    datatype = kernel_psyir.children[0].symbol_table.lookup(
        "testkern_type").datatype
    metadata = LFRicKernelMetadata()
    reader = FortranStringReader(datatype.declaration)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    binding = spec_part.children[2]
    binding.children[1] = binding.children[0]
    with pytest.raises(ParseError) as info:
        metadata._get_procedure_name(spec_part)
    assert ("Expecting a specific binding for the type-bound procedure, "
            "but found" in str(info.value))

    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM.replace(
        "code", "hode"))
    datatype = kernel_psyir.children[0].symbol_table.lookup(
        "testkern_type").datatype
    metadata = LFRicKernelMetadata()
    reader = FortranStringReader(datatype.declaration)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    with pytest.raises(ParseError) as info:
        metadata._get_procedure_name(spec_part)
    assert ("Expecting the type-bound procedure binding-name to be 'code' "
            "if there is a procedure name, but found 'hode'"
            in str(info.value))


def test_get_procedure_name(fortran_reader):
    '''Test utility function that takes metadata in an fparser2 tree and
    returns the procedure metadata name, or None is there is no
    procedure name.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    datatype = kernel_psyir.children[0].symbol_table.lookup(
        "testkern_type").datatype
    metadata = LFRicKernelMetadata()
    reader = FortranStringReader(datatype.declaration)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    assert metadata._get_procedure_name(spec_part) == \
        "testkern_code"

    # No procedure name
    del spec_part.children[2]
    result = metadata._get_procedure_name(spec_part)
    assert result is None


def test_fortran_string():
    '''Test that the metadata can be written out as a fortran string and
    that it raises the expected exception if there is an error.

    '''
    instance = LFRicKernelMetadata()
    with pytest.raises(ValueError) as info:
        instance.fortran_string()
    assert ("Values for operates_on, meta_args, procedure_name and name must "
            "be provided before calling the fortran_string method, but found "
            "'None', 'None', 'None' and 'None' respectively."
            in str(info.value))

    metadata = LFRicKernelMetadata.create_from_fortran_string(METADATA)
    result = metadata.fortran_string()
    expected = (
        "TYPE, PUBLIC, EXTENDS(kernel_type) :: testkern_type\n"
        "  type(ARG_TYPE) :: META_ARGS(7) = (/ &\n"
        "    arg_type(gh_scalar, gh_real, gh_read), &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w1), &\n"
        "    arg_type(gh_field*3, gh_real, gh_read, w2), &\n"
        "    arg_type(gh_field, gh_real, gh_read, w2, mesh_arg=gh_coarse), &\n"
        "    arg_type(gh_field*3, gh_real, gh_read, w2, mesh_arg=gh_fine), &\n"
        "    arg_type(gh_operator, gh_real, gh_read, w2, w3), &\n"
        "    arg_type(gh_columnwise_operator, gh_real, gh_read, w3, w0)/)\n"
        "  type(FUNC_TYPE) :: META_FUNCS(2) = (/ &\n"
        "    func_type(w1, gh_basis), &\n"
        "    func_type(w2, gh_basis, gh_diff_basis)/)\n"
        "  type(REFERENCE_ELEMENT_DATA_TYPE) :: "
        "META_REFERENCE_ELEMENT(2) = (/ &\n"
        "    reference_element_data_type(normals_to_horizontal_faces), &\n"
        "    reference_element_data_type(normals_to_vertical_faces)/)\n"
        "  type(MESH_DATA_TYPE) :: META_MESH(1) = (/ &\n"
        "    mesh_data_type(adjacent_face)/)\n"
        "  INTEGER :: GH_SHAPE = gh_quadrature_xyoz\n"
        "  INTEGER :: GH_EVALUATOR_TARGETS(2) = (/w0, w3/)\n"
        "  INTEGER :: OPERATES_ON = cell_column\n"
        "  CONTAINS\n"
        "    PROCEDURE, NOPASS :: testkern_code\n"
        "END TYPE testkern_type\n")
    assert result == expected


def test_setter_getter_operates_on():
    '''Test that the LFRicKernelMetadata operates_on setters and getters
    work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.operates_on is None
    with pytest.raises(ValueError) as info:
        metadata.operates_on = "invalid"
    assert ("The OPERATES_ON metadata should be a recognised value "
            "(one of ['cell_column', 'domain', 'dof']) but found "
            "'invalid'." in str(info.value))
    metadata.operates_on = "DOMAIN"
    assert metadata.operates_on == "domain"


def test_setter_getter_shapes():
    '''Test that the LFRicKernelMetadata shapes setters and getters work
    as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.shapes is None
    with pytest.raises(TypeError) as info:
        metadata.shapes = "invalid"
    assert ("ShapesMetadata values should be provided as a list but found "
            "'str'." in str(info.value))
    shapes = ["gh_quadrature_face", "gh_evaluator"]
    metadata.shapes = shapes
    assert metadata.shapes == shapes


def test_setter_getter_evaluator_targets():
    '''Test that the LFRicKernelMetadata evaluator_targets setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.evaluator_targets is None
    with pytest.raises(TypeError) as info:
        metadata.evaluator_targets = "invalid"
    assert ("EvaluatorTargetsMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))
    evaluator_targets = ["w0", "w1", "w2", "w3"]
    metadata.evaluator_targets = evaluator_targets
    assert metadata.evaluator_targets == evaluator_targets


def test_setter_getter_meta_args():
    '''Test that the LFRicKernelMetadata meta_args setters and getters
    work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.meta_args is None
    with pytest.raises(TypeError) as info:
        metadata.meta_args = "error"
    assert ("MetaArgsMetadata values should be provided as a list but found "
            "'str'." in str(info.value))
    with pytest.raises(TypeError) as info:
        metadata.meta_args = []
    assert ("The MetaArgsMetadata list should contain at least one entry, "
            "but it is empty." in str(info.value))
    with pytest.raises(TypeError) as info:
        metadata.meta_args = ["error"]
    assert ("The MetaArgsMetadata list should be a list containing objects of "
            "type CommonMetaArgMetadata but found 'error', which is of type "
            "'str'." in str(info.value))

    scalar_arg = ScalarArgMetadata("GH_REAL", "GH_READ")
    meta_args = [scalar_arg]
    metadata.meta_args = meta_args
    assert metadata.meta_args == meta_args


def test_setter_getter_meta_funcs():
    '''Test that the LFRicKernelMetadata meta_funcs setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.meta_funcs is None
    with pytest.raises(TypeError) as info:
        metadata.meta_funcs = "invalid"
    assert ("MetaFuncsMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))
    meta_funcs_arg = MetaFuncsArgMetadata("w0", basis_function=True)
    meta_funcs = [meta_funcs_arg]
    metadata.meta_funcs = meta_funcs
    assert metadata.meta_funcs == meta_funcs


def test_setter_getter_meta_ref_element():
    '''Test that the LFRicKernelMetadata meta_ref_element setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.meta_ref_element is None
    with pytest.raises(TypeError) as info:
        metadata.meta_ref_element = "invalid"
    assert ("MetaRefElementMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))
    meta_ref_element_arg = MetaRefElementArgMetadata("normals_to_faces")
    meta_ref_element = [meta_ref_element_arg]
    metadata.meta_ref_element = meta_ref_element
    assert metadata.meta_ref_element == meta_ref_element


def test_setter_getter_meta_mesh():
    '''Test that the LFRicKernelMetadata meta_mesh setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.meta_mesh is None
    with pytest.raises(TypeError) as info:
        metadata.meta_mesh = "invalid"
    assert ("MetaMeshMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))
    meta_mesh_arg = MetaMeshArgMetadata("adjacent_face")
    meta_mesh = [meta_mesh_arg]
    metadata.meta_mesh = meta_mesh
    assert metadata.meta_mesh == meta_mesh


def test_setter_getter_procedure_name():
    '''Test that the LFRicKernelMetadata procedure_name setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.procedure_name is None
    with pytest.raises(ValueError) as info:
        metadata.procedure_name = "1_invalid"
    assert ("Expected procedure_name to be a valid Fortran name but found "
            "'1_invalid'." in str(info.value))
    metadata.procedure_name = "KERN_CODE"
    assert metadata.procedure_name == "KERN_CODE"


def test_setter_getter_name():
    '''Test that the LFRicKernelMetadata name setters and getters work as
    expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.name is None
    with pytest.raises(ValueError) as info:
        metadata.name = "1_invalid"
    assert ("Expected name to be a valid Fortran name but found "
            "'1_invalid'." in str(info.value))
    metadata.name = "kern_type"
    assert metadata.name == "kern_type"
