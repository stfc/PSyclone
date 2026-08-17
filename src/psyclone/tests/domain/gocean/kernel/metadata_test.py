# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Tests for GOcean metadata implementation."""

from dataclasses import FrozenInstanceError

import pytest

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.gocean.kernel import (
    GOceanArgDescriptor, GOceanContainer, GOceanFieldArgMetadata,
    GOceanGridPropertyArgMetadata, GOceanKernelMetadata,
    GOceanScalarArgMetadata, GOceanStencilMetadata, find_metadata_symbol)
from psyclone.domain.gocean.kernel import metadata as metadata_mod
from psyclone.domain.gocean.transformations import RaisePSyIR2GOceanKernTrans
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.kernel import get_kernel_psyir, KernelTypeFactory
from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import (
    DataTypeSymbol, ScalarType, UnsupportedFortranType)


METADATA = """\
type, extends(kernel_type) :: compute_cu
  type(go_arg), dimension(4) :: meta_args = (/ &
    go_arg(go_write, go_cu, go_pointwise), &
    go_arg(go_read, go_ct, go_stencil(000, 011, 000)), &
    go_arg(go_read, go_grid_area_t), &
    go_arg(go_read, go_r_scalar, go_pointwise) /)
  integer :: iterates_over = go_all_pts
  integer :: index_offset = go_offset_sw
contains
  procedure, nopass :: code => compute_cu_code
end type compute_cu
"""

PROGRAM = f"""\
module dummy
  {METADATA}
contains
  subroutine compute_cu_code()
  end subroutine compute_cu_code
end module dummy
"""


def test_metadata_from_psyir(fortran_reader):
    """Metadata is extracted from a complete language-level PSyIR tree."""
    root = fortran_reader.psyir_from_source(PROGRAM)
    metadata = GOceanKernelMetadata.create_from_kernel_psyir(
        root, "compute_cu")

    assert metadata.name == "compute_cu"
    assert metadata.procedure_name == "compute_cu_code"
    assert metadata.iterates_over == "go_all_pts"
    assert metadata.index_offset == "go_offset_sw"
    assert isinstance(metadata.meta_args[0], GOceanFieldArgMetadata)
    assert metadata.meta_args[1].stencil.rows == ("000", "011", "000")
    assert isinstance(metadata.meta_args[2], GOceanGridPropertyArgMetadata)
    assert isinstance(metadata.meta_args[3], GOceanScalarArgMetadata)
    assert metadata.procedure.ast.name == "compute_cu_code"
    with pytest.raises(FrozenInstanceError):
        metadata.name = "changed"


def test_metadata_fortran_round_trip(fortran_reader):
    """Generated metadata remains valid Fortran and preserves its values."""
    metadata = GOceanKernelMetadata.create_from_fortran_string(METADATA)
    regenerated = GOceanKernelMetadata.create_from_fortran_string(
        metadata.fortran_string())
    assert regenerated == metadata

    root = fortran_reader.psyir_from_source(PROGRAM)
    RaisePSyIR2GOceanKernTrans("compute_cu").apply(root)
    assert isinstance(root.children[0], GOceanContainer)
    root.children[0].lower_to_language_level()
    assert isinstance(root.children[0], Container)
    assert root.children[0].symbol_table.lookup("compute_cu")


def test_kernel_type_factory_path():
    """Test the standard parser and factory path requested by clients."""
    Config.get().api = "gocean"
    mdata_code = PROGRAM
    kernel_metadata = get_kernel_psyir(mdata_code)
    ktype = KernelTypeFactory(api="gocean").create(
        kernel_metadata, name="compute_cu")

    assert isinstance(ktype, GOceanKernelMetadata)
    assert ktype.name == "compute_cu"
    assert ktype.nargs == 3


def test_stencil_metadata():
    """Test pointwise and explicit stencils, including invalid inputs."""
    pointwise = GOceanStencilMetadata()
    assert not pointwise.has_stencil
    assert pointwise.name == "go_pointwise"
    assert pointwise.depth(0, 0) == 1
    assert pointwise.depth(1, 0) == 0

    stencil = GOceanStencilMetadata(("012", "345", "678"))
    assert stencil.has_stencil
    assert stencil.name is None
    assert stencil.depth(-1, 1) == 0
    assert stencil.depth(1, -1) == 8
    with pytest.raises(GenerationError, match="between -1 and 1"):
        stencil.depth(2, 0)
    for rows, error, message in (
            (("000",), ValueError, "exactly three"),
            (("000", 10, "001"), TypeError, "must be strings"),
            (("000", "01x", "001"), ValueError, "decimal depths"),
            (("000", "000", "000"), ValueError, "go_pointwise"),
            (("000", "010", "000"), ValueError, "go_pointwise")):
        with pytest.raises(error, match=message):
            GOceanStencilMetadata(rows)


def test_argument_metadata_validation():
    """Test GOcean argument metadata construction and serialisation."""
    field = GOceanFieldArgMetadata("GO_READ", "GO_CU")
    assert field.form == "go_pointwise"
    assert field.fortran_string() == (
        "go_arg(go_read, go_cu, go_pointwise)")
    stencil = GOceanStencilMetadata(("000", "011", "000"))
    field = GOceanFieldArgMetadata("go_write", "go_ct", stencil)
    assert field.form == "go_stencil"
    assert field.fortran_string() == (
        "go_arg(go_write, go_ct, go_stencil(000, 011, 000))")
    with pytest.raises(TypeError, match="Field stencil"):
        GOceanFieldArgMetadata("go_read", "go_cu", "go_pointwise")
    with pytest.raises(TypeError, match="field access.*'int'"):
        GOceanFieldArgMetadata(1, "go_cu")
    with pytest.raises(ValueError, match="field grid-point type"):
        GOceanFieldArgMetadata("go_read", "invalid")

    scalar = GOceanScalarArgMetadata(
        "GO_READ", "GO_R_SCALAR", "GO_POINTWISE")
    assert scalar.fortran_string() == (
        "go_arg(go_read, go_r_scalar, go_pointwise)")
    grid = GOceanGridPropertyArgMetadata("GO_READ", "GO_GRID_AREA_T")
    assert grid.fortran_string() == "go_arg(go_read, go_grid_area_t)"
    with pytest.raises(ValueError, match="grid-property name"):
        GOceanGridPropertyArgMetadata("go_read", "invalid")


def test_argument_descriptor():
    """Test validation and representation of legacy argument descriptors."""
    descriptor = GOceanArgDescriptor(
        AccessType.READ, "go_cu", 2, GOceanStencilMetadata(), "field")
    assert repr(descriptor) == "Descriptor(READ, go_cu, 2)"
    assert str(descriptor) == repr(descriptor)
    with pytest.raises(TypeError, match="access must be an AccessType"):
        GOceanArgDescriptor(
            "go_read", "go_cu", 0, GOceanStencilMetadata(), "field")
    for index in (-1, "0"):
        with pytest.raises(InternalError, match="metadata index"):
            GOceanArgDescriptor(
                AccessType.READ, "go_cu", index,
                GOceanStencilMetadata(), "field")


def test_kernel_metadata_validation():
    """Test top-level metadata normalisation and argument constraints."""
    field = GOceanFieldArgMetadata("go_write", "go_cu")
    metadata = GOceanKernelMetadata(
        "GO_ALL_PTS", "GO_OFFSET_SW", [field], "CODE", "KERNEL")
    assert metadata.iterates_over == "go_all_pts"
    assert metadata.meta_args == (field,)
    assert metadata.procedure.name == "code"
    assert metadata.procedure.ast is None
    assert metadata._ast is None
    with pytest.raises(TypeError, match="meta_args entries"):
        GOceanKernelMetadata(
            "go_all_pts", "go_offset_sw", [object()], "code", "kernel")
    with pytest.raises(ParseError, match="does not have any field"):
        GOceanKernelMetadata(
            "go_all_pts", "go_offset_sw",
            [GOceanGridPropertyArgMetadata("go_read", "go_grid_area_t")],
            "code", "kernel")


def test_kernel_metadata_compatibility_properties():
    """Test procedure, descriptors, counts, lowering and string output."""
    metadata = GOceanKernelMetadata.create_from_kernel_psyir(
        FortranReader().psyir_from_source(PROGRAM), "compute_cu")
    assert metadata._ast is metadata.psyir
    assert metadata.procedure.ast.name == "compute_cu_code"
    descriptors = metadata.arg_descriptors
    assert [arg.argument_type for arg in descriptors] == [
        "field", "field", "grid_property", "scalar"]
    assert descriptors[2].grid_prop == "go_grid_area_t"
    assert descriptors[3].function_space == "go_r_scalar"
    assert metadata.nargs == 3
    assert metadata.lower_to_psyir().name == "compute_cu"
    assert str(metadata) == (
        "GOcean kernel compute_cu, index-offset = go_offset_sw, "
        "iterates-over = go_all_pts")


def test_create_from_psyir_errors():
    """Test error handling when constructing metadata from a PSyIR symbol."""
    with pytest.raises(TypeError, match="Expected a DataTypeSymbol"):
        GOceanKernelMetadata.create_from_psyir("not a symbol")
    symbol = DataTypeSymbol("bad", ScalarType.real_type())
    with pytest.raises(InternalError, match="UnsupportedFortranType"):
        GOceanKernelMetadata.create_from_psyir(symbol)

    declaration = (
        "type, extends(kernel_type) :: bad\n"
        "type(go_arg), dimension(1) :: meta_args = "
        "(/go_arg(invalid, go_cu, go_pointwise)/)\n"
        "integer :: iterates_over = go_all_pts\n"
        "integer :: index_offset = go_offset_sw\n"
        "contains\nprocedure, nopass :: code => bad_code\nend type bad")
    bad_symbol = DataTypeSymbol(
        "bad", UnsupportedFortranType(declaration))
    with pytest.raises(ParseError, match="Invalid GOcean metadata"):
        GOceanKernelMetadata.create_from_psyir(bad_symbol)


def test_create_from_kernel_psyir_missing_routine():
    """Test that the implementation named by metadata must exist."""
    source = PROGRAM.replace(
        "subroutine compute_cu_code()", "subroutine different_code()").replace(
            "end subroutine compute_cu_code",
            "end subroutine different_code")
    root = FortranReader().psyir_from_source(source)
    with pytest.raises(ParseError, match="compute_cu_code.*not found"):
        GOceanKernelMetadata.create_from_kernel_psyir(root, "compute_cu")


def test_create_from_fortran_string_errors(monkeypatch):
    """Test invalid inputs to the declaration-string constructor."""
    with pytest.raises(TypeError, match="source must be a string"):
        GOceanKernelMetadata.create_from_fortran_string(1)
    with pytest.raises(ParseError, match="exactly one"):
        GOceanKernelMetadata.create_from_fortran_string("integer :: value")
    with pytest.raises(ParseError, match="exactly one"):
        GOceanKernelMetadata.create_from_fortran_string(
            METADATA + METADATA.replace("compute_cu", "compute_cv"))

    def broken_reader(_self, _source):
        """Stand in for a frontend failure."""
        raise RuntimeError("broken frontend")

    monkeypatch.setattr(FortranReader, "psyir_from_source", broken_reader)
    with pytest.raises(ValueError, match="Expected kernel metadata"):
        GOceanKernelMetadata.create_from_fortran_string("not Fortran")


def test_parser_helpers():
    """Test low-level expression and constructor helpers."""
    assert metadata_mod._name(metadata_mod._expression("GO_CU")) == "go_cu"
    assert metadata_mod._name(metadata_mod._expression("'ABC'")) == "abc"
    assert metadata_mod._call_name(
        metadata_mod._expression("go_arg(a, b)")) == "go_arg"
    with pytest.raises(ParseError, match="Failed to parse"):
        metadata_mod._expression("(/ broken")
    with pytest.raises(ParseError, match="metadata constructor"):
        metadata_mod._call_name(metadata_mod._expression("go_cu"))
    with pytest.raises(ParseError, match="metadata name or literal"):
        metadata_mod._name(metadata_mod._expression("a + b"))
    assert metadata_mod._extent(
        "type(go_arg) :: meta_args(2)") == 2
    assert metadata_mod._extent(
        "type(go_arg), dimension(3) :: meta_args") == 3


@pytest.mark.parametrize("expression, error, message", [
    ("other(go_read, go_cu, go_pointwise)", ParseError,
     "go_arg constructor"),
    ("go_arg(go_read)", ParseError, "two or three arguments"),
    ("go_arg(go_read, go_cu, go_pointwise, extra)", ParseError,
     "two or three arguments"),
    ("go_arg(go_read, go_cu, other(000, 011, 000))", ParseError,
     "must use go_stencil"),
    ("go_arg(go_read, go_cu, invalid)", ValueError,
     "field access form"),
    ("go_arg(go_read, invalid, go_pointwise)", ParseError,
     "identify a field or scalar"),
])
def test_parse_meta_arg_errors(expression, error, message):
    """Test invalid go_arg constructor forms."""
    with pytest.raises(error, match=message):
        metadata_mod._parse_meta_arg(metadata_mod._expression(expression))


def test_parse_meta_arg_variants():
    """Test parsing grid-property, field, stencil and scalar arguments."""
    cases = [
        ("go_arg(go_read, go_grid_area_t)",
         GOceanGridPropertyArgMetadata),
        ("go_arg(go_read, go_cu, go_pointwise)",
         GOceanFieldArgMetadata),
        ("go_arg(go_read, go_cu, go_stencil(000, 011, 000))",
         GOceanFieldArgMetadata),
        ("go_arg(go_read, go_r_scalar, go_pointwise)",
         GOceanScalarArgMetadata),
    ]
    for expression, expected in cases:
        assert isinstance(metadata_mod._parse_meta_arg(
            metadata_mod._expression(expression)), expected)


@pytest.mark.parametrize("declaration, message", [
    ("type :: bad\nend type bad", "must extend kernel_type"),
    ("type, extends(kernel_type) :: bad\nend type bad",
     "Missing GOcean metadata"),
    ("""type, extends(kernel_type) :: bad
      type(go_arg) :: meta_args = go_arg(go_read, go_cu, go_pointwise)
      integer :: iterates_over = go_all_pts
      integer :: index_offset = go_offset_sw
      procedure, nopass :: code => bad_code
      end type bad""", "must be an array constructor"),
    ("""type, extends(kernel_type) :: bad
      type(go_arg) :: meta_args = (/go_arg(go_read, go_cu, go_pointwise)/)
      integer :: iterates_over = go_all_pts
      integer :: index_offset = go_offset_sw
      procedure, nopass :: code => bad_code
      end type bad""", "literal extent"),
    ("type, extends(kernel_type) :: bad\n"
     "type(go_arg), dimension(2) :: meta_args = "
     "(/go_arg(go_read, go_cu, go_pointwise)/)\n"
     "integer :: iterates_over = go_all_pts\n"
     "integer :: index_offset = go_offset_sw\n"
     "procedure, nopass :: code => bad_code\nend type bad", "extent 2"),
    ("type, extends(kernel_type) :: bad\n"
     "type(go_arg), dimension(1) :: meta_args = "
     "(/go_arg(go_read, go_cu, go_pointwise)/)\n"
     "integer :: iterates_over = go_all_pts\n"
     "integer :: index_offset = go_offset_sw\nend type bad",
     "must bind a kernel procedure"),
])
def test_declaration_errors(declaration, message):
    """Test structural validation of GOcean metadata declarations."""
    with pytest.raises(ParseError, match=message):
        metadata_mod._metadata_from_declaration("bad", declaration)


def test_find_metadata_symbol_errors():
    """Test type, absence and ambiguity errors in metadata discovery."""
    with pytest.raises(TypeError, match="Expected PSyIR"):
        find_metadata_symbol("not psyir")
    reader = FortranReader()
    root = reader.psyir_from_source("module empty\nend module empty")
    with pytest.raises(ParseError, match="does not exist"):
        find_metadata_symbol(root)
    root = reader.psyir_from_source(
        "module both\n" + METADATA + METADATA.replace(
            "compute_cu", "compute_cv") + "\nend module both")
    with pytest.raises(ParseError, match="not unique"):
        find_metadata_symbol(root)
    with pytest.raises(ParseError, match="'absent'.*does not exist"):
        find_metadata_symbol(root, "absent")
