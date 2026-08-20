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
from psyclone.domain.gocean.kernel import (
    GOceanContainer, GOceanFieldArgMetadata,
    GOceanGridPropertyArgMetadata, GOceanKernelMetadata,
    GOceanScalarArgMetadata, GOceanStencilMetadata)
from psyclone.domain.gocean.kernel import metadata as metadata_mod
from psyclone.domain.gocean.transformations import RaisePSyIR2GOceanKernTrans
from psyclone.errors import GenerationError
from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import (
    DataTypeSymbol, ScalarType, StructureType)


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

MODULE = f"""\
module dummy
  {METADATA}
contains
  subroutine compute_cu_code()
  end subroutine compute_cu_code
end module dummy
"""


def _expression(source):
    """Create a PSyIR expression for node-level metadata tests."""
    return FortranReader().psyir_from_expression(source)


def test_metadata_from_psyir(fortran_reader):
    """Metadata is extracted from a complete language-level PSyIR tree."""
    root = fortran_reader.psyir_from_source(MODULE)
    kernel = GOceanKernelMetadata.create_from_kernel_psyir(
        root, "compute_cu")
    metadata = kernel.metadata

    assert metadata.name == "compute_cu"
    assert metadata.procedure_name == "compute_cu_code"
    assert metadata.iterates_over == "go_all_pts"
    assert metadata.index_offset == "go_offset_sw"
    assert isinstance(metadata.meta_args[0], GOceanFieldArgMetadata)
    assert metadata.meta_args[1].stencil.rows == ("000", "011", "000")
    assert isinstance(metadata.meta_args[2], GOceanGridPropertyArgMetadata)
    assert isinstance(metadata.meta_args[3], GOceanScalarArgMetadata)
    assert kernel.procedures[0].name == "compute_cu_code"
    with pytest.raises(FrozenInstanceError):
        metadata.name = "changed"


def test_metadata_fortran_round_trip(fortran_reader):
    """Generated metadata remains valid Fortran and preserves its values."""
    source = f"module metadata_mod\n{METADATA}\nend module metadata_mod\n"
    metadata = GOceanKernelMetadata.create_from_psyir(
        fortran_reader.psyir_from_source(source))
    regenerated_source = (
        "module metadata_mod\n"
        f"{metadata.fortran_string()}"
        "end module metadata_mod\n")
    regenerated = GOceanKernelMetadata.create_from_psyir(
        fortran_reader.psyir_from_source(regenerated_source))
    assert regenerated == metadata

    root = fortran_reader.psyir_from_source(MODULE)
    RaisePSyIR2GOceanKernTrans("compute_cu").apply(root)
    assert isinstance(root.children[0], GOceanContainer)
    root.children[0].lower_to_language_level()
    assert isinstance(root.children[0], Container)
    assert root.children[0].symbol_table.lookup("compute_cu")


def test_kernel_info_path():
    """Test the common kernel-information loading path."""
    Config.get().api = "gocean"
    mdata_code = MODULE
    kernel_metadata = FortranReader().psyir_from_source(mdata_code)
    ktype = GOceanKernelMetadata.create_from_kernel_psyir(
        kernel_metadata, name="compute_cu").metadata

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


def test_argument_access_type():
    """Typed arguments expose their generic access without a descriptor."""
    argument = GOceanFieldArgMetadata("go_read", "go_cu")
    assert str(argument.access_type) == "READ"


def test_kernel_metadata_validation():
    """Test top-level metadata normalisation and argument constraints."""
    field = GOceanFieldArgMetadata("go_write", "go_cu")
    metadata = GOceanKernelMetadata(
        "GO_ALL_PTS", "GO_OFFSET_SW", [field], "CODE", "KERNEL")
    assert metadata.iterates_over == "go_all_pts"
    assert metadata.meta_args == (field,)
    assert metadata.procedure_name == "code"
    with pytest.raises(TypeError, match="meta_args entries"):
        GOceanKernelMetadata(
            "go_all_pts", "go_offset_sw", [object()], "code", "kernel")
    with pytest.raises(ParseError, match="does not have any field"):
        GOceanKernelMetadata(
            "go_all_pts", "go_offset_sw",
            [GOceanGridPropertyArgMetadata("go_read", "go_grid_area_t")],
            "code", "kernel")


def test_kernel_metadata_common_properties():
    """Test kernel information, counts, lowering and string output."""
    kernel = GOceanKernelMetadata.create_from_kernel_psyir(
        FortranReader().psyir_from_source(MODULE), "compute_cu")
    metadata = kernel.metadata
    assert kernel.procedures[0].name == "compute_cu_code"
    assert isinstance(metadata.meta_args[2], GOceanGridPropertyArgMetadata)
    assert metadata.meta_args[2].name == "go_grid_area_t"
    assert metadata.meta_args[3].datatype == "go_r_scalar"
    assert metadata.nargs == 3
    lowered = metadata.lower_to_psyir()
    assert lowered.name == "compute_cu"
    assert isinstance(lowered.datatype, StructureType)
    assert str(metadata) == (
        "GOcean kernel compute_cu, index-offset = go_offset_sw, "
        "iterates-over = go_all_pts")


def test_create_from_psyir_errors():
    """Test error handling when constructing metadata from a PSyIR symbol."""
    with pytest.raises(TypeError, match="Expected a DataTypeSymbol"):
        GOceanKernelMetadata.create_from_psyir("not a symbol")
    symbol = DataTypeSymbol("bad", ScalarType.real_type())
    with pytest.raises(TypeError, match="StructureType"):
        GOceanKernelMetadata.create_from_psyir(symbol)

    declaration = (
        "type, extends(kernel_type) :: bad\n"
        "type(go_arg), dimension(1) :: meta_args = "
        "(/go_arg(invalid, go_cu, go_pointwise)/)\n"
        "integer :: iterates_over = go_all_pts\n"
        "integer :: index_offset = go_offset_sw\n"
        "contains\nprocedure, nopass :: code => bad_code\nend type bad")
    root = FortranReader().psyir_from_source(
        f"module bad_mod\n{declaration}\nend module bad_mod\n")
    bad_symbol = root.walk(Container)[1].symbol_table.lookup("bad")
    assert isinstance(bad_symbol.datatype, StructureType)
    with pytest.raises(ParseError, match="Invalid GOcean metadata"):
        GOceanKernelMetadata.create_from_psyir(bad_symbol)


def test_create_from_kernel_psyir_missing_routine():
    """Test that the implementation named by metadata must exist."""
    source = MODULE.replace(
        "subroutine compute_cu_code()", "subroutine different_code()").replace(
            "end subroutine compute_cu_code",
            "end subroutine different_code")
    root = FortranReader().psyir_from_source(source)
    with pytest.raises(ParseError, match="compute_cu_code.*not found"):
        GOceanKernelMetadata.create_from_kernel_psyir(root, "compute_cu")


def test_create_from_psyir_discovery_errors(fortran_reader):
    """Test invalid metadata discovery from complete PSyIR."""
    no_metadata = fortran_reader.psyir_from_source(
        "module metadata_mod\ninteger :: value\nend module metadata_mod")
    with pytest.raises(ParseError, match="exactly one"):
        GOceanKernelMetadata.create_from_psyir(no_metadata)

    two_metadata = fortran_reader.psyir_from_source(
        "module metadata_mod\n" + METADATA + METADATA.replace(
            "compute_cu", "compute_cv") + "\nend module metadata_mod")
    with pytest.raises(ParseError, match="exactly one"):
        GOceanKernelMetadata.create_from_psyir(two_metadata)


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
        metadata_mod._parse_meta_arg(_expression(expression))


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
            _expression(expression)), expected)


@pytest.mark.parametrize("declaration, message", [
    ("type :: bad\nend type bad", "must extend kernel_type"),
    ("type, extends(kernel_type) :: bad\nend type bad",
     "Missing GOcean metadata"),
    ("""type, extends(kernel_type) :: bad
      type(go_arg) :: meta_args = go_arg(go_read, go_cu, go_pointwise)
      integer :: iterates_over = go_all_pts
      integer :: index_offset = go_offset_sw
      contains
      procedure, nopass :: code => bad_code
      end type bad""", "must be an array constructor"),
    ("""type, extends(kernel_type) :: bad
      type(go_arg) :: meta_args = (/go_arg(go_read, go_cu, go_pointwise)/)
      integer :: iterates_over = go_all_pts
      integer :: index_offset = go_offset_sw
      contains
      procedure, nopass :: code => bad_code
      end type bad""", "must be an array"),
    ("type, extends(kernel_type) :: bad\n"
     "type(go_arg), dimension(2) :: meta_args = "
     "(/go_arg(go_read, go_cu, go_pointwise)/)\n"
     "integer :: iterates_over = go_all_pts\n"
     "integer :: index_offset = go_offset_sw\n"
     "contains\n"
     "procedure, nopass :: code => bad_code\nend type bad", "extent 2"),
    ("type, extends(kernel_type) :: bad\n"
     "type(go_arg), dimension(1) :: meta_args = "
     "(/go_arg(go_read, go_cu, go_pointwise)/)\n"
     "integer :: iterates_over = go_all_pts\n"
     "integer :: index_offset = go_offset_sw\nend type bad",
     "must bind a kernel procedure"),
])
def test_declaration_errors(declaration, message):
    """Test structural validation of GOcean metadata StructureTypes."""
    root = FortranReader().psyir_from_source(
        f"module bad_mod\n{declaration}\nend module bad_mod\n")
    symbol = root.walk(Container)[1].symbol_table.lookup("bad")
    with pytest.raises(ParseError, match=message):
        GOceanKernelMetadata.create_from_psyir(symbol)


def test_create_from_kernel_psyir_discovery_errors():
    """Test type, absence and ambiguity errors in metadata discovery."""
    with pytest.raises(TypeError, match="Expected PSyIR"):
        GOceanKernelMetadata.create_from_kernel_psyir("not psyir")
    reader = FortranReader()
    root = reader.psyir_from_source("module empty\nend module empty")
    with pytest.raises(ParseError, match="does not exist"):
        GOceanKernelMetadata.create_from_kernel_psyir(root)
    root = reader.psyir_from_source(
        "module both\n" + METADATA + METADATA.replace(
            "compute_cu", "compute_cv") + "\nend module both")
    with pytest.raises(ParseError, match="not unique"):
        GOceanKernelMetadata.create_from_kernel_psyir(root)
    with pytest.raises(ParseError, match="'absent'.*does not exist"):
        GOceanKernelMetadata.create_from_kernel_psyir(root, "absent")
