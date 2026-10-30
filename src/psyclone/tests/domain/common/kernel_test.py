# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council.
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Tests for common kernel metadata support."""

import pytest

from psyclone.domain.common.kernel import (
    Kernel, KernelContainer, KernelMetadata)
from psyclone.domain.gocean.kernel import (
    GOceanKernelContainer, GOceanKernelMetadata)
from psyclone.domain.lfric.kernel import LFRicKernelContainer
from psyclone.parse.utils import ParseError
from psyclone.psyir.nodes import Container, FileContainer, Literal, Routine
from psyclone.psyir.symbols import ArrayType, DataSymbol, ScalarType


GOCEAN_SOURCE = """
module demo_mod
  type, extends(kernel_type) :: demo_type
    type(go_arg), dimension(1) :: meta_args = (/ &
      go_arg(go_read, go_r_scalar, go_pointwise) /)
    integer :: iterates_over = go_all_pts
    integer :: index_offset = go_offset_ne
  contains
    procedure, nopass :: code => demo_code
  end type demo_type
contains
  subroutine demo_code()
  end subroutine demo_code
end module demo_mod
"""


def _create_gocean_metadata(psyir):
    """Create GOcean metadata from the sole declaration in a PSyIR tree."""
    return GOceanKernelMetadata.create_from_psyir(
        GOceanKernelMetadata.find_symbol(psyir)[1])


@pytest.mark.parametrize("container_type", [
    KernelContainer, LFRicKernelContainer, GOceanKernelContainer])
def test_kernel_container_lifecycle(container_type, fortran_reader):
    """Shared construction and lowering preserve tree and symbol ownership."""
    source = fortran_reader.psyir_from_source(GOCEAN_SOURCE)
    metadata = _create_gocean_metadata(source)
    original = source.children[0]
    table = original.symbol_table
    children = original.pop_all_children()
    # Use the source's table without its metadata declaration, as raising does.
    table._symbols.pop("demo_type")
    container = container_type.create(
        original.name, metadata, table, children)
    assert type(container) is container_type
    assert isinstance(container, KernelContainer)
    assert container.metadata is metadata
    assert container.symbol_table is table
    assert table.node is container
    assert all(child.parent is container for child in children)
    root = FileContainer("source")
    root.addchild(container)

    lowered = container.lower_to_language_level()
    assert type(lowered) is Container
    assert root.children[0] is lowered
    assert lowered.name == "demo_mod"
    assert lowered.symbol_table is table
    assert table.node is lowered
    assert all(actual is expected for actual, expected
               in zip(lowered.children, children))
    assert all(child.parent is lowered for child in children)
    assert container.parent is None
    assert not container.children
    assert GOceanKernelMetadata.create_from_psyir(
        table.lookup("demo_type")) == metadata


def test_metadata_array_noninteger_extent():
    """A noninteger literal bound is diagnosed as invalid metadata."""
    datatype = ArrayType(ScalarType.integer_type(), [
        (Literal("1", ScalarType.integer_type()),
         Literal("2.0", ScalarType.real_type()))])
    component = DataSymbol("meta_args", datatype)
    with pytest.raises(ParseError, match="must have a literal extent") as err:
        KernelMetadata._validate_and_get_array_values(component, "meta_args")
    assert isinstance(err.value.__cause__, ValueError)


def test_find_symbols_invalid_input():
    """Metadata discovery requires a PSyIR node."""
    with pytest.raises(TypeError, match="Expected PSyIR Node.*NoneType"):
        KernelMetadata._find_symbols(None)


def test_kernel_from_psyir(fortran_reader):
    """The common container extracts metadata and routines from PSyIR."""
    psyir = fortran_reader.psyir_from_source(GOCEAN_SOURCE)
    kernel = Kernel.create_from_psyir(GOceanKernelMetadata, psyir)
    assert kernel.metadata.name == "demo_type"
    assert kernel.procedure_name == "demo_code"
    assert [routine.name for routine in kernel.procedures] == ["demo_code"]

    with pytest.raises(TypeError, match="must implement KernelMetadata"):
        Kernel(object())
    with pytest.raises(TypeError, match="tuple of Routines"):
        Kernel(kernel.metadata, procedures=(object(),))


def test_kernel_procedure_name(fortran_reader):
    """Construction stores either an explicit name or the metadata name."""
    metadata = _create_gocean_metadata(
        fortran_reader.psyir_from_source(GOCEAN_SOURCE))
    kernel = Kernel(metadata)
    assert kernel.procedure_name == "demo_code"
    assert Kernel(metadata, procedure_name="other_code").procedure_name == (
        "other_code")
    assert Kernel(metadata, procedure_name=None).procedure_name == "demo_code"
    with pytest.raises(TypeError, match="procedure name must be a string"):
        Kernel(metadata, procedure_name=123)


@pytest.mark.parametrize("metadata_type", [None, object])
def test_kernel_unsupported_metadata(metadata_type, fortran_reader):
    """Full kernel assembly requires a supported API metadata class."""
    psyir = fortran_reader.psyir_from_source(GOCEAN_SOURCE)
    with pytest.raises(TypeError, match="Expected a KernelMetadata subclass"):
        Kernel.create_from_psyir(metadata_type, psyir)


def test_kernel_custom_metadata(fortran_reader):
    """Kernel assembly supports another API through the metadata interface."""
    class CustomMetadata(KernelMetadata):
        @classmethod
        def find_symbol(cls, psyir, name=None):
            return cls._find_symbols(psyir, name)[0]

        @classmethod
        def create_from_psyir(cls, psyir, name=None):
            metadata = cls()
            metadata.name = psyir.name
            return metadata

        def resolve_procedures(self, container):
            routines = tuple(container.walk(Routine))
            return routines[0].name, routines

        @property
        def algorithm_arg_count(self):
            return 1

        def fortran_string(self):
            return ""

    metadata = CustomMetadata()
    with pytest.raises(ValueError, match="provided explicitly or"):
        Kernel(metadata)
    metadata.procedure_name = None
    with pytest.raises(ValueError, match="provided explicitly or"):
        Kernel(metadata)
    assert Kernel(metadata, procedure_name="explicit_code").procedure_name == (
        "explicit_code")
    metadata.procedure_name = "original_code"
    kernel = Kernel(metadata)
    metadata.procedure_name = "changed_code"
    assert kernel.procedure_name == "original_code"

    psyir = fortran_reader.psyir_from_source(GOCEAN_SOURCE)
    kernel = Kernel.create_from_psyir(CustomMetadata, psyir, name="demo_type")
    assert isinstance(kernel.metadata, CustomMetadata)
    assert kernel.metadata.name == "demo_type"
    assert kernel.psyir is psyir
    assert kernel.procedure_name == "demo_code"
    assert kernel.procedures == tuple(psyir.walk(Routine))
