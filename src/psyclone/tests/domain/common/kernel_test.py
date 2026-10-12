# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council.
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Tests for common kernel metadata support."""

import pytest

from psyclone.domain.common.kernel import Kernel, KernelMetadata
from psyclone.domain.gocean.kernel import GOceanKernelMetadata
from psyclone.parse.utils import ParseError
from psyclone.psyir.nodes import Literal, Routine
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
        KernelMetadata.find_symbols(None)


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
            return cls.find_symbols(psyir, name)[0]

        @classmethod
        def create_from_psyir(cls, psyir, name=None):
            metadata = cls()
            metadata.name = psyir.name
            return metadata

        def resolve_procedures(self, container):
            routines = tuple(container.walk(Routine))
            return routines[0].name, routines

        @property
        def nargs(self):
            return 1

        def fortran_string(self):
            return ""

    psyir = fortran_reader.psyir_from_source(GOCEAN_SOURCE)
    kernel = Kernel.create_from_psyir(CustomMetadata, psyir, name="demo_type")
    assert isinstance(kernel.metadata, CustomMetadata)
    assert kernel.metadata.name == "demo_type"
    assert kernel.psyir is psyir
    assert kernel.procedure_name == "demo_code"
    assert kernel.procedures == tuple(psyir.walk(Routine))
