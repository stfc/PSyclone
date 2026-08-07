# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
# All rights reserved.
# -----------------------------------------------------------------------------
"""Tests for the immutable, PSyIR-first GOcean metadata implementation."""

from dataclasses import FrozenInstanceError

import pytest

from psyclone.domain.gocean.kernel import (
    GOceanContainer, GOceanFieldArgMetadata, GOceanGridPropertyArgMetadata,
    GOceanKernelMetadata, GOceanScalarArgMetadata)
from psyclone.domain.gocean.transformations import RaisePSyIR2GOceanKernTrans
from psyclone.psyir.nodes import Container


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
