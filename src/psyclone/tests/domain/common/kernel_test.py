# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council.
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Tests for common kernel metadata and source support."""

import pytest

from psyclone.domain.common.kernel import (
    find_kernel_file, KernelInfo, parse_fortran_source)
from psyclone.domain.gocean.kernel import GOceanKernelMetadata
from psyclone.parse.utils import ParseError


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


def test_kernel_info_from_source():
    """The common container delegates language parsing to the API model."""
    kernel = KernelInfo.create_from_source(
        GOceanKernelMetadata, GOCEAN_SOURCE)
    assert kernel.metadata.name == "demo_type"
    assert kernel.procedure_name == "demo_code"
    assert [routine.name for routine in kernel.procedures] == ["demo_code"]

    with pytest.raises(TypeError, match="must implement KernelMetadata"):
        KernelInfo(object())
    with pytest.raises(TypeError, match="tuple of Routines"):
        KernelInfo(kernel.metadata, procedures=(object(),))


def test_kernel_source_helpers(tmp_path):
    """Source discovery is case-insensitive and reports ambiguity."""
    first = tmp_path / "one"
    first.mkdir()
    kernel_file = first / "demo_mod.F90"
    kernel_file.write_text(GOCEAN_SOURCE, encoding="utf-8")

    assert find_kernel_file("Demo_Mod", [str(first)]) == str(kernel_file)
    assert parse_fortran_source(GOCEAN_SOURCE)
    with pytest.raises(TypeError, match="supplied as a string"):
        parse_fortran_source(None)

    second = tmp_path / "two"
    second.mkdir()
    (second / "DEMO_MOD.f90").write_text(GOCEAN_SOURCE, encoding="utf-8")
    with pytest.raises(ParseError, match="More than one match"):
        find_kernel_file("demo_mod", [str(tmp_path)])
