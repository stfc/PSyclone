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
# Modified: S. Siso, STFC Daresbury Lab

'''Module containing tests for the KernelMetadataSymbol
kernel-layer-specific symbol. The tests include translation of
language-level PSyIR to PSyclone LFRic Kernel PSyIR and PSyclone LFRic
Kernel PSyIR to language-level PSyIR.

'''
import pytest

from psyclone.domain.lfric.kernel import (
    LFRicKernelMetadata, LFRicKernelContainer)
from psyclone.domain.lfric.transformations.raise_psyir_2_lfric_kern_trans \
    import RaisePSyIR2LFRicKernTrans
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import SymbolTable

METADATA = (
    "type, public, extends(kernel_type) :: w3_solver_kernel_type\n"
    "    private\n"
    "    type(arg_type) :: meta_args(4) = (/                           &\n"
    "        arg_type(GH_FIELD,   GH_REAL,    GH_WRITE, W3),           &\n"
    "        arg_type(GH_FIELD,   GH_REAL,    GH_READ,      W3),       &\n"
    "        arg_type(GH_FIELD*3, GH_REAL,    GH_READ,      Wchi),     &\n"
    "        arg_type(GH_FIELD,   GH_INTEGER, GH_READ,"
    "      ANY_DISCONTINUOUS_SPACE_3) &\n"
    "        /)\n"
    "    type(func_type) :: meta_funcs(2) = (/                         &\n"
    "        func_type(W3,   GH_BASIS),                                &\n"
    "        func_type(Wchi, GH_BASIS, GH_DIFF_BASIS)                  &\n"
    "        /)\n"
    "    integer :: operates_on = CELL_COLUMN\n"
    "    integer :: gh_shape = GH_QUADRATURE_XYoZ\n"
    "  contains\n"
    "    procedure, nopass :: solver_w3_code\n"
    "  end type\n")

PROGRAM = (
    f"module dummy\n"
    f"{METADATA}"
    f"contains\n"
    f"  subroutine kern()\n"
    f"  end subroutine kern\n"
    f"end module dummy\n")


# Class LFRicKernelContainer

def test_lfrickernelcontainer_init():
    '''Test that an instance of LFRicKernelContainer can be created. '''
    container = LFRicKernelContainer("name", None)
    assert container.name == "name"
    assert container._metadata is None


def test_lfrickernelcontainer_create():
    '''Test that the LFRicKernelContainer create method works as
    expected. Includes raising any exceptions. Also make use of the
    metadata property.

    '''
    metadata = LFRicKernelMetadata.create_from_fortran_string(METADATA)
    container = LFRicKernelContainer.create(
        "name", metadata, SymbolTable(), [])
    assert container.name == "name"
    expected = (
        "TYPE, PUBLIC, EXTENDS(kernel_type) :: w3_solver_kernel_type\n"
        "  type(ARG_TYPE) :: META_ARGS(4) = (/ &\n"
        "    arg_type(gh_field, gh_real, gh_write, w3), &\n"
        "    arg_type(gh_field, gh_real, gh_read, w3), &\n"
        "    arg_type(gh_field*3, gh_real, gh_read, wchi), &\n"
        "    arg_type(gh_field, gh_integer, gh_read, "
        "any_discontinuous_space_3)/)\n"
        "  type(FUNC_TYPE) :: META_FUNCS(2) = (/ &\n"
        "    func_type(w3, gh_basis), &\n"
        "    func_type(wchi, gh_basis, gh_diff_basis)/)\n"
        "  INTEGER :: GH_SHAPE = gh_quadrature_xyoz\n"
        "  INTEGER :: OPERATES_ON = cell_column\n"
        "  CONTAINS\n"
        "    PROCEDURE, NOPASS :: solver_w3_code\n"
        "END TYPE w3_solver_kernel_type\n")
    assert container.metadata.fortran_string() == expected
    assert container.children == []
    with pytest.raises(ValueError) as info:
        _ = LFRicKernelMetadata.create_from_fortran_string("Not valid")
    assert ("Expected kernel metadata to be a Fortran Derived_Type_Def, but "
            "found 'Not valid'." in str(str(info.value)))


def test_lfrickernelcontainer_lower(fortran_reader):
    '''Test that the LFRicKernelContainer lower_to_language_level method
    works as expected.

    '''
    # First load program and perform checks
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    assert isinstance(kernel_psyir.children[0], Container)
    assert not isinstance(kernel_psyir.children[0], LFRicKernelContainer)
    assert kernel_psyir.children[0].symbol_table.lookup(
        "w3_solver_kernel_type")

    # Now raise to LFRic PSyIR and perform checks
    kern_trans = RaisePSyIR2LFRicKernTrans()
    kern_trans.apply(kernel_psyir, {"metadata_name": "w3_solver_kernel_type"})
    assert isinstance(kernel_psyir.children[0], LFRicKernelContainer)
    with pytest.raises(KeyError):
        kernel_psyir.children[0].symbol_table.lookup("w3_solver_kernel_type")

    # Now use lower_to_language_level and perform checks
    container = kernel_psyir.children[0]
    lowered = container.lower_to_language_level()
    assert lowered is kernel_psyir.children[0]
    assert isinstance(kernel_psyir.children[0], Container)
    assert not isinstance(kernel_psyir.children[0], LFRicKernelContainer)
    assert kernel_psyir.children[0].symbol_table.lookup(
        "w3_solver_kernel_type")
