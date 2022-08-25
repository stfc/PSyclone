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

'''Module containing tests for the KernelMetadataSymbol
kernel-layer-specific symbol. The tests include translation of
language-level PSyIR to PSyclone LFRic Kernel PSyIR and PSyclone
LFRic Kernel PSyIR to language-level PSyIR.

'''
import pytest

from psyclone.domain.lfric.kernel.psyir import LFRicContainer
from psyclone.domain.lfric.kernel.lfric_kernel_metadata import \
    LFRicKernelMetadata
from psyclone.psyir.symbols import SymbolTable
from psyclone.psyir.nodes import Container
from psyclone.domain.lfric.transformations.raise_psyir_2_lfric_kern_trans \
    import RaisePSyIR2LFRicKernTrans


METADATA = (
    "type, extends(kernel_type) :: testkern_type\n"
    "   type(arg_type), dimension(1) :: meta_args =  (/     &\n"
    "           arg_type(gh_field,    gh_real, gh_inc,  w1) &\n"
    "         /)\n"
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


def test_container_init():
    '''Test that an instance of LFRicContainer can be created. '''
    container = LFRicContainer("name", None)
    assert container.name == "name"
    assert container._metadata is None


def test_container_create():
    '''Test that the LFRicContainer create method works as
    expected. Includes raising any exceptions. Also make use of the
    metadata property.

    '''
    metadata = LFRicKernelMetadata.create_from_fortran_string(METADATA)
    container = LFRicContainer.create("name", metadata, SymbolTable(), [])
    assert container.name == "name"
    expected = (
        "TYPE, PUBLIC, EXTENDS(kernel_type) :: testkern_type\n"
        "  TYPE(arg_type) :: meta_args(1) = (/ &\n"
        "arg_type(GH_FIELD, gh_real, gh_inc, w1)/)\n"
        "  INTEGER :: OPERATES_ON = cell_column\n"
        "  CONTAINS\n"
        "    PROCEDURE, NOPASS :: testkern_code\n"
        "END TYPE testkern_type\n")
    assert container.metadata.fortran_string() == expected
    assert container.children == []
    with pytest.raises(ValueError) as info:
        _ = LFRicKernelMetadata.create_from_fortran_string("Not valid")
    assert ("Expected kernel metadata to be a Fortran derived type, but "
            "found 'Not valid'." in str(str(info.value)))


def test_container_lower(fortran_reader):
    '''Test that the LFRicContainer lower_to_language_level method works
    as expected.

    '''
    # First load program and perform checks
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    assert isinstance(kernel_psyir.children[0], Container)
    assert not isinstance(kernel_psyir.children[0], LFRicContainer)
    assert kernel_psyir.children[0].symbol_table.lookup("testkern_type")

    # Now raise to LFRic PSyIR and perform checks
    kern_trans = RaisePSyIR2LFRicKernTrans("testkern_type")
    kern_trans.apply(kernel_psyir)
    assert isinstance(kernel_psyir.children[0], LFRicContainer)
    with pytest.raises(KeyError):
        kernel_psyir.children[0].symbol_table.lookup("testkern_type")

    # Now use lower_to_language_level and perform checks
    container = kernel_psyir.children[0]
    container.lower_to_language_level()
    assert isinstance(kernel_psyir.children[0], Container)
    assert not isinstance(kernel_psyir.children[0], LFRicContainer)
    assert kernel_psyir.children[0].symbol_table.lookup("testkern_type")
