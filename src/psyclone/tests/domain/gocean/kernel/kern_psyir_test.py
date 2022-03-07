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
kernel-layer-specific symbol. The tests include
translation of PSyIR to PSyclone Kernel PSyIR and PSyclone
Kernel PSyIR to processed PSyIR.

'''
from psyclone.domain.gocean.transformations import KernTrans
from psyclone.domain.gocean.kernel import KernelMetadataSymbol

# TODO STENCIL, GRIDVALUE
METADATA_GOCEAN = (
    "program dummy\n"
    "  type, extends(kernel_type) :: compute_cu\n"
    "     type(go_arg), dimension(2) :: meta_args =                 &\n"
    "          (/ go_arg(GO_WRITE, GO_CU, GO_POINTWISE),            &\n"
    "             go_arg(GO_READ,  GO_CT, GO_STENCIL(000,011,000)), &\n"
    "             go_arg(GO_READ,  GO_GRID_AREA_T)                  &\n"
    "           /)\n"
    "     integer :: ITERATES_OVER = GO_ALL_PTS\n"
    "     integer :: index_offset = GO_OFFSET_SW\n"
    "  contains\n"
    "    procedure, nopass :: code => compute_cu_code\n"
    "  end type compute_cu\n"
    "end program dummy\n")


def test_kernelmetadatasymbol_create(fortran_reader):
    '''Test we can create a KernelMetadataSymbol and capture the expected
    values. We use the gocean1.0 API.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(METADATA_GOCEAN)
    kern_trans = KernTrans()
    kern_trans.metadata_name = "compute_cu"
    kern_trans.apply(kernel_psyir)
    symbol = kernel_psyir.children[0].symbol_table.lookup("compute_cu")
    assert isinstance(symbol, KernelMetadataSymbol)
    assert symbol.iterates_over == "GO_ALL_PTS"
    assert symbol.index_offset == "GO_OFFSET_SW"
    assert symbol.code == "compute_cu_code"
    assert isinstance(symbol.args, list)
    assert len(symbol.args) == 3
    assert isinstance(symbol.args[0], KernelMetadataSymbol.FieldArg)
    assert symbol.args[0].access == "GO_WRITE"
    assert symbol.args[0].stagger == "GO_CU"
    assert symbol.args[0].form == "GO_POINTWISE"
    assert symbol.args[0].stencil is None
    assert isinstance(symbol.args[1], KernelMetadataSymbol.FieldArg)
    assert symbol.args[1].access == "GO_READ"
    assert symbol.args[1].stagger == "GO_CT"
    assert symbol.args[1].form == "GO_STENCIL"
    assert isinstance(symbol.args[1].stencil, list)
    assert symbol.args[1].stencil == ["000", "011", "000"]
    assert isinstance(symbol.args[2], KernelMetadataSymbol.GridArg)
    assert symbol.args[2].access == "GO_READ"
    assert symbol.args[2].name == "GO_GRID_AREA_T"
