# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab

'''Test that the LFRic-specific PSyIR classes are created and declared
correctly'''

# pylint: disable=no-name-in-module
from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import SymbolTable, ArgumentInterface
from psyclone.domain.lfric import KernelInterface
from psyclone.domain.lfric.psyir import \
    MeshHeightDataSymbol, CellPositionDataSymbol


def test_init():
    '''Test that we can create an instance of the KernelInterface class
    and that any defaults are set as expected

    '''
    kernel_interface = KernelInterface(None)
    assert isinstance(kernel_interface._read_access, ArgumentInterface)
    assert (kernel_interface._read_access.access ==
            ArgumentInterface.Access.READ)
    assert isinstance(kernel_interface._symbol_table, SymbolTable)
    assert kernel_interface._arglist == []


def _check_scalar_symbol(kernel_interface, data_type, tag):
    '''Utility function that tests that a particular class is created,
    added to the kernel_interface symbol table and _arglist
    attribute.

    '''
    assert kernel_interface._symbol_table.lookup(tag)
    arglist = kernel_interface._arglist
    assert len(arglist) == 1
    assert isinstance(arglist[0], data_type)
    assert arglist[0].name == tag

# TBD
# def test_generate():
#     pass


def test_cell_position():
    '''Test that the KernelInterface class cell_position method adds the
    expected class to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.cell_position()
    _check_scalar_symbol(kernel_interface, CellPositionDataSymbol, "cell")


def test_mesh_height():
    '''Test that the KernelInterface class mesh_height method adds the
    expected class to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.mesh_height()
    _check_scalar_symbol(kernel_interface, MeshHeightDataSymbol, "nlayers")


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_mesh_ncell2d():
    '''Test that the KernelInterface class mesh_ncell2d method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.mesh_ncell2d()


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_cell_map():
    '''Test that the KernelInterface class cell_map method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.cell_map()

# TBD
# def test_field_vector():
#     pass
# def test_field():
#     pass


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_stencil_unknown_extent():
    '''Test that the KernelInterface class stencil_unknown_extent method
    adds the expected class(es) to the symbol table and the _arglist
    list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.stencil_unknown_extent(None)


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_stencil_unknown_direction():
    '''Test that the KernelInterface class stencil_unknown_direction method
    adds the expected class(es) to the symbol table and the _arglist
    list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.stencil_unknown_direction(None)


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_stencil():
    '''Test that the KernelInterface class stencil method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.stencil(None)

# TBD
# def test_operator():
#     pass


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_cma_operator():
    '''Test that the KernelInterface class cma_operator method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.cma_operator(None)

# TBD
# def test_scalar():
#     pass
# def test_fs_common():
#     pass


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_fs_intergrid():
    '''Test that the KernelInterface class fs_intergrid method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.fs_intergrid(None)

# TBD
# def test_fs_compulsory_field():
#     pass


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_banded_dofmap():
    '''Test that the KernelInterface class banded_dofmap method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.banded_dofmap(None)


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_indirection_dofmap():
    '''Test that the KernelInterface class indirection_dofmap method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.indirection_dofmap(None)

# TBD
# def test_basis():
#     pass
# def test_diff_basis():
#     pass


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_orientation():
    '''Test that the KernelInterface class orientation method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.orientation(None)


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_field_bcs_kernel():
    '''Test that the KernelInterface class field_bcs_kernel method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.field_bcs_kernel(None)


@pytest.mark.xfail(reason="Issue #xxx: this callback is not yet implemented")
def test_operator_bcs_kernel():
    '''Test that the KernelInterface class operator_bcs_kernel method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.operator_bcs_kernel(None)


def test_ref_element_properties():
    '''Test that the KernelInterface class ref_element_properties method can be
    called successfully. This callback method does not contribute any
    kernel arguments so does nothing.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.ref_element_properties()


def test_mesh_properties():
    '''Test that the KernelInterface class mesh_properties method can be
    called successfully. This callback method does not contribute any
    kernel arguments so does nothing.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.mesh_properties()

# TBD
# def test_quad_rule():
#     pass

# TBD
# def test_create_symbol():
#     pass

# TBD
# def test_create_basis():
#     pass

# TBD
# def test_basis_first_dim_value():
#     pass

# TBD
# def test_diff_basis_first_dim_value():
#     pass
