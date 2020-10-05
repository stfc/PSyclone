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
import os
import pytest
from psyclone.psyir.symbols import SymbolTable, ArgumentInterface
from psyclone.domain.lfric import KernelInterface, FunctionSpace
from psyclone.domain.lfric.psyir import \
    MeshHeightDataSymbol, CellPositionDataSymbol, \
    RealVectorFieldDataDataSymbol, NumberOfUniqueDofsDataSymbol, \
    RealFieldDataDataSymbol, NumberOfDofsDataSymbol, \
    OperatorDataSymbol, NumberOfCellsDataSymbol, \
    LfricIntegerScalarDataSymbol, DofMapDataSymbol
from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING
from psyclone.psyGen import PSyFactory
from psyclone.parse.algorithm import parse

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "..", "..", "test_files", "dynamo0p3")


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


# TBD
# def test_generate():
#     pass


def test_cell_position():
    '''Test that the KernelInterface class cell_position method adds the
    expected class to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.cell_position()
    symbol = kernel_interface._symbol_table.lookup("cell")
    assert isinstance(symbol, CellPositionDataSymbol)
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is symbol


def test_mesh_height():
    '''Test that the KernelInterface class mesh_height method adds the
    expected class to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.mesh_height()
    symbol = kernel_interface._symbol_table.lookup("nlayers")
    assert isinstance(symbol, MeshHeightDataSymbol)
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is symbol


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_mesh_ncell2d():
    '''Test that the KernelInterface class mesh_ncell2d method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.mesh_ncell2d()


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_cell_map():
    '''Test that the KernelInterface class cell_map method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.cell_map()


def test_field_vector():
    '''Test the KernelInterface class field_vector method. We want to
    check that the correct symbol is referenced for the dimension of
    the vector field symbols so the simplest solution is to use one of
    the Fortran test examples.

    '''
    kernel_interface = KernelInterface(None)
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    vector_arg = kernel.args[1]
    kernel_interface.field_vector(vector_arg)

    # undf symbol declared
    undf_tag = "undf_{0}".format(vector_arg.function_space.orig_name)
    undf_symbol = kernel_interface._symbol_table.lookup(undf_tag)
    assert isinstance(undf_symbol, NumberOfUniqueDofsDataSymbol)
    assert isinstance(undf_symbol.interface, ArgumentInterface)
    assert (undf_symbol.interface.access ==
            kernel_interface._read_access.access)

    # vector fields declared, added to argument list, correct function
    # space specified and dimensioned correctly
    for idx in range(vector_arg.vector_size):
        tag = "{0}_v{1}".format(vector_arg.name, idx)
        symbol = kernel_interface._symbol_table.lookup(tag)
        assert isinstance(symbol, RealVectorFieldDataDataSymbol)
        assert isinstance(symbol.interface, ArgumentInterface)
        assert (symbol.interface.access ==
                ArgumentInterface(INTENT_MAPPING[vector_arg.intent]).access)
        assert kernel_interface._arglist[idx-3] is symbol
        assert symbol.fs == vector_arg.function_space.orig_name
        assert len(symbol.shape) == 1
        assert symbol.shape[0] is undf_symbol


def test_field():
    '''Test the KernelInterface class field method. We want to check that
    the correct symbol is referenced for the dimension of the field
    symbol so the simplest solution is to use one of the Fortran test
    examples.

    '''
    kernel_interface = KernelInterface(None)
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    field_arg = kernel.args[1]
    kernel_interface.field(field_arg)

    # undf symbol declared
    undf_tag = "undf_{0}".format(field_arg.function_space.orig_name)
    undf_symbol = kernel_interface._symbol_table.lookup(undf_tag)
    assert isinstance(undf_symbol, NumberOfUniqueDofsDataSymbol)
    assert isinstance(undf_symbol.interface, ArgumentInterface)
    assert (undf_symbol.interface.access ==
            kernel_interface._read_access.access)

    # field declared, added to argument list, correct function
    # space specified and dimensioned correctly
    tag = field_arg.name
    symbol = kernel_interface._symbol_table.lookup(tag)
    assert isinstance(symbol, RealFieldDataDataSymbol)
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            ArgumentInterface(INTENT_MAPPING[field_arg.intent]).access)
    assert kernel_interface._arglist[-1] is symbol
    assert symbol.fs == field_arg.function_space.orig_name
    assert len(symbol.shape) == 1
    assert symbol.shape[0] is undf_symbol


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_stencil_unknown_extent():
    '''Test that the KernelInterface class stencil_unknown_extent method
    adds the expected class(es) to the symbol table and the _arglist
    list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.stencil_unknown_extent(None)


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_stencil_unknown_direction():
    '''Test that the KernelInterface class stencil_unknown_direction method
    adds the expected class(es) to the symbol table and the _arglist
    list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.stencil_unknown_direction(None)


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_stencil():
    '''Test that the KernelInterface class stencil method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.stencil(None)


def test_operator():
    '''Test the KernelInterface class operator method. We want to check
    that the correct symbol is referenced for the dimension of the
    operator symbol so the simplest solution is to use one of the Fortran
    test examples.

    '''
    kernel_interface = KernelInterface(None)
    _, invoke_info = parse(os.path.join(BASE_PATH, "10_operator.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    operator_arg = kernel.args[0]
    kernel_interface.operator(operator_arg)

    # fs_from symbol declared
    fs_from_name = operator_arg.function_space_from.orig_name
    fs_from_tag = "ndf_{0}".format(fs_from_name)
    fs_from_symbol = kernel_interface._symbol_table.lookup(fs_from_tag)
    assert isinstance(fs_from_symbol, NumberOfDofsDataSymbol)
    assert fs_from_symbol.fs == fs_from_name
    assert isinstance(fs_from_symbol.interface, ArgumentInterface)
    assert (fs_from_symbol.interface.access ==
            kernel_interface._read_access.access)

    # fs_to symbol declared
    fs_to_name = operator_arg.function_space_from.orig_name
    fs_to_tag = "ndf_{0}".format(fs_to_name)
    fs_to_symbol = kernel_interface._symbol_table.lookup(fs_to_tag)
    assert isinstance(fs_to_symbol, NumberOfDofsDataSymbol)
    assert fs_to_symbol.fs == fs_to_name
    assert isinstance(fs_to_symbol.interface, ArgumentInterface)
    assert (fs_to_symbol.interface.access ==
            kernel_interface._read_access.access)

    # ncells symbol declared
    ncells_symbol = kernel_interface._symbol_table.lookup("ncell_3d")
    assert isinstance(ncells_symbol, NumberOfCellsDataSymbol)
    assert isinstance(ncells_symbol.interface, ArgumentInterface)
    assert (ncells_symbol.interface.access ==
            kernel_interface._read_access.access)

    # operator declared, added to argument list, correct function
    # spaces specified and dimensioned correctly
    tag = operator_arg.name
    symbol = kernel_interface._symbol_table.lookup(tag)
    assert isinstance(symbol, OperatorDataSymbol)
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            ArgumentInterface(INTENT_MAPPING[operator_arg.intent]).access)
    assert kernel_interface._arglist[-1] is symbol
    assert symbol.fs_from == operator_arg.function_space_from.orig_name
    assert symbol.fs_to == operator_arg.function_space_to.orig_name
    assert len(symbol.shape) == 3
    assert symbol.shape[0] is fs_from_symbol
    assert symbol.shape[1] is fs_to_symbol
    assert symbol.shape[2] is ncells_symbol


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_cma_operator():
    '''Test that the KernelInterface class cma_operator method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.cma_operator(None)


def test_scalar():
    '''Test that the KernelInterface class scalar method adds the
    expected class to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1.6.1_single_invoke_1_int_scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    scalar_arg = kernel.args[1]
    kernel_interface = KernelInterface(None)
    kernel_interface.scalar(scalar_arg)
    symbol = kernel_interface._symbol_table.lookup(scalar_arg.name)
    assert isinstance(symbol, LfricIntegerScalarDataSymbol)
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            INTENT_MAPPING[scalar_arg.intent])
    assert kernel_interface._arglist[-1] is symbol


def test_fs_common():
    '''Test that the KernelInterface class fs_common method adds the
    expected class to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    function_space = FunctionSpace("w3", None)
    kernel_interface.fs_common(function_space)
    fs_name = function_space.orig_name
    symbol = kernel_interface._symbol_table.lookup("ndf_{0}".format(fs_name))
    assert isinstance(symbol, NumberOfDofsDataSymbol)
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is symbol


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_fs_intergrid():
    '''Test that the KernelInterface class fs_intergrid method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.fs_intergrid(None)


def test_fs_compulsory_field():
    '''Test that the KernelInterface class fs_compulsory_field method adds
    the expected classes to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    function_space = FunctionSpace("w3", None)
    kernel_interface.fs_compulsory_field(function_space)
    fs_name = function_space.orig_name

    # undf declared and added to argument list
    symbol = kernel_interface._symbol_table.lookup("undf_{0}".format(fs_name))
    assert isinstance(symbol, NumberOfUniqueDofsDataSymbol)
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-2] is symbol

    # ndf declared
    ndf_symbol = kernel_interface._symbol_table.lookup(
        "ndf_{0}".format(fs_name))
    assert isinstance(ndf_symbol, NumberOfDofsDataSymbol)
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)

    # dofmap declared, added to argument list, correct function
    # space specified and dimensioned correctly
    tag = "dofmap_{0}".format(fs_name)
    symbol = kernel_interface._symbol_table.lookup(tag)
    assert isinstance(symbol, DofMapDataSymbol)
    assert isinstance(symbol.interface, ArgumentInterface)
    assert symbol.interface.access == kernel_interface._read_access.access
    assert kernel_interface._arglist[-1] is symbol
    assert symbol.fs == fs_name
    assert len(symbol.shape) == 1
    assert symbol.shape[0] is ndf_symbol


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_banded_dofmap():
    '''Test that the KernelInterface class banded_dofmap method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.banded_dofmap(None)


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
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


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_orientation():
    '''Test that the KernelInterface class orientation method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.orientation(None)


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_field_bcs_kernel():
    '''Test that the KernelInterface class field_bcs_kernel method adds the
    expected class(es) to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.field_bcs_kernel(None)


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
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
