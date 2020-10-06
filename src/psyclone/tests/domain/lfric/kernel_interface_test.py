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

'''Test that the expected kernel arguments, based on the kernel
metadata, are created and declared within a symbol table using
LFRic-specific PSyIR classes.

'''

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
    LfricIntegerScalarDataSymbol, DofMapDataSymbol, \
    NumberOfQrPointsInHorizontalDataSymbol, \
    NumberOfQrPointsInVerticalDataSymbol, \
    BasisFunctionQrXyozDataSymbol, NumberOfFacesDataSymbol, \
    NumberOfQrPointsDataSymbol, BasisFunctionQrFaceDataSymbol, \
    DiffBasisFunctionQrXyozDataSymbol, NumberOfEdgesDataSymbol, \
    QrWeightsDataSymbol, QrWeightsInHorizontalDataSymbol, \
    QrWeightsInVerticalDataSymbol, BasisFunctionQrEdgeDataSymbol
from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING
from psyclone.psyGen import PSyFactory
from psyclone.parse.algorithm import parse
from psyclone.errors import InternalError, GenerationError

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


def test_generate():
    '''Test that the KernelInterface class generate method creates the
    expected symbols and adds them to the symbol table and its
    argument list (in the required order).

    '''
    # Example 14.10 is chosen as it only has two fields in a kernel
    # which reduces the number of arguments to check.
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "14.10_halo_continuous_cell_w_to_r.f90"),
        api="dynamo0.3")

    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel0 = schedule[0].loop_body[0]
    kernel_interface = KernelInterface(kernel0)
    kernel_interface.generate()
    # Check symbols
    nlayers_symbol = kernel_interface._symbol_table.lookup("nlayers")
    assert isinstance(nlayers_symbol, MeshHeightDataSymbol)
    undf_w0_symbol = kernel_interface._symbol_table.lookup("undf_w0")
    assert isinstance(undf_w0_symbol, NumberOfUniqueDofsDataSymbol)
    f1_field_symbol = kernel_interface._symbol_table.lookup("f1")
    assert isinstance(f1_field_symbol, RealFieldDataDataSymbol)
    f2_field_symbol = kernel_interface._symbol_table.lookup("f2")
    assert isinstance(f2_field_symbol, RealFieldDataDataSymbol)
    ndf_w0_symbol = kernel_interface._symbol_table.lookup("ndf_w0")
    assert isinstance(ndf_w0_symbol, NumberOfDofsDataSymbol)
    dofmap_w0_symbol = kernel_interface._symbol_table.lookup("dofmap_w0")
    assert isinstance(dofmap_w0_symbol, DofMapDataSymbol)
    # Check function spaces
    assert undf_w0_symbol.fs == "w0"
    assert f1_field_symbol.fs == "w0"
    assert f2_field_symbol.fs == "w0"
    assert ndf_w0_symbol.fs == "w0"
    assert dofmap_w0_symbol.fs == "w0"
    # Check array dimensions
    assert len(f1_field_symbol.shape) == 1
    assert f1_field_symbol.shape[0] is undf_w0_symbol
    assert len(f2_field_symbol.shape) == 1
    assert f2_field_symbol.shape[0] is undf_w0_symbol
    assert len(dofmap_w0_symbol.shape) == 1
    assert dofmap_w0_symbol.shape[0] is ndf_w0_symbol
    # Check argument list
    arg_list = kernel_interface._symbol_table.argument_datasymbols
    assert len(arg_list) == 6
    assert arg_list[0] is nlayers_symbol
    assert arg_list[1] is undf_w0_symbol
    assert arg_list[2] is f1_field_symbol
    assert arg_list[3] is f2_field_symbol
    assert arg_list[4] is ndf_w0_symbol
    assert arg_list[5] is dofmap_w0_symbol


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


def test_scalar(monkeypatch):
    '''Test that the KernelInterface class scalar method adds the expected
    class to the symbol table and the _arglist list. Also check that
    it raises the expected exception if the scalar type is not
    recognised.

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
    # Force an error
    monkeypatch.setattr(scalar_arg, "_intrinsic_type", "invalid")
    with pytest.raises(NotImplementedError) as info:
        kernel_interface.scalar(scalar_arg)
    assert("scalar of type 'invalid' not implemented in KernelInterface "
           "class." in str(info.value))


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


def test_basis_xyoz():
    '''Test that the KernelInterface class basis method adds the expected
    classes to the symbol table and the _arglist list for xyoz quadrature

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1.1.0_single_invoke_xyoz_qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]

    # "w1" requires a basis function and is the first entry in the
    # unique function spaces list
    w1_fs = kernel.arguments.unique_fss[0]
    fs_name = w1_fs.orig_name

    kernel_interface = KernelInterface(kernel)
    kernel_interface.basis(w1_fs)

    # ndf declared
    ndf_symbol = kernel_interface._symbol_table.lookup(
        "ndf_{0}".format(fs_name))
    assert isinstance(ndf_symbol, NumberOfDofsDataSymbol)
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp_h declared
    nqph_symbol = kernel_interface._symbol_table.lookup("nqp_h")
    assert isinstance(nqph_symbol, NumberOfQrPointsInHorizontalDataSymbol)
    assert isinstance(nqph_symbol.interface, ArgumentInterface)
    assert (nqph_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp_v declared
    nqpv_symbol = kernel_interface._symbol_table.lookup("nqp_v")
    assert isinstance(nqpv_symbol, NumberOfQrPointsInVerticalDataSymbol)
    assert isinstance(nqpv_symbol.interface, ArgumentInterface)
    assert (nqpv_symbol.interface.access ==
            kernel_interface._read_access.access)
    # basis declared and added to argument list
    basis_symbol = kernel_interface._symbol_table.lookup("basis_w1_qr_xyoz")
    assert isinstance(basis_symbol, BasisFunctionQrXyozDataSymbol)
    assert isinstance(basis_symbol.interface, ArgumentInterface)
    assert (basis_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is basis_symbol
    assert len(basis_symbol.shape) == 4
    assert basis_symbol.shape[0] == 3
    assert basis_symbol.shape[1] is ndf_symbol
    assert basis_symbol.shape[2] is nqph_symbol
    assert basis_symbol.shape[3] is nqpv_symbol


def test_basis_face():
    '''Test that the KernelInterface class basis method adds the expected
    classes to the symbol table and the _arglist list for face quadrature

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1.1.6_face_qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]

    # "w1" requires a basis function and is the first entry in the
    # unique function spaces list
    w1_fs = kernel.arguments.unique_fss[0]
    fs_name = w1_fs.orig_name

    kernel_interface = KernelInterface(kernel)
    kernel_interface.basis(w1_fs)

    # ndf declared
    ndf_symbol = kernel_interface._symbol_table.lookup(
        "ndf_{0}".format(fs_name))
    assert isinstance(ndf_symbol, NumberOfDofsDataSymbol)
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nfaces declared
    nfaces_symbol = kernel_interface._symbol_table.lookup("nfaces")
    assert isinstance(nfaces_symbol, NumberOfFacesDataSymbol)
    assert isinstance(nfaces_symbol.interface, ArgumentInterface)
    assert (nfaces_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp declared
    nqp_symbol = kernel_interface._symbol_table.lookup("nqp")
    assert isinstance(nqp_symbol, NumberOfQrPointsDataSymbol)
    assert isinstance(nqp_symbol.interface, ArgumentInterface)
    assert (nqp_symbol.interface.access ==
            kernel_interface._read_access.access)
    # basis declared and added to argument list
    basis_symbol = kernel_interface._symbol_table.lookup("basis_w1_qr_face")
    assert isinstance(basis_symbol, BasisFunctionQrFaceDataSymbol)
    assert isinstance(basis_symbol.interface, ArgumentInterface)
    assert (basis_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is basis_symbol
    assert len(basis_symbol.shape) == 4
    assert basis_symbol.shape[0] == 3
    assert basis_symbol.shape[1] is ndf_symbol
    assert basis_symbol.shape[2] is nqp_symbol
    assert basis_symbol.shape[3] is nfaces_symbol


def test_basis_edge():
    '''Test that the KernelInterface class basis method adds the expected
    classes to the symbol table and the _arglist list for edge quadrature

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1.1.5_edge_qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]

    # "w1" requires a basis function and is the first entry in the
    # unique function spaces list
    w1_fs = kernel.arguments.unique_fss[0]
    fs_name = w1_fs.orig_name

    kernel_interface = KernelInterface(kernel)
    kernel_interface.basis(w1_fs)

    # ndf declared
    ndf_symbol = kernel_interface._symbol_table.lookup(
        "ndf_{0}".format(fs_name))
    assert isinstance(ndf_symbol, NumberOfDofsDataSymbol)
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nedges declared
    nedges_symbol = kernel_interface._symbol_table.lookup("nedges")
    assert isinstance(nedges_symbol, NumberOfEdgesDataSymbol)
    assert isinstance(nedges_symbol.interface, ArgumentInterface)
    assert (nedges_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp declared
    nqp_symbol = kernel_interface._symbol_table.lookup("nqp")
    assert isinstance(nqp_symbol, NumberOfQrPointsDataSymbol)
    assert isinstance(nqp_symbol.interface, ArgumentInterface)
    assert (nqp_symbol.interface.access ==
            kernel_interface._read_access.access)
    # basis declared and added to argument list
    basis_symbol = kernel_interface._symbol_table.lookup("basis_w1_qr_edge")
    assert isinstance(basis_symbol, BasisFunctionQrEdgeDataSymbol)
    assert isinstance(basis_symbol.interface, ArgumentInterface)
    assert (basis_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is basis_symbol
    assert len(basis_symbol.shape) == 4
    assert basis_symbol.shape[0] == 3
    assert basis_symbol.shape[1] is ndf_symbol
    assert basis_symbol.shape[2] is nqp_symbol
    assert basis_symbol.shape[3] is nedges_symbol


def test_diff_basis():
    '''Test that the KernelInterface class basis method adds the expected
    classes to the symbol table and the _arglist list. We use xyoz
    quadrature for this test, but other quadrature could equally have
    been used.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1.1.0_single_invoke_xyoz_qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]

    # "w2" requires a diff basis function and is the second entry in the
    # unique function spaces list
    w2_fs = kernel.arguments.unique_fss[1]
    fs_name = w2_fs.orig_name

    kernel_interface = KernelInterface(kernel)
    kernel_interface.diff_basis(w2_fs)

    # ndf declared
    ndf_symbol = kernel_interface._symbol_table.lookup(
        "ndf_{0}".format(fs_name))
    assert isinstance(ndf_symbol, NumberOfDofsDataSymbol)
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp_h declared
    nqph_symbol = kernel_interface._symbol_table.lookup("nqp_h")
    assert isinstance(nqph_symbol, NumberOfQrPointsInHorizontalDataSymbol)
    assert isinstance(nqph_symbol.interface, ArgumentInterface)
    assert (nqph_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp_v declared
    nqpv_symbol = kernel_interface._symbol_table.lookup("nqp_v")
    assert isinstance(nqpv_symbol, NumberOfQrPointsInVerticalDataSymbol)
    assert isinstance(nqpv_symbol.interface, ArgumentInterface)
    assert (nqpv_symbol.interface.access ==
            kernel_interface._read_access.access)
    # diff basis declared and added to argument list
    diff_basis_symbol = kernel_interface._symbol_table.lookup(
        "diff_basis_w2_qr_xyoz")
    assert isinstance(diff_basis_symbol, DiffBasisFunctionQrXyozDataSymbol)
    assert isinstance(diff_basis_symbol.interface, ArgumentInterface)
    assert (diff_basis_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is diff_basis_symbol
    assert len(diff_basis_symbol.shape) == 4
    assert diff_basis_symbol.shape[0] == 1
    assert diff_basis_symbol.shape[1] is ndf_symbol
    assert diff_basis_symbol.shape[2] is nqph_symbol
    assert diff_basis_symbol.shape[3] is nqpv_symbol


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


def test_quad_rule_xyoz():
    '''Test that the KernelInterface class quad_rule method adds the expected
    classes to the symbol table and the _arglist list for xyoz quadrature

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1.1.0_single_invoke_xyoz_qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    kernel_interface = KernelInterface(kernel)
    kernel_interface.quad_rule()

    # nqp_h declared and added to argument list
    nqph_symbol = kernel_interface._symbol_table.lookup("nqp_h")
    assert isinstance(nqph_symbol, NumberOfQrPointsInHorizontalDataSymbol)
    assert isinstance(nqph_symbol.interface, ArgumentInterface)
    assert (nqph_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-4] is nqph_symbol
    # nqp_v declared and added to argument list
    nqpv_symbol = kernel_interface._symbol_table.lookup("nqp_v")
    assert isinstance(nqpv_symbol, NumberOfQrPointsInVerticalDataSymbol)
    assert isinstance(nqpv_symbol.interface, ArgumentInterface)
    assert (nqpv_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-3] is nqpv_symbol
    # weights_h declared and added to argument list
    weightsh_symbol = kernel_interface._symbol_table.lookup("weights_h")
    assert isinstance(weightsh_symbol, QrWeightsInHorizontalDataSymbol)
    assert isinstance(weightsh_symbol.interface, ArgumentInterface)
    assert (weightsh_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-2] is weightsh_symbol
    assert len(weightsh_symbol.shape) == 1
    assert weightsh_symbol.shape[0] is nqph_symbol
    # weights_v declared and added to argument list
    weightsv_symbol = kernel_interface._symbol_table.lookup("weights_v")
    assert isinstance(weightsv_symbol, QrWeightsInVerticalDataSymbol)
    assert isinstance(weightsv_symbol.interface, ArgumentInterface)
    assert (weightsv_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is weightsv_symbol
    assert len(weightsv_symbol.shape) == 1
    assert weightsv_symbol.shape[0] is nqpv_symbol


def test_quad_rule_face():
    '''Test that the KernelInterface class quad_rule method adds the expected
    classes to the symbol table and the _arglist list for face quadrature

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1.1.6_face_qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    kernel_interface = KernelInterface(kernel)
    kernel_interface.quad_rule()

    # nfaces declared and added to argument list
    nfaces_symbol = kernel_interface._symbol_table.lookup("nfaces")
    assert isinstance(nfaces_symbol, NumberOfFacesDataSymbol)
    assert isinstance(nfaces_symbol.interface, ArgumentInterface)
    assert (nfaces_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-3] is nfaces_symbol
    # nqp declared and added to argument list
    nqp_symbol = kernel_interface._symbol_table.lookup("nqp")
    assert isinstance(nqp_symbol, NumberOfQrPointsDataSymbol)
    assert isinstance(nqp_symbol.interface, ArgumentInterface)
    assert (nqp_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-2] is nqp_symbol
    # weights declared and added to argument list
    weights_symbol = kernel_interface._symbol_table.lookup("weights")
    assert isinstance(weights_symbol, QrWeightsDataSymbol)
    assert isinstance(weights_symbol.interface, ArgumentInterface)
    assert (weights_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is weights_symbol
    assert len(weights_symbol.shape) == 1
    assert weights_symbol.shape[0] is nqp_symbol


def test_quad_rule_edge():
    '''Test that the KernelInterface class quad_rule method adds the expected
    classes to the symbol table and the _arglist list for edge quadrature

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1.1.5_edge_qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    kernel_interface = KernelInterface(kernel)
    kernel_interface.quad_rule()

    # nedges declared and added to argument list
    nedges_symbol = kernel_interface._symbol_table.lookup("nedges")
    assert isinstance(nedges_symbol, NumberOfEdgesDataSymbol)
    assert isinstance(nedges_symbol.interface, ArgumentInterface)
    assert (nedges_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-3] is nedges_symbol
    # nqp declared and added to argument list
    nqp_symbol = kernel_interface._symbol_table.lookup("nqp")
    assert isinstance(nqp_symbol, NumberOfQrPointsDataSymbol)
    assert isinstance(nqp_symbol.interface, ArgumentInterface)
    assert (nqp_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-2] is nqp_symbol
    # weights declared and added to argument list
    weights_symbol = kernel_interface._symbol_table.lookup("weights")
    assert isinstance(weights_symbol, QrWeightsDataSymbol)
    assert isinstance(weights_symbol.interface, ArgumentInterface)
    assert (weights_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is weights_symbol
    assert len(weights_symbol.shape) == 1
    assert weights_symbol.shape[0] is nqp_symbol


def test_quad_rule_error(monkeypatch):
    '''Test that the KernelInterface class quad_rule method raises the
    expected exception when an unsupported quadrature shape is
    provided.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "6.1_eval_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    kernel_interface = KernelInterface(kernel)
    # Force an unsupported shape
    monkeypatch.setattr(kernel, "_qr_rules", ["invalid_shape"])
    with pytest.raises(InternalError) as info:
        kernel_interface.quad_rule()
    assert(
        "Unsupported quadrature shape 'invalid_shape' found in "
        "kernel_interface." in str(info.value))


def test_create_symbol_tag():
    ''' Test that an existing symbol with the same tag as this is returned '''
    symbol1 = CellPositionDataSymbol("test")
    kernel_interface = KernelInterface(None)
    kernel_interface._symbol_table.add(symbol1, tag="test")
    symbol2 = kernel_interface._create_symbol("test", CellPositionDataSymbol)
    assert symbol2 is symbol1


def test_create_symbol_tag_error():
    '''Test that an existing symbol with the same tag as this raises an
    exception if its type differs from the supplied type.

    '''
    symbol1 = CellPositionDataSymbol("test")
    kernel_interface = KernelInterface(None)
    kernel_interface._symbol_table.add(symbol1, tag="test")
    with pytest.raises(InternalError) as info:
        _ = kernel_interface._create_symbol("test", MeshHeightDataSymbol)
    assert (
        "Expected symbol with tag 'test' to be of type 'MeshHeightDataSymbol' "
        "but found type 'CellPositionDataSymbol'." in str(info.value))


def test_create_symbol_new():
    '''Test that a new symbol is created correctly and added to the symbol
    table succesfuly with the supplied tag and the default interface
    if one is not supplied. Also check the symbol is returned from the
    method.

    '''
    kernel_interface = KernelInterface(None)
    symbol1 = kernel_interface._create_symbol("test", CellPositionDataSymbol)
    symbol2 = kernel_interface._symbol_table.lookup_with_tag("test")
    assert symbol2 is symbol1
    assert isinstance(symbol1.interface, ArgumentInterface)
    assert (symbol1.interface.access ==
            kernel_interface._read_access.access)


def test_create_symbol_args():
    '''Test that a new symbol is created correctly and added to the symbol
    table successfully when it makes use of the extra_args, dims and
    interface optional arguments. We don't check or test for errors in
    argument types and values as this is an internal method.

    '''
    kernel_interface = KernelInterface(None)
    size_symbol = NumberOfUniqueDofsDataSymbol("ndofs", "w3")
    symbol1 = kernel_interface._create_symbol(
        "test", RealFieldDataDataSymbol, extra_args=["w3"], dims=[size_symbol],
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))

    symbol2 = kernel_interface._symbol_table.lookup_with_tag("test")
    assert symbol2 is symbol1
    assert symbol1.fs == "w3"
    assert len(symbol1.shape) == 1
    assert symbol1.shape[0] == size_symbol
    assert isinstance(symbol1.interface, ArgumentInterface)
    assert symbol1.interface.access == ArgumentInterface.Access.READWRITE


def test_create_basis_errors(monkeypatch):
    '''Check that the appropriate exceptions are raised when a) an
    evaluator shape is provided, as they are not yet supported, and b)
    an unrecognised quadrature or evaluator shape is found.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "6.1_eval_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    kernel_interface = KernelInterface(kernel)

    # "w1" requires a basis function and is the first entry in the
    # unique function spaces list
    w1_fs = kernel.arguments.unique_fss[0]
    # Evaluator shapes are not yet supported.
    with pytest.raises(NotImplementedError) as info:
        kernel_interface.basis(w1_fs)
    assert ("Evaluator shapes not implemented in kernel_interface class."
            in str(info.value))
    # Force an unsupported shape
    monkeypatch.setattr(kernel, "_eval_shapes", ["invalid_shape"])
    with pytest.raises(InternalError) as info:
        kernel_interface.basis(w1_fs)
        assert (
            "Unrecognised quadrature or evaluator shape 'invalid_shape'. "
            "Expected one of: ['gh_quadrature_xyoz', 'gh_quadrature_face', "
            "'gh_quadrature_edge', 'gh_evaluator']." in str(info.value))


def test_basis_first_dim_value():
    '''Check that the kernel_interface class basis_first_dim_value method
    returns the expected results and raises the expected exception.

    '''
    kernel_interface = KernelInterface(None)
    w3_fs = FunctionSpace("w3", None)
    assert kernel_interface._basis_first_dim_value(w3_fs) == "1"
    w2_fs = FunctionSpace("w2", None)
    assert kernel_interface._basis_first_dim_value(w2_fs) == "3"
    any_space_fs = FunctionSpace("any_space_1", None)
    with pytest.raises(GenerationError) as info:
        _ = kernel_interface._basis_first_dim_value(any_space_fs)
    assert (
        "Unsupported space for basis function, expecting one of ['w3', "
        "'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', 'w1', 'w2', "
        "'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi'] but found "
        "'any_space_1'" in str(info.value))


def test_diff_basis_first_dim_value():
    '''Check that the kernel_interface class diff_basis_first_dim_value method
    returns the expected results and raises the expected exception.

    '''
    kernel_interface = KernelInterface(None)
    w3_fs = FunctionSpace("w3", None)
    assert kernel_interface._diff_basis_first_dim_value(w3_fs) == "3"
    w2_fs = FunctionSpace("w2", None)
    assert kernel_interface._diff_basis_first_dim_value(w2_fs) == "1"
    any_space_fs = FunctionSpace("any_space_1", None)
    with pytest.raises(GenerationError) as info:
        _ = kernel_interface._diff_basis_first_dim_value(any_space_fs)
    assert (
        "Unsupported space for differential basis function, expecting one of "
        "['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', 'w1', 'w2', "
        "'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi'] but found "
        "'any_space_1'" in str(info.value))
