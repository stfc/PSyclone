# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified by: I. Kavcic, Met Office
# Modified by: J. Henrichs, Bureau of Meteorology

'''Test that the expected kernel arguments, based on the kernel
metadata, are created and declared within a symbol table using
LFRic-specific PSyIR classes.

'''

import os
import pytest

from psyclone.core import AccessType, Signature, VariablesAccessInfo
from psyclone.domain.lfric import FunctionSpace, KernelInterface, LFRicTypes
from psyclone.errors import InternalError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.symbols import SymbolTable, ArgumentInterface
from psyclone.psyir.nodes import Reference, Literal
from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING

# pylint complains about all isinstance tests involving LFRicTypes,
# since it doesn't know what the actual return type is. So disable
# it here globally for this file:
# pylint: disable=isinstance-second-argument-not-valid-type

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, os.pardir, "test_files", "dynamo0p3")


def test_init():
    '''Test that we can create an instance of the KernelInterface class
    and that any defaults are set as expected

    '''
    kernel_interface = KernelInterface(None)
    assert isinstance(kernel_interface._read_access, ArgumentInterface)
    assert (kernel_interface._read_access.access ==
            ArgumentInterface.Access.READ)
    assert isinstance(kernel_interface._symtab, SymbolTable)
    assert kernel_interface._arglist == []


@pytest.mark.parametrize("var_accesses", [None, VariablesAccessInfo()])
def test_generate(var_accesses):
    '''Test that the KernelInterface class generate method creates the
    expected symbols and adds them to the symbol table and its
    argument list (in the required order).

    '''
    # Test 14.10 is chosen as it only has two fields in a kernel
    # which reduces the number of arguments to check.
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "14.10_halo_continuous_cell_w_to_r.f90"),
        api="dynamo0.3")

    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel0 = schedule[0].loop_body[0]
    kernel_interface = KernelInterface(kernel0)
    kernel_interface.generate(var_accesses=var_accesses)
    # Check symbols
    nlayers_symbol = kernel_interface._symtab.lookup("nlayers")
    assert isinstance(nlayers_symbol, LFRicTypes("MeshHeightDataSymbol"))
    undf_w0_symbol = kernel_interface._symtab.lookup("undf_w0")
    assert isinstance(undf_w0_symbol,
                      LFRicTypes("NumberOfUniqueDofsDataSymbol"))
    f1_field_symbol = kernel_interface._symtab.lookup("f1")
    assert isinstance(f1_field_symbol, LFRicTypes("RealFieldDataSymbol"))
    f2_field_symbol = kernel_interface._symtab.lookup("f2")
    assert isinstance(f2_field_symbol, LFRicTypes("RealFieldDataSymbol"))
    ndf_w0_symbol = kernel_interface._symtab.lookup("ndf_w0")
    assert isinstance(ndf_w0_symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    dofmap_w0_symbol = kernel_interface._symtab.lookup("dofmap_w0")
    assert isinstance(dofmap_w0_symbol, LFRicTypes("DofMapDataSymbol"))
    # Check function spaces
    assert undf_w0_symbol.fs == "w0"
    assert f1_field_symbol.fs == "w0"
    assert f2_field_symbol.fs == "w0"
    assert ndf_w0_symbol.fs == "w0"
    assert dofmap_w0_symbol.fs == "w0"
    # Check array dimensions
    assert len(f1_field_symbol.shape) == 1
    assert isinstance(f1_field_symbol.shape[0].upper, Reference)
    assert f1_field_symbol.shape[0].upper.symbol is undf_w0_symbol
    assert len(f2_field_symbol.shape) == 1
    assert isinstance(f2_field_symbol.shape[0].upper, Reference)
    assert f2_field_symbol.shape[0].upper.symbol is undf_w0_symbol
    assert len(dofmap_w0_symbol.shape) == 1
    assert isinstance(dofmap_w0_symbol.shape[0].upper, Reference)
    assert dofmap_w0_symbol.shape[0].upper.symbol is ndf_w0_symbol
    # Check argument list
    arg_list = kernel_interface._symtab.argument_datasymbols
    assert len(arg_list) == 6
    assert arg_list[0] is nlayers_symbol
    assert arg_list[1] is undf_w0_symbol
    assert arg_list[2] is f1_field_symbol
    assert arg_list[3] is f2_field_symbol
    assert arg_list[4] is ndf_w0_symbol
    assert arg_list[5] is dofmap_w0_symbol
    if var_accesses:
        # Check that the names of variables and their intent has been
        # captured by the data dependence analysis
        assert len(var_accesses.all_signatures) == 6

        # Test all read-only variables
        for var in ["nlayers", "undf_w0", "f2", "ndf_w0", "dofmap_w0"]:
            accesses = var_accesses[Signature(var)]
            assert len(accesses.all_accesses) == 1
            assert accesses[0].access_type == AccessType.READ

        # Test the read-write variable
        accesses = var_accesses[Signature("f1")]
        assert len(accesses.all_accesses) == 1
        assert accesses[0].access_type == AccessType.READWRITE


def test_cell_position():

    '''Test that the KernelInterface class cell_position method adds the
    expected type of Symbol to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.cell_position()
    symbol = kernel_interface._symtab.lookup("cell")
    assert isinstance(symbol, LFRicTypes("CellPositionDataSymbol"))
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is symbol


def test_mesh_height():
    '''Test that the KernelInterface class mesh_height method adds the
    expected type of Symbol to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.mesh_height()
    symbol = kernel_interface._symtab.lookup("nlayers")
    assert isinstance(symbol, LFRicTypes("MeshHeightDataSymbol"))
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is symbol


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_mesh_ncell2d():
    '''Test that the KernelInterface.mesh_ncell2d() method adds the
    expected symbols to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface._mesh_ncell2d()


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_mesh_ncell2d_no_halos():
    '''Test that the KernelInterface.mesh_ncell2d_no_halos() method
    adds the expected symbols to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface._mesh_ncell2d_no_halos()


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_cell_map():
    '''Test that the KernelInterface class cell_map method adds the
    expected symbols to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.cell_map()


def test_field_vector(monkeypatch):
    '''Test the KernelInterface class field_vector method. We want to
    check that the correct symbol is referenced for the dimension of
    the vector field symbols so the simplest solution is to use one of
    the Fortran test examples. Also check that the expected exception
    is raised if the type of the vector field is not supported.

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
    undf_tag = f"undf_{vector_arg.function_space.orig_name}"
    undf_symbol = kernel_interface._symtab.lookup(undf_tag)
    assert isinstance(undf_symbol, LFRicTypes("NumberOfUniqueDofsDataSymbol"))
    assert isinstance(undf_symbol.interface, ArgumentInterface)
    assert (undf_symbol.interface.access ==
            kernel_interface._read_access.access)

    # vector fields declared, added to argument list, correct function
    # space specified and dimensioned correctly
    for idx in range(vector_arg.vector_size):
        tag = f"{vector_arg.name}_v{idx}"
        symbol = kernel_interface._symtab.lookup(tag)
        assert isinstance(symbol,
                          LFRicTypes("RealVectorFieldDataSymbol"))
        assert isinstance(symbol.interface, ArgumentInterface)
        assert (symbol.interface.access ==
                ArgumentInterface(INTENT_MAPPING[vector_arg.intent]).access)
        assert kernel_interface._arglist[idx-3] is symbol
        assert symbol.fs == vector_arg.function_space.orig_name
        assert len(symbol.shape) == 1
        assert isinstance(symbol.shape[0].upper, Reference)
        assert symbol.shape[0].upper.symbol is undf_symbol

    # Force an exception by setting the intrinsic type to an unsupported value
    monkeypatch.setattr(vector_arg, "_intrinsic_type", "unsupported")
    with pytest.raises(NotImplementedError) as info:
        kernel_interface.field_vector(vector_arg)
    assert ("kernel interface does not support a vector field of type "
            "'unsupported'." in str(info.value))


def test_field(monkeypatch):
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
    undf_tag = f"undf_{field_arg.function_space.orig_name}"
    undf_symbol = kernel_interface._symtab.lookup(undf_tag)
    assert isinstance(undf_symbol, LFRicTypes("NumberOfUniqueDofsDataSymbol"))
    assert isinstance(undf_symbol.interface, ArgumentInterface)
    assert (undf_symbol.interface.access ==
            kernel_interface._read_access.access)

    # field declared, added to argument list, correct function
    # space specified and dimensioned correctly
    tag = field_arg.name
    symbol = kernel_interface._symtab.lookup(tag)
    assert isinstance(symbol, LFRicTypes("RealFieldDataSymbol"))
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            ArgumentInterface(INTENT_MAPPING[field_arg.intent]).access)
    assert kernel_interface._arglist[-1] is symbol
    assert symbol.fs == field_arg.function_space.orig_name
    assert len(symbol.shape) == 1
    assert isinstance(symbol.shape[0].upper, Reference)
    assert symbol.shape[0].upper.symbol is undf_symbol

    # Force an exception by setting the intrinsic type to an unsupported value
    monkeypatch.setattr(field_arg, "_intrinsic_type", "unsupported")
    with pytest.raises(NotImplementedError) as info:
        kernel_interface.field(field_arg)
    assert ("kernel interface does not support a field of type "
            "'unsupported'." in str(info.value))


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_stencil_unknown_extent():
    '''Test that the KernelInterface class stencil_unknown_extent method
    adds the expected symbols to the symbol table and the _arglist
    list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.stencil_unknown_extent(None)


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_stencil_unknown_direction():
    '''Test that the KernelInterface class stencil_unknown_direction method
    adds the expected symbols to the symbol table and the _arglist
    list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.stencil_unknown_direction(None)


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_stencil():
    '''Test that the KernelInterface class stencil method adds the
    expected symbols to the symbol table and the _arglist list.

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
    fs_from_tag = f"ndf_{fs_from_name}"
    fs_from_symbol = kernel_interface._symtab.lookup(fs_from_tag)
    assert isinstance(fs_from_symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    assert fs_from_symbol.fs == fs_from_name
    assert isinstance(fs_from_symbol.interface, ArgumentInterface)
    assert (fs_from_symbol.interface.access ==
            kernel_interface._read_access.access)

    # fs_to symbol declared
    fs_to_name = operator_arg.function_space_from.orig_name
    fs_to_tag = f"ndf_{fs_to_name}"
    fs_to_symbol = kernel_interface._symtab.lookup(fs_to_tag)
    assert isinstance(fs_to_symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    assert fs_to_symbol.fs == fs_to_name
    assert isinstance(fs_to_symbol.interface, ArgumentInterface)
    assert (fs_to_symbol.interface.access ==
            kernel_interface._read_access.access)

    # ncells symbol declared
    ncells_symbol = kernel_interface._symtab.lookup("ncell_3d")
    assert isinstance(ncells_symbol, LFRicTypes("NumberOfCellsDataSymbol"))
    assert isinstance(ncells_symbol.interface, ArgumentInterface)
    assert (ncells_symbol.interface.access ==
            kernel_interface._read_access.access)

    # operator declared, added to argument list, correct function
    # spaces specified and dimensioned correctly
    tag = operator_arg.name
    symbol = kernel_interface._symtab.lookup(tag)
    assert isinstance(symbol, LFRicTypes("OperatorDataSymbol"))
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            ArgumentInterface(INTENT_MAPPING[operator_arg.intent]).access)
    assert kernel_interface._arglist[-1] is symbol
    assert symbol.fs_from == operator_arg.function_space_from.orig_name
    assert symbol.fs_to == operator_arg.function_space_to.orig_name
    assert len(symbol.shape) == 3
    assert isinstance(symbol.shape[0].upper, Reference)
    assert symbol.shape[0].upper.symbol is fs_from_symbol
    assert isinstance(symbol.shape[1].upper, Reference)
    assert symbol.shape[1].upper.symbol is fs_to_symbol
    assert isinstance(symbol.shape[2].upper, Reference)
    assert symbol.shape[2].upper.symbol is ncells_symbol


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_cma_operator():
    '''Test that the KernelInterface class cma_operator method adds the
    expected symbols to the symbol table and the _arglist list.

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
    symbol = kernel_interface._symtab.lookup(scalar_arg.name)
    assert isinstance(symbol, LFRicTypes("LFRicIntegerScalarDataSymbol"))
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            INTENT_MAPPING[scalar_arg.intent])
    assert kernel_interface._arglist[-1] is symbol
    # Force an error
    monkeypatch.setattr(scalar_arg, "_intrinsic_type", "invalid")
    with pytest.raises(NotImplementedError) as info:
        kernel_interface.scalar(scalar_arg)
    assert ("scalar of type 'invalid' not implemented in KernelInterface "
            "class." in str(info.value))


def test_fs_common():
    '''Test that the KernelInterface class fs_common method adds the
    expected type of Symbol to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    function_space = FunctionSpace("w3", None)
    kernel_interface.fs_common(function_space)
    fs_name = function_space.orig_name
    symbol = kernel_interface._symtab.lookup(f"ndf_{fs_name}")
    assert isinstance(symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is symbol


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_fs_intergrid():
    '''Test that the KernelInterface class fs_intergrid method adds the
    expected symbols to the symbol table and the _arglist list.

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
    symbol = kernel_interface._symtab.lookup(f"undf_{fs_name}")
    assert isinstance(symbol, LFRicTypes("NumberOfUniqueDofsDataSymbol"))
    assert isinstance(symbol.interface, ArgumentInterface)
    assert (symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-2] is symbol

    # ndf declared
    ndf_symbol = kernel_interface._symtab.lookup(f"ndf_{fs_name}")
    assert isinstance(ndf_symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)

    # dofmap declared, added to argument list, correct function
    # space specified and dimensioned correctly
    tag = f"dofmap_{fs_name}"
    symbol = kernel_interface._symtab.lookup(tag)
    assert isinstance(symbol, LFRicTypes("DofMapDataSymbol"))
    assert isinstance(symbol.interface, ArgumentInterface)
    assert symbol.interface.access == kernel_interface._read_access.access
    assert kernel_interface._arglist[-1] is symbol
    assert symbol.fs == fs_name
    assert len(symbol.shape) == 1
    assert isinstance(symbol.shape[0].upper, Reference)
    assert symbol.shape[0].upper.symbol is ndf_symbol


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_banded_dofmap():
    '''Test that the KernelInterface class banded_dofmap method adds the
    expected symbols to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.banded_dofmap(None)


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_indirection_dofmap():
    '''Test that the KernelInterface class indirection_dofmap method adds the
    expected symbols to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.indirection_dofmap(None)


def test_basis_xyoz():
    '''Test that the KernelInterface class basis method adds the expected
    classes to the symbol table and the _arglist list for xyoz
    quadrature.

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
    ndf_symbol = kernel_interface._symtab.lookup(f"ndf_{fs_name}")
    assert isinstance(ndf_symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp_xy declared
    nqph_symbol = kernel_interface._symtab.lookup("nqp_xy")
    assert isinstance(nqph_symbol,
                      LFRicTypes("NumberOfQrPointsInXyDataSymbol"))
    assert isinstance(nqph_symbol.interface, ArgumentInterface)
    assert (nqph_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp_z declared
    nqpv_symbol = kernel_interface._symtab.lookup("nqp_z")
    assert isinstance(nqpv_symbol,
                      LFRicTypes("NumberOfQrPointsInZDataSymbol"))
    assert isinstance(nqpv_symbol.interface, ArgumentInterface)
    assert (nqpv_symbol.interface.access ==
            kernel_interface._read_access.access)
    # basis declared and added to argument list
    basis_symbol = kernel_interface._symtab.lookup("basis_w1_qr_xyoz")
    assert isinstance(basis_symbol,
                      LFRicTypes("BasisFunctionQrXyozDataSymbol"))
    assert isinstance(basis_symbol.interface, ArgumentInterface)
    assert (basis_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is basis_symbol
    assert len(basis_symbol.shape) == 4
    assert isinstance(basis_symbol.shape[0].upper, Literal)
    assert basis_symbol.shape[0].upper.value == "3"
    assert isinstance(basis_symbol.shape[1].upper, Reference)
    assert basis_symbol.shape[1].upper.symbol is ndf_symbol
    assert isinstance(basis_symbol.shape[2].upper, Reference)
    assert basis_symbol.shape[2].upper.symbol is nqph_symbol
    assert isinstance(basis_symbol.shape[3].upper, Reference)
    assert basis_symbol.shape[3].upper.symbol is nqpv_symbol


def test_basis_face():
    '''Test that the KernelInterface class basis method adds the expected
    classes to the symbol table and the _arglist list for face
    quadrature.

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
    ndf_symbol = kernel_interface._symtab.lookup(f"ndf_{fs_name}")
    assert isinstance(ndf_symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nfaces declared
    nfaces_symbol = kernel_interface._symtab.lookup("nfaces")
    assert isinstance(nfaces_symbol, LFRicTypes("NumberOfFacesDataSymbol"))
    assert isinstance(nfaces_symbol.interface, ArgumentInterface)
    assert (nfaces_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp declared
    nqp_symbol = kernel_interface._symtab.lookup("nqp_faces")
    assert isinstance(
        nqp_symbol, LFRicTypes("NumberOfQrPointsInFacesDataSymbol"))
    assert isinstance(nqp_symbol.interface, ArgumentInterface)
    assert (nqp_symbol.interface.access ==
            kernel_interface._read_access.access)
    # basis declared and added to argument list
    basis_symbol = kernel_interface._symtab.lookup("basis_w1_qr_face")
    assert isinstance(basis_symbol,
                      LFRicTypes("BasisFunctionQrFaceDataSymbol"))
    assert isinstance(basis_symbol.interface, ArgumentInterface)
    assert (basis_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is basis_symbol
    assert len(basis_symbol.shape) == 4
    assert isinstance(basis_symbol.shape[0].upper, Literal)
    assert basis_symbol.shape[0].upper.value == "3"
    assert isinstance(basis_symbol.shape[1].upper, Reference)
    assert basis_symbol.shape[1].upper.symbol is ndf_symbol
    assert isinstance(basis_symbol.shape[2].upper, Reference)
    assert basis_symbol.shape[2].upper.symbol is nqp_symbol
    assert isinstance(basis_symbol.shape[3].upper, Reference)
    assert basis_symbol.shape[3].upper.symbol is nfaces_symbol


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
    ndf_symbol = kernel_interface._symtab.lookup(f"ndf_{fs_name}")
    assert isinstance(ndf_symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nedges declared
    nedges_symbol = kernel_interface._symtab.lookup("nedges")
    assert isinstance(nedges_symbol, LFRicTypes("NumberOfEdgesDataSymbol"))
    assert isinstance(nedges_symbol.interface, ArgumentInterface)
    assert (nedges_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp declared
    nqp_symbol = kernel_interface._symtab.lookup("nqp_edges")
    assert isinstance(
        nqp_symbol, LFRicTypes("NumberOfQrPointsInEdgesDataSymbol"))
    assert isinstance(nqp_symbol.interface, ArgumentInterface)
    assert (nqp_symbol.interface.access ==
            kernel_interface._read_access.access)
    # basis declared and added to argument list
    basis_symbol = kernel_interface._symtab.lookup("basis_w1_qr_edge")
    assert isinstance(basis_symbol,
                      LFRicTypes("BasisFunctionQrEdgeDataSymbol"))
    assert isinstance(basis_symbol.interface, ArgumentInterface)
    assert (basis_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is basis_symbol
    assert len(basis_symbol.shape) == 4
    assert isinstance(basis_symbol.shape[0].upper, Literal)
    assert basis_symbol.shape[0].upper.value == "3"
    assert isinstance(basis_symbol.shape[1].upper, Reference)
    assert basis_symbol.shape[1].upper.symbol is ndf_symbol
    assert isinstance(basis_symbol.shape[2].upper, Reference)
    assert basis_symbol.shape[2].upper.symbol is nqp_symbol
    assert isinstance(basis_symbol.shape[3].upper, Reference)
    assert basis_symbol.shape[3].upper.symbol is nedges_symbol


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
    ndf_symbol = kernel_interface._symtab.lookup(f"ndf_{fs_name}")
    assert isinstance(ndf_symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp_xy declared
    nqph_symbol = kernel_interface._symtab.lookup("nqp_xy")
    assert isinstance(nqph_symbol,
                      LFRicTypes("NumberOfQrPointsInXyDataSymbol"))
    assert isinstance(nqph_symbol.interface, ArgumentInterface)
    assert (nqph_symbol.interface.access ==
            kernel_interface._read_access.access)
    # nqp_z declared
    nqpv_symbol = kernel_interface._symtab.lookup("nqp_z")
    assert isinstance(
        nqpv_symbol, LFRicTypes("NumberOfQrPointsInZDataSymbol"))
    assert isinstance(nqpv_symbol.interface, ArgumentInterface)
    assert (nqpv_symbol.interface.access ==
            kernel_interface._read_access.access)
    # diff basis declared and added to argument list
    diff_basis_symbol = kernel_interface._symtab.lookup(
        "diff_basis_w2_qr_xyoz")
    assert isinstance(
        diff_basis_symbol, LFRicTypes("DiffBasisFunctionQrXyozDataSymbol"))
    assert isinstance(diff_basis_symbol.interface, ArgumentInterface)
    assert (diff_basis_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is diff_basis_symbol
    assert len(diff_basis_symbol.shape) == 4
    assert isinstance(diff_basis_symbol.shape[0].upper, Literal)
    assert diff_basis_symbol.shape[0].upper.value == "1"
    assert isinstance(diff_basis_symbol.shape[1].upper, Reference)
    assert diff_basis_symbol.shape[1].upper.symbol is ndf_symbol
    assert isinstance(diff_basis_symbol.shape[2].upper, Reference)
    assert diff_basis_symbol.shape[2].upper.symbol is nqph_symbol
    assert isinstance(diff_basis_symbol.shape[3].upper, Reference)
    assert diff_basis_symbol.shape[3].upper.symbol is nqpv_symbol


def test_field_bcs_kernel(monkeypatch):
    '''Test that the KernelInterface class field_bcs_kernel method adds the
    expected symbols to the symbol table and the _arglist list.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "12.2_enforce_bc_kernel.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    kernel_interface = KernelInterface(kernel)
    kernel_interface.field_bcs_kernel(None)
    fld_name = kernel.arguments.args[0].name
    fspace = kernel.arguments.unique_fss[0]
    fs_name = fspace.orig_name
    # ndf declared
    ndf_symbol = kernel_interface._symtab.lookup(f"ndf_{fs_name}")
    assert isinstance(ndf_symbol, LFRicTypes("NumberOfDofsDataSymbol"))
    assert isinstance(ndf_symbol.interface, ArgumentInterface)
    assert (ndf_symbol.interface.access ==
            kernel_interface._read_access.access)
    # vertical-boundary dofs mask declared
    mask_sym = kernel_interface._symtab.lookup(f"boundary_dofs_{fld_name}")
    assert isinstance(mask_sym,
                      LFRicTypes("VerticalBoundaryDofMaskDataSymbol"))
    assert isinstance(mask_sym.interface, ArgumentInterface)
    assert mask_sym.interface.access == kernel_interface._read_access.access
    assert len(mask_sym.shape) == 2
    assert isinstance(mask_sym.shape[0].upper, Reference)
    assert mask_sym.shape[0].upper.symbol is ndf_symbol
    assert isinstance(mask_sym.shape[1].upper, Literal)
    assert mask_sym.shape[1].upper.value == "2"


def test_field_bcs_kernel_errors(monkeypatch):
    '''
    Test that the field_bcs_kernel method raises the expected errors if the
    kernel does not have exactly one argument that is itself a field on the
    'ANY_SPACE_1' function space.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1_single_invoke.f90"), api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    kernel_interface = KernelInterface(kernel)
    with pytest.raises(InternalError) as err:
        kernel_interface.field_bcs_kernel(None)
    assert ("Kernel 'testkern_code' applies boundary conditions to a field "
            "and therefore should have a single, field argument (one of "
            "['gh_field']) but got ['gh_scalar', 'gh_field'" in str(err.value))
    # Repeat for a kernel that *does* have an argument on ANY_SPACE_1.
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "12.2_enforce_bc_kernel.f90"), api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule[0].loop_body[0]
    # Monkeypatch the argument so that it appears to be on the wrong space.
    monkeypatch.setattr(
        kernel.arguments._args[0]._function_spaces[0],
        "_orig_name", "ANY_SPACE_2")
    kernel_interface = KernelInterface(kernel)
    with pytest.raises(InternalError) as err:
        kernel_interface.field_bcs_kernel(None)
    assert ("Kernel 'enforce_bc_code' applies boundary conditions to a field "
            "but the supplied argument, 'a', is on 'ANY_SPACE_2' rather than "
            "the expected 'ANY_SPACE_1'" in str(err.value))


@pytest.mark.xfail(reason="Issue #928: this callback is not yet implemented")
def test_operator_bcs_kernel():
    '''Test that the KernelInterface class operator_bcs_kernel method adds the
    expected symbols to the symbol table and the _arglist list.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.operator_bcs_kernel(None)


def test_ref_element_properties():
    '''Test that the KernelInterface class ref_element_properties method
    can be called successfully. This callback method does not
    contribute any kernel arguments so does nothing. As a basic test,
    we check that the number of symbols in the symbol table and the
    number of arguments in the argument list do not change.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.ref_element_properties()
    assert len(kernel_interface._symtab.symbols) == 0
    assert len(kernel_interface._arglist) == 0


def test_mesh_properties():
    '''Test that the KernelInterface class mesh_properties method can be
    called successfully. This callback method does not contribute any
    kernel arguments so does nothing. As a basic test, we check that
    the number of symbols in the symbol table and the number of
    arguments in the argument list do not change.

    '''
    kernel_interface = KernelInterface(None)
    kernel_interface.mesh_properties()
    assert len(kernel_interface._symtab.symbols) == 0
    assert len(kernel_interface._arglist) == 0


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

    # nqp_xy declared and added to argument list
    nqph_symbol = kernel_interface._symtab.lookup("nqp_xy")
    assert isinstance(
        nqph_symbol, LFRicTypes("NumberOfQrPointsInXyDataSymbol"))
    assert isinstance(nqph_symbol.interface, ArgumentInterface)
    assert (nqph_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-4] is nqph_symbol
    # nqp_z declared and added to argument list
    nqpv_symbol = kernel_interface._symtab.lookup("nqp_z")
    assert isinstance(
        nqpv_symbol, LFRicTypes("NumberOfQrPointsInZDataSymbol"))
    assert isinstance(nqpv_symbol.interface, ArgumentInterface)
    assert (nqpv_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-3] is nqpv_symbol
    # weights_xy declared and added to argument list
    weightsh_symbol = kernel_interface._symtab.lookup("weights_xy")
    assert isinstance(
        weightsh_symbol, LFRicTypes("QrWeightsInXyDataSymbol"))
    assert isinstance(weightsh_symbol.interface, ArgumentInterface)
    assert (weightsh_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-2] is weightsh_symbol
    assert len(weightsh_symbol.shape) == 1
    assert isinstance(weightsh_symbol.shape[0].upper, Reference)
    assert weightsh_symbol.shape[0].upper.symbol is nqph_symbol
    # weights_z declared and added to argument list
    weightsz_symbol = kernel_interface._symtab.lookup("weights_z")
    assert isinstance(
        weightsz_symbol, LFRicTypes("QrWeightsInZDataSymbol"))
    assert isinstance(weightsz_symbol.interface, ArgumentInterface)
    assert (weightsz_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is weightsz_symbol
    assert len(weightsz_symbol.shape) == 1
    assert isinstance(weightsz_symbol.shape[0].upper, Reference)
    assert weightsz_symbol.shape[0].upper.symbol is nqpv_symbol


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
    nfaces_symbol = kernel_interface._symtab.lookup("nfaces")
    assert isinstance(nfaces_symbol, LFRicTypes("NumberOfFacesDataSymbol"))
    assert isinstance(nfaces_symbol.interface, ArgumentInterface)
    assert (nfaces_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-3] is nfaces_symbol
    # nqp declared and added to argument list
    nqp_symbol = kernel_interface._symtab.lookup("nqp_faces")
    assert isinstance(
        nqp_symbol, LFRicTypes("NumberOfQrPointsInFacesDataSymbol"))
    assert isinstance(nqp_symbol.interface, ArgumentInterface)
    assert (nqp_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-2] is nqp_symbol
    # weights declared and added to argument list
    weights_symbol = kernel_interface._symtab.lookup("weights_faces")
    assert isinstance(weights_symbol,
                      LFRicTypes("QrWeightsInFacesDataSymbol"))
    assert isinstance(weights_symbol.interface, ArgumentInterface)
    assert (weights_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is weights_symbol
    assert len(weights_symbol.shape) == 1
    assert isinstance(weights_symbol.shape[0].upper, Reference)
    assert weights_symbol.shape[0].upper.symbol is nqp_symbol


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
    nedges_symbol = kernel_interface._symtab.lookup("nedges")
    assert isinstance(nedges_symbol, LFRicTypes("NumberOfEdgesDataSymbol"))
    assert isinstance(nedges_symbol.interface, ArgumentInterface)
    assert (nedges_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-3] is nedges_symbol
    # nqp declared and added to argument list
    nqp_symbol = kernel_interface._symtab.lookup("nqp_edges")
    assert isinstance(
        nqp_symbol, LFRicTypes("NumberOfQrPointsInEdgesDataSymbol"))
    assert isinstance(nqp_symbol.interface, ArgumentInterface)
    assert (nqp_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-2] is nqp_symbol
    # weights declared and added to argument list
    weights_symbol = kernel_interface._symtab.lookup("weights_edges")
    assert isinstance(weights_symbol,
                      LFRicTypes("QrWeightsInEdgesDataSymbol"))
    assert isinstance(weights_symbol.interface, ArgumentInterface)
    assert (weights_symbol.interface.access ==
            kernel_interface._read_access.access)
    assert kernel_interface._arglist[-1] is weights_symbol
    assert len(weights_symbol.shape) == 1
    assert isinstance(weights_symbol.shape[0].upper, Reference)
    assert weights_symbol.shape[0].upper.symbol is nqp_symbol


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
    assert ("Unsupported quadrature shape 'invalid_shape' found in "
            "kernel_interface." in str(info.value))


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
