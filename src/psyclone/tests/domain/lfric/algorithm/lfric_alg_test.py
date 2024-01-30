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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified by: R. W. Ford, STFC Daresbury Lab
#              L. Turner, Met Office

''' pytest tests for the LFRic-specific algorithm-generation functionality. '''

import os
import pytest

from fparser import api as fpapi
from psyclone.domain.lfric import KernCallInvokeArgList, LFRicKern
from psyclone.domain.lfric.algorithm.lfric_alg import LFRicAlg
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Container, Routine
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, UnresolvedType, DataTypeSymbol,
    ImportInterface, ArrayType, ScalarType, INTEGER_TYPE)
# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__))))),
    "test_files", "dynamo0p3")


@pytest.mark.usefixtures("parser")
@pytest.fixture(name="prog", scope="function")
def create_prog_fixture():
    '''
    :returns: a PSyIR Routine node representing a program.
    :rtype: :py:class:`psyclone.psyir.nodes.Routine`
    '''
    prog = Routine("test_prog", is_program=True)
    mesh_mod = prog.symbol_table.new_symbol("mesh_mod",
                                            symbol_type=ContainerSymbol)
    prog.symbol_table.new_symbol("mesh", symbol_type=DataSymbol,
                                 datatype=UnresolvedType(),
                                 interface=ImportInterface(mesh_mod))
    fs_mod = prog.symbol_table.new_symbol("function_space_mod",
                                          symbol_type=ContainerSymbol)
    prog.symbol_table.new_symbol("function_space_type", symbol_type=DataSymbol,
                                 datatype=UnresolvedType(),
                                 interface=ImportInterface(fs_mod))
    fsc_mod = prog.symbol_table.new_symbol("function_space_collection_mod",
                                           symbol_type=ContainerSymbol)
    prog.symbol_table.new_symbol("function_space_collection",
                                 symbol_type=DataTypeSymbol,
                                 datatype=UnresolvedType(),
                                 interface=ImportInterface(fsc_mod))
    return prog


@pytest.fixture(name="lfric_alg", scope="function")
def create_alg_fixture():
    '''
    :returns: an LFRicAlg instance.
    :rtype: :py:class:`psyclone.domain.lfric.algorithm.LFRicAlg`
    '''
    return LFRicAlg()


def test_create_alg_routine_wrong_arg_type(lfric_alg):
    '''
    Test that create_alg_routine() rejects arguments of the wrong type.
    '''
    with pytest.raises(TypeError) as err:
        lfric_alg.create_alg_routine(5)
    assert ("Supplied routine name must be a str but got 'int'" in
            str(err.value))


def test_create_alg_routine(lfric_alg, fortran_writer):
    ''' Test the correct operation of create_alg_routine(). '''
    psyir = lfric_alg.create_alg_routine("my_test_alg")
    assert isinstance(psyir, Container)

    # TODO #284 ideally we'd test that the generated code compiles but that
    # would require a full PSyclone pass and then compilation of the resulting
    # Algorithm and PSy code.
    gen = fortran_writer(psyir).lower()

    assert "subroutine my_test_alg(mesh, chi, panel_id)" in gen
    assert "use field_mod, only : field_type" in gen
    assert "use function_space_mod, only : function_space_type" in gen
    assert ("use function_space_collection_mod, only : "
            "function_space_collection\n" in gen)
    assert "use mesh_mod, only : mesh_type" in gen
    assert "type(mesh_type), pointer, intent(in) :: mesh" in gen
    assert "type(field_type), dimension(3), intent(in), optional :: chi" in gen
    assert "type(field_type), intent(in), optional :: panel_id" in gen


def test_create_function_spaces_no_spaces(lfric_alg, prog):
    ''' Check that a Routine is populated as expected, even when there
    are no actual function spaces. '''
    lfric_alg._create_function_spaces(prog, [])
    fe_config_mod = prog.symbol_table.lookup("finite_element_config_mod")
    element_order = prog.symbol_table.lookup("element_order")
    assert element_order.interface.container_symbol == fe_config_mod
    assert prog.symbol_table.lookup("element_order")
    assert isinstance(prog.symbol_table.lookup("fs_continuity_mod"),
                      ContainerSymbol)


def test_create_function_spaces_invalid_space(lfric_alg, prog):
    ''' Check that the expected error is raised if an invalid function-space
    name is supplied. '''
    with pytest.raises(InternalError) as err:
        lfric_alg._create_function_spaces(prog, ["wwrong", "w1"])
    assert ("Function space 'wwrong' is not a valid LFRic function space "
            "(one of [" in str(err.value))


def test_create_function_spaces(lfric_alg, prog, fortran_writer):
    ''' Check that a Routine is populated correctly when valid function-space
    names are supplied. '''
    lfric_alg._create_function_spaces(prog, ["w3", "w1"])
    fe_config_mod = prog.symbol_table.lookup("finite_element_config_mod")
    element_order = prog.symbol_table.lookup("element_order")
    assert element_order.interface.container_symbol == fe_config_mod
    fs_mod_sym = prog.symbol_table.lookup("fs_continuity_mod")
    gen = fortran_writer(prog)
    for space in ["w1", "w3"]:
        sym = prog.symbol_table.lookup(space)
        assert sym.interface.container_symbol is fs_mod_sym
        assert (f"TYPE(function_space_type), POINTER :: "
                f"vector_space_{space}_ptr" in gen)
        assert (f"vector_space_{space}_ptr => function_space_collection % "
                f"get_fs(mesh, element_order, {space})" in gen)


def test_initialise_field(lfric_alg, prog, fortran_writer):
    ''' Test that the initialise_field() function works as expected for both
    individual fields and field vectors. '''
    table = prog.symbol_table
    fmod = table.new_symbol("field_mod", symbol_type=ContainerSymbol)
    ftype = table.new_symbol("field_type", symbol_type=DataTypeSymbol,
                             datatype=UnresolvedType(),
                             interface=ImportInterface(fmod))
    # Add symbols for the necessary function spaces but for simplicity
    # make them of integer type.
    table.new_symbol("vector_space_w3_ptr", symbol_type=DataSymbol,
                     datatype=INTEGER_TYPE)
    table.new_symbol("vector_space_w2_ptr", symbol_type=DataSymbol,
                     datatype=INTEGER_TYPE)
    # First - a single field argument.
    sym = table.new_symbol("field1", symbol_type=DataSymbol, datatype=ftype)
    lfric_alg.initialise_field(prog, sym, "w3")
    gen = fortran_writer(prog)
    assert ("call field1 % initialise(vector_space=vector_space_w3_ptr, "
            "name='field1')" in gen)
    # Second - a field vector.
    dtype = ArrayType(ftype, [3])
    sym = table.new_symbol("fieldv2", symbol_type=DataSymbol, datatype=dtype)
    lfric_alg.initialise_field(prog, sym, "w2")
    gen = fortran_writer(prog)
    for idx in range(1, 4):
        assert (f"call fieldv2({idx}_i_def) % initialise(vector_space="
                f"vector_space_w2_ptr, name='fieldv2')" in gen)
    # Third - invalid type.
    sym._datatype = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    with pytest.raises(InternalError) as err:
        lfric_alg.initialise_field(prog, sym, "w2")
    assert ("Expected a field symbol to either be of ArrayType or have a type "
            "specified by a DataTypeSymbol but found Scalar" in str(err.value))


def test_initialise_quadrature(lfric_alg, prog, fortran_writer):
    ''' Tests for the initialise_quadrature function with the supported
    XYoZ shape. '''
    table = prog.symbol_table
    table.new_symbol("element_order", tag="element_order",
                     symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    # Setup symbols that would normally be created in KernCallInvokeArgList.
    quad_container = table.new_symbol(
        "quadrature_xyoz_mod", symbol_type=ContainerSymbol)
    quad_type = table.new_symbol(
        "quadrature_xyoz_type", symbol_type=DataTypeSymbol,
        datatype=UnresolvedType(), interface=ImportInterface(quad_container))
    sym = table.new_symbol("qr", symbol_type=DataSymbol, datatype=quad_type)

    lfric_alg.initialise_quadrature(prog, sym, "gh_quadrature_xyoz")
    # Check that new symbols have been added.
    assert table.lookup("quadrature_rule_gaussian_mod")
    qtype = table.lookup("quadrature_rule_gaussian_type")
    qrule = table.lookup("quadrature_rule")
    assert qrule.datatype is qtype
    # Check that the constructor is called in the generated code.
    gen = fortran_writer(prog)
    assert ("qr = quadrature_xyoz_type(element_order + 3,quadrature_rule)"
            in gen)


def test_initialise_quadrature_unsupported_shape(lfric_alg, prog):
    ''' Test that the initialise_quadrature function raises the expected error
    for an unsupported quadrature shape. '''
    table = prog.symbol_table
    table.new_symbol("element_order", tag="element_order",
                     symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    # Setup symbols that would normally be created in KernCallInvokeArgList.
    quad_container = table.new_symbol(
        "quadrature_xyz_mod", symbol_type=ContainerSymbol)
    quad_type = table.new_symbol(
        "quadrature_xyz_type", symbol_type=DataTypeSymbol,
        datatype=UnresolvedType(), interface=ImportInterface(quad_container))
    sym = table.new_symbol("qr", symbol_type=DataSymbol, datatype=quad_type)

    with pytest.raises(NotImplementedError) as err:
        lfric_alg.initialise_quadrature(prog, sym, "gh_quadrature_xyz")
    assert ("Initialisation for quadrature of type 'gh_quadrature_xyz' is "
            "not yet implemented." in str(err.value))


def test_kernel_from_metadata(lfric_alg):
    '''
    Tests for the kernel_from_metadata() utility.

    '''
    # Invalid parse tree should raise an error.
    with pytest.raises(ValueError) as err:
        lfric_alg.kernel_from_metadata("not fortran", "john")
    assert ("Failed to find kernel 'john' in supplied code: 'not fortran'. "
            "Is it a valid LFRic kernel? Original error was 'Parse Error: "
            "Kernel type john does not exist'." in str(err.value))
    code = '''\
module testkern_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_type
     type(arg_type), dimension(2) :: meta_args =        &
          (/ arg_type(gh_scalar, gh_real, gh_read),     &
             arg_type(gh_field,  gh_real, gh_inc,  w1)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_code
  end type testkern_type

contains

  subroutine testkern_code()
  end subroutine testkern_code
end module testkern_mod
'''
    # Valid parse tree but wrong name.
    ptree = fpapi.parse(code)
    with pytest.raises(ValueError) as err:
        lfric_alg.kernel_from_metadata(ptree, "john")
    assert "Failed to find kernel 'john' in supplied code: '" in str(err.value)
    assert ("Is it a valid LFRic kernel? Original error was 'Parse Error: "
            "Kernel type john does not exist'." in str(err.value))
    # Valid parse tree and correct name.
    kern = lfric_alg.kernel_from_metadata(ptree, "testkern_type")
    assert isinstance(kern, LFRicKern)


def test_construct_kernel_args(lfric_alg, prog, lfrickern, fortran_writer):
    ''' Tests for the construct_kernel_args() function. Since this function
    primarily calls _create_function_spaces(), initialise_field(),
    KernCallInvokeArgList.generate() and initialise_quadrature(), all of which
    have their own tests, there isn't a lot to test here. '''
    field_mod = prog.symbol_table.new_symbol("field_mod",
                                             symbol_type=ContainerSymbol)
    prog.symbol_table.new_symbol("field_type", symbol_type=DataTypeSymbol,
                                 datatype=UnresolvedType(),
                                 interface=ImportInterface(field_mod))
    kargs = lfric_alg.construct_kernel_args(prog, lfrickern)

    assert isinstance(kargs, KernCallInvokeArgList)
    gen = fortran_writer(prog)
    spaces = ["w0", "w1", "w2", "w3", "wtheta"]
    assert f"use fs_continuity_mod, only : {', '.join(spaces)}" in gen

    for space in spaces:
        assert (f"vector_space_{space}_ptr => function_space_collection % "
                f"get_fs(mesh, element_order, {space})" in gen)
    for idx in range(2, 7):
        assert f"call field_{idx}" in gen
    assert ("qr_xyoz = quadrature_xyoz_type(element_order + 3,"
            "quadrature_rule)" in gen)
    # TODO #240 - test for compilation.


def test_create_from_kernel_invalid_kernel(lfric_alg, tmpdir):
    ''' Check that the create_from_kernel() function raises
    NotImplementedError if the supplied kernel file does not follow LFRic
    naming conventions. '''
    kern_file = os.path.join(tmpdir, "fake_kern.f90")
    with open(kern_file, "w", encoding='utf-8') as ffile:
        print('''module my_mod_wrong
end module my_mod_wrong''', file=ffile)
    with pytest.raises(NotImplementedError) as err:
        lfric_alg.create_from_kernel("test", kern_file)
    assert ("fake_kern.f90) contains a module named 'my_mod_wrong' which does "
            "not follow " in str(err.value))


def test_create_from_kernel_invalid_field_type(lfric_alg, monkeypatch):
    ''' Check that we get the expected internal error if a field object of
    the wrong type is encountered. '''
    # This requires that we monkeypatch the KernCallInvokeArgList class so
    # that it returns an invalid field symbol.
    monkeypatch.setattr(KernCallInvokeArgList, "fields",
                        [(DataSymbol("fld", UnresolvedType()), None)])
    with pytest.raises(InternalError) as err:
        lfric_alg.create_from_kernel("test", os.path.join(BASE_PATH,
                                                          "testkern_mod.F90"))
    assert ("field symbol to either be of ArrayType or have a type specified "
            "by a DataTypeSymbol but found UnresolvedType for field 'fld'" in
            str(err.value))


def test_create_from_kernel_with_scalar(lfric_alg, fortran_writer):
    ''' Check that create_from_kernel() returns the expected Fortran for a
    valid LFRic kernel that has a scalar argument. '''
    psyir = lfric_alg.create_from_kernel("test",
                                         os.path.join(BASE_PATH,
                                                      "testkern_mod.F90"))
    code = fortran_writer(psyir)
    assert "module test_mod" in code
    assert "use constants_mod, only : i_def, r_def" in code
    assert "real(kind=r_def) :: rscalar_1" in code
    assert ("    rscalar_1 = 1_i_def\n"
            "    call invoke(setval_c(field_2, 1.0_r_def), "
            "setval_c(field_3, 1.0_r_def), "
            "setval_c(field_4, 1.0_r_def), "
            "setval_c(field_5, 1.0_r_def), "
            "testkern_type(rscalar_1, field_2, field_3, field_4, field_5))\n"
            in code)


def test_create_from_kernel_with_vector(lfric_alg, fortran_writer):
    ''' Test that create_from_kernel() returns the expected Fortran for a
    valid LFRic kernel that takes a field vector. '''
    psyir = lfric_alg.create_from_kernel(
        "test",
        os.path.join(BASE_PATH,
                     "testkern_coord_w0_mod.f90"))
    code = fortran_writer(psyir)
    assert "use constants_mod, only : i_def, r_def" in code
    assert '''\
    type(field_type) :: field_1
    type(field_type), dimension(3) :: field_2
    type(field_type) :: field_3
''' in code

    assert ("    call field_1 % initialise(vector_space=vector_space_w0_ptr, "
            "name='field_1')\n"
            "    call field_2(1_i_def) % initialise(vector_space="
            "vector_space_w0_ptr, name='field_2')\n"
            "    call field_2(2_i_def) % initialise(vector_space="
            "vector_space_w0_ptr, name='field_2')\n"
            "    call field_2(3_i_def) % initialise(vector_space="
            "vector_space_w0_ptr, name='field_2')\n"
            "    call field_3 % initialise(vector_space=vector_space_w0_ptr, "
            "name='field_3')\n"
            "    call invoke(setval_c(field_1, 1.0_r_def), "
            "setval_c(field_2, 1.0_r_def), "
            "setval_c(field_3, 1.0_r_def), "
            "testkern_coord_w0_type(field_1, field_2, field_3))\n"
            in code)
