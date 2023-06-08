# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2023, Science and Technology Facilities Council.
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
# Author: R. W. Ford STFC Daresbury Lab
# Modified: I. Kavcic Met Office
#           J. Henrichs, Bureau of Meteorology
#           A. R. Porter, STFC Daresbury Laboratory

'''This module tests the DynKern class within dynamo0p3 using
pytest. At the moment the tests here do not fully cover DynKern as
tests for other classes end up covering the rest.'''

import os
import pytest

from fparser import api as fpapi

import psyclone
from psyclone.core import AccessType
from psyclone.domain.lfric import LFRicConstants, LFRicTypes
from psyclone.dynamo0p3 import DynKernMetadata, DynKern, DynLoop
from psyclone.errors import InternalError, GenerationError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Reference, KernelSchedule
from psyclone.psyir.symbols import ArgumentInterface, DataSymbol, REAL_TYPE, \
    INTEGER_TYPE, ArrayType
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import Dynamo0p3ColourTrans

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), meta_args(6) =                              &
          (/ arg_type(gh_scalar,   gh_real,    gh_read),         &
             arg_type(gh_field,    gh_real,    gh_inc, w1),      &
             arg_type(gh_field,    gh_real,    gh_read, w2),     &
             arg_type(gh_operator, gh_real,    gh_read, w2, w2), &
             arg_type(gh_field,    gh_real,    gh_read, w3),     &
             arg_type(gh_scalar,   gh_integer, gh_read)          &
           /)
     type(func_type), dimension(3) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w2, gh_diff_basis),          &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_qr_code
  end type testkern_qr_type
contains
  subroutine testkern_qr_code(a, b, c, d)
  end subroutine testkern_qr_code
end module testkern_qr
'''


def test_scalar_kernel_load_meta_err():
    ''' Check that the DynKern.load_meta() method raises the expected
    internal error if it encounters an unrecognised data type for
    a scalar descriptor.

    '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    kernel = DynKern()
    # Get a scalar argument descriptor and set an invalid data type
    scalar_arg = metadata.arg_descriptors[5]
    scalar_arg._data_type = "gh_triple"
    with pytest.raises(InternalError) as err:
        kernel.load_meta(metadata)
    const = LFRicConstants()
    assert (f"Expected one of {const.VALID_SCALAR_DATA_TYPES} data types for "
            f"a scalar argument but found 'gh_triple'." in str(err.value))


def test_kern_colourmap(monkeypatch):
    ''' Tests for error conditions in the colourmap getter of DynKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    kern = psy.invokes.invoke_list[0].schedule.children[4].loop_body[0]
    with pytest.raises(InternalError) as err:
        _ = kern.colourmap
    assert ("Kernel 'testkern_code' is not inside a coloured loop"
            in str(err.value))
    monkeypatch.setattr(kern, "is_coloured", lambda: True)
    monkeypatch.setattr(kern, "_is_intergrid", True)
    with pytest.raises(InternalError) as err:
        _ = kern.colourmap
    assert ("Colourmap information for kernel 'testkern_code' has not yet "
            "been initialised" in str(err.value))


def test_kern_ncolours(monkeypatch):
    ''' Tests for error conditions in the ncolours getter of DynKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    kern = psy.invokes.invoke_list[0].schedule.children[4].loop_body[0]
    with pytest.raises(InternalError) as err:
        _ = kern.ncolours_var
    assert ("Kernel 'testkern_code' is not inside a coloured loop"
            in str(err.value))
    monkeypatch.setattr(kern, "is_coloured", lambda: True)
    monkeypatch.setattr(kern, "_is_intergrid", True)
    with pytest.raises(InternalError) as err:
        _ = kern.ncolours_var
    assert ("Colourmap information for kernel 'testkern_code' has not yet "
            "been initialised" in str(err.value))


def test_get_kernel_schedule():
    '''Test that a PSyIR kernel schedule is created by get_kernel_schedule
    if one does not exist and that the same kernel schedule is
    returned if one has already been created.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "12_kernel_specific.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # matrix vector kernel
    kernel = schedule[2].loop_body[0]

    assert kernel._kern_schedule is None

    kernel_schedule = kernel.get_kernel_schedule()
    assert isinstance(kernel_schedule, KernelSchedule)
    assert kernel._kern_schedule is kernel_schedule

    kernel_schedule_2 = kernel.get_kernel_schedule()
    assert kernel_schedule is kernel_schedule_2


def test_get_kernel_schedule_mixed_precision():
    '''
    Test that we can get the correct schedule for a mixed-precision kernel.

    '''
    _, invoke = get_invoke("26.8_mixed_precision_args.f90", TEST_API,
                           name="invoke_0", dist_mem=False)
    sched = invoke.schedule
    kernels = sched.walk(DynKern, stop_type=DynKern)
    # 26.8 contains an invoke of five kernels, one each at the following
    # precisions.
    kernel_precisions = ["r_def", "r_solver", "r_tran", "r_bl", "r_phys"]
    # Get the precision (in bytes) for each of these.
    precisions = [LFRicConstants.PRECISION_MAP[name] for
                  name in kernel_precisions]
    # Check that the correct kernel implementation is obtained for each
    # one in the invoke.
    for precision, kern in zip(precisions, kernels):
        sched = kern.get_kernel_schedule()
        assert isinstance(sched, KernelSchedule)
        assert sched.name == f"mixed_code_{8*precision}"


def test_get_kernel_sched_mixed_precision_no_match(monkeypatch):
    '''
    Test that we get the expected error if there's no matching implementation
    for a mixed-precision kernel.

    '''
    _, invoke = get_invoke("26.8_mixed_precision_args.f90", TEST_API,
                           name="invoke_0", dist_mem=False)
    sched = invoke.schedule
    kernels = sched.walk(DynKern, stop_type=DynKern)

    # To simplify things we just monkeypatch the 'validate_kernel_code_args'
    # method so that it never succeeds.
    def fake_validate(_1, _2):
        raise GenerationError("Just a test")

    monkeypatch.setattr(DynKern, "validate_kernel_code_args",
                        fake_validate)
    with pytest.raises(GenerationError) as err:
        _ = kernels[0].get_kernel_schedule()
    assert ("Failed to find a kernel implementation with an interface that "
            "matches the invoke of 'mixed_code'. (Tried routines "
            "['mixed_code_32', 'mixed_code_64'].)" in str(err.value))


def test_validate_kernel_code_args(monkeypatch):
    '''Test that a coded kernel that conforms to the expected kernel
    metadadata is validated successfully. Also check that the
    appropriate exception is raised if the number of arguments in the
    coded kernel does not match the number of arguments expected by
    the kernel metadata.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "12_kernel_specific.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # matrix vector kernel
    kernel = schedule[2].loop_body[0]
    sched = kernel.get_kernel_schedule()
    kernel.validate_kernel_code_args(sched.symbol_table)

    # Force DynKern to think that this kernel is an 'apply' kernel and
    # therefore does not need the mesh height argument.
    monkeypatch.setattr(kernel, "_cma_operation", "apply")
    with pytest.raises(GenerationError) as info:
        kernel.validate_kernel_code_args(
            kernel.get_kernel_schedule().symbol_table)
    assert (
        "In kernel 'matrix_vector_code' the number of arguments indicated by "
        "the kernel metadata is 8 but the actual number of kernel arguments "
        "found is 9." in str(info.value))


def test_validate_kernel_code_arg(monkeypatch):
    '''Test that this method returns successfully if its two arguments
    have identical content, otherwise test that the expected
    exceptions are raised.

    '''
    kernel = DynKern()
    # Kernel name needs to be set when testing exceptions.
    kernel._name = "dummy"
    read_access = ArgumentInterface(ArgumentInterface.Access.READ)

    real_scalar_symbol = DataSymbol(
        "generic_real_scalar", REAL_TYPE, interface=read_access)
    int_scalar_symbol = DataSymbol(
        "generic_int_scalar", INTEGER_TYPE, interface=read_access)
    real_scalar_rw_symbol = DataSymbol(
        "generic_scalar_rw", REAL_TYPE,
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
    lfric_real_scalar_symbol = LFRicTypes("LFRicRealScalarDataSymbol")(
        "scalar", interface=read_access)
    lfric_int_scalar_symbol = LFRicTypes("LFRicIntegerScalarDataSymbol")(
        "scalar", interface=read_access)
    lfric_real_field_symbol = LFRicTypes("RealFieldDataSymbol")(
        "field", dims=[1], fs="w0", interface=read_access)

    kernel._validate_kernel_code_arg(
        lfric_real_scalar_symbol, lfric_real_scalar_symbol)

    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(
            lfric_real_scalar_symbol, lfric_int_scalar_symbol)
    assert (
        "Kernel argument 'scalar' has datatype 'Intrinsic.REAL' in kernel "
        "'dummy' but the LFRic API expects 'Intrinsic.INTEGER'"
        in str(info.value))

    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(
            real_scalar_symbol, lfric_real_scalar_symbol)
    assert ("but argument 'generic_real_scalar' to kernel 'dummy' has "
            "precision Precision.UNDEFINED" in str(info.value))

    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(lfric_real_scalar_symbol,
                                         real_scalar_rw_symbol)
    assert ("Kernel argument 'scalar' has intent 'READ' in "
            "kernel 'dummy' but the LFRic API expects intent "
            "'READWRITE'." in str(info.value))

    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(lfric_real_field_symbol,
                                         lfric_real_scalar_symbol)
    assert ("Argument 'field' to kernel 'dummy' should be a scalar "
            "according to the LFRic API, but it is not." in str(info.value))

    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(lfric_real_scalar_symbol,
                                         lfric_real_field_symbol)
    assert ("Argument 'scalar' to kernel 'dummy' should be an array "
            "according to the LFRic API, but it is not." in str(info.value))

    undf = LFRicTypes("NumberOfUniqueDofsDataSymbol")("undf", fs="w0",
                                                      interface=read_access)
    lfric_real_field_symbol2 = LFRicTypes("RealFieldDataSymbol")(
        "field", dims=[Reference(undf)], fs="w0", interface=read_access)
    # if one of the dimensions is not a datasymbol then the arguments
    # are not checked.
    kernel._validate_kernel_code_arg(lfric_real_field_symbol,
                                     lfric_real_field_symbol2)
    kernel._validate_kernel_code_arg(lfric_real_field_symbol2,
                                     lfric_real_field_symbol)

    lfric_real_field_symbol3 = LFRicTypes("RealFieldDataSymbol")(
        "field", dims=[Reference(undf)], fs="w0", interface=read_access)
    monkeypatch.setattr(lfric_real_field_symbol3.datatype, "_shape",
                        [Reference(undf), Reference(undf)])
    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(lfric_real_field_symbol2,
                                         lfric_real_field_symbol3)
    assert ("Argument 'field' to kernel 'dummy' should be an array with 2 "
            "dimension(s) according to the LFRic API, but found 1."
            in str(info.value))

    # Lower array bound of 2 rather than 1
    monkeypatch.setattr(lfric_real_field_symbol3.datatype, "_shape",
                        [ArrayType.ArrayBounds(2, Reference(undf))])
    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(lfric_real_field_symbol3,
                                         lfric_real_field_symbol3)
    assert ("All array arguments to LFRic kernels must have lower bounds of 1 "
            "for all dimensions. However, array 'field' has a lower bound of "
            "'2' for dimension 0" in str(info.value))

    lfric_real_field_symbol4 = LFRicTypes("RealFieldDataSymbol")(
        "field", dims=[Reference(int_scalar_symbol)], fs="w0",
        interface=read_access)
    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(
            lfric_real_field_symbol4, lfric_real_field_symbol2)
    assert (
        "For dimension 1 in array argument 'field' to kernel 'dummy' the "
        "following error was found: An argument to an LFRic kernel must have a"
        " precision defined by either a recognised LFRic type parameter (one "
        "of ['i_def', 'l_def', 'r_bl', 'r_def', 'r_double', 'r_ncdf', "
        "'r_phys', 'r_quad', 'r_second', 'r_single', 'r_solver', 'r_tran', "
        "'r_um']) or an integer number of bytes but argument "
        "'generic_int_scalar' to kernel 'dummy' has precision "
        "Precision.UNDEFINED" in str(info.value))

    # monkeypatch lfric_real_scalar_symbol to return that it is not a
    # scalar in order to force the required exception. We do this by
    # changing the ScalarType as it is used when determining whether
    # the symbol is a scalar.
    monkeypatch.setattr(psyclone.psyir.symbols.datatypes, "ScalarType", str)
    with pytest.raises(InternalError) as info:
        kernel._validate_kernel_code_arg(
            lfric_real_scalar_symbol, lfric_real_scalar_symbol)
    assert (
        "unexpected argument type found for 'scalar' in kernel 'dummy'. "
        "Expecting a scalar or an array." in str(info.value))


def test_kern_last_cell_all_colours_errors(monkeypatch):
    ''' Tests for the checks in the last_cell_all_colours property
    of DynKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    kern = sched.walk(DynKern)[0]
    # Kernel is not coloured.
    with pytest.raises(InternalError) as err:
        _ = kern.last_cell_all_colours_symbol
    assert "'testkern_code' is not inside a coloured loop" in str(err.value)
    # Monkeypatch the Kernel so that it appears to be coloured.
    monkeypatch.setattr(kern, "is_coloured", lambda: True)
    kern._is_intergrid = True
    with pytest.raises(InternalError) as err:
        _ = kern.last_cell_all_colours_symbol
    assert ("Colourmap information for kernel 'testkern_code' has not yet "
            "been initialised" in str(err.value))


def test_kern_last_cell_all_colours():
    ''' Tests for the last_cell_all_colours property of DynKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loop = sched.walk(DynLoop)[0]
    # Apply a colouring transformation to the loop.
    trans = Dynamo0p3ColourTrans()
    trans.apply(loop)
    # We have to perform code generation as that sets-up the symbol table.
    # pylint:disable=pointless-statement
    psy.gen
    assert (loop.kernel.last_cell_all_colours_symbol.name
            == "last_halo_cell_all_colours")


def test_kern_last_cell_all_colours_intergrid():
    ''' Test the last_cell_all_colours property for an inter-grid DynKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.1_intergrid_restrict.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loop = sched.walk(DynLoop)[0]
    # Apply a colouring transformation to the loop.
    trans = Dynamo0p3ColourTrans()
    trans.apply(loop)
    # We have to perform code generation as that sets-up the symbol table.
    # pylint:disable=pointless-statement
    psy.gen
    assert (loop.kernel.last_cell_all_colours_symbol.name ==
            "last_edge_cell_all_colours_field1")


def test_kern_all_updates_are_writes():
    ''' Tests for the 'all_updates_are_writes' property of DynKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loop = sched.walk(DynLoop)[0]
    # The only argument updated by this kernel has GH_INC access.
    assert not loop.kernel.all_updates_are_writes
    # Patch the kernel so that a different argument has GH_WRITE access.
    loop.kernel.args[2]._access = AccessType.WRITE
    # There is still a GH_INC argument.
    assert not loop.kernel.all_updates_are_writes
    # Change the GH_INC to be GH_WRITE.
    loop.kernel.args[1]._access = AccessType.WRITE
    assert loop.kernel.all_updates_are_writes
