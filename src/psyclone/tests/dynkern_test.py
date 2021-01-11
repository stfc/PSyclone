# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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

'''This module tests the DynKern class within dynamo0p3 using
pytest. A the moment the tests here do not fully cover DynKern as
tests for other classes end up covering the rest.'''

# pylint: disable=no-name-in-module

from __future__ import absolute_import
import os
import pytest

from fparser import api as fpapi

import psyclone
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.dynamo0p3 import DynKernMetadata, DynKern
from psyclone.errors import InternalError, GenerationError
from psyclone.domain.lfric import LFRicArgDescriptor
from psyclone.psyir.symbols import ArgumentInterface, DataSymbol, REAL_TYPE, \
    INTEGER_TYPE
from psyclone.psyir.nodes import KernelSchedule, Reference
from psyclone.domain.lfric.psyir import LfricRealScalarDataSymbol, \
    RealFieldDataDataSymbol, LfricIntegerScalarDataSymbol, \
    NumberOfUniqueDofsDataSymbol
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), meta_args(6) =                   &
          (/ arg_type(gh_scalar, gh_real, gh_read),   &
             arg_type(gh_field, gh_inc, w1),          &
             arg_type(gh_field, gh_read, w2),         &
             arg_type(gh_operator, gh_read, w2, w2),  &
             arg_type(gh_field, gh_read, w3),         &
             arg_type(gh_scalar, gh_integer, gh_read) &
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
    assert ("DynKern.load_meta(): expected one of {0} data types for "
            "a scalar argument but found 'gh_triple'.".
            format(LFRicArgDescriptor.VALID_SCALAR_DATA_TYPES) in
            str(err.value))


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

    kernel.validate_kernel_code_args()

    # Force DynKern to think that this kernel is an 'apply' kernel and
    # therefore does not need the mesh height argument.
    monkeypatch.setattr(kernel, "_cma_operation", "apply")
    with pytest.raises(GenerationError) as info:
        kernel.validate_kernel_code_args()
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
    lfric_real_scalar_symbol = LfricRealScalarDataSymbol(
        "scalar", interface=read_access)
    lfric_int_scalar_symbol = LfricIntegerScalarDataSymbol(
        "scalar", interface=read_access)
    lfric_real_field_symbol = RealFieldDataDataSymbol(
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
    assert ("Kernel argument 'generic_real_scalar' has precision 'UNDEFINED' "
            "in kernel 'dummy' but the LFRic API expects 'r_def'."
            in str(info.value))

    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(real_scalar_symbol,
                                         real_scalar_rw_symbol)
    assert ("Kernel argument 'generic_real_scalar' has intent 'READ' in "
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

    undf = NumberOfUniqueDofsDataSymbol("undf", fs="w0", interface=read_access)
    lfric_real_field_symbol2 = RealFieldDataDataSymbol(
        "field", dims=[Reference(undf)], fs="w0", interface=read_access)
    # if one of the dimensions is not a datasymbol then the arguments
    # are not checked.
    kernel._validate_kernel_code_arg(lfric_real_field_symbol,
                                     lfric_real_field_symbol2)
    kernel._validate_kernel_code_arg(lfric_real_field_symbol2,
                                     lfric_real_field_symbol)

    lfric_real_field_symbol3 = RealFieldDataDataSymbol(
        "field", dims=[Reference(undf)], fs="w0", interface=read_access)
    monkeypatch.setattr(lfric_real_field_symbol3.datatype, "_shape",
                        [Reference(undf), Reference(undf)])
    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(lfric_real_field_symbol2,
                                         lfric_real_field_symbol3)
    assert ("Argument 'field' to kernel 'dummy' should be an array with 2 "
            "dimension(s) according to the LFRic API, but found 1."
            in str(info.value))

    lfric_real_field_symbol4 = RealFieldDataDataSymbol(
        "field", dims=[Reference(int_scalar_symbol)], fs="w0",
        interface=read_access)
    with pytest.raises(GenerationError) as info:
        kernel._validate_kernel_code_arg(
            lfric_real_field_symbol4, lfric_real_field_symbol2)
    assert (
        "For dimension 1 in array argument 'field' to kernel 'dummy' the "
        "following error was found: Kernel argument 'generic_int_scalar' "
        "has precision 'UNDEFINED' in kernel 'dummy' but the LFRic API "
        "expects 'i_def'" in str(info.value))

    # monkeypatch lfric_real_scalar_symbol to return that it is not a
    # scalar in order to force the required exception. We do this by
    # changing the ScalarType as it is used when determining whether
    # the symbol is a scalar.
    monkeypatch.setattr(psyclone.psyir.symbols, "ScalarType", str)
    with pytest.raises(InternalError) as info:
        kernel._validate_kernel_code_arg(
            lfric_real_scalar_symbol, lfric_real_scalar_symbol)
    assert (
        "unexpected argument type found for 'scalar' in kernel 'dummy'. "
        "Expecting a scalar or an array." in str(info.value))
