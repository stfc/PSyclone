# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Modified: I. Kavcic, L. Turner and O. Brunt, Met Office
#           J. Henrichs, Bureau of Meteorology
#           A. R. Porter, STFC Daresbury Laboratory

'''This module tests the LFRicKern class within LFRic using
pytest. At the moment the tests here do not fully cover LFRicKern as
tests for other classes end up covering the rest.'''

import os
import pytest

from fparser import api as fpapi

import psyclone
from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.lfric import (LFRicConstants, LFRicTypes, LFRicKern,
                                   LFRicKernMetadata, LFRicLoop)
from psyclone.errors import InternalError, GenerationError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Container, KernelSchedule, Reference, Routine
from psyclone.psyir.symbols import (
    ArgumentInterface, ArrayType, DataSymbol, GenericInterfaceSymbol,
    INTEGER_TYPE, REAL_TYPE)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import LFRicColourTrans
from psyclone.psyir.backend.visitor import VisitorError

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
                os.path.abspath(__file__)))), "test_files", "lfric")
TEST_API = "lfric"

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
    ''' Check that the LFRicKern.load_meta() method raises the expected
    internal error if it encounters an unrecognised data type for
    a scalar descriptor.

    '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = LFRicKernMetadata(ast, name=name)
    kernel = LFRicKern()
    # Get a scalar argument descriptor and set an invalid data type
    scalar_arg = metadata.arg_descriptors[5]
    scalar_arg._data_type = "gh_triple"
    with pytest.raises(InternalError) as err:
        kernel.load_meta(metadata)
    const = LFRicConstants()
    assert (f"Expected one of {const.VALID_SCALAR_DATA_TYPES} data types for "
            f"a scalar argument but found 'gh_triple'." in str(err.value))


def test_kern_getter_errors():
    ''' Tests for error conditions in the getter properties of LFRicKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    kern = psy.invokes.invoke_list[0].schedule.children[4].loop_body[0]
    with pytest.raises(InternalError) as err:
        _ = kern.colourmap
    assert ("Kernel 'testkern_code' is not inside a coloured loop"
            in str(err.value))
    with pytest.raises(InternalError) as err:
        _ = kern.ncolours_var
    assert ("Kernel 'testkern_code' is not inside a coloured loop"
            in str(err.value))
    with pytest.raises(InternalError) as err:
        _ = kern.tilecolourmap
    assert ("Kernel 'testkern_code' is not inside a coloured loop"
            in str(err.value))
    with pytest.raises(InternalError) as err:
        _ = kern.ntilecolours_var
    assert ("Kernel 'testkern_code' is not inside a coloured loop"
            in str(err.value))


def test_kern_get_callees(monkeypatch):
    '''Test that a PSyIR kernel schedule is created by get_callees
    if one does not exist and that the same kernel schedule is
    returned if one has already been created.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "12_kernel_specific.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # matrix vector kernel
    kernel = schedule[2].loop_body[0]

    assert kernel._schedules is None

    kernel_schedules = kernel.get_callees()
    assert len(kernel_schedules) == 1
    assert isinstance(kernel_schedules[0], KernelSchedule)
    assert kernel._schedules[0] is kernel_schedules[0]
    # Not a polymorphic kernel so has no interface symbol
    assert kernel.get_interface_symbol() is None

    kernel_schedules_2 = kernel.get_callees()
    assert kernel_schedules[0] is kernel_schedules_2[0]
    # Check the internal error for the case where we fail to get any
    # implementation for the kernel.
    kernel._schedules = None
    # Monkeypatch the frontend so that it just returns an empty Container.
    monkeypatch.setattr(Fparser2Reader, "generate_psyir",
                        lambda _1, _2: Container("dummy_mod"))
    with pytest.raises(InternalError) as err:
        kernel.get_callees()
    assert ("Failed to find any routines for Kernel 'matrix_vector_code'"
            in str(err.value))


def test_get_callees_same_container(monkeypatch):
    '''
    Check that get_callees() first examines all routines in the same
    Container.

    '''
    _, invoke = get_invoke("12_kernel_specific.f90", TEST_API, idx=0)
    sched = invoke.schedule
    # Module-inline the kernels so that they are in the same Container as the
    # call site.
    mod_inline_trans = KernelModuleInlineTrans()
    for kern in sched.walk(LFRicKern):
        mod_inline_trans.apply(kern)
        # Remove the cached schedule to force get_callees() to search.
        monkeypatch.setattr(kern, "_schedules", None)
        schedules = kern.get_callees()
        # The returned schedule should be the one in the local Container.
        assert schedules[0] in sched.ancestor(Container).walk(Routine)


def test_get_callees_mixed_precision():
    '''
    Test that get_callees() and get_interface_symbol() work for a
    mixed-precision kernel.

    '''
    _, invoke = get_invoke("26.8_mixed_precision_args.f90", TEST_API,
                           name="invoke_0", dist_mem=False)
    sched = invoke.schedule
    for kern in sched.walk(LFRicKern, stop_type=LFRicKern):
        assert len(kern.get_callees()) == 2
        isym = kern.get_interface_symbol()
        assert isinstance(isym, GenericInterfaceSymbol)
        assert isym.name == "mixed_code"


@pytest.mark.xfail(reason="get_callees has been extended to return all"
                   " implementations of a polymorphic kernel. We need to "
                   "put back (and fix) the ability to resolve which "
                   "implementation is being called.")
def test_get_callees_mixed_precision_match():
    '''
    Test that we can get the correct schedule for a mixed-precision kernel.

    '''
    api_config = Config.get().api_conf(TEST_API)
    _, invoke = get_invoke("26.8_mixed_precision_args.f90", TEST_API,
                           name="invoke_0", dist_mem=False)
    sched = invoke.schedule
    kernels = sched.walk(LFRicKern, stop_type=LFRicKern)
    # 26.8 contains an invoke of five kernels, one each at the following
    # precisions.
    kernel_precisions = ["r_def", "r_solver", "r_tran", "r_bl", "r_phys"]
    # Get the precision (in bytes) for each of these.
    precisions = [api_config.precision_map[name] for
                  name in kernel_precisions]
    # Check that the correct kernel implementation is obtained for each
    # one in the invoke.
    for precision, kern in zip(precisions, kernels):
        sched = kern.get_callees()
        assert isinstance(sched, KernelSchedule)
        assert sched.name == f"mixed_code_{8*precision}"


@pytest.mark.xfail(reason="get_callees() has been extended to return all"
                   " implementations of a polymorphic kernel. We need to "
                   "put back (and fix) the ability to resolve which "
                   "implementation is being called.")
def test_get_callees_mixed_precision_no_match(monkeypatch):
    '''
    Test that we get the expected error if there's no matching implementation
    for a mixed-precision kernel.

    '''
    _, invoke = get_invoke("26.8_mixed_precision_args.f90", TEST_API,
                           name="invoke_0", dist_mem=False)
    sched = invoke.schedule
    kernels = sched.walk(LFRicKern, stop_type=LFRicKern)

    # To simplify things we just monkeypatch the 'validate_kernel_code_args'
    # method so that it never succeeds.
    def fake_validate(_1, _2):
        raise GenerationError("Just a test")

    monkeypatch.setattr(LFRicKern, "validate_kernel_code_args",
                        fake_validate)
    with pytest.raises(GenerationError) as err:
        _ = kernels[0].get_callees()
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
    schedules = kernel.get_callees()
    sched = schedules[0]
    kernel.validate_kernel_code_args(sched.symbol_table)

    # Force LFRicKern to think that this kernel is an 'apply' kernel and
    # therefore does not need the mesh height argument.
    monkeypatch.setattr(kernel, "_cma_operation", "apply")
    with pytest.raises(GenerationError) as info:
        kernel.validate_kernel_code_args(
            sched.symbol_table)
    assert (
        "In kernel 'matrix_vector_code' the number of arguments indicated by "
        "the kernel metadata is 8 but the actual number of kernel arguments "
        "found is 9." in str(info.value))


def test_validate_kernel_code_arg(monkeypatch):
    '''Test that this method returns successfully if its two arguments
    have identical content, otherwise test that the expected
    exceptions are raised.

    '''
    kernel = LFRicKern()
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
    # If one of the dimensions is not a datasymbol then the arguments
    # are not checked.
    kernel._validate_kernel_code_arg(lfric_real_field_symbol,
                                     lfric_real_field_symbol2)
    kernel._validate_kernel_code_arg(lfric_real_field_symbol2,
                                     lfric_real_field_symbol)

    # Check for the correct number of array dimensions.
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
    # Monkeypatch the shape of lfric_real_field_symbol3 from ArrayBounds
    # to a Reference to check the 'continue' statement is triggered.
    monkeypatch.setattr(lfric_real_field_symbol3.datatype, "_shape",
                        [Reference(undf)])
    kernel._validate_kernel_code_arg(lfric_real_field_symbol3,
                                     lfric_real_field_symbol2)
    # Lower array bound of 2 rather than 1
    monkeypatch.setattr(lfric_real_field_symbol3.datatype, "_shape",
                        [ArrayType.ArrayBounds(Literal("2", INTEGER_TYPE),
                                               Reference(undf))])
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

    # Monkeypatch lfric_real_scalar_symbol to return that it is not a
    # scalar in order to force the required exception. We do this by
    # changing the ScalarType as it is used when determining whether
    # the symbol is a scalar.
    monkeypatch.setattr(psyclone.psyir.symbols.datatypes, "ScalarType", str)
    with pytest.raises(InternalError) as info:
        kernel._validate_kernel_code_arg(
            lfric_real_scalar_symbol, lfric_real_scalar_symbol)
    assert (
        "Unexpected argument type found for 'scalar' in kernel 'dummy'. "
        "Expecting a scalar or an array." in str(info.value))


def test_kern_last_cell_all_colours_errors(monkeypatch):
    ''' Tests for the checks in the last_cell_all_colours property
    of LFRicKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    kern = sched.walk(LFRicKern)[0]
    # Kernel is not coloured.
    with pytest.raises(InternalError) as err:
        _ = kern.last_cell_all_colours_symbol
    assert "'testkern_code' is not inside a coloured loop" in str(err.value)
    # Monkeypatch the Kernel so that it appears to be coloured.
    monkeypatch.setattr(kern, "is_coloured", lambda: True)


def test_kern_last_cell_all_colours():
    ''' Tests for the last_cell_all_colours property of LFRicKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loop = sched.walk(LFRicLoop)[0]
    # Apply a colouring transformation to the loop.
    trans = LFRicColourTrans()
    trans.apply(loop)

    symbol = loop.kernel.last_cell_all_colours_symbol
    assert symbol.name == "last_halo_cell_all_colours"
    assert len(symbol.datatype.shape) == 2  # It's a 2-dimensional array

    # Delete the symbols and try again inside a loop wihtout a halo
    sched.symbol_table._symbols.pop("last_halo_cell_all_colours")
    loop.kernel.parent.parent._upper_bound_name = "not-a-halo"
    symbol = loop.kernel.last_cell_all_colours_symbol
    assert symbol.name == "last_edge_cell_all_colours"
    assert len(symbol.datatype.shape) == 1  # It's a 1-dimensional array


def test_kern_last_cell_all_colours_intergrid():
    ''' Test the last_cell_all_colours property for an inter-grid
    LFRicKern.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.1_intergrid_restrict.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loop = sched.walk(LFRicLoop)[0]
    # Apply a colouring transformation to the loop.
    trans = LFRicColourTrans()
    trans.apply(loop)
    # We have to perform code generation as that sets-up the symbol table.
    # pylint:disable=pointless-statement
    psy.gen
    assert (loop.kernel.last_cell_all_colours_symbol.name ==
            "last_edge_cell_all_colours_field1")


def test_kern_all_updates_are_writes(monkeypatch):
    ''' Tests for the 'all_updates_are_writes' property of
    LFRicKern. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loop = sched.walk(LFRicLoop)[0]
    kernel = loop.kernel
    # The only argument updated by this kernel has GH_INC access.
    assert not kernel.all_updates_are_writes
    # Patch the kernel so that two arguments have GH_WRITE access.
    kernel.args[2]._access = AccessType.WRITE
    kernel.args[1]._access = AccessType.WRITE
    assert kernel.all_updates_are_writes
    # Patch the kernel so that both updated field arguments appear to be
    # on a discontinuous space.
    monkeypatch.setattr(
        kernel.arguments._args[1]._function_spaces[0],
        "_orig_name", "w3")
    monkeypatch.setattr(
        kernel.arguments._args[2]._function_spaces[0],
        "_orig_name", "w3")
    assert kernel.all_updates_are_writes


def test_kern_not_coloured_inc(monkeypatch):
    ''' Tests that there is no kernel argument with INC access when OpenMP
    is applied without colouring.
    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    kern = sched.walk(LFRicKern)[0]
    # Kernel is not coloured.
    assert kern.is_coloured() is False
    # Monkeypatch the Kernel so that it appears to be OpenMP parallel.
    monkeypatch.setattr(kern, "is_openmp_parallel", lambda: True)
    assert kern.is_openmp_parallel() is True
    with pytest.raises(VisitorError) as err:
        _ = psy.gen
    assert ("Kernel 'testkern_code' has an argument with INC access and "
            "therefore must be coloured in order to be parallelised with "
            "OpenMP." in str(err.value))


def test_undf_name():
    '''Tests that the LFRicKern.undf_name property returns the correct
    result when called.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    kern = sched.walk(LFRicKern)[0]

    assert kern.undf_name == "undf_w1"


def test_argument_kinds():
    ''' Test the LFRicKern.argument_kinds property. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    kern = sched.walk(LFRicKern)[0]

    assert len(kern.argument_kinds) == 2
    assert "i_def" in kern.argument_kinds
    assert "r_def" in kern.argument_kinds
