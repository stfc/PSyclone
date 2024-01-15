# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology

''' This module tests the LFric classes based on ArgOrdering.'''

import os
import re
import pytest

from psyclone.core import AccessType, VariablesAccessInfo, Signature
from psyclone.domain.lfric import (KernCallArgList, KernStubArgList,
                                   LFRicConstants, LFRicKern,
                                   LFRicKernMetadata, LFRicLoop,
                                   LFRicSymbolTable)
from psyclone.domain.lfric.arg_ordering import ArgOrdering
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import ArrayReference, Literal, Reference
from psyclone.psyir.symbols import INTEGER_TYPE, ScalarType
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_ast, get_base_path, get_invoke

TEST_API = "dynamo0.3"


def check_psyir_results(create_arg_list, fortran_writer):
    '''Helper function to check if the PSyIR representation of the arguments
     is identical to the old style textual representation. It checks that each
     member of the psyir_arglist is a Reference, and that the textual
     representation matches the textual presentation (which was already
     verified).

     '''
    # Check the PSyIR representation
    result = []
    for node in create_arg_list.psyir_arglist:
        assert isinstance(node, Reference)
        out = fortran_writer(node)
        # We're comparing old and new (textual versus PSyIR) here and only
        # the new, PSyIR approach supports the addition of array-slice notation
        # (e.g. 'array(:)'). Therefore, we remove it before comparing.
        result.append(re.sub(r"[(]\s*:(,\s*:)*\s*[)]$", "", out))

    assert result == create_arg_list._arglist


def test_argordering_append():
    '''
    Tests for the append() method of ArgOrdering.

    '''
    full_path = os.path.join(get_base_path(TEST_API),
                             "1.0.1_single_named_invoke.f90")
    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.walk(LFRicKern)[0]
    arg_list = ArgOrdering(kern)
    arg_list.append("roger")
    assert len(arg_list._arglist) == 1
    assert arg_list._arg_index_to_metadata_index[0] is None
    arg_list.append("susan", metadata_posn=3)
    assert len(arg_list._arglist) == 2
    assert arg_list._arg_index_to_metadata_index[1] == 3
    # Access info captured.
    vinfo = VariablesAccessInfo()
    arg_list.append("beckfoot", var_accesses=vinfo, mode=AccessType.WRITE)
    assert len(arg_list._arglist) == 3
    assert vinfo.all_signatures == [Signature("beckfoot")]
    # Alternate name supplied for the access.
    arg_list.append("john", var_access_name="john_walker",
                    var_accesses=vinfo, mode=AccessType.WRITE)
    assert vinfo.is_written(Signature("john_walker"))


def test_argordering_get_array_reference():
    '''
    Tests for the get_array_reference() method of ArgOrdering.

    '''

    psy, _ = get_invoke("1.0.1_single_named_invoke.f90",
                        TEST_API, 0)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.walk(LFRicKern)[0]
    arg_list = ArgOrdering(kern)

    # First test access using an index, e.g. `array(1)`
    one = Literal("1", INTEGER_TYPE)
    ref = arg_list.get_array_reference("array1", [one],
                                       ScalarType.Intrinsic.REAL)
    assert isinstance(ref, ArrayReference)
    ref = arg_list.get_array_reference("array2", [":"],
                                       ScalarType.Intrinsic.INTEGER)
    assert not isinstance(ref, ArrayReference)

    # Now test access using ":" only, e.g. `array(:)` -> this should
    # be returned just a reference to `array`
    ref = arg_list.get_array_reference("array3", [":", ":"],
                                       ScalarType.Intrinsic.REAL)
    assert isinstance(ref, Reference)
    assert not isinstance(ref, ArrayReference)
    ref = arg_list.get_array_reference("array4", [":", ":"],
                                       ScalarType.Intrinsic.INTEGER)
    assert isinstance(ref, Reference)
    assert not isinstance(ref, ArrayReference)

    # Now specify a symbol, but an incorrect array name:
    with pytest.raises(InternalError) as err:
        arg_list.get_array_reference("wrong-name", [":", ":"],
                                     ScalarType.Intrinsic.INTEGER,
                                     symbol=ref.symbol)
    assert ("Specified symbol 'array4' has a different name than the "
            "specified array name 'wrong-name'" in str(err.value))

    with pytest.raises(TypeError) as err:
        arg_list.get_array_reference("does-not-exist", [":"], "invalid")
    assert ("Unsupported data type 'invalid' in find_or_create_array"
            in str(err.value))

    with pytest.raises(TypeError) as err:
        arg_list.get_array_reference("array4", [":"],
                                     ScalarType.Intrinsic.INTEGER)
    assert ("Array 'array4' already exists, but has 2 dimensions, not 1."
            in str(err.value))


def test_argordering_extend():
    '''
    Tests for the extend() method of ArgOrdering.

    '''
    full_path = os.path.join(get_base_path(TEST_API),
                             "10.7_operator_read.f90")
    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.walk(LFRicKern)[0]
    arg_list = ArgOrdering(kern)
    arg_list.append("roger")
    arg_list.extend(["peggy", "nancy"])
    assert len(arg_list._arglist) == 3
    vinfo = VariablesAccessInfo()
    arg_list.extend(["flint", "captain"], var_accesses=vinfo,
                    mode=AccessType.WRITE)
    assert len(arg_list._arglist) == 5
    assert Signature("flint") in vinfo.all_signatures
    assert Signature("captain") in vinfo.all_signatures
    assert vinfo.is_written(Signature("flint"))
    arg_list.extend(["richard", "dorothea"], var_accesses=vinfo,
                    mode=AccessType.READ, list_metadata_posn=[5, 7])
    assert len(arg_list._arglist) == 7
    assert arg_list._arg_index_to_metadata_index[5] == 5
    assert arg_list._arg_index_to_metadata_index[6] == 7


def test_unexpected_type_error(dist_mem):
    ''' Check that we raise an exception if an unexpected datatype is found
    when running the ArgOrdering generate method. As it is abstract we use
    the KernCallArgList sub class.

    '''
    full_path = os.path.join(get_base_path(TEST_API),
                             "1.0.1_single_named_invoke.f90")
    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.walk(LFRicKern)[0]
    # Sabotage one of the arguments to make it have an invalid type.
    kernel.arguments.args[0]._argument_type = "invalid"
    # Now call KernCallArgList to raise an exception
    create_arg_list = KernCallArgList(kernel)
    with pytest.raises(GenerationError) as excinfo:
        create_arg_list.generate()
    const = LFRicConstants()
    assert (
        f"ArgOrdering.generate(): Unexpected argument "
        f"type found. Expected one of '{const.VALID_ARG_TYPE_NAMES}' "
        f"but found 'invalid'" in str(excinfo.value))


def test_kernel_stub_invalid_scalar_argument():
    ''' Check that we raise an exception if an unexpected datatype is found
    when using the KernStubArgList scalar method. '''
    ast = get_ast(TEST_API, "testkern_one_int_scalar_mod.f90")

    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    # Sabotage the scalar argument to make it have an invalid type.
    arg = kernel.arguments.args[1]
    arg._argument_type = "invalid"
    # Now call KernStubArgList to raise an exception
    create_arg_list = KernStubArgList(kernel)
    with pytest.raises(InternalError) as excinfo:
        create_arg_list.scalar(arg)
    const = LFRicConstants()
    assert (f"Expected argument type to be one of {const.VALID_SCALAR_NAMES} "
            f"but got 'invalid'" in str(excinfo.value))


def test_arg_ordering_generate_domain_kernel(dist_mem, fortran_writer):
    '''
    Check that the LFRic ArgOrdering class generates the expected arguments
    for a kernel that iterates over the 'domain'.

    '''
    # Get hold of a valid Kernel object
    full_path = os.path.join(get_base_path(TEST_API),
                             "25.0_domain.f90")
    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.kernels()[0]

    create_arg_list = KernCallArgList(kernel)
    assert not create_arg_list._arglist
    assert not create_arg_list._psyir_arglist
    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'ncell_2d_no_halos', 'b', 'f1_data', 'ndf_w3',
        'undf_w3', 'map_w3']

    check_psyir_results(create_arg_list, fortran_writer)


def test_arg_ordering_generate_cma_kernel(dist_mem, fortran_writer):
    '''
    Check that the LFRic ArgOrdering class generates the expected arguments
    for a CMA kernel.

    '''
    # Get hold of a valid Kernel object
    full_path = os.path.join(get_base_path(TEST_API),
                             "20.0_cma_assembly.f90")
    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.kernels()[0]

    create_arg_list = KernCallArgList(kernel)
    assert not create_arg_list._arglist
    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'cell', 'nlayers', 'ncell_2d', 'lma_op1_proxy%ncell_3d',
        'lma_op1_local_stencil', 'cma_op1_cma_matrix', 'cma_op1_nrow',
        'cma_op1_ncol', 'cma_op1_bandwidth', 'cma_op1_alpha', 'cma_op1_beta',
        'cma_op1_gamma_m', 'cma_op1_gamma_p', 'ndf_adspc1_lma_op1',
        'cbanded_map_adspc1_lma_op1', 'ndf_adspc2_lma_op1',
        'cbanded_map_adspc2_lma_op1']

    check_psyir_results(create_arg_list, fortran_writer)
    psyir_arglist = create_arg_list.psyir_arglist

    sym_tab = LFRicSymbolTable()
    arr_2d = sym_tab.find_or_create_array("doesnt_matter", 2,
                                          ScalarType.Intrinsic.INTEGER)
    # Check datatype of the cbanded_map parameters are indeed 2d int arrays
    for i in [14, 16]:
        assert psyir_arglist[i].datatype == arr_2d.datatype


def test_arg_ordering_mdata_index():
    '''
    Check that the metadata_index_from_actual_index() method of ArgOrdering
    works as expected.

    '''
    full_path = os.path.join(get_base_path(TEST_API),
                             "1.0.1_single_named_invoke.f90")
    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernels = schedule.walk(LFRicKern)
    arg_list = ArgOrdering(kernels[0])
    arg_list.generate()
    # Scalar argument.
    assert arg_list.metadata_index_from_actual_index(0) == 0
    with pytest.raises(KeyError):
        arg_list.metadata_index_from_actual_index(20)


def test_kernel_stub_ind_dofmap_errors():
    '''Check that we raise the expected exceptions if the wrong arguments
    are supplied to KernelStubArgList.indirection_dofmap() '''
    ast = get_ast(TEST_API, "testkern_one_int_scalar_mod.f90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    # Now call KernStubArgList to raise an exception
    create_arg_list = KernStubArgList(kernel)
    # First call it without an argument object
    with pytest.raises(InternalError) as excinfo:
        create_arg_list.indirection_dofmap("w3")
    assert "No CMA operator supplied" in str(excinfo.value)
    # Second, call it with an argument object but one that is not
    # an operator
    with pytest.raises(InternalError) as excinfo:
        create_arg_list.indirection_dofmap("w3", kernel.arguments.args[1])
    assert ("A CMA operator (gh_columnwise_operator) must be supplied but "
            "got 'gh_scalar'") in str(excinfo.value)


def test_kerncallarglist_args_error(dist_mem):
    '''Check that we raise an exception if we call the methods that return
    information in kerncallarglist without first calling the generate
    method

    '''
    psy, _ = get_invoke("1.0.1_single_named_invoke.f90", api=TEST_API,
                        dist_mem=dist_mem, idx=0)
    schedule = psy.invokes.invoke_list[0].schedule
    if dist_mem:
        loop = schedule.children[4]
    else:
        loop = schedule.children[0]
    create_arg_list = KernCallArgList(loop.loop_body[0])

    # nlayers_positions method
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.nlayers_positions
    assert (
        "KernCallArgList: the generate() method should be called before "
        "the nlayers_positions() method") in str(excinfo.value)

    # nqp_positions method
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.nqp_positions
    assert (
        "KernCallArgList: the generate() method should be called before "
        "the nqp_positions() method") in str(excinfo.value)

    # ndf_positions method
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.ndf_positions
    assert (
        "KernCallArgList: the generate() method should be called before "
        "the ndf_positions() method") in str(excinfo.value)

    # arglist method
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.arglist
    assert (
        "The argument list in KernCallArgList "
        "is empty. Has the generate() method been called?"
        ) in str(excinfo.value)

    # arglist method
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.psyir_arglist
    assert (
        "The PSyIR argument list in KernCallArgList "
        "is empty. Has the generate() method been called?"
        ) in str(excinfo.value)


def test_kerncallarglist_quad_rule_error(dist_mem, tmpdir):
    ''' Check that we raise the expected exception if we encounter an
    unsupported quadrature shape in the quad_rule() method. '''
    psy, _ = get_invoke("6_multiple_QR_per_invoke.f90", TEST_API,
                        dist_mem=dist_mem, idx=0)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.walk(LFRicLoop)[0]
    create_arg_list = KernCallArgList(loop.loop_body[0])
    # Add an invalid shape to the dict of qr rules
    create_arg_list._kern.qr_rules["broken"] = None
    with pytest.raises(NotImplementedError) as err:
        create_arg_list.quad_rule()
    assert ("no support implemented for quadrature with a shape of 'broken'"
            in str(err.value))


def test_kerncallarglist_metadata_index_op_vector():
    '''
    Check that the lookup_metadata_index() and
    metadata_index_from_actual_index() methods of KernCallArgList work
    as expected for operator and field-vector arguments.

    '''
    full_path = os.path.join(get_base_path(TEST_API),
                             "4.4_multikernel_invokes.f90")
    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernels = schedule.walk(LFRicKern)
    arg_list = KernCallArgList(kernels[0])
    arg_list.generate()
    # Operator
    assert arg_list.metadata_index_from_actual_index(3) == 0
    # All three members of the vector originate from a single argument
    # description in the metadata.
    assert arg_list.metadata_index_from_actual_index(4) == 1
    assert arg_list.metadata_index_from_actual_index(5) == 1
    assert arg_list.metadata_index_from_actual_index(6) == 1
    # Scalar
    assert arg_list.metadata_index_from_actual_index(7) == 2


def test_kernstubarglist_arglist_error():
    '''Check that we raise an exception if we call the arglist method in
    kernstubarglist without first calling the generate method'''
    ast = get_ast(TEST_API, "testkern_one_int_scalar_mod.f90")

    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    # Now call KernStubArgList to raise an exception
    create_arg_list = KernStubArgList(kernel)
    with pytest.raises(InternalError) as excinfo:
        _ = create_arg_list.arglist
    assert (
        "The argument list in KernStubArgList is "
        "empty. Has the generate() method been "
        "called?") in str(excinfo.value)


def test_kernstubarglist_eval_shape_error():
    ''' Check that we raise the expected exception if we call the basis() or
    diff_basis() methods and one of the kernel's evaluator shapes is
    invalid. '''
    ast = get_ast(TEST_API, "testkern_qr_faces_mod.F90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    create_arg_list = KernStubArgList(kernel)
    # Break the list of qr rules
    kernel.eval_shapes.insert(0, "broken")
    with pytest.raises(InternalError) as err:
        create_arg_list.basis(None)
    assert ("Unrecognised evaluator shape ('broken'). Expected one of: "
            "['gh_quadrature_xyoz'" in str(err.value))
    with pytest.raises(InternalError) as err:
        create_arg_list.diff_basis(None)
    assert ("Unrecognised evaluator shape ('broken'). Expected one of: "
            "['gh_quadrature_xyoz'" in str(err.value))


def test_refelem_stub_arglist_err():
    ''' Check that the KernStubArgList.ref_element_properties method raises
    the expected error if it encounters an unsupported property. '''
    # Create the Kernel object
    ast = get_ast(TEST_API, "testkern_ref_elem_all_faces_mod.F90")
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    # Break the list of ref-element properties required by the Kernel
    kernel.reference_element.properties.append("Wrong property")
    with pytest.raises(InternalError) as err:
        KernStubArgList(kernel).generate()
    assert "('Wrong property') " in str(err.value)
    assert (
        "Supported properties are: ['Property.NORMALS_TO_HORIZONTAL_FACES', "
        "'Property.NORMALS_TO_VERTICAL_FACES', 'Property.NORMALS_TO_FACES', "
        "'Property.OUTWARD_NORMALS_TO_HORIZONTAL_FACES', "
        "'Property.OUTWARD_NORMALS_TO_VERTICAL_FACES', "
        "'Property.OUTWARD_NORMALS_TO_FACES']" in str(err.value))
