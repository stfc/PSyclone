# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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

'''
Module containing pytest tests for the reference-element functionality
of the Dynamo0.3 API.
'''

from __future__ import absolute_import, print_function
import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.configuration import Config
from psyclone.dynamo0p3 import DynKernMetadata
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.tests.lfric_build import LFRicBuild

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

REF_ELEM_MDATA = '''
module testkern_refelem_mod
  type, extends(kernel_type) :: testkern_refelem_type
    type(arg_type), dimension(2) :: meta_args = &
        (/ arg_type(gh_field, gh_read, w1),     &
           arg_type(gh_field, gh_write, w0) /)
    type(reference_element_data_type), dimension(2) ::               &
      meta_reference_element =                                       &
        (/ reference_element_data_type(normals_to_horizontal_faces), &
           reference_element_data_type(normals_to_vertical_faces) /)
     integer, parameter :: iterates_over = cells
   contains
     procedure, nopass :: code => testkern_refelem_code
  end type testkern_refelem_type
contains
  subroutine testkern_refelem_code(a, b, c, d)
  end subroutine testkern_refelem_code
end module testkern_refelem_mod
'''

# Tests for parsing the metadata


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"


def test_mdata_parse():
    ''' Check that we get the correct list of reference-element properties. '''
    from psyclone.dynamo0p3 import RefElementMetaData
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = REF_ELEM_MDATA
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_refelem_type"
    dkm = DynKernMetadata(ast, name=name)
    assert dkm.reference_element.properties == \
        [RefElementMetaData.Property.NORMALS_TO_HORIZONTAL_FACES,
         RefElementMetaData.Property.NORMALS_TO_VERTICAL_FACES]


def test_mdata_invalid_property():
    ''' Check that we raise the expected error if an unrecognised property
    is requested. '''
    from psyclone.parse.utils import ParseError
    code = REF_ELEM_MDATA.replace("normals_to_vertical_faces",
                                  "not_a_property")
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_refelem_type"
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name=name)
    assert ("property: 'not_a_property'. Supported values are: "
            "['NORMALS_TO_HORIZONTAL_FACES'," in str(err.value))


def test_mdata_wrong_arg_count():
    ''' Check that we raise the expected error if the wrong dimension value
    is specified for the meta_reference_element array. '''
    from psyclone.parse.utils import ParseError
    code = REF_ELEM_MDATA.replace("element_data_type), dimension(2)",
                                  "element_data_type), dimension(3)")
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_refelem_type"
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name=name)
    assert ("'meta_reference_element' metadata, the number of args" in
            str(err.value))


def test_mdata_wrong_name():
    ''' Check that we raise the expected error if the array holding properties
    of the reference_element is given the wrong name. '''
    from psyclone.parse.utils import ParseError
    code = REF_ELEM_MDATA.replace("meta_reference_element =",
                                  "meta_ref_elem =")
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_refelem_type"
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name=name)
    assert ("No kernel metadata with type name 'meta_reference_element' found"
            in str(err.value))


def test_mdata_wrong_type_var():
    ''' Check that we raise the expected error if the array holding properties
    of the reference element contains an item of the wrong type. '''
    from psyclone.parse.utils import ParseError
    code = REF_ELEM_MDATA.replace("reference_element_data_type(normals_to",
                                  "ref_element_data_type(normals_to")
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_refelem_type"
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name=name)
    assert ("'meta_reference_element' metadata must consist of an array of "
            "structure constructors, all of type 'reference_element_data_type'"
            " but found: ['ref_element_data_type'," in str(err.value))


# Tests for generating the PSy-layer code


def test_refelem_arglist_err():
    ''' Check that the KernCallArgList.ref_element_properties method raises
    the expected error if it encounters an unsupported property. '''
    from psyclone.psyGen import Kern, InternalError
    _, invoke_info = parse(os.path.join(BASE_PATH, "23.1_ref_elem_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    # Get hold of the Kernel object
    kernels = sched.walk(Kern)
    kernel = kernels[0]
    # Break the list of ref-element properties required by the Kernel
    kernel.reference_element.properties.append("Not a property")
    with pytest.raises(InternalError) as err:
        kernel.arguments.raw_arg_list()
    assert ("Unsupported reference-element property ('Not a property') found "
            "when generating arguments for kernel 'testkern_ref_elem_code'. "
            "Supported properties are: ['Property." in str(err.value))


def test_refelem_gen(tmpdir):
    ''' Basic test for code-generation for an invoke containing a single
    kernel requiring reference-element properties. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "23.1_ref_elem_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()
    assert "use reference_element_mod, only: reference_element_type" in gen
    assert "integer nfaces_re_h, nfaces_re_v" in gen
    assert ("real(kind=r_def), allocatable :: normals_to_horiz_faces(:,:), "
            "normals_to_vert_faces(:,:)" in gen)
    assert ("class(reference_element_type), pointer :: reference_element "
            "=> null()" in gen)
    # We need a mesh object in order to get a reference_element object
    assert "mesh => f1_proxy%vspace%get_mesh()" in gen
    assert "reference_element => mesh%get_reference_element()" in gen
    assert ("nfaces_re_h = reference_element%get_number_horizontal_faces()"
            in gen)
    assert "nfaces_re_v = reference_element%get_number_vertical_faces()" in gen
    assert ("call reference_element%get_normals_to_horizontal_faces("
            "normals_to_horiz_faces)" in gen)
    assert ("call reference_element%get_normals_to_vertical_faces("
            "normals_to_vert_faces)" in gen)
    # The kernel call
    assert ("call testkern_ref_elem_code(nlayers, a, f1_proxy%data, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces)" in gen)


def test_duplicate_refelem_gen(tmpdir):
    ''' Test for code-generation for an invoke containing two kernels that
    require the same properties of the reference-element. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "23.2_multi_ref_elem_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()
    assert gen.count(
        "real(kind=r_def), allocatable :: normals_to_horiz_faces(:,:)"
        ", normals_to_vert_faces(:,:)") == 1
    assert gen.count(
        "reference_element => mesh%get_reference_element") == 1
    assert gen.count(
        "nfaces_re_h = reference_element%get_number_horizontal_faces()") == 1
    assert gen.count(
        "nfaces_re_v = reference_element%get_number_vertical_faces()") == 1
    assert gen.count("call reference_element%get_normals_to_horizontal_faces("
                     "normals_to_horiz_faces)") == 1
    assert gen.count("call reference_element%get_normals_to_vertical_faces("
                     "normals_to_vert_faces)") == 1
    assert ("call testkern_ref_elem_code(nlayers, a, f1_proxy%data, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces)" in gen)
    assert ("call testkern_ref_elem_code(nlayers, a, f3_proxy%data, "
            "f4_proxy%data, m3_proxy%data, m4_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces)" in gen)


def test_union_refelem_gen(tmpdir):
    ''' Check that code generation works for an invoke with kernels that
    only have a sub-set of reference-element properties in common. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "23.3_shared_ref_elem_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen = str(psy.gen).lower()

    assert (
        "      reference_element => mesh%get_reference_element()\n"
        "      nfaces_re_h = reference_element%get_number_horizontal_faces()\n"
        "      nfaces_re_v = reference_element%get_number_vertical_faces()\n"
        "      call reference_element%get_normals_to_horizontal_faces("
        "normals_to_horiz_faces)\n"
        "      call reference_element%get_outward_normals_to_horizontal_faces("
        "out_normals_to_horiz_faces)\n"
        "      call reference_element%get_normals_to_vertical_faces("
        "normals_to_vert_faces)\n"
        "      call reference_element%get_outward_normals_to_vertical_faces("
        "out_normals_to_vert_faces)\n" in gen)
    assert ("call testkern_ref_elem_code(nlayers, a, f1_proxy%data, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3,"
            " map_w3(:,cell), nfaces_re_h, nfaces_re_v, "
            "normals_to_horiz_faces, normals_to_vert_faces)" in gen)
    assert ("call testkern_ref_elem_out_code(nlayers, a, f3_proxy%data, "
            "f4_proxy%data, m3_proxy%data, m4_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3,"
            " map_w3(:,cell), nfaces_re_h, nfaces_re_v, "
            "out_normals_to_horiz_faces, normals_to_vert_faces, "
            "out_normals_to_vert_faces)" in gen)
