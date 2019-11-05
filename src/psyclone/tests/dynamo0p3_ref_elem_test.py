# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
import pytest
import fparser
from fparser import api as fpapi
from psyclone.configuration import Config
from psyclone.dynamo0p3 import DynKernMetadata

# Constants

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
            "['normals_to_horizontal_faces'," in str(err.value))


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
    assert ("'meta_reference_element' meta-data must consist of an array of "
            "structure constructors, all of type 'reference_element_data_type'"
            " but found: ['ref_element_data_type'," in str(err.value))
