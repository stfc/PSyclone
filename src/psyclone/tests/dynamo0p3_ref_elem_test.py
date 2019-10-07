# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council
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
of the dynamo0.3 API.
'''

from __future__ import absolute_import, print_function
import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.configuration import Config
from psyclone.dynamo0p3 import DynKernMetadata

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

REF_ELEM_MDATA = '''
module testkern_relem_mod
  type, extends(kernel_type) :: testkern_relem_type
    type(arg_type), dimension(2) :: meta_args = &
        (/ arg_type(gh_field, gh_read, w1),     &
           arg_type(gh_field, gh_write, w0) /)
    type(reference_element_data_type), dimension(2) ::               &
      meta_reference_element =                                       &
        (/ reference_element_data_type(normals_to_horizontal_faces), &
           reference_element_data_type(normals_to_vertical_faces) /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_relem_code
  end type testkern_relem_type
contains
  subroutine testkern_relem_code(a,b,c,d)
  end subroutine testkern_relem_code
end module testkern_relem_mod
'''


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"


def test_mdata_parse():
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = REF_ELEM_MDATA
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_relem_type"
    dkm = DynKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[1])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_columnwise_operator'\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space_to[2]='any_space_1'\n"
        "  function_space_from[3]='any_space_2'\n")
    print(dkm_str)
    assert expected in dkm_str
