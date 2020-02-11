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
# Author A. R. Porter, STFC Daresbury Lab

''' pytest tests for LFRic kernels specifying data-layout and
    data-addressing. '''

from __future__ import absolute_import, print_function
import pytest
import fparser
from fparser import api as fpapi

from psyclone.configuration import Config
from psyclone.parse.utils import ParseError
from psyclone.dynamo0p3 import DynKernMetadata


CODE = '''
module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg_type), meta_args(3) =                  &
          (/ arg_type(gh_real, gh_read),             &
             arg_type(gh_field, gh_inc, w1),         &
             arg_type(gh_field, gh_read, w2)         &
           /)
     integer, parameter :: iterates_over = cells
     integer, parameter :: data_layout = layout_zxy
     integer, parameter :: data_addressing(2) = (/direct_z, indirect_xy/)
   contains
     procedure, nopass :: code => testkern_code
  end type testkern_type
contains
  subroutine testkern_code(a,b,c)
  end subroutine testkern_code
end module testkern
'''


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"


def test_data_layout_valid():
    ''' Basic test that we can parse the data_layout/addressing metadata
    entries. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    dkm = DynKernMetadata(ast, name="testkern_type")
    assert dkm.data_layout == "layout_zxy"
    assert dkm.data_addressing == ["direct_z", "indirect_xy"]


@pytest.mark.parametrize("layout", ["layout", "layout_wrong", "zxy", "x"])
def test_data_layout_invalid(layout):
    ''' Check that the parser rejects unrecognised values for data layout. '''
    new_code = CODE.replace("layout_zxy", layout)
    ast = fpapi.parse(new_code, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = DynKernMetadata(ast, name="testkern_type")
    assert ("Kernel 'testkern_type' specifies an un-recognised data-layout "
            "('{0}'). Supported layouts are: [".format(layout)
            in str(err.value))


def test_data_addressing_invalid():
    ''' Check that the parser rejects unrecognised values for data layout. '''
    new_code = CODE.replace("(/direct_z, indirect_xy/)",
                            "(/direct_z, direct_x/)")
    ast = fpapi.parse(new_code, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = DynKernMetadata(ast, name="testkern_type")
    assert ("kernel 'testkern_type' contains a data_addressing entry but does "
            "not specify how to address dimension 'y': ["
            in str(err.value))
    new_code = CODE.replace("data_addressing(2) = (/direct_z, indirect_xy/)",
                            "data_addressing(1) = (/indirect_xy/)")
    ast = fpapi.parse(new_code, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = DynKernMetadata(ast, name="testkern_type")
    assert ("kernel 'testkern_type' contains a data_addressing entry but does "
            "not specify how to address dimension 'z': ["
            in str(err.value))
    new_code = CODE.replace("data_addressing(2) = (/direct_z, indirect_xy/)",
                            "data_addressing(2) = (/indirect_x, direct_x/)")
    ast = fpapi.parse(new_code, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = DynKernMetadata(ast, name="testkern_type")
    assert ("for kernel 'testkern_type' specifies how to address dimension "
            "'x' more than once: " in str(err.value))
