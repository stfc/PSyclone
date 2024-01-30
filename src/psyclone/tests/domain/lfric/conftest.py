# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Modified by: L. Turner, Met Office

''' Module containing pytest fixtures for the LFRic-specific tests. '''

import pytest
from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicKern
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory


@pytest.fixture(scope="module", autouse=True)
def api_setup_fixture():
    '''Make sure that all tests here use LFRic (Dynamo0.3) as API.'''
    Config.get().api = "dynamo0.3"
    yield
    Config._instance = None


@pytest.fixture(name="lfrickern", scope="module")
def lfrickern_fixture():
    '''
    :returns: a LFRicKern object created from example metadata.
    :rtype: :py:class:`psyclone.domain.lfric.LFRicKern`
    '''
    mdata_code = '''
module testkern_field_mod
  type, extends(kernel_type) :: testkern_field_type
     type(arg_type), meta_args(8) =                               &
          (/ arg_type(gh_scalar, gh_real,    gh_read),            &
             arg_type(gh_field,  gh_real,    gh_readinc, w0),     &
             arg_type(gh_field,  gh_real,    gh_inc,     w1),     &
             arg_type(gh_field*3,gh_integer, gh_read,    w2),     &
             arg_type(gh_field,  gh_integer, gh_write,   wtheta), &
             arg_type(gh_field,  gh_integer, gh_read,    w3),     &
             arg_type(gh_scalar, gh_integer, gh_read),            &
             arg_type(gh_scalar, gh_logical, gh_read)             &
           /)
     type(func_type), dimension(2) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_field_code
  end type testkern_field_type
contains
  subroutine testkern_field_code()
  end subroutine testkern_field_code
end module testkern_field_mod
'''
    # This fixture doesn't need a parser fixture as currently the metadata
    # parsing is handled by fparser1.
    # Once we switch over to using fparser2 (#1631) then this fixture may
    # need to ensure that fparser2 is initialised correctly.
    kernel_metadata = get_kernel_parse_tree(mdata_code)
    ktype = KernelTypeFactory(api="dynamo0.3").create(
        kernel_metadata, name="testkern_field_type")
    kern = LFRicKern()
    kern.load_meta(ktype)
    return kern


@pytest.fixture(name="lfrickern_op", scope="module")
def lfrickern_op_fixture():
    '''
    :returns: a LFRicKern object created from example metadata that includes \
              an operator argument.
    :rtype: :py:class:`psyclone.domain.lfric.LFRicKern`
    '''
    mdata_code = '''
module testkern_field_mod
  type, extends(kernel_type) :: testkern_field_type
     type(arg_type), meta_args(5) =                               &
          (/ arg_type(gh_scalar, gh_real,    gh_read),            &
             arg_type(gh_field,  gh_real,    gh_readinc, w0),     &
             arg_type(gh_field,  gh_real,    gh_inc,     w1),     &
             arg_type(gh_scalar, gh_integer, gh_read),            &
             arg_type(gh_operator,gh_real,   gh_read,    w2, w3)  &
           /)
     type(func_type), dimension(2) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_field_code
  end type testkern_field_type
contains
  subroutine testkern_field_code()
  end subroutine testkern_field_code
end module testkern_field_mod
'''
    # This fixture doesn't need a parser fixture as currently the metadata
    # parsing is handled by fparser1.
    # Once we switch over to using fparser2 (#1631) then this fixture may
    # need to ensure that fparser2 is initialised correctly.
    kernel_metadata = get_kernel_parse_tree(mdata_code)
    ktype = KernelTypeFactory(api="dynamo0.3").create(
        kernel_metadata, name="testkern_field_type")
    kern = LFRicKern()
    kern.load_meta(ktype)
    return kern
