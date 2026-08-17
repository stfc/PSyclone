# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing pytest fixtures for the LFRic-specific tests. '''

import pytest
from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_kern import LFRicKern
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory


@pytest.fixture(autouse=True)
def api_setup_fixture():
    '''Make sure that all tests here use LFRic as API.'''
    Config.get().api = "lfric"


@pytest.fixture(name="lfrickern")
def lfrickern_fixture():
    '''
    :returns: an LFRicKern object created from example metadata.
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

    kernel_metadata = get_kernel_parse_tree(mdata_code)
    ktype = KernelTypeFactory(api="lfric").create(
        kernel_metadata, name="testkern_field_type")
    kern = LFRicKern()
    kern.load_meta(ktype)
    return kern


@pytest.fixture(name="lfrickern_halo")
def lfrichalokern_fixture():
    '''
    :returns: an LFRicKern object created from example metadata which specifies
              that the kernel operates on cell halos.
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
     integer :: operates_on = halo_cell_column
   contains
     procedure, nopass :: code => testkern_field_code
  end type testkern_field_type
contains
  subroutine testkern_field_code()
  end subroutine testkern_field_code
end module testkern_field_mod
'''

    kernel_metadata = get_kernel_parse_tree(mdata_code)
    ktype = KernelTypeFactory(api="lfric").create(
        kernel_metadata, name="testkern_field_type")
    kern = LFRicKern()
    kern.load_meta(ktype)
    return kern


@pytest.fixture(name="lfrickern_op")
def lfrickern_op_fixture():
    '''
    :returns: an LFRicKern object created from example metadata that includes
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

    kernel_metadata = get_kernel_parse_tree(mdata_code)
    ktype = KernelTypeFactory(api="lfric").create(
        kernel_metadata, name="testkern_field_type")
    kern = LFRicKern()
    kern.load_meta(ktype)
    return kern
