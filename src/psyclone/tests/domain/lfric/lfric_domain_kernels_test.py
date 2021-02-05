# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Modified: I. Kavcic, Met Office

''' This module contains pytest tests for LFRic kernels which operate on
    the 'domain'. '''

from __future__ import absolute_import
import pytest
from fparser import api as fpapi
from psyclone.configuration import Config
from psyclone.dynamo0p3 import DynKernMetadata
from psyclone.parse.utils import ParseError

TEST_API = "dynamo0.3"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = TEST_API


def test_domain_kernel():
    ''' Check that we can successfully parse metadata that specifies a
    kernel with operates_on = DOMAIN. '''
    ast = fpapi.parse('''
module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), meta_args(5) =                             &
          (/ arg_type(gh_scalar, gh_real,    gh_read),          &
             arg_type(gh_field,  gh_real,    gh_readwrite, w3), &
             arg_type(gh_field,  gh_real,    gh_read,      w3), &
             arg_type(gh_field,  gh_real,    gh_read,      w3), &
             arg_type(gh_scalar, gh_integer, gh_read)           &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code(a, b, c, d)
  end subroutine testkern_domain_code
end module testkern_domain_mod
''', ignore_comments=False)
    dkm = DynKernMetadata(ast, name="testkern_domain_type")
    assert dkm.iterates_over == "domain"


def test_invalid_arg_domain_kernel():
    ''' Check that we reject a domain kernel if its metadata specifies
    an operator argument. '''
    ast = fpapi.parse('''module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), meta_args(4) =                               &
          (/ arg_type(gh_scalar,   gh_real, gh_read),             &
             arg_type(gh_field,    gh_real, gh_readwrite, w3),    &
             arg_type(gh_field,    gh_real, gh_read,      w3),    &
             arg_type(gh_operator, gh_real, gh_read,      w2, w2) &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code(a, b, c, d)
  end subroutine testkern_domain_code
end module testkern_domain_mod
''', ignore_comments=False)
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name="testkern_domain_type")
    assert ("'domain' is only permitted to accept scalar and field arguments "
            "but the metadata for kernel 'testkern_domain_type' includes an "
            "argument of type 'gh_operator'" in str(err.value))


def test_invalid_space_domain_kernel():
    ''' Check that we reject a domain kernel if its metadata specifies a
    field argument on a continuous space. '''
    ast = fpapi.parse('''module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), meta_args(3) =                          &
          (/ arg_type(gh_scalar, gh_real, gh_read),          &
             arg_type(gh_field,  gh_real, gh_readwrite, w3), &
             arg_type(gh_field,  gh_real, gh_read,      w2)  &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code(a, b, c, d)
  end subroutine testkern_domain_code
end module testkern_domain_mod
''', ignore_comments=False)
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name="testkern_domain_type")
    assert ("domain only accept field arguments on discontinuous function "
            "spaces but found 'w2' in 'arg_type(gh_field, gh_real, "
            "gh_read, w2)'" in str(err.value))


def test_no_stencil_domain_kernel():
    ''' Check that we reject a domain kernel if it has an argument with a
    stencil access. '''
    ast = fpapi.parse('''module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), meta_args(3) =                                         &
          (/ arg_type(gh_scalar, gh_real, gh_read),                         &
             arg_type(gh_field,  gh_real, gh_readwrite, w3),                &
             arg_type(gh_field,  gh_real, gh_read,      w3, stencil(cross)) &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code(a, b, c, d)
  end subroutine testkern_domain_code
end module testkern_domain_mod
''', ignore_comments=False)
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name="testkern_domain_type")
    assert ("domain are not permitted to have arguments with a stencil "
            "access but found: 'arg_type(gh_field, gh_real, gh_read, "
            "w3, stencil(cross))'" in str(err.value))


def test_invalid_basis_domain_kernel():
    ''' Check that we reject a kernel with operates_on=domain if it requires
    basis functions. '''
    ast = fpapi.parse('''
module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), meta_args(3) =                          &
          (/ arg_type(gh_scalar, gh_real, gh_read),          &
             arg_type(gh_field,  gh_real, gh_readwrite, w3), &
             arg_type(gh_field,  gh_real, gh_read,      w3)  &
           /)
     type(func_type), dimension(1) :: meta_funcs =  &
          (/ func_type(w3, gh_basis)                &
           /)
     integer :: operates_on = domain
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code(a, b, c, d)
  end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name="testkern_domain_type")
    assert ("'domain' cannot be passed basis/differential basis functions "
            "but the metadata for kernel 'testkern_domain_type' contains an "
            "entry for 'meta_funcs'" in str(err.value))


def test_invalid_mesh_props_domain_kernel():
    ''' Check that we reject a kernel with operates_on=domain if it requires
    properties of the mesh. '''
    ast = fpapi.parse('''
module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), meta_args(2) =                         &
          (/ arg_type(gh_scalar, gh_real, gh_read),         &
             arg_type(gh_field,  gh_real, gh_readwrite, w3) &
           /)
     type(mesh_data_type), dimension(1) :: meta_mesh = &
                        (/ mesh_data_type(adjacent_face) /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code(a, b, c, d)
  end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name="testkern_domain_type")
    assert ("'testkern_domain_type' operates on the domain but requests "
            "properties of the mesh ([" in str(err.value))
    assert "ADJACENT_FACE" in str(err.value)


def test_invalid_ref_elem_props_domain_kernel():
    ''' Check that we reject a kernel with operates_on=domain if it requires
    properties of the reference element. '''
    ast = fpapi.parse('''
module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), meta_args(2) =                         &
          (/ arg_type(gh_scalar, gh_real, gh_read),         &
             arg_type(gh_field,  gh_real, gh_readwrite, w3) &
           /)
     type(reference_element_data_type), dimension(1) :: &
         meta_reference_element =                       &
            (/ reference_element_data_type(normals_to_horizontal_faces) /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code(a, b, c, d)
  end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name="testkern_domain_type")
    assert ("'testkern_domain_type' operates on the domain but requests "
            "properties of the reference element ([" in str(err.value))
    assert "NORMALS_TO_HORIZONTAL_FACES" in str(err.value)


def test_invalid_mg_domain_kernel():
    ''' Check that we reject a kernel with operates_on=domain if it involves
    multi-grid (fields on different grids). '''
    ast = fpapi.parse('''
module restrict_mod
type, public, extends(kernel_type) :: restrict_kernel_type
   private
   type(arg_type) :: meta_args(2) = (/                          &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE,                &
                ANY_DISCONTINUOUS_SPACE_1, mesh_arg=GH_COARSE), &
       arg_type(GH_FIELD, GH_REAL, GH_READ,                     &
                ANY_DISCONTINUOUS_SPACE_2, mesh_arg=GH_FINE  )  &
       /)
  integer :: operates_on = domain
contains
  procedure, nopass :: restrict_kernel_code
end type restrict_kernel_type
contains
  subroutine restrict_kernel_code()
  end subroutine restrict_kernel_code
end module restrict_mod
''')
    with pytest.raises(ParseError) as err:
        DynKernMetadata(ast, name="restrict_kernel_type")
    assert ("'restrict_kernel_type' operates on the domain but has fields on "
            "different mesh resolutions" in str(err.value))
