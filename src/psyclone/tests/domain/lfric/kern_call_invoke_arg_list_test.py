# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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

''' Module containing pytest tests for the xxx class. '''

import os
import pytest

from psyclone.domain.lfric.kern_call_invoke_arg_list import (
    KernCallInvokeArgList)
from psyclone.dynamo0p3 import DynKern
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.symbols import SymbolTable


BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

FIELD_CODE = '''
module testkern_field_mod
  type, extends(kernel_type) :: testkern_field_type
     type(arg_type), meta_args(7) =                               &
          (/ arg_type(gh_scalar, gh_real,    gh_read),            &
             arg_type(gh_field,  gh_real,    gh_readinc, w0),     &
             arg_type(gh_field,  gh_real,    gh_inc,     w1),     &
             arg_type(gh_field,  gh_real,    gh_read,    w2),     &
             arg_type(gh_field,  gh_integer, gh_write,   wtheta), &
             arg_type(gh_field,  gh_integer, gh_read,    w3),     &
             arg_type(gh_scalar, gh_integer, gh_read)             &
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


def test_kcial_construct(parser):
    ''' Tests for the KernCallInvokeArgList constructor. '''
    parse_tree = get_kernel_parse_tree(FIELD_CODE)
    ktype = KernelTypeFactory(api="dynamo0.3").create(
        parse_tree, name="testkern_field_type")
    # Construct a DynKern using the metadata.
    kern = DynKern()
    kern.load_meta(ktype)
    with pytest.raises(TypeError) as err:
        KernCallInvokeArgList(kern, None)
    assert ("Argument 'symbol_table' must be a SymbolTable instance but got "
            "'NoneType'" in str(err.value))
    obj = KernCallInvokeArgList(kern, SymbolTable())
    assert obj.fields == []
    assert obj.scalars == []
    assert obj.quadrature_objects == []
