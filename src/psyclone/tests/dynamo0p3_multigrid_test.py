# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2016.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' This module contains tests for the multi-grid part of the Dynamo 0.3 API
    using pytest. '''

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access

# imports
import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.dynamo0p3 import DynKernMetadata
from psyclone.parse import parse, ParseError

RESTRICT_MDATA = '''
module restrict_mod
type, public, extends(kernel_type) :: restrict_kernel_type
   private 
   type(arg_type) :: meta_args(2) = (/                                 &
       arg_type(GH_FIELD, GH_INC, ANY_SPACE_1, mesh_arg=GH_COARSE),    &
       arg_type(GH_FIELD, GH_READ,  ANY_SPACE_2, mesh_arg=GH_FINE   )  &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: restrict_kernel_code
end type restrict_kernel_type
contains
  subroutine restrict_kernel_code()
  end subroutine restrict_kernel_code
end module restrict_mod
'''


def test_args_same_space_error():
    ''' Check that we reject a kernel if arguments on different meshes
    are specified as being on the same function space '''
    fparser.logging.disable('CRITICAL')
    code = RESTRICT_MDATA.replace("ANY_SPACE_2", "ANY_SPACE_1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("arguments on different meshes must be on different function "
            "spaces but " in str(excinfo))
