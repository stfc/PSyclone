# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''Module containing tests for the LFRicMetaDataParser.'''

import pytest

from fparser.common.sourceinfo import FortranFormat
from fparser.common.readfortran import FortranStringReader

from psyclone.parse.lfric_metadata_parser import LFRicMetaDataParser

CODE = '''
module testkern_qr
  type, public, extends(kernel_type) :: testkern_qr_type
     private
     type(arg_type) :: meta_args(6) =  (/                        &
             arg_type(gh_scalar,   gh_real,    gh_read),         &
             arg_type(gh_field,    gh_real,    gh_inc,  w1),     &
             arg_type(gh_field,    gh_real,    gh_read, w2),     &
             arg_type(gh_operator, gh_real,    gh_read, w2, w2), &
             arg_type(gh_field,    gh_real,    gh_read, w3),     &
             arg_type(gh_scalar,   gh_integer, gh_read) /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_qr_code
  end type testkern_qr_type
contains
  subroutine testkern_qr_code(a, b, c, d)
  end subroutine testkern_qr_code
end module testkern_qr
'''
#CODE = '''
#module testkern_qr
# type, public, extends(kernel_type) :: tl_hydrostatic_kernel_type
#    private
#    type(arg_type) :: meta_args(8) = (/                      &
#        arg_type(GH_FIELD,   GH_REAL, GH_INC,  ANY_W2),      &
#        arg_type(GH_FIELD,   GH_REAL, GH_READ, W3),          &
#        arg_type(GH_FIELD,   GH_REAL, GH_READ, Wtheta),      &
#        arg_type(GH_FIELD*3, GH_REAL, GH_READ, Wtheta),      &
#        arg_type(GH_FIELD,   GH_REAL, GH_READ, W3),          &
#        arg_type(GH_FIELD,   GH_REAL, GH_READ, Wtheta),      &
#        arg_type(GH_FIELD*3, GH_REAL, GH_READ, Wtheta),      &
#        arg_type(GH_SCALAR,  GH_REAL, GH_READ)               &
#        /)
#    type(func_type) :: meta_funcs(3) = (/                &
#        func_type(ANY_W2,      GH_BASIS, GH_DIFF_BASIS), &
#        func_type(W3,          GH_BASIS),                &
#        func_type(Wtheta,      GH_BASIS, GH_DIFF_BASIS)  &
#        /)
#    integer :: operates_on = CELL_COLUMN
#    integer :: gh_shape = GH_QUADRATURE_XYoZ
#  contains
#    procedure, nopass ::tl_hydrostatic_code
#  end type
#contains
#  subroutine tl_hydrostatic_code()
#  end subroutine tl_hydrostatic_code
#end module testkern_qr
#'''


def test_create_mdata_parser(parser):
    ''' Check that we can construct the metadata parser. '''
    reader = FortranStringReader(CODE)
    reader.set_format(FortranFormat(True, True))
    obj = LFRicMetaDataParser(parser(reader))
    assert isinstance(obj, LFRicMetaDataParser)


def test_mdata_parser_operates_on(parser):
    ''' Check that the metadata parser correctly extracts and stores the
    'operates_on' property. '''
    reader = FortranStringReader(CODE)
    reader.set_format(FortranFormat(True, True))
    obj = LFRicMetaDataParser(parser(reader))
    assert obj.operates_on == 'CELL_COLUMN'
