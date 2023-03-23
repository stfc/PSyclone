# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab


'''Performs pytest tests on the support for intrinsic statements that
    become IntrinsicCall's in the fparser2 PSyIR front-end.

'''
import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import IntrinsicCall, CodeBlock


def test_intrinsic_names_error(fortran_reader):
    '''Check that the expected exception is raised if the intrinsic
    arguments do not follow the rule that all named arguments follow
    all positional arguments. Use the sum intrinsic as an example.

    '''
    code = '''
subroutine test_intrinsic(var1, var2, dim, mask)
  real, intent(in) :: var1(:)
  integer, intent(in) :: dim
  logical, intent(in) :: mask
  real, intent(out) :: var2
  var2 = sum(var1, dim=dim, mask)
end subroutine test_intrinsic
'''
    with pytest.raises(InternalError) as info:
        _ = fortran_reader.psyir_from_source(code)
    assert ("In Fortran, all named arguments should follow all positional "
            "arguments, but found 'SUM(var1, dim = dim, mask)'."
            in str(info.value))


def test_intrinsic_named_arg_error(fortran_reader):
    '''Check that the expected exception is raised if all arguments are
    named and there is no 'array' named argument (as this is
    compulsory).

    '''
    code = '''
subroutine test_intrinsic(var1, var2, dim, mask)
  real, intent(in) :: var1(:)
  integer, intent(in) :: dim
  logical, intent(in) :: mask
  real, intent(out) :: var2
  var2 = sum(var=var1, dim=dim, mask=mask)
end subroutine test_intrinsic
'''
    with pytest.raises(InternalError) as info:
        _ = fortran_reader.psyir_from_source(code)
    assert ("Invalid intrinsic arguments found. Expecting one of the named "
            "arguments to be 'array', but found 'SUM(var = var1, dim = dim, "
            "mask = mask)'." in str(info.value))


@pytest.mark.parametrize("args", [
    "array=var1, dim=dim, mask=mask", "DIM=dim, ARRAY=var1, MASK=mask",
    "dim=dim, mask=mask, Array=var1"])
def test_intrinsic_all_named_args(fortran_reader, args):
    '''Check that the PSyIR's canonical form for arguments is returned
    when all Fortran arguments are named. Use the sum intrinsic as an
    example.

    '''
    code = f'''
subroutine test_intrinsic(var1, var2, dim, mask)
  real, intent(in) :: var1(:)
  integer, intent(in) :: dim
  logical, intent(in) :: mask
  real, intent(out) :: var2
  var2 = sum({args})
end subroutine test_intrinsic
'''
    psyir = fortran_reader.psyir_from_source(code)
    calls = psyir.walk(IntrinsicCall)
    assert len(calls) == 1
    call = calls[0]
    assert len(call.children) == 3
    assert call.children[0].symbol.name == "var1"
    assert call.children[1].symbol.name == "dim"
    assert call.children[2].symbol.name == "mask"
    assert len(call.argument_names) == 3
    assert call.argument_names[0] is None
    assert call.argument_names[1].lower() == "dim"
    assert call.argument_names[2].lower() == "mask"


# @pytest.mark.parametrize("intrinsic_name", ["minval", "maxval", "sum"])
@pytest.mark.parametrize("arg_names", ["var1, dim", "var1, mask"])
def test_intrinsic_mms(fortran_reader, arg_names):
    '''Check that a code block is returned when the type of the second
    argument is not known. Test with the sum intrinsic.

    '''
    code = f'''
subroutine test_intrinsic(var1, var2, dim, mask)
  real, intent(in) :: var1(:)
  integer, intent(in) :: dim
  logical, intent(in) :: mask
  real, intent(out) :: var2
  var2 = sum({arg_names})
end subroutine test_intrinsic
'''
    psyir = fortran_reader.psyir_from_source(code)
    calls = psyir.walk(IntrinsicCall)
    assert len(calls) == 0
    code_blocks = psyir.walk(CodeBlock)
    assert len(code_blocks) == 1
    assert str(code_blocks[0].ast) == f"SUM({arg_names})"


@pytest.mark.parametrize("args", ["var1, dim, mask=mask", "var1, dim, mask"])
def test_intrinsic_not_named_args(fortran_reader, args):
    '''Check that the PSyIR's canonical form for arguments is returned
    when three arguments are supplied and only one or no arguments are
    named. In this case the ordering of the arguments is known.

    '''
    code = f'''
subroutine test_intrinsic(var1, var2, dim, mask)
  real, intent(in) :: var1(:)
  integer, intent(in) :: dim
  logical, intent(in) :: mask
  real, intent(out) :: var2
  var2 = sum({args})
end subroutine test_intrinsic
'''
    psyir = fortran_reader.psyir_from_source(code)
    calls = psyir.walk(IntrinsicCall)
    assert len(calls) == 1
    call = calls[0]
    assert len(call.children) == 3
    assert call.children[0].symbol.name == "var1"
    assert call.children[1].symbol.name == "dim"
    assert call.children[2].symbol.name == "mask"
    assert len(call.argument_names) == 3
    assert call.argument_names[0] is None
    assert call.argument_names[1].lower() == "dim"
    assert call.argument_names[2].lower() == "mask"
