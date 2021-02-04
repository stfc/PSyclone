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
# Author R. W. Ford, STFC Daresbury Lab


'''Performs py.test tests on the fparser2psyir function'''

from __future__ import absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyir.frontend.fparser2 import fparser2psyir
from psyclone.psyir.symbols import RoutineSymbol, UnresolvedInterface, \
    GlobalInterface
from psyclone.psyir.nodes import Literal, CodeBlock, Schedule, Call, \
    Reference, BinaryOperation
from psyclone.psyir.backend.fortran import FortranWriter


# module with no subroutines
MODULE1_IN = '''
module test
end module test
'''
MODULE1_OUT = \
'''module test
  contains
end module test'''
# module with subroutines
MODULE2_IN = '''
module test
  contains
  subroutine sub1()
  end subroutine sub1
  subroutine sub2()
  end subroutine sub2
end module test
'''
MODULE2_OUT = \
'''module test
  public :: sub1, sub2
  contains
  subroutine sub1()
  end subroutine sub1
  subroutine sub2()
  end subroutine sub2
end module test'''
# subroutine
SUBROUTINE1_IN = '''
subroutine test()
end subroutine test
'''
SUBROUTINE1_OUT = \
'''subroutine test()
end subroutine test'''


@pytest.mark.parametrize("input_code,expected_code",
                         [(MODULE1_IN, MODULE1_OUT),
                          (MODULE2_IN, MODULE2_OUT),
                          (SUBROUTINE1_IN, SUBROUTINE1_OUT)])
def test_fparser2psyir(f2008_parser, input_code, expected_code):
    ''' xxx '''
    reader = FortranStringReader(input_code)
    ast = f2008_parser(reader)
    psyir = fparser2psyir(ast)
    writer = FortranWriter()
    result = writer(psyir)
    result_strip = "\n".join([line for line in result.splitlines() if line.rstrip()])
    assert result_strip == expected_code

TWO_SUBROUTINES = '''
subroutine sub1()
end subroutine sub1
subroutine sub2()
end subroutine sub2
'''
TWO_MODULES = '''
module mod1
end module mod1
module mod1
end module mod1
'''
TWO_MIXED = '''
module mod1
end module mod1
subroutine sub2()
end subroutine sub2
'''


@pytest.mark.parametrize("input_code", [TWO_SUBROUTINES, TWO_MODULES,
                                        TWO_MIXED])
def test_multi_routine(f2008_parser, input_code):
    ''' xxx '''
    reader = FortranStringReader(input_code)
    ast = f2008_parser(reader)
    with pytest.raises(NotImplementedError) as info:
        _ = fparser2psyir(ast)
    assert ("The PSyIR is currently limited to a single top level "
            "module/subroutine/program/function, but 2 were found."
            in str(info.value))


def test_no_program(f2008_parser):
    ''' xxx '''
    input_code = (
        "program test\n"
        "end program test\n")
    reader = FortranStringReader(input_code)
    ast = f2008_parser(reader)
    with pytest.raises(NotImplementedError) as info:
        _ = fparser2psyir(ast)
    assert ("The PSyIR currently only supports a module or a subroutine as its "
            "top level concept, but found 'Main_Program'." in str(info.value))
