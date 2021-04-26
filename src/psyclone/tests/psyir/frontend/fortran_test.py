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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Fortran PSyIR front-end '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from fparser.two import Fortran2003
from fparser.two.utils import FortranSyntaxError
from psyclone.psyir.nodes import Routine

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader

# The 'contiguous' keyword is just valid with Fortran 2008
ONLY_2008_CODE = '''
subroutine sub(a)
    integer, dimension(:), contiguous, intent(inout) :: a
end subroutine sub
'''

CODE = '''
subroutine sub(a)
    integer, dimension(:), intent(inout) :: a
    a = a + 1
end subroutine sub
'''


def test_fortran_reader_constructor():
    freader = FortranReader()
    assert freader._parser is Fortran2003.Program
    assert isinstance(freader._processor, Fparser2Reader)

    # By default is a f2008 parser
    freader.psyir_from_source(ONLY_2008_CODE)  # Succeeds

    # Can be instantiated with different std version
    #freader2 = FortranReader(std="f2003")
    #with pytest.raises(FortranSyntaxError):
    #    freader2.psyir_from_source(ONLY_2008_CODE)  # Fails

    #freader.psyir_from_source(ONLY_2008_CODE)  # Succeeds

    reader = FortranStringReader(ONLY_2008_CODE)
    parser = ParserFactory().create(std="f2008")
    code = parser(reader)
    another_parser = ParserFactory().create(std="f2003")
    code = parser(reader)


def test_fortran_psyir_from_source():

    fortran_reader = FortranReader()
    subroutine = fortran_reader.psyir_from_source(CODE)
    assert isinstance(subroutine, Routine)
