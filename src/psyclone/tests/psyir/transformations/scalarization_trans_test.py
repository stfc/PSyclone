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
# ----------------------------------------------------------------------------
# Author: A. B. G. Chalk, STFC Daresbury Lab

'''This module tests the scalarization transformation.
'''

import os
import pytest

from psyclone.configuration import Config
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Call, IntrinsicCall, Reference, Routine, Loop
from psyclone.psyir.symbols import (
    ArgumentInterface, AutomaticInterface, DataSymbol, INTEGER_TYPE,
    RoutineSymbol, SymbolTable, UnresolvedType)
from psyclone.psyir.transformations import (
    ScalarizationTrans, TransformationError)
from psyclone.tests.utilities import Compile


def test_scal_quick(fortran_reader, fortran_writer):
    code = '''subroutine test()
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b
        integer, dimension(1:100) :: c

        do i = 1, 100
           arr(i) = i
           arr(i) = exp(arr(i))
           k = i
           b(i) = arr(i) * 3
           c(k) = i
        end do
        do i = 1, 100
           b(i) = b(i) + 1
        end do
    end subroutine
    '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)

    loop = psyir.children[0].children[0]
    strans.apply(loop)
    print(fortran_writer(psyir))
    assert False
