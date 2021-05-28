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

''' Module containing tests for the AdjointLoopTrans class. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.transformations import TransformationError, \
    AdjointLoopTrans
from psyclone.psyir.nodes import Loop
from psyclone.tests.utilities import Compile

CODE = '''
program my_prog
  integer :: ji, var(10)
  do ji = 1, 10
    var(ji) = ji
  end do
end program my_prog
'''


def test_adjloop_trans_validate(monkeypatch, fortran_reader):
    ''' Test the validation checks on the loop node provided to the
    transformation. '''
    prog = fortran_reader.psyir_from_source(CODE)
    trans = AdjointLoopTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(prog)
    assert ("Target of AdjointLoopTrans transformation must be a sub-"
            "class of Loop but got 'Routine'" in str(err.value))
    # Check that validate is OK with a valid loop
    loop = prog.walk(Loop)[0]
    trans.validate(loop)
    # Pretend that the loop is of 'null' type
    monkeypatch.setattr(loop, "_loop_type", "null")
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert ("Cannot apply a AdjointLoopTrans transformation to a "
            "'null' loop" in str(err.value))


def test_adjloop_trans_validate_body(fortran_reader):
    ''' Test the validation checks on the body of the loop provided to the
    transformation. '''
    code = CODE.replace("var(ji) = ji", "var(ji+1) = var(ji)")
    code = code.replace(", 10", ", 9")
    prog = fortran_reader.psyir_from_source(code)
    loop = prog.walk(Loop)[0]
    trans = AdjointLoopTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert ("Loop contains complex dependencies: Warning: Variable var is "
            "written" in str(err.value))


def test_adjloop_trans_validate_codeblock(fortran_reader):
    ''' Check that the transformation rejects a loop that contains a
    CodeBlock. '''
    code = CODE.replace("var(ji) = ji", "write(*,*) ji")
    prog = fortran_reader.psyir_from_source(code)
    loop = prog.walk(Loop)[0]
    trans = AdjointLoopTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert ("Nodes of type 'CodeBlock' cannot be" in str(err.value))


def test_adjloop_trans_basic_apply(tmpdir, fortran_reader, fortran_writer):
    ''' Check the transformation applied to a basic loop. '''
    prog = fortran_reader.psyir_from_source(CODE)
    trans = AdjointLoopTrans()
    loop = prog.walk(Loop)[0]
    trans.apply(loop)
    gen = fortran_writer(prog)
    assert "do ji = 10, 1, -1\n" in gen
    assert Compile(tmpdir).string_compiles(gen)


def test_adjloop_trans_negative_step(tmpdir, fortran_reader, fortran_writer):
    ''' Check the transformation applied to a loop that already has a
    negative step. '''
    code = CODE.replace("ji = 1, 10", "ji = 10, 1, -2")
    prog = fortran_reader.psyir_from_source(code)
    trans = AdjointLoopTrans()
    loop = prog.walk(Loop)[0]
    trans.apply(loop)
    gen = fortran_writer(prog)
    assert "do ji = 1, 10, -(-2)\n" in gen
    assert Compile(tmpdir).string_compiles(gen)
