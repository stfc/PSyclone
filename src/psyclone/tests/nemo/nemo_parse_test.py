# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# Authors: R. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing py.test tests for the parsing of NEMO code. '''

from __future__ import print_function, absolute_import
import os
from fparser.common.readfortran import FortranStringReader
from fparser.two.utils import walk_ast
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from psyclone.parse import parse
from psyclone import nemo

# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_identify_implicit_loop():
    ''' Check that we correctly identify implicit loops in the fparser2 AST '''
    parser = ParserFactory().create()
    reader = FortranStringReader("program test_prog\n"
                                 "umask(:, :, :, :) = 0.0D0\n"
                                 "do jk = 1, jpk\n"
                                 "  umask(1,1,jk) = -1.0d0\n"
                                 "end do\n"
                                 "end program test_prog\n")
    ast = parser(reader)
    assert not nemo.NemoImplicitLoop.match(ast)
    stmts = walk_ast(ast.content, [Fortran2003.Assignment_Stmt])
    assert not nemo.NemoImplicitLoop.match(stmts[1])
    assert nemo.NemoImplicitLoop.match(stmts[0])


def test_not_implicit_loop():
    ''' Check we do not incorrectly identify an implicit loop when array
    notation is used in the arguments to a function call. '''
    code = "z3d(1,:,:) =  ptr_sjk( pvtr(:,:,:), btmsk(:,:,jn)*btm30(:,:) )"
    reader = FortranStringReader(code)
    assign = Fortran2003.Assignment_Stmt(reader)
    assert not nemo.NemoImplicitLoop.match(assign)
