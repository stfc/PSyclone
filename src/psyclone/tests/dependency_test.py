# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Authors: J. Henrichs, Bureau of Meteorology


''' Module containing py.test tests for dependency analysis.'''

from __future__ import print_function, absolute_import
import os
import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone import nemo
from psyclone.psyGen import Assignment, IfBlock, Loop, PSyFactory
from psyclone.core.access_info import VariablesAccessInfo


# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_assignment(parser):
    ''' Check that assignments set the right read/write accesses.
    '''
    reader = FortranStringReader('''program test_prog
                                 a = b
                                 c(i,j) = d(i,j+1)+e+f(x,y)
                                 c(i) = c(i) + 1
                                 d(i,j) = sqrt(e(i,j))
                                 end program test_prog''')
    ast = parser(reader)
    psy = PSyFactory(API).create(ast)
    schedule = psy.invokes.get("test_prog").schedule

    # Simple scalar assignment:  a = b
    scalar_assignment = schedule.children[0]
    assert isinstance(scalar_assignment, Assignment)
    var_accesses = VariablesAccessInfo()
    scalar_assignment.reference_accesses(var_accesses)
    # Test some test functions explicitly:
    assert var_accesses.is_written("a")
    assert not var_accesses.is_read("a")
    assert not var_accesses.is_written("b")
    assert var_accesses.is_read("b")

    # Array element assignment: c(i,j) = d(i,j+1)+e+f(x,y)
    array_assignment = schedule.children[1]
    assert isinstance(array_assignment, Assignment)
    var_accesses = VariablesAccessInfo()
    array_assignment.reference_accesses(var_accesses)
    assert str(var_accesses) == "c: WRITE, d: READ, e: READ, f: READ, i: READ, "\
                            "j: READ, x: READ, y: READ"

    # Increment operation: c(i) = c(i)+1
    increment_access = schedule.children[2]
    assert isinstance(increment_access, Assignment)
    var_accesses = VariablesAccessInfo()
    increment_access.reference_accesses(var_accesses)
    assert str(var_accesses) == "c: READWRITE, i: READ"

    # Using an intrinsic (looks like an array access): d(i, j) = sqrt(e(i, j))
    sqrt_access = schedule.children[3]
    assert isinstance(sqrt_access, Assignment)
    var_accesses = VariablesAccessInfo()
    sqrt_access.reference_accesses(var_accesses)
    assert str(var_accesses) == "d: WRITE, e: READ, i: READ, j: READ"


def test_indirect_addressing(parser):
    ''' Check that we correctly handle indirect addressing, especially
    on the LHS. '''
    reader = FortranStringReader('''program test_prog
                                 g(h(i)) = a
                                 end program test_prog''')
    ast = parser(reader)
    psy = PSyFactory(API).create(ast)
    schedule = psy.invokes.get("test_prog").schedule

    indirect_addressing = schedule.children[0]
    assert isinstance(indirect_addressing, Assignment)
    var_accesses = VariablesAccessInfo()
    indirect_addressing.reference_accesses(var_accesses)
    assert str(var_accesses) == "a: READ, g: WRITE, h: READ, i: READ"


def test_if_statement(parser):
    ''' Tests handling an if statement
    '''
    reader = FortranStringReader('''program test_prog
                                 if (a .eq. b) then
                                    p(i) = q(i)
                                 else
                                   q(i) = r(i)
                                 endif
                                 end program test_prog''')
    ast = parser(reader)
    psy = PSyFactory(API).create(ast)
    schedule = psy.invokes.get("test_prog").schedule

    if_stmt = schedule.children[0]
    assert isinstance(if_stmt, IfBlock)
    var_accesses = VariablesAccessInfo()
    if_stmt.reference_accesses(var_accesses)
    assert str(var_accesses) == "a: READ, b: READ, i: READ, p: WRITE, "\
                            "q: READWRITE, r: READ"


@pytest.mark.xfail(reason="Calls in nemo not yet supported")
def test_call(parser):
    ''' Check that we correctly identify implicit loops in the fparser2 AST '''
    reader = FortranStringReader('''program test_prog
                                 call sub(a,b)
                                 end program test_prog''')
    ast = parser(reader)
    psy = PSyFactory(API).create(ast)
    schedule = psy.invokes.get("test_prog").schedule

    code_block = schedule.children[0]
    call_stmt = code_block.statements[0]
    var_accesses = VariablesAccessInfo()
    call_stmt.reference_accesses(var_accesses)
    assert str(var_accesses) == "a: UNKNOWN, b: UNKNOWN"


def test_do_loop(parser):
    ''' Check the handling of do loops.
    TODO: this only tests a nemo do loop. At this stage the loop
    boundaries in nemo are only strings (not instances of Reference or so),
    so in case of a loop like: "do jj=1, n", N would
    '''
    reader = FortranStringReader('''program test_prog
                                 do jj=1, 10
                                    do ji=1, 10
                                       s(ji, jj)=t(ji, jj)+1
                                    enddo
                                 enddo
                                 end program test_prog''')
    ast = parser(reader)
    psy = PSyFactory(API).create(ast)
    schedule = psy.invokes.get("test_prog").schedule

    do_loop = schedule.children[0]
    assert isinstance(do_loop, nemo.NemoLoop)
    var_accesses = VariablesAccessInfo()
    do_loop.reference_accesses(var_accesses)
    assert str(var_accesses) == "ji: READWRITE, jj: READWRITE, s: WRITE, t: READ"


@pytest.mark.xfail(reason="Nemo converts all loop limits to strings")
def test_do_loop_not_working_yet(parser):
    ''' Check the handling of do loops.
    At this stage the loop boundaries in nemo are only strings (not
    instances of Reference or so), so lower or upper loop boundaries
    are not reported as 'READ'.
    '''
    reader = FortranStringReader('''program test_prog
                                 do jj=1, n
                                    do ji=1, 10
                                       s(ji, jj)=t(ji, jj)+1
                                    enddo
                                 enddo
                                 end program test_prog''')
    ast = parser(reader)
    psy = PSyFactory(API).create(ast)
    schedule = psy.invokes.get("test_prog").schedule

    do_loop = schedule.children[0]
    assert isinstance(do_loop, nemo.NemoLoop)
    var_accesses = VariablesAccessInfo()
    do_loop.reference_accesses(var_accesses)
    # TODO: n is not reported at the moment
    assert str(var_accesses) == "ji: READWRITE, jj: READWRITE, n: READ, "\
                            "s: WRITE, t: READ"


@pytest.mark.xfail(reason="Gocean loops boundaries are also strings")
def test_goloop():
    ''' Check the handling of non-NEMO do loops.
    TODO: Does not work atm, GOLoops also have start/stop as strings,
    which are even not defined. Only after genCode will they be defined.
    '''
    from psyclone.parse.algorithm import parse

    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invoke_info)
    do_loop = psy.invokes.get("invoke_0").schedule.children[0]
    assert isinstance(do_loop, Loop)
    var_accesses = VariablesAccessInfo()
    do_loop.reference_accesses(var_accesses)
    print(var_accesses)

