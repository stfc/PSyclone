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
# ----------------------------------------------------------------------------
# Author: R. W. Ford, STFC Daresbury Lab

'''This module tests the hoist transformation.
'''

from __future__ import absolute_import, print_function
import pytest

from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Literal, Loop, Assignment, Reference, \
    IfBlock, ArrayReference
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE, INTEGER_TYPE, \
    BOOLEAN_TYPE, ArrayType
from psyclone.psyir.transformations import HoistTrans, TransformationError
from psyclone.tests.utilities import Compile


# init

def test_init():
    '''Test a hoist transformation can be succesfully created.'''
    hoist_trans = HoistTrans()
    assert isinstance(hoist_trans, HoistTrans)


# apply

def test_apply(tmpdir):
    '''Test the apply method moves the loop invariant assignment out of
    the loop and places it immediately before the loop.

    '''
    code = (
        "program test\n"
        "  integer :: i\n"
        "  real :: a\n"
        "  do i=1,1\n"
        "    a = 1.0\n"
        "  end do\n"
        "end program\n")
    expected_code = (
        "program test\n"
        "  integer :: i\n"
        "  real :: a\n\n"
        "  a = 1.0\n"
        "  do i = 1, 1, 1\n"
        "  enddo\n\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    assignment = psyir.walk(Assignment)[0]
    hoist_trans = HoistTrans()
    hoist_trans.apply(assignment)
    writer = FortranWriter()
    output = writer(psyir)
    assert output == expected_code
    assert Compile(tmpdir).string_compiles(output)


def test_apply_validate():

    '''Test the apply method calls the validate method.'''
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.apply(None)
    assert ("The target of the HoistTrans transformation should be an "
            "assignment, but found 'NoneType'." in str(info.value))


# validate

def test_validate_node():
    '''Test the expected exception is raised if an invalid node is
    supplied to the transformation.

    '''
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(None)
    assert ("The target of the HoistTrans transformation should be an "
            "assignment, but found 'NoneType'." in str(info.value))


def test_validate_ancestor_loop():
    '''Test the expected exception is raised if the supplied assignment is
    not within a loop.

    '''
    assignment = Assignment.create(
        Reference(DataSymbol("a", REAL_TYPE)), Literal("1.0", REAL_TYPE))
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The supplied assignment node 'a = 1.0\n' should be within a "
            "loop, but no loop was found." in str(info.value))


def test_validate_direct_loop():
    '''Test the expected exception is raised if the supplied assignment is
    not the direct child of a loop (ignoring schedule).

    '''
    assignment = Assignment.create(
        Reference(DataSymbol("a", REAL_TYPE)), Literal("1.0", REAL_TYPE))
    condition = Literal("true", BOOLEAN_TYPE)
    if_condition = IfBlock.create(condition, [assignment])
    one = Literal("1", INTEGER_TYPE)
    _ = Loop.create(
        DataSymbol("i", INTEGER_TYPE), one, one.copy(), one.copy(),
        [if_condition])
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The supplied assignment node 'a = 1.0\n' should be directly "
            "within a loop but found 'if (.true.) then\n  a = 1.0\nend if\n'."
            in str(info.value))


@pytest.mark.xfail(
    reason="issue #1378: dependence analysis needs to be added.")
def test_validate_dependent_variable():
    '''Test the expected exception is raised if the supplied assignment
    depends on the loop iterator.

    '''
    array_symbol = DataSymbol("a", ArrayType(REAL_TYPE, [10]))
    loop_iterator = DataSymbol("i", INTEGER_TYPE)
    array_reference = ArrayReference.create(
        array_symbol, [Reference(loop_iterator)])
    assignment = Assignment.create(array_reference, Literal("1.0", REAL_TYPE))
    one = Literal("1", INTEGER_TYPE)
    _ = Loop.create(
        loop_iterator, one, one.copy(), one.copy(), [assignment])
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The supplied assignment node 'a(i) = 1.0\n' depends on the "
            "parent loop iterator 'i'." in str(info.value))


# name and str

def test_name_str():
    '''Test the hoist transformations name and str methods return the
    expected results.

    '''
    hoist_trans = HoistTrans()
    assert hoist_trans.name == "HoistTrans"
    assert str(hoist_trans) == "Hoist an assignment outside of its parent loop"
