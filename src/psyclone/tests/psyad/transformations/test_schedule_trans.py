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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
#
'''Module to test the psyad schedule transformation.'''

from __future__ import absolute_import
import pytest

from psyclone.psyad.transformations import AssignmentTrans, \
    TangentLinearError, ScheduleTrans

from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import BinaryOperation, Reference, Assignment, \
    Literal, UnaryOperation, Schedule
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE, INTEGER_TYPE, \
    ScalarType
from psyclone.psyir.transformations import TransformationError

from psyclone.tests.utilities import Compile


def check_adjoint(tl_fortran, active_variable_names, expected_ad_fortran,
                  tmpdir):
    '''Utility routine that takes tangent linear fortran code as input in
    the argument 'tl_fortran', transforms this code into its adjoint
    using the active variables specified in the
    'active_variable_names' argument and tests whether the result is
    the same as the expected result in the 'expected_ad_fortran'
    argument.

    To help keep the test code short this routine also adds the
    subroutine / end subroutine lines to the incoming code.

    :param str tl_fortran: tangent linear code.
    :param list of str active_variable_names: a list of active \
        variable names.
    :param str tl_fortran: the expected adjoint code to be produced.
    :param str tmpdir: temporary directory created by pytest in which
    to perform compilation.

    '''
    # Add "subroutine / end subroutine" lines to the incoming code.
    input_code = ("subroutine test()\n{0}end subroutine test\n"
                  "".format(tl_fortran))
    expected_output_code = ("subroutine test()\n{0}end subroutine test\n"
                            "".format(expected_ad_fortran))

    # Translate the tangent linear code to PSyIR.
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)

    # Find the schedule in the PSyIR.
    schedule = psyir.children[0]
    assert isinstance(schedule, Schedule)

    # Find the symbols in the symbol table associated with the
    # supplied active variable strings as the transformation expects
    # active variables to be provided as symbols.
    symbol_table = schedule.scope.symbol_table
    active_variables = []
    for variable_name in active_variable_names:
        active_variables.append(symbol_table.lookup(variable_name))

    # Apply the tangent linear to adjoint transformation.
    trans = ScheduleTrans(active_variables)
    trans.apply(schedule)

    # Translate the adjoint code to Fortran.
    writer = FortranWriter()
    ad_fortran = writer(psyir)

    # Check that the code produced is the same as the expected code
    # provided.
    assert ad_fortran == expected_output_code

    # Check that the code produced will compile.
    assert Compile(tmpdir).string_compiles(ad_fortran)

# initialisation


def test_init():
    '''Test the ScheduleTrans transformation can be created and has the
    expected properties.'''
    trans = ScheduleTrans([DataSymbol("a", REAL_TYPE)])
    assert isinstance(trans, ScheduleTrans)

# apply method


def test_apply_one_active(tmpdir):
    '''Test the ScheduleTrans apply method works when there is a single
    active assignment in the schedule.

    '''
    tl_fortran = (
        "  real a, b, c, d\n"
        "  real w, x, y, z\n"
        "  a = w*a+x*b+y*c+d*z\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n"
        "  real :: w\n  real :: x\n"
        "  real :: y\n  real :: z\n\n"
        "  b = b + x * a\n"
        "  c = c + y * a\n"
        "  d = d + a * z\n"
        "  a = w * a\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_apply_multi_active(tmpdir):
    '''Test the ScheduleTrans apply method works when there are multiple
    active assignments in the schedule.

    '''
    tl_fortran = (
        "  real a, b, c, d\n"
        "  real w, x, y, z\n"
        "  a = w*a+x*b+y*c+d*z\n"
        "  b = b+x*d\n"
        "  c = y*a\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n"
        "  real :: w\n  real :: x\n"
        "  real :: y\n  real :: z\n\n"
        ""
        "  a = a + y * c\n"
        "  c = 0.0\n"
        ""
        "  d = d + x * b\n"
        ""
        "  b = b + x * a\n"
        "  c = c + y * a\n"
        "  d = d + a * z\n"
        "  a = w * a\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_apply_one_inactive(tmpdir):
    '''Test the ScheduleTrans apply method works when there is a single
    inactive assignment in the schedule.

    '''
    tl_fortran = (
        "  real a, b, c, d\n"
        "  real w, x, y, z\n"
        "  w = x*y*z\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n"
        "  real :: w\n  real :: x\n"
        "  real :: y\n  real :: z\n\n"
        "  w = x * y * z\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_apply_multi_inactive(tmpdir):
    '''Test the ScheduleTrans apply method works when there are multiple
    inactive assignments in the schedule.

    '''
    tl_fortran = (
        "  real a, b, c, d\n"
        "  real w, x, y, z\n"
        "  w = x*y*z\n"
        "  x = y\n"
        "  z = z/w\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n"
        "  real :: w\n  real :: x\n"
        "  real :: y\n  real :: z\n\n"
        "  w = x * y * z\n"
        "  x = y\n"
        "  z = z / w\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_apply_multi_active(tmpdir):
    '''Test the ScheduleTrans apply method works when there is a mixture
   of active and inactive assignments in the schedule.

    '''
    tl_fortran = (
        "  real a, b, c, d\n"
        "  real w, x, y, z\n"
        "  c = y*a\n"
        "  x = y*z\n"
        "  b = b+x*d\n"
        "  z = x+y\n"
        "  a = w*a+x*b+y*c+d*z\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n"
        "  real :: w\n  real :: x\n"
        "  real :: y\n  real :: z\n\n"
        ""
        "  x = y * z\n"
        "  z = x + y\n"
        ""
        "  b = b + x * a\n"
        "  c = c + y * a\n"
        "  d = d + a * z\n"
        "  a = w * a\n"
        ""
        "  d = d + x * b\n"
        ""
        "  a = a + y * c\n"
        "  c = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


@pytest.mark.xfail(reason="Incorrect code is output if the variable in an inactive "
                   "assignment is read by an earlier statement.")
def test_apply_multi_dependent_active(tmpdir):
    '''Test the ScheduleTrans apply method works when there is a mixture
   of active and inactive assignments in the schedule, the inactive
   variables are updated and have both forward and backward
   dependencies (i.e. the updated inactive variable is read both
   before and after it is updated).

    '''
    tl_fortran = (
        "  real a, b, c\n"
        "  real y\n"
        "  y = 2\n"
        "  a = a + y*b\n"
        "  y = 3\n"
        "  b = b + y*c\n")
    active_variables = ["a", "b", "c"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: y\n\n"
        "  y = 3\n"
        "  c = c + y * b\n"
        "  y = 2\n"
        "  b = b + y * a\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)

# validate method


def test_validate_invalid_schedule():
    '''Test the validate method raises the expected exception if the node
    is not a Schedule. Also check that the validate method is called
    from the apply() method.

    '''
    schedule_trans = ScheduleTrans([DataSymbol("x", REAL_TYPE)])
    with pytest.raises(TransformationError) as info:
        schedule_trans.validate(None)
    assert ("Node argument in schedule transformation should be a PSyIR "
            "Schedule, but found 'NoneType'." in str(info.value))
    with pytest.raises(TransformationError) as info:
        schedule_trans.apply(None)
    assert ("Node argument in schedule transformation should be a PSyIR "
            "Schedule, but found 'NoneType'." in str(info.value))

# TODO: A question. If we do use transformations to recurse through
# the tree then should the validate's recurse as well or is it OK for
# the apply to call validate when it recurses?


def test_apply_multi_dependent_active():
    '''Test the ScheduleTrans validate method raises the expected
    exception when one of the assignments in the schedule is not in a
    valid tangent-linear form. This is actually picked up by the
    AssignmentTrans transformation but we check it here to make sure
    it is working correctly.

    '''
    tl_fortran = (
        "  real a, b\n"
        "  real y\n"
        "  a = a + y*b\n"
        "  b = b + y*y\n")
    active_variables = ["a", "b"]
    with pytest.raises(TangentLinearError) as info:
        check_adjoint(tl_fortran, active_variables, None, None)
    assert ("Each non-zero term on the RHS of the assigment "
            "'b = b + y * y\n' must have an active variable but 'y * y' "
            "does not." in str(info.value))


# str() and name() methods


def test_str():
    ''' Test the str operation returns the expected result.'''

    schedule_trans = ScheduleTrans([DataSymbol("x", REAL_TYPE)])
    assert (str(schedule_trans) == "Convert a tangent-linear PSyIR "
            "Schedule to its adjoint form")


def test_name():
    ''' Test the name method returns the expected result.'''

    schedule_trans = ScheduleTrans([DataSymbol("x", REAL_TYPE)])
    assert schedule_trans.name == "ScheduleTrans"
