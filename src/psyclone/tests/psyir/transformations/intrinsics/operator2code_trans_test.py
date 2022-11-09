# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Laboratory
# Modified: A. R. Porter, STFC Daresbury Laboratory

'''Module containing tests for the operator abstract class which
provides common functionality for the intrinsic operator
transformations (such as MIN, ABS and SIGN).'''

from __future__ import absolute_import
import pytest
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.transformations.intrinsics.operator2code_trans import \
    Operator2CodeTrans
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.psyir.nodes import Reference, UnaryOperation, Assignment, Literal


def test_create():
    # pylint: disable=abstract-class-instantiated
    '''Check that Operator2CodeTrans is abstract.'''
    with pytest.raises(TypeError) as excinfo:
        _ = Operator2CodeTrans()
    msg = str(excinfo.value)
    # Have to split this check as Python >= 3.10 spots that 'method'
    # should be singular.
    assert ("Can't instantiate abstract class Operator2CodeTrans with "
            "abstract method" in msg)
    assert " apply" in msg


class DummyTrans(Operator2CodeTrans):
    '''Dummy transformation class used to test Operator2CodeTrans
    methods.'''
    # pylint: disable=arguments-differ, no-method-argument
    def apply():
        '''Dummy apply method.'''


def test_init():
    '''Check that internal variables are initialised as expected.'''

    dummy = DummyTrans()
    assert dummy._operator_name is None
    assert dummy._classes is None
    assert dummy._operators is None


def test_str_name():
    '''Check that str and name methods behave as expected.'''

    dummy = DummyTrans()
    # operator_name is usually set by the Transformation's __init__
    # method but set it manually here to avoid creating multiple
    # implementations of DummyTrans.
    dummy._operator_name = "hello"
    assert (str(dummy) == "Convert the PSyIR HELLO intrinsic to equivalent "
            "PSyIR code.")
    assert dummy.name == "Hello2CodeTrans"


def test_validate():
    '''Check that the validate method raises exceptions as expected.'''

    dummy = DummyTrans()
    # operator_name, classes and operators are usually set by the
    # Transformation's __init__ method but set them manually here to
    # avoid creating multiple implementations of DummyTrans.
    dummy._operator_name = "hello"
    dummy._classes = (UnaryOperation,)
    dummy._operators = (UnaryOperation.Operator.ABS,)

    var = Literal("0.0", REAL_TYPE)
    operator = UnaryOperation.create(UnaryOperation.Operator.ABS, var)

    with pytest.raises(TransformationError) as excinfo:
        dummy.validate(operator)
    assert ("This transformation requires the operator to be part of an "
            "assignment statement, but no such assignment was found."
            in str(excinfo.value))

    reference = Reference(DataSymbol("fred", REAL_TYPE))
    _ = Assignment.create(lhs=reference, rhs=operator)

    with pytest.raises(TransformationError) as excinfo:
        dummy.validate(None)
    assert ("The supplied node argument is not a hello operator, found "
            "'NoneType'." in str(excinfo.value))

    with pytest.raises(TransformationError) as excinfo:
        dummy.validate(UnaryOperation(UnaryOperation.Operator.SIN, var))
    assert ("Error in Hello2CodeTrans transformation. The supplied node "
            "operator is invalid, found 'Operator.SIN', but expected one "
            "of '['ABS']'." in str(excinfo.value))

    dummy.validate(operator)
