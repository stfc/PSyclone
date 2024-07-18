# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Modified: A. R. Porter and S. Siso, STFC Daresbury Laboratory

'''Module containing tests for the Intrinsic2CodeTrans abstract class which
provides common functionality for the intrinsic transformations (such as MIN,
ABS and SIGN).'''

import pytest
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.transformations.intrinsics.intrinsic2code_trans import (
    Intrinsic2CodeTrans)
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.psyir.nodes import (
    Reference, Assignment, Literal, IntrinsicCall)


def test_create():
    # pylint: disable=abstract-class-instantiated
    '''Check that Intrinsic2CodeTrans is abstract.'''
    with pytest.raises(TypeError) as excinfo:
        _ = Intrinsic2CodeTrans()
    msg = str(excinfo.value)
    # Python >= 3.9 spots that 'method' should be singular. Prior to this it
    # was plural. Python >= 3.12 tweaks the error message yet again to mention
    # the lack of an implementation and to quote the method name.
    # We split the check to accomodate for this.
    assert ("Can't instantiate abstract class Intrinsic2CodeTrans with" in msg)
    assert ("abstract method" in msg)
    assert ("apply" in msg)


class DummyTrans(Intrinsic2CodeTrans):
    '''Dummy transformation class used to test Intrinsic2CodeTrans
    methods.'''
    # pylint: disable=arguments-differ, no-method-argument
    def apply():
        '''Dummy apply method.'''


def test_init():
    '''Check that internal variables are initialised as expected.'''

    dummy = DummyTrans()
    assert dummy._intrinsic is None


def test_str_name():
    '''Check that str and name methods behave as expected.'''

    dummy = DummyTrans()
    dummy._intrinsic = IntrinsicCall.Intrinsic.SUM
    assert (str(dummy) == "Convert the PSyIR 'SUM' intrinsic to equivalent "
            "PSyIR code.")
    assert dummy.name == "DummyTrans"


def test_validate():
    '''Check that the validate method raises exceptions as expected.'''

    dummy = DummyTrans()
    dummy._intrinsic = IntrinsicCall.Intrinsic.ABS

    var = Literal("0.0", REAL_TYPE)
    intrinsic = IntrinsicCall.create(IntrinsicCall.Intrinsic.ABS, [var])

    with pytest.raises(TransformationError) as excinfo:
        dummy.validate(intrinsic)
    assert ("This transformation requires the operator to be part of an "
            "assignment statement, but no such assignment was found."
            in str(excinfo.value))

    reference = Reference(DataSymbol("fred", REAL_TYPE))
    _ = Assignment.create(lhs=reference, rhs=intrinsic)

    with pytest.raises(TransformationError) as excinfo:
        dummy.validate(None)
    assert ("The supplied node must be an 'IntrinsicCall', but found "
            "'NoneType'." in str(excinfo.value))

    with pytest.raises(TransformationError) as excinfo:
        dummy.validate(IntrinsicCall.create(
                           IntrinsicCall.Intrinsic.COS, [var.detach()]))
    assert ("Error in DummyTrans transformation. The supplied IntrinsicCall "
            "must be a 'ABS' but found: 'COS'." in str(excinfo.value))

    dummy.validate(intrinsic)
