# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the Intrinsic2CodeBaseTrans abstract class which
provides common functionality for the intrinsic transformations (such as MIN,
ABS and SIGN).'''

import pytest
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.transformations.intrinsics.intrinsic2code_basetrans \
    import Intrinsic2CodeBaseTrans
from psyclone.psyir.symbols import DataSymbol, ScalarType
from psyclone.psyir.nodes import (
    Reference, Assignment, Literal, IntrinsicCall)


def test_create():
    # pylint: disable=abstract-class-instantiated
    '''Check that Intrinsic2CodeBaseTrans is abstract.'''
    with pytest.raises(TypeError) as excinfo:
        _ = Intrinsic2CodeBaseTrans()
    msg = str(excinfo.value)
    # Python >= 3.9 spots that 'method' should be singular. Prior to this it
    # was plural. Python >= 3.12 tweaks the error message yet again to mention
    # the lack of an implementation and to quote the method name.
    # We split the check to accommodate for this.
    assert ("Can't instantiate abstract class Intrinsic2CodeBaseTrans with" in msg)
    assert ("abstract method" in msg)
    assert ("apply" in msg)


class DummyTrans(Intrinsic2CodeBaseTrans):
    '''Dummy transformation class used to test Intrinsic2CodeBaseTrans
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

    var = Literal("0.0", ScalarType.real_type())
    intrinsic = IntrinsicCall.create(IntrinsicCall.Intrinsic.ABS, [var])

    with pytest.raises(TransformationError) as excinfo:
        dummy.validate(intrinsic)
    assert ("This transformation requires the operator to be part of an "
            "assignment statement, but no such assignment was found."
            in str(excinfo.value))

    reference = Reference(DataSymbol("fred", ScalarType.real_type()))
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
