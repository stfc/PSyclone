# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Reference PSyIR node. '''

import pytest
from psyclone.core.access_info import VariablesAccessInfo
from psyclone.psyGen import GenerationError
from psyclone.psyir.nodes import (
    Reference, ArrayReference, Assignment,
    Literal, BinaryOperation, Range, KernelSchedule)
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import (
    DataSymbol, ArrayType, ScalarType,
    REAL_SINGLE_TYPE, INTEGER_SINGLE_TYPE, REAL_TYPE, INTEGER_TYPE)


def test_reference_bad_init():
    '''Check that the __init__ method of the Reference class raises the
    expected exception if the symbol argument is not of the right
    type.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = Reference("hello")
    assert ("The Reference symbol setter expects a PSyIR Symbol object but "
            "found 'str'." in str(excinfo.value))


def test_reference_equality():
    '''
    Check that the __eq__ method of the Reference class behaves as expected,
    i.e. == is true iff:
    1. Both are the same type (Reference)
    2. They Reference the same symbol name
    '''
    symbol1 = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    symbol2 = DataSymbol("rname2", INTEGER_SINGLE_TYPE)

    ref1 = Reference(symbol1)
    ref2 = Reference(symbol1)
    ref3 = Reference(symbol2)

    assert ref2 == ref1
    assert ref1 != ref3

    # Create another symbol with the same name (but not the same instance)
    symbol3 = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    ref4 = Reference(symbol3)
    assert ref1 == ref4


def test_reference_node_str():
    ''' Check the node_str method of the Reference class.'''
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment()
    ref = Reference(symbol, parent=assignment)
    coloredtext = colored("Reference", Reference._colour)
    assert coloredtext+"[name:'rname']" in ref.node_str()


def test_reference_can_be_printed():
    '''Test that a Reference instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment()
    ref = Reference(symbol, parent=assignment)
    assert "Reference[name:'rname']" in str(ref)


def test_reference_optional_parent():
    '''Test that the parent attribute is None if the optional parent
    argument is not supplied.

    '''
    ref = Reference(DataSymbol("rname", REAL_SINGLE_TYPE))
    assert ref.parent is None


def test_reference_children_validation():
    '''Test that children added to Reference are validated. A Reference node
    does not accept any children.

    '''
    ref = Reference(DataSymbol("rname", REAL_SINGLE_TYPE))
    with pytest.raises(GenerationError) as excinfo:
        ref.addchild(Literal("2", INTEGER_SINGLE_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'Reference'. Reference is a"
            " LeafNode and doesn't accept children.") in str(excinfo.value)


def test_reference_is_array():
    '''Test that a non-array reference is marked as not an array.
    '''
    reference = Reference(DataSymbol("test", REAL_TYPE))
    assert reference.is_array is False


def test_reference_datatype():
    '''Test the datatype property.

    '''
    reference = Reference(DataSymbol("test", REAL_TYPE))
    assert isinstance(reference.datatype, ScalarType)
    assert reference.datatype.intrinsic == ScalarType.Intrinsic.REAL


def test_reference_accesses():
    '''Test that the reference_accesses method behaves as expected in the
    usual case (see the next test for the unusual case).

    '''
    reference = Reference(DataSymbol("test", REAL_TYPE))
    var_access_info = VariablesAccessInfo()
    reference.reference_accesses(var_access_info)
    assert (str(var_access_info)) == "test: READ"


@pytest.mark.parametrize("operator_type", [BinaryOperation.Operator.LBOUND,
                                           BinaryOperation.Operator.UBOUND])
def test_reference_accesses_bounds(operator_type):
    '''Test that the reference_accesses method behaves as expected when
    the reference is the first argument to either the lbound or ubound
    intrinsic as that is simply looking up the array bounds (therefore
    var_access_info should be empty) and when the reference is the
    second argument of either the lbound or ubound intrinsic (in which
    case the access should be a read).

    '''
    # Note, one would usually expect UBOUND to provide the upper bound
    # of a range but to simplify the test both LBOUND and UBOUND are
    # used for the lower bound. This does not affect the test.
    one = Literal("1", INTEGER_TYPE)
    array_symbol = DataSymbol("test", ArrayType(REAL_TYPE, [10]))
    array_ref1 = Reference(array_symbol)
    array_ref2 = Reference(array_symbol)
    array_access = ArrayReference.create(array_symbol, [one])

    # test when first or second argument to LBOUND or UBOUND is an
    # array reference
    operator = BinaryOperation.create(operator_type, array_ref1, array_ref2)
    array_access.children[0] = Range.create(operator, one.copy(), one.copy())
    var_access_info = VariablesAccessInfo()
    array_ref1.reference_accesses(var_access_info)
    assert str(var_access_info) == ""
    var_access_info = VariablesAccessInfo()
    array_ref2.reference_accesses(var_access_info)
    assert str(var_access_info) == "test: READ"


def test_reference_can_be_copied():
    ''' Test that a reference can be copied. '''

    array_symbol = DataSymbol("symbol", ArrayType(REAL_TYPE, [10]))
    scalar_symbol = DataSymbol("other", REAL_TYPE)

    ref = Reference(array_symbol)

    ref1 = ref.copy()
    assert isinstance(ref1, Reference)
    assert ref1 is not ref
    assert ref1.symbol is array_symbol

    # Modifying the new reference does not affect the original
    ref1._symbol = scalar_symbol
    assert ref.symbol is array_symbol
