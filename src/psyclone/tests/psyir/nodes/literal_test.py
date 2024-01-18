# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Literal PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Literal
from psyclone.psyir.symbols import ScalarType, ArrayType, \
    REAL_DOUBLE_TYPE, INTEGER_SINGLE_TYPE, BOOLEAN_TYPE, CHARACTER_TYPE
from psyclone.errors import GenerationError
from psyclone.psyir.nodes.node import colored


def test_literal_init():
    '''Test the initialisation Literal object with ScalarType and
    ArrayType and different precisions.

    '''
    array_type = ArrayType(REAL_DOUBLE_TYPE, [10, 10])
    literal = Literal("1", array_type)
    assert literal.value == "1"
    assert isinstance(literal.datatype, ArrayType)
    assert literal.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert literal.datatype.precision == ScalarType.Precision.DOUBLE
    assert len(literal.datatype.shape) == 2
    assert isinstance(literal.datatype.shape[0], ArrayType.ArrayBounds)
    assert isinstance(literal.datatype.shape[0].lower, Literal)
    assert isinstance(literal.datatype.shape[0].upper, Literal)
    assert literal.datatype.shape[0].lower.value == '1'
    assert literal.datatype.shape[0].upper.value == '10'
    assert (literal.datatype.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (literal.datatype.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert isinstance(literal.datatype.shape[1].upper, Literal)
    assert literal.datatype.shape[1].lower.value == '1'
    assert literal.datatype.shape[1].upper.value == '10'
    assert (literal.datatype.shape[1].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (literal.datatype.shape[1].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)

    literal = Literal("true", BOOLEAN_TYPE)
    assert literal.value == "true"
    assert literal.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert literal.datatype.precision == ScalarType.Precision.UNDEFINED

    literal = Literal("false", BOOLEAN_TYPE)
    assert literal.value == "false"
    assert literal.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert literal.datatype.precision == ScalarType.Precision.UNDEFINED


def test_literal_init_invalid_1():
    '''Test the initialisation of a Literal object with invalid parameters.'''

    # Test invalid datatype type
    with pytest.raises(TypeError) as err:
        Literal("1", 1)
    assert ("The datatype of a Literal must be an instance of psyir.symbols."
            "ScalarType or psyir.symbols.ArrayType but found"
            in str(err.value))

    # Test invalid value type
    with pytest.raises(TypeError) as err:
        Literal(1, INTEGER_SINGLE_TYPE)
    assert ("Literals must be supplied with a value encoded as a string but "
            "found" in str(err.value))

    # Test invalid boolean value
    with pytest.raises(ValueError) as err:
        Literal("invalid", BOOLEAN_TYPE)
    assert ("A scalar boolean literal can only be: 'true' or 'false' "
            "but found 'invalid'." in str(err.value))

    with pytest.raises(ValueError) as err:
        Literal("TRUE", BOOLEAN_TYPE)
    assert ("A scalar boolean literal can only be: 'true' or 'false' "
            "but found 'TRUE'." in str(err.value))

    with pytest.raises(ValueError) as err:
        Literal("False", BOOLEAN_TYPE)
    assert ("A scalar boolean literal can only be: 'true' or 'false' "
            "but found 'False'." in str(err.value))


@pytest.mark.parametrize("value", ["*2", "++2", ".", ".2", "2..3", "2.3.4",
                                   "3.2d0", "3.2e+", "3.2e2*", "3e2e2"])
def test_literal_init_invalid_2(value):
    '''Test the initialisation of a Literal object with invalid real
    values raises the expected exception.

    '''
    with pytest.raises(ValueError) as err:
        Literal(value, REAL_DOUBLE_TYPE)
    assert (f"A scalar real literal value must conform to the supported "
            f"format ('^[+-]?[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?$') but "
            f"found '{value}'." in str(err.value))


@pytest.mark.parametrize("value", ["++2", "12.34", "*2", ".3"])
def test_literal_init_invalid_3(value):
    '''Test the initialisation of a Literal object with invalid int
    values raises the expected exception.

    '''
    with pytest.raises(ValueError) as err:
        Literal(value, INTEGER_SINGLE_TYPE)
    assert (f"A scalar integer literal value must conform to the "
            f"supported format ('(([+-]?[0-9]+)|(NOT_INITIALISED))') "
            f"but found '{value}'." in str(err.value))


def test_literal_init_empty_value():
    '''Test the initialisation of a Literal object with an empty value
    argument raises the expected exception unless it is of CHARACTER_TYPE.

    '''
    with pytest.raises(ValueError) as err:
        Literal("", REAL_DOUBLE_TYPE)
    assert "A non-character literal value cannot be empty." in str(err.value)
    lit = Literal("", CHARACTER_TYPE)
    assert lit.value == ""


@pytest.mark.parametrize("value",
                         ["2", "+2", "-2", "2.", "23", "23.4", "-23.45",
                          "+23.45e0", "23.45e10", "-23.45e-10", "000.1",
                          "+23.45e+10", "+23e-10", "23.e10", "2.4E-5"])
def test_real_literal_init_valid_value(value):
    '''Test the initialisation of a Literal object with valid real values.
    Include check that we are not case sensitive. '''
    _ = Literal(value, REAL_DOUBLE_TYPE)


@pytest.mark.parametrize("value",
                         ["2", "+2", "-2", "1234", "000", "-000"])
def test_integer_literal_init_valid_value(value):
    '''Test the initialisation of a Literal object with valid int values. '''
    _ = Literal(value, INTEGER_SINGLE_TYPE)


def test_literal_value():
    '''Test the value property returns the value of the Literal object.'''
    integer_type = ScalarType(
        ScalarType.Intrinsic.INTEGER, ScalarType.Precision.DOUBLE)
    literal = Literal("1", integer_type)
    assert literal.value == "1"


@pytest.mark.parametrize("value", ["1.0E+3", "1.0", "0.01E-3", "3.14e-2"])
def test_literal_real_value(value):
    ''' Test the value property returns the expected string for a Literal
    representing a real quantity. '''
    real_type = ScalarType(
        ScalarType.Intrinsic.REAL, ScalarType.Precision.DOUBLE)
    literal = Literal(value, real_type)
    assert literal.value == value.lower()


def test_literal_node_str():
    ''' Check the node_str method of the Literal class.'''
    # scalar literal
    literal = Literal("1", INTEGER_SINGLE_TYPE)
    coloredtext = colored("Literal", Literal._colour)
    assert (coloredtext+"[value:'1', Scalar<INTEGER, SINGLE>]"
            in literal.node_str())

    # array literal
    array_type = ArrayType(REAL_DOUBLE_TYPE, [10, 10])
    literal = Literal("1", array_type)
    coloredtext = colored("Literal", Literal._colour)
    assert (coloredtext+"[value:'1', Array<Scalar<REAL, DOUBLE>, "
            "shape=[10, 10]>]"
            in literal.node_str())


def test_literal_can_be_printed():
    '''Test that a Literal instance can always be printed (i.e. is
    initialised fully)'''
    array_type = ArrayType(REAL_DOUBLE_TYPE, [10, 10])
    literal = Literal("1", array_type)
    assert ("Literal[value:'1', Array<Scalar<REAL, DOUBLE>, shape=[10, 10]>]"
            in str(literal))


def test_literal_children_validation():
    '''Test that children added to Literals are validated. A Literal node does
    not accept any children.

    '''
    literal = Literal("1", INTEGER_SINGLE_TYPE)
    with pytest.raises(GenerationError) as excinfo:
        literal.addchild(Literal("2", INTEGER_SINGLE_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'Literal'. Literal is a"
            " LeafNode and doesn't accept children.") in str(excinfo.value)


def test_literal_can_be_copied():
    ''' Test that a Literal node can be copied. '''

    literal = Literal("1", INTEGER_SINGLE_TYPE)

    literal1 = literal.copy()
    assert isinstance(literal1, Literal)
    assert literal1 is not literal
    assert literal1.value == "1"
    assert literal1.datatype is INTEGER_SINGLE_TYPE

    # Modifying the new literal does not affect the original
    literal1._value = "2"
    assert literal1.value == "2"
    assert literal.value == "1"


def test_literal_equality():
    ''' Test the __eq__ method of the Literal node. '''
    literal = Literal("1", INTEGER_SINGLE_TYPE)
    literal2 = Literal("1", INTEGER_SINGLE_TYPE)
    literal3 = Literal("10", INTEGER_SINGLE_TYPE)
    literal4 = Literal("1", REAL_DOUBLE_TYPE)

    assert literal == literal2
    assert literal != literal3
    assert literal != literal4
