# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Literal PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Literal
from psyclone.psyir.symbols import DataType, ScalarType, ArrayType


def test_literal_init():
    '''Test the initialisation Literal object with ScalarType and
    ArrayType and different precisions.

    '''
    scalar_type = ScalarType(
        ScalarType.Name.REAL, ScalarType.Precision.DOUBLE)
    array_type = ArrayType(scalar_type, [10,10])
    literal = Literal("1", array_type)
    assert literal.value == "1"
    assert isinstance(literal.datatype, ArrayType)
    assert literal.datatype.name == ScalarType.Name.REAL
    assert literal.datatype.precision == ScalarType.Precision.DOUBLE
    assert literal.datatype.shape == [10,10]

    scalar_type = ScalarType(
        ScalarType.Name.BOOLEAN, ScalarType.Precision.SINGLE)
    literal = Literal("true", scalar_type)
    assert literal.value == "true"
    assert literal.datatype.name == ScalarType.Name.BOOLEAN
    assert literal.datatype.precision == ScalarType.Precision.SINGLE

    literal = Literal("false", scalar_type)
    assert literal.value == "false"
    assert literal.datatype.name == ScalarType.Name.BOOLEAN
    assert literal.datatype.precision == ScalarType.Precision.SINGLE


def test_literal_init_invalid():
    '''Test the initialisation of a Literal object with invalid parameters.'''

    # Test invalid datatype type
    with pytest.raises(TypeError) as err:
        Literal("1", 1)
    assert ("The datatype of a Literal must be an instance of psyir.symbols."
            "ScalarType or psyir.symbols.ArrayType but found"
            in str(err.value))

    # Test invalid value type
    integer_type = ScalarType(
        ScalarType.Name.INTEGER, ScalarType.Precision.SINGLE)
    with pytest.raises(TypeError) as err:
        Literal(1, integer_type)
    assert ("Literals must be supplied with a value encoded as a string but "
            "found" in str(err.value))

    # Test invalid boolean value
    boolean_type = ScalarType(
        ScalarType.Name.BOOLEAN, ScalarType.Precision.SINGLE)
    with pytest.raises(ValueError) as err:
        Literal("invalid", boolean_type)
    assert ("A scalar boolean literal can only be: 'true' or 'false' "
            "but found 'invalid'." in str(err.value))

    with pytest.raises(ValueError) as err:
        Literal("TRUE", boolean_type)
    assert ("A scalar boolean literal can only be: 'true' or 'false' "
            "but found 'TRUE'." in str(err.value))

    with pytest.raises(ValueError) as err:
        Literal("False", boolean_type)
    assert ("A scalar boolean literal can only be: 'true' or 'false' "
        "but found 'False'." in str(err.value))


def test_literal_value():
    '''Test the value property returns the value of the Literal object.'''
    integer_type = ScalarType(
        ScalarType.Name.INTEGER, ScalarType.Precision.DOUBLE)
    literal = Literal("1", integer_type)
    assert literal.value == "1"


def test_literal_node_str():
    ''' Check the node_str method of the Literal class.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    
    # scalar literal
    integer_type = ScalarType(
        ScalarType.Name.INTEGER, ScalarType.Precision.SINGLE)
    literal = Literal("1", integer_type)
    coloredtext = colored("Literal", SCHEDULE_COLOUR_MAP["Literal"])
    assert (coloredtext+"[value:'1', Name.INTEGER, Precision.SINGLE]"
            in literal.node_str())

    # array literal
    scalar_type = ScalarType(
        ScalarType.Name.REAL, ScalarType.Precision.DOUBLE)
    array_type = ArrayType(scalar_type, [10,10])
    literal = Literal("1", array_type)
    coloredtext = colored("Literal", SCHEDULE_COLOUR_MAP["Literal"])
    assert (coloredtext+"[value:'1', Name.REAL, Precision.DOUBLE, "
            "shape=[10, 10]]" in literal.node_str())


def test_literal_can_be_printed():
    '''Test that a Literal instance can always be printed (i.e. is
    initialised fully)'''
    scalar_type = ScalarType(
        ScalarType.Name.REAL, ScalarType.Precision.DOUBLE)
    array_type = ArrayType(scalar_type, [10,10])
    literal = Literal("1", array_type)
    assert ("Literal[value:'1', Name.REAL, Precision.DOUBLE, shape=[10, 10]]"
            in str(literal))
