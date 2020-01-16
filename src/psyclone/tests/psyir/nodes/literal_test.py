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
# Authors S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Literal PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Literal
from psyclone.psyir.symbols import DataType


def test_literal_init():
    '''Test the initialisation Literal object.'''
    literal = Literal("1", DataType.INTEGER)
    assert literal._value == "1"
    assert literal._datatype == DataType.INTEGER

    literal = Literal("true", DataType.BOOLEAN)
    assert literal._value == "true"
    assert literal._datatype == DataType.BOOLEAN

    literal = Literal("false", DataType.BOOLEAN)
    assert literal._value == "false"
    assert literal._datatype == DataType.BOOLEAN


def test_literal_init_invalid():
    '''Test the initialisation of a Literal object with invalid parameters.'''

    # Test invalid datatype type
    with pytest.raises(TypeError) as err:
        Literal("1", 1)
    assert ("The datatype of a Literal must be an instance of psyir.symbols."
            "DataType but got" in str(err.value))

    # Test invalid datatype
    with pytest.raises(ValueError) as err:
        Literal("1", DataType.DEFERRED)
    assert "The datatype of a Literal must be one of" in str(err.value)

    # Test invalid value type
    with pytest.raises(TypeError) as err:
        Literal(1, DataType.INTEGER)
    assert "Literals must be supplied with a value encoded as a string but " \
        "got: " in str(err.value)

    # Test invalid boolean value
    with pytest.raises(ValueError) as err:
        Literal("invalid", DataType.BOOLEAN)
    assert "A DataType.BOOLEAN Literal can only be: 'true' or 'false' " \
        "but got 'invalid' instead." in str(err.value)

    with pytest.raises(ValueError) as err:
        Literal("TRUE", DataType.BOOLEAN)
    assert "A DataType.BOOLEAN Literal can only be: 'true' or 'false' " \
        "but got 'TRUE' instead." in str(err.value)

    with pytest.raises(ValueError) as err:
        Literal("False", DataType.BOOLEAN)
    assert "A DataType.BOOLEAN Literal can only be: 'true' or 'false' " \
        "but got 'False' instead." in str(err.value)


def test_literal_value():
    '''Test the value property returns the value of the Literal object.'''
    literal = Literal("1", DataType.INTEGER)
    assert literal.value == "1"


def test_literal_node_str():
    ''' Check the node_str method of the Literal class.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    literal = Literal("1", DataType.INTEGER)
    coloredtext = colored("Literal", SCHEDULE_COLOUR_MAP["Literal"])
    assert coloredtext+"[value:'1', DataType.INTEGER]" in literal.node_str()


def test_literal_can_be_printed():
    '''Test that an Literal instance can always be printed (i.e. is
    initialised fully)'''
    literal = Literal("1", DataType.INTEGER)
    assert "Literal[value:'1', DataType.INTEGER]" in str(literal)
