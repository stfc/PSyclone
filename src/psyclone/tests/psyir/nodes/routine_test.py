# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the pytest tests for the Routine class. '''

import pytest
from psyclone.psyir.nodes import Routine, Assignment
from psyclone.psyir.symbols import INTEGER_TYPE


def test_routine_constructor():
    ''' Check the constructor and associated type checking. '''
    with pytest.raises(TypeError) as err:
        Routine(1)
    assert "must be a str but got" in str(err.value)
    with pytest.raises(TypeError) as err:
        Routine("hello", entry_point=1)
    assert "'entry_point' must be a bool" in str(err.value)
    with pytest.raises(TypeError) as err:
        Routine("hello", return_type=1)
    assert "'return_type' must be of type DataType" in str(err.value)
    node = Routine("hello")
    assert node._name == "hello"


def test_routine_properties():
    ''' Check the various properties of the Routine class. '''
    node1 = Routine("hello")
    assert node1.dag_name == "routine_hello_0"
    assert node1.return_type is None
    assert node1.entry_point is False
    assert node1.name == "hello"
    # Give the Routine a child to get full coverage of __str__ method
    node1.addchild(Assignment())
    assert "Schedule[hello]:\nAssignment" in str(node1)

    node2 = Routine("bonjour", return_type=INTEGER_TYPE)
    assert node2.return_type == INTEGER_TYPE
    assert node2.entry_point is False

    node3 = Routine("gutentag", entry_point=True)
    assert node3.return_type is None
    assert node3.entry_point

    node4 = Routine("welcome", entry_point=True, return_type=INTEGER_TYPE)
    assert node4.return_type == INTEGER_TYPE
    assert node4.entry_point
