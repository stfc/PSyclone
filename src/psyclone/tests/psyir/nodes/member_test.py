# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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

''' This module contains pytest tests for the Member class. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir import nodes


def test_member_constructor():
    ''' Test that we can construct an instance of Member. '''
    mem = nodes.Member("fred")
    assert mem.name == "fred"
    assert mem.children == []


def test_member_constructor_errors():
    ''' Test the validation checks in the constructor. '''
    with pytest.raises(TypeError) as err:
        nodes.Member("hello", parent="wrong")
    assert ("parent of a Member must be either a "
            "(ArrayOf)Structure(s)Reference or (ArrayOf)Structure(s)Member "
            "but found 'str'" in str(err.value))


def test_member_can_be_copied():
    ''' Test that a Member node can be copied. '''

    member = nodes.Member("name1")

    member1 = member.copy()
    assert isinstance(member1, nodes.Member)
    assert member1 is not member
    assert member1.name == "name1"

    # Modifying the new member does not affect the original
    member1._component_name = "name2"
    assert member1.name == "name2"
    assert member.name == "name1"
