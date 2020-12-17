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

''' This module contains pytest tests for the ArrayMemberReference class. '''

import pytest
from psyclone.psyir import symbols, nodes
from psyclone.errors import GenerationError


def test_amr_constructor():
    ''' Test that we can construct an ArrayMember. '''
    region_type = symbols.StructureType.create([
        ("sub_mesh",
         symbols.ArrayType(symbols.INTEGER_TYPE,
                           [nodes.Literal("10", symbols.INTEGER_TYPE)]),
         symbols.Symbol.Visibility.PUBLIC)])
    idx = nodes.Literal("3", symbols.INTEGER_TYPE)
    amr = nodes.ArrayMember(region_type, "sub_mesh", indices=[idx])
    assert len(amr.children) == 1
    assert amr.children[0] is idx


def test_amr_validate_child():
    ''' Test the _validate_child method of ArrayMember. '''
    region_type = symbols.StructureType.create([
        ("sub_mesh",
         symbols.ArrayType(symbols.INTEGER_TYPE,
                           [nodes.Literal("10", symbols.INTEGER_TYPE)]),
         symbols.Symbol.Visibility.PUBLIC)])
    idx = nodes.Literal("3", symbols.INTEGER_TYPE)
    amr = nodes.ArrayMember(region_type, "sub_mesh")
    with pytest.raises(GenerationError) as err:
        amr.addchild("wrong")
    assert "'str' can't be child 0 of 'ArrayMember'" in str(err.value)
    amr.addchild(idx)
    assert amr.children[0] is idx
