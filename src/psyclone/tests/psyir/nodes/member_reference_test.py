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

''' This module contains pytest tests for the MemberReference class. '''

import pytest
from psyclone.psyir import symbols, nodes


def test_mr_constructor_errors():
    ''' Test the validation checks in the constructor. '''
    with pytest.raises(TypeError) as err:
        nodes.MemberReference(None, "hello")
    assert ("expecting the type of the structure to be specified with either "
            "a StructureType or a TypeSymbol but found 'NoneType'" in
            str(err.value))

    with pytest.raises(TypeError) as err:
        nodes.MemberReference(symbols.StructureType(), "hello",
                              parent="wrong")
    assert ("parent of a MemberReference must be either a "
            "(Array)StructureReference or (Array)StructureMemberReference but "
            "found 'str'" in str(err.value))
    # Attempt to reference something that is not a component of the supplied
    # type.
    region_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    with pytest.raises(TypeError) as err:
        nodes.MemberReference(region_type, "missing")
    assert ("supplied StructureType has no component named 'missing'" in
            str(err.value))


def test_mr_constructor_missing_typedef():
    ''' Check that we raise a NotImplementedError if the component being
    referenced is of DeferredType. '''
    # Create a TypeSymbol with deferred type (i.e. no type definition is
    # currently available).
    region_type_sym = symbols.TypeSymbol("region_type",
                                         symbols.DeferredType())
    with pytest.raises(NotImplementedError) as err:
        nodes.StructureMemberReference(region_type_sym, "area")
    assert ("structure if its type is available but member 'area' has type "
            "'DeferredType'" in str(err.value))
