# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Author: Joerg Henrichs, Bureau of Meteorology
# Modified by N. Nobre, STFC Daresbury Lab

'''This module tests the Signature class.'''

from __future__ import absolute_import
import pytest

from psyclone.core import ComponentIndices, Signature
from psyclone.errors import InternalError
from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Reference
from psyclone.psyir.symbols import DataSymbol, INTEGER_SINGLE_TYPE


def test_signature():
    '''Test the Signature class.
    '''

    assert str(Signature("a")) == "a"
    assert str(Signature(("a",))) == "a"
    assert str(Signature(("a", "b", "c"))) == "a%b%c"
    assert repr(Signature("a")) == "Signature(a)"
    assert repr(Signature(("a",))) == "Signature(a)"
    assert repr(Signature(("a", "b", "c"))) == "Signature(a%b%c)"
    assert repr(Signature(["a", "b", "c"])) == "Signature(a%b%c)"
    assert Signature("a") != "a"
    sig = Signature(("a", "b", "c"))
    assert sig.is_structure
    assert sig[0] == "a"
    assert sig[2] == "c"
    assert sig[-1] == "c"
    assert sig[0:2] == Signature(("a", "b"))
    assert len(sig) == 3
    assert Signature(["a", "b", "c"]).is_structure
    assert not Signature(("a")).is_structure

    # Check that structure expressions (using '%') are automatically split
    # into components:
    sig = Signature("a%b")
    assert sig[0] == "a"
    assert sig[1] == "b"


def test_signature_errors():
    '''Tests error handling of Signature class.
    '''
    with pytest.raises(InternalError) as err:
        _ = Signature(1)

    assert "Got unexpected type 'int' in Signature" in str(err.value)


def test_signature_dict():
    '''Test that Signature instances work as expected as dictionary keys.
    '''
    sig1 = Signature("a")
    sig2 = Signature("a")
    assert sig1 is not sig2

    # Make sure that different instances representing the same signature
    # will have the same hash:
    test_dict = {}
    test_dict[sig1] = "a"
    assert test_dict[sig2] == "a"

    sig3 = Signature(("a", "b"))
    test_dict[sig3] = "ab"
    sig4 = Signature(("a", "c"))
    test_dict[sig4] = "ac"

    assert len(test_dict) == 3


def test_concatenate_signature():
    '''Tests that signature can be concatenated.'''
    sig_b = Signature("b")
    sig_a_b = Signature("a", sig_b)
    assert str(sig_a_b) == "a%b"
    sig_b_a_b = Signature(sig_b, sig_a_b)
    assert str(sig_b_a_b) == "b%a%b"
    sig_c_d_b_a_b = Signature(("c", "d"), sig_b_a_b)
    assert str(sig_c_d_b_a_b) == "c%d%b%a%b"


def test_var_name():
    '''Test that the variable name is returned as expected.'''
    sig_a = Signature("a")
    assert sig_a.var_name == "a"
    sig_a_b = Signature(sig_a, Signature("b"))
    assert str(sig_a_b) == "a%b"
    assert sig_a_b.var_name == "a"


def test_signature_sort():
    '''Test that signatures can be sorted.'''

    sig_list = [Signature("c"), Signature("a"), Signature("b"),
                Signature(("b", "a")),
                Signature(("a", "c")), Signature(("a", "b"))]

    assert str(sig_list) == "[Signature(c), Signature(a), Signature(b), " \
                            "Signature(b%a), Signature(a%c), Signature(a%b)]"

    sig_list.sort()
    assert str(sig_list) == "[Signature(a), Signature(a%b), Signature(a%c), " \
                            "Signature(b), Signature(b%a), Signature(c)]"


def test_signature_comparison():
    ''' Test that two Signatures can be compared for equality and not
    equality.
    '''
    # pylint: disable=unneeded-not
    assert Signature(("a", "b")) == Signature(("a", "b"))
    assert not Signature(("a", "b")) == Signature(("a", "c"))

    assert Signature(("a", "b")) != Signature(("a", "c"))
    assert not Signature(("a", "b")) != Signature(("a", "b"))
    assert Signature(("a", "c")) >= Signature(("a", "b"))
    assert not Signature(("a", "b")) >= Signature(("a", "c"))
    assert Signature(("a", "c")) > Signature(("a", "b"))
    assert not Signature(("a", "b")) > Signature(("a", "c"))
    assert Signature(("a", "b")) <= Signature(("a", "c"))
    assert not Signature(("a", "c")) <= Signature(("a", "b"))
    assert Signature(("a", "b")) < Signature(("a", "c"))
    assert not Signature(("a", "c")) < Signature(("a", "b"))

    # Comparison with other types should work for == and !=:
    assert not Signature(("a", "b")) == 2
    assert Signature(("a", "b")) != 2
    # pylint: enable=unneeded-not

    # Error cases: comparison of signature with other type.
    with pytest.raises(TypeError) as err:
        _ = Signature(("a", "b")) < 1
    assert "'<' not supported between instances of 'Signature' and 'int'" \
        in str(err.value)

    with pytest.raises(TypeError) as err:
        _ = Signature(("a", "b")) <= "a"
    assert "'<=' not supported between instances of 'Signature' and 'str'" \
        in str(err.value)

    with pytest.raises(TypeError) as err:
        _ = Signature(("a", "b")) > [1]
    assert "'>' not supported between instances of 'Signature' and 'list'" \
        in str(err.value)

    with pytest.raises(TypeError) as err:
        _ = Signature(("a", "b")) >= (1, 2)
    assert "'>=' not supported between instances of 'Signature' and 'tuple'" \
        in str(err.value)


def test_to_language_fortran():
    '''Test that conversion of a Signature with a ComponentIndices argument
    gives the expected results.
    '''
    sig = Signature("a")
    comp = ComponentIndices()
    assert sig.to_language(comp) == "a"

    comp = ComponentIndices(["i"])
    assert sig.to_language(comp) == "a(i)"

    ref = Reference(DataSymbol("j", INTEGER_SINGLE_TYPE))
    comp = ComponentIndices([ref])
    assert sig.to_language(comp) == "a(j)"

    # Test error condition if number of components in signature does not
    # match the number of indices in ComponentIndices
    comp = ComponentIndices([[1], [2]])
    with pytest.raises(InternalError) as err:
        sig.to_language(comp)
    assert ("Signature 'a' has 1 components, but component_indices [[1], [2]] "
            "has 2." in str(err.value))

    sig = Signature(("a", "b", "c"))
    comp = ComponentIndices([[1], [], ["i", "j"]])
    assert sig.to_language(comp) == "a(1)%b%c(i,j)"
    comp = ComponentIndices([[1, 2], [], []])
    assert sig.to_language(comp) == "a(1,2)%b%c"
    comp = ComponentIndices([[], [], []])
    assert sig.to_language(comp) == "a%b%c"
    assert sig.to_language() == "a%b%c"


def test_output_languages():
    '''Tests that error messages can be created in different languages.
    '''

    sig = Signature(("a"))
    comp = ComponentIndices([["i", "j"]])
    f_writer = FortranWriter()
    c_writer = CWriter()
    # Check that it defaults to Fortran
    assert sig.to_language(comp) == "a(i,j)"
    assert sig.to_language(comp, f_writer) == "a(i,j)"
    assert sig.to_language(comp, c_writer) == "a[i + j * aLEN1]"

    sig = Signature(("a", "b", "c"))
    comp = ComponentIndices([[1], [], ["i", "j"]])
    # Check that it defaults to Fortran
    assert sig.to_language(comp) == "a(1)%b%c(i,j)"
    assert sig.to_language(comp, f_writer) == "a(1)%b%c(i,j)"
    assert sig.to_language(comp, c_writer) == "a[1].b.c[i + j * cLEN1]"
