# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''This module implements tests for the generic utility functions.'''

import sys

from psyclone.utils import within_virtual_env, a_or_an


def test_within_virtual_env(monkeypatch):
    '''Test the 'within_virtual_env' function. We need to monkeypatch as
    there is no way to control what environment we are in.

    '''
    # There is a 'real_prefix' attribute.
    monkeypatch.setattr(sys, 'real_prefix', "real_prefix", raising=False)
    assert within_virtual_env()
    # There is no 'real_prefix' attribute. There is a 'base_prefix'
    # attribute with a different name to the 'prefix' attribute.
    monkeypatch.delattr(sys, 'real_prefix')
    monkeypatch.setattr(sys, 'base_prefix', "base_prefix", raising=False)
    monkeypatch.setattr(sys, 'prefix', "prefix")
    assert within_virtual_env()
    # There is no 'real_prefix' attribute. There is a 'base_prefix'
    # atribute with the same name to the 'prefix' attribute.
    monkeypatch.setattr(sys, 'base_prefix', "prefix")
    assert not within_virtual_env()
    # There are no 'real_prefix' or 'base_prefix' attributes.
    monkeypatch.delattr(sys, 'base_prefix')
    assert not within_virtual_env()


def test_a_or_an():
    '''Test the a_or_an() function.'''
    assert a_or_an("Aardvark") == "an"
    assert a_or_an("aardvark") == "an"
    assert a_or_an("honor") == "an"
    assert a_or_an("real") == "a"
    assert a_or_an("integer") == "an"
    assert a_or_an("logical") == "a"
    # Test remaining vowels.
    for vowel in ["e", "o", "u"]:
        assert a_or_an(vowel) == "an"
