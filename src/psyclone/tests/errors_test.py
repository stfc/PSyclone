# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''pytest tests for the errors module.'''

from __future__ import absolute_import
import pytest

from psyclone.errors import LazyString, PSycloneError


# LazyString class

def test_lazystring():
    ''' Test the LazyString class works as expected.'''

    def func():
        ''' Utility function to test LazyString behaviour.

        returns: the string "hello"
        rtype: str

        '''
        return "hello"
    lazy_string = LazyString(func)
    assert isinstance(lazy_string, LazyString)
    assert lazy_string._func is func
    assert str(lazy_string) == lazy_string._func()


def test_lazystring_error():
    '''Test the LazyString class raises the expected exceptions.'''
    with pytest.raises(TypeError) as info:
        _ = LazyString("hello")
    assert ("The func argument for the LazyString class should be a function, "
            "but found 'str'." in str(info.value))
    lazy_string = LazyString(lambda: None)
    with pytest.raises(TypeError) as info:
        str(lazy_string)
    assert ("The function supplied to the LazyString class should return a "
            "string, but found 'NoneType'." in str(info.value))


# PSycloneError class

def test_psycloneerror():
    '''Test that the PSycloneError class behaves as expected.'''
    error = PSycloneError("hello")
    assert isinstance(error, PSycloneError)
    assert isinstance(error.value, LazyString)
    assert repr(error) == "PSycloneError()"
    assert str(error) == "PSyclone Error: hello"
