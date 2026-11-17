# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''pytest tests for the errors module.'''

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
