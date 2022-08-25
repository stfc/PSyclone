# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the CommonArg class.

'''
import pytest

from psyclone.domain.lfric.kernel.common_arg import CommonArg


def test_init_noargs():
    '''Test that a CommonArg instance can be created successfully when no
    arguments are provided.

    '''
    field_arg = CommonArg()
    assert isinstance(field_arg, CommonArg)
    assert field_arg._datatype is None
    assert field_arg._access is None


def test_init_invalid():
    '''No exceptions are raised when constructing an instance of the CommonArg
    class as we don't know what is valid.

    '''
    with pytest.raises(NotImplementedError) as info:
        _ = CommonArg(datatype="invalid")
    assert "Not implemented." in str(info.value)
    with pytest.raises(NotImplementedError) as info:
        _ = CommonArg(access="invalid")
    assert "Not implemented." in str(info.value)


def test_init_args():
    '''Test that valid initial values provided when constructing an
    instance of CommonArg are stored as expected.

    '''
    with pytest.raises(NotImplementedError) as info:
        _ = CommonArg("GH_REAL", "GH_READ")
    assert "Not implemented." in str(info.value)


def test_setter_getter():
    '''Test that the setters and getters work as expected.'''
    field_arg = CommonArg()
    assert field_arg.datatype is None
    with pytest.raises(NotImplementedError) as info:
        field_arg.datatype = "gh_integer"
    assert "Not implemented." in str(info.value)
    assert field_arg.datatype is None
    with pytest.raises(NotImplementedError) as info:
        field_arg.datatype = "GH_INTEGER"
    assert "Not implemented." in str(info.value)
    assert field_arg.datatype is None

    assert field_arg.access is None
    with pytest.raises(NotImplementedError) as info:
        field_arg.access = "gh_read"
    assert "Not implemented." in str(info.value)
    assert field_arg.access is None
    with pytest.raises(NotImplementedError) as info:
        field_arg.access = "GH_READ"
    assert "Not implemented." in str(info.value)
    assert field_arg.access is None
