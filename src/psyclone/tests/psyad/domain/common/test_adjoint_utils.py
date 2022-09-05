# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
A module to perform pytest tests on the utilities in the
psyad/domain/common/adjoint_utils.py file.

'''
import pytest

from psyclone.errors import InternalError
from psyclone.psyad.domain.common import create_adjoint_name, find_container
from psyclone.psyir.nodes import Container, FileContainer, Return


# create_adjoint_name function

def test_create_adjoint_name():
    '''Test that the create_adjoint_name() function works as expected.

    '''
    assert create_adjoint_name("name") == "adj_name"
    assert create_adjoint_name("NAME") == "adj_name"
    assert create_adjoint_name("tl_name") == "adj_name"
    assert create_adjoint_name("Tl_NaMe") == "adj_name"


#  find_container function

def test_find_container():
    ''' Tests for the helper function find_container(). '''
    assert find_container(Return()) is None
    assert find_container(FileContainer("test")) is None
    cont = Container("my_mod")
    assert find_container(cont) is cont
    cont.addchild(FileContainer("test"))
    with pytest.raises(InternalError) as err:
        find_container(cont)
    assert ("The supplied PSyIR contains two Containers but the innermost is "
            "a FileContainer. This should not be possible" in str(err.value))
    cont = Container("my_mod")
    cont.addchild(Container("another_mod"))
    with pytest.raises(NotImplementedError) as err:
        find_container(cont)
    assert ("supplied PSyIR contains two Containers and the outermost one is "
            "not a FileContainer. This is not supported." in str(err.value))
    file_cont = FileContainer("test")
    cont = Container("my_mod")
    file_cont.addchild(cont)
    assert find_container(file_cont) is cont
    file_cont.addchild(cont.copy())
    with pytest.raises(NotImplementedError) as err:
        find_container(file_cont)
    assert ("The supplied PSyIR contains more than two Containers. This is "
            "not supported." in str(err.value))
