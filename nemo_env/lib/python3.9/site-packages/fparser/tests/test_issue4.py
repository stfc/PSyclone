# Modified work Copyright (c) 2017 Science and Technology Facilities Council
# Original work Copyright (c) 1999-2008 Pearu Peterson

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

from fparser import api


def test_reproduce_issue_private():
    source_str = """
    module m
    contains
    subroutine a
    end subroutine a
    end module m
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)


def test_private_subroutine():
    source_str = """
    module m
    public
    private a
    contains
    subroutine a
    end subroutine a
    subroutine b
    end subroutine b
    end module m
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    a = tree.content[0].content[3]
    b = tree.content[0].content[4]

    assert not a.is_public()
    assert a.is_private()

    assert b.is_public()
    assert not b.is_private()


def test_related_issue_type():
    source_str = """
    module m
    type private :: a
    end type a
    type public :: b
    end type b
    type :: c
    end type c
    end module m
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    a, b, c = tree.content[0].content[:3]
    assert a.is_private()
    assert not a.is_public()

    assert not b.is_private()
    assert b.is_public()

    assert not c.is_private()
    assert c.is_public()


def test_private_type():
    source_str = """
    module m
    private
    public b
    type :: a
    end type a
    type :: b
    end type b
    type public :: c
    end type c
    end module m
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    a, b, c = tree.content[0].content[2:5]
    assert a.is_private()
    assert not a.is_public()
    assert not b.is_private()
    assert b.is_public()
    assert not c.is_private()
    assert c.is_public()


def test_reproduce_issue_len():
    source_str = """
    subroutine foo(a)
    character(lenmax) a
    character(lenmax, kind=4) b
    character(len=lenmax, kind=4) c
    character(kind=4, len=lenmax) d
    character(lenmax, 4) e
    end subroutine foo
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    a, b, c, d, e = tree.content[0].content[:5]
    assert a.selector == ("lenmax", "")
    assert b.selector == ("lenmax", "4")
    assert c.selector == ("lenmax", "4")
    assert d.selector == ("lenmax", "4")
    assert e.selector == ("lenmax", "4")
