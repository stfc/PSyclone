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


def test_reproduce_issue():
    source_str = """\
      subroutine bndfp()
      use m_struc_def
C-
C 
C
C
C
C
      end
"""
    tree = api.get_reader(
        source_str, isfree=False, isstrict=False, ignore_comments=False
    )
    tree = list(tree)
    s, u, c, e = tree[:3] + tree[-1:]
    assert s.span == (1, 1), repr(s.span)
    assert u.span == (2, 2), repr(u.span)
    assert c.span == (3, 3), repr(c.span)
    assert e.span == (9, 9), repr(e.span)


def test_reproduce_issue_fix77():
    source_str = """\
      subroutine foo()
      real a
c
c
      end
"""
    tree = api.get_reader(
        source_str, isfree=False, isstrict=True, ignore_comments=False
    )
    tree = list(tree)
    foo, a, comment, end = tree[:3] + tree[-1:]
    assert foo.span == (1, 1)
    assert a.span == (2, 4), repr(a)
    assert comment.span == (5, 5), repr(comment.span)
    assert end.span == (5, 5), repr(end.span)


def test_reproduce_issue_fix90():
    source_str = """\
      subroutine foo()
      real a
c 1
c 2
      end
"""
    tree = api.get_reader(source_str, isfree=False, isstrict=False)
    tree = list(tree)
    foo, a, comment, end = tree[:3] + tree[-1:]
    assert foo.span == (1, 1)
    assert a.span == (2, 2), repr(a.span)
    assert end.span == (5, 5), repr(end.span)

    source_str = """\
      subroutine foo()
      real a
c-
c
      end
"""
    tree = api.get_reader(
        source_str, isfree=False, isstrict=False, ignore_comments=False
    )
    tree = list(tree)
    foo, a, comment, end = tree[:3] + tree[-1:]
    assert foo.span == (1, 1)
    assert a.span == (2, 2), repr(a.span)
    assert end.span == (5, 5), repr(end.span)

    source_str = """\
      subroutine foo()
      real a
c
c
      end
"""
    tree = api.get_reader(
        source_str, isfree=False, isstrict=False, ignore_comments=False
    )
    tree = list(tree)
    foo, a, comment, end = tree[:3] + tree[-1:]
    assert foo.span == (1, 1)
    assert a.span == (2, 2), repr(a.span)
    assert comment.span == (3, 3)
    assert end.span == (5, 5), repr(end.span)


def test_comment_cont_fix90():
    source_str = """\
          subroutine foo()
      real
c 1
     & a
c 2
      end
"""
    tree = api.get_reader(
        source_str, isfree=False, isstrict=False, ignore_comments=False
    )
    tree = list(tree)
    foo, a, comment, end = tree[:3] + tree[-1:]
    assert foo.span == (1, 1)
    assert a.span == (2, 4), repr(a.span)
    assert comment.span == (3, 3), repr(comment.span)
    assert end.span == (6, 6)

    source_str = """\
          subroutine foo()
      real
c
     & a
c 2
      end
"""
    tree = api.get_reader(
        source_str, isfree=False, isstrict=False, ignore_comments=False
    )
    tree = list(tree)
    foo, a, comment, end = tree[:3] + tree[-1:]
    assert foo.span == (1, 1)
    assert a.span == (2, 4), repr(a.span)
    assert comment.span == (3, 3), repr(comment.span)
    assert end.span == (6, 6)

    source_str = """\
          subroutine foo()
      real
c 1
     & a
c
      end
"""
    tree = api.get_reader(
        source_str, isfree=False, isstrict=False, ignore_comments=False
    )
    tree = list(tree)
    foo, a, comment, end = tree[:3] + tree[-1:]
    assert foo.span == (1, 1)
    assert a.span == (2, 4), repr(a.span)
    assert comment.span == (3, 3), repr(comment.span)
    assert end.span == (6, 6)

    source_str = """\
          subroutine foo()
      real
c 1
     & a
c 2
     &,b
      end
"""
    tree = api.get_reader(
        source_str, isfree=False, isstrict=False, ignore_comments=False
    )
    tree = list(tree)
    foo, ab, comment, end = tree[:3] + tree[-1:]
    assert foo.span == (1, 1)
    assert ab.span == (2, 6), repr(a.span)
    assert comment.span == (3, 3), repr(comment.span)
    assert end.span == (7, 7)
