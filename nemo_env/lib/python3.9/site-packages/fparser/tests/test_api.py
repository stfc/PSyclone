# Modified work Copyright (c) 2017-2018 Science and Technology
# Facilities Council
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
#
# --------------------------------------------------------------------
#
# The original software (in the f2py project) was distributed under
# the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.
#
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

"""
Test parsing of whole fortran files; 'blackbox' tests here.
"""

from os.path import join, dirname
from fparser import api

SOURCE_STR = """\
    ! before foo
    subroutine foo
    integer i, r
    do i = 1,100
      r = r + 1
    end do
    ! after end do
    end subroutine foo
    """


def test_use_module():
    """
    Test that we parse a function that uses a module when we supply the
    latter via the `source_only` argument.
    """
    cwd = dirname(__file__)
    sources = [join(cwd, "modfile.f95"), join(cwd, "funcfile.f95")]
    file_to_parse = sources[1]
    _ = api.parse(file_to_parse, isfree=True, isstrict=False, source_only=sources)


def test_dimension_attr():
    """
    Tests for parsing the `dimension` attribute of variable declarations.
    """
    source_str = (
        "    subroutine foo\n"
        "    integer, dimension( -10 : 10, -   2147483648 : "
        "2147483648) :: a( -2 : 2, 1000000 : 1000001 )\n"
        "    real, dimension(-20:20, 100:113, -  512  : 713) :: b\n"
        "    end\n"
    )
    tree = api.parse(source_str, isfree=True, isstrict=False)
    subr = tree.a.external_subprogram["foo"]
    avar = subr.a.variables["a"]

    assert avar.dimension == [("-10", "10"), ("-   2147483648", "2147483648")]
    assert avar.bounds == [("-2", "2"), ("1000000", "1000001")]
    assert avar.shape == ["5", "2"]

    bvar = subr.a.variables["b"]

    assert bvar.dimension == [("-20", "20"), ("100", "113"), ("-  512", "713")]
    assert bvar.shape == ["41", "14", "1226"]


def test_provides():
    """
    Tests for non-Fortran (f2py markup) module_provides and use_provides
    key words. This is a legacy feature that is due to fparser's origins
    in the f2py project (https://docs.scipy.org/doc/numpy/f2py/).
    """
    source_str = """
    module mod1
    implicit none
    integer, parameter :: GP = 6
    integer :: a,b,c,d,e
    ! module_provides = {GP,a,b,c,d,e}
    ! use_provides = {}
    end module mod1

    module mod2
    implicit none
    integer, parameter :: SP = 5
    real :: a,b,c
    ! module_provides = {SP,a,b,c}
    ! use_provides = {}
    end module mod2

    module mod3
    use mod1
    implicit none
    integer, parameter :: DP = 0
    ! module_provides = {DP}
    ! use_provides = {GP,a,b,c,d,e}
    end module mod3

    module mod4
    use mod2
    implicit none
    ! module_provides = {}
    ! use_provides = {SP,a,b,c}
    end module mod4

    module mod5
    use mod3, only: lGP => GP, a,b,e
    use mod4, only: a2 => a, b2 => b
    implicit none

    integer, parameter :: FP = 1000
    integer(kind=kind(0)) :: dummy
    parameter (dummy = 20)
    integer, private :: x,y,z

    ! module_provides = {FP, dummy}
    ! use_provides = {lGP, a, b, e, a2, b2}
    end module mod5

    module mod6
    use mod5, qgp => lgp
    implicit none
    ! module_provides = {}
    ! use_provides = {FP, dummy, a2, b2, qgp, a, b, e}
    end module mod6

      """

    tree = api.parse(source_str, isfree=True, isstrict=False)
    mod5 = tree.a.module["mod5"]
    mod6 = tree.a.module["mod6"]
    assert list(mod5.a.module_provides.keys()).sort() == ["fp", "dummy"].sort()
    assert (
        list(mod5.a.use_provides.keys()).sort()
        == ["a", "b", "e", "a2", "b2", "lgp"].sort()
    )
    assert list(mod6.a.module_provides.keys()) == []
    assert (
        list(mod6.a.use_provides.keys()).sort()
        == ["fp", "dummy", "b", "e", "qgp", "a2", "a", "b2"].sort()
    )
    assert mod6.a.use_provides["qgp"].name == "gp"


def test_walk():
    """
    Test the walk() method of the api module.
    """
    tree = api.parse(SOURCE_STR, isfree=True, isstrict=False, ignore_comments=False)
    for stmt, depth in api.walk(tree, 1):
        print(depth, stmt.item)


def test_caching():
    """Tests relating to enabling/disabling of parser cache."""
    tree1 = api.parse(SOURCE_STR, isfree=True, isstrict=False, ignore_comments=False)
    # If we keep the cache from the last call to parse then we
    # should get the same object back
    tree2 = api.parse(
        SOURCE_STR,
        isfree=True,
        isstrict=False,
        ignore_comments=False,
        clear_cache=False,
    )
    assert tree1 is tree2
    # Now wipe the cache and check that we get a new object
    tree2 = api.parse(
        SOURCE_STR, isfree=True, isstrict=False, ignore_comments=False, clear_cache=True
    )
    assert tree1 is not tree2
    # Check that wiping the cache is the default behaviour
    tree3 = api.parse(SOURCE_STR, isfree=True, isstrict=False, ignore_comments=False)
    assert tree3 is not tree2
