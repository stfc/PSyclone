# Modified work Copyright (c) 2017-2022 Science and Technology Facilities Council.
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
      subroutine gwinput_v2x(ifi,konf,ncore)
      integer :: ifi, !File handle. Write
     &  konf(0:lmxamx,nclass),! Principle
                              ! For examp
                              ! Core orbi
                              !   1, 2,..
                              !   2, 3,..
                              !   3, 4,..
                              !
     &  ncore(nclass)   ! ncore = \sum_l 
                        ! Number of diffe
      end
"""
    tree = api.parse(source_str, isfree=False, isstrict=False, ignore_comments=False)
    expected = """
      !      BEGINSOURCE <cStringIO.StringI object at 0x1e52ea0> mode=fix90
        SUBROUTINE gwinput_v2x(ifi, konf, ncore)
          INTEGER ifi, konf(0:lmxamx,nclass), ncore(nclass)
          !File handle. Write
          ! Principle
          ! For examp
          ! Core orbi
          !   1, 2,..
          !   2, 3,..
          !   3, 4,..
          !
          ! ncore = \sum_l
          ! Number of diffe
        END SUBROUTINE gwinput_v2x
    """
    assert str(tree).strip().split("\n")[1:] == expected.strip().split("\n")[1:]


def test_Issue_r25_1():
    source_str = """
      subroutine bndfp()
      logical:: mlog
c test abb
c
#1232       
!

c
cabi
      real(8):: abc,a(5),
     &  abcx  !hhhh1
      abc=3   !hhhh2
      do i=1,5
         a(i)=i
      enddo
!

!
      end
    """
    tree = api.parse(source_str, isfree=False, isstrict=False, ignore_comments=False)
    expected = """
      !      BEGINSOURCE <cStringIO.StringI object at 0xb52b40> mode=fix90

        SUBROUTINE bndfp()
          LOGICAL mlog
          ! test abb
          !
          !1232
          !

          !
          !abi
          REAL(KIND=8) abc, a(5), abcx
          !hhhh1
          abc = 3
          !hhhh2
          DO  i=1,5
            a(i) = i
          END DO 
          !

          !
        END SUBROUTINE bndfp
    """
    assert str(tree).strip().split("\n")[1:] == expected.strip().split("\n")[1:]


def test_Issue_r25_1():
    source_str = """
      subroutine rsedit()
        print 311
  311   format(/
     .  t4,'read [fn]',t15,

     .  t14,'  Third argument = flip: exchange n+, n- ')
      end

    """
    tree = api.parse(source_str, isfree=False, isstrict=False, ignore_comments=False)
    expected = """
      !      BEGINSOURCE <cStringIO.StringI object at 0x121bab0> mode=fix90

        SUBROUTINE rsedit()
          PRINT 311
 311      FORMAT (/  t4, 'read [fn]', t15, t14, '  Third argument = flip: exchange n+, n- ')

        END SUBROUTINE rsedit
    """
    assert str(tree).strip().split("\n")[1:] == expected.strip().split("\n")[1:]


def test_comment_4():
    source_str = """
      subroutine m_struc_def()
 !abc test
      end
    """
    tree = api.parse(source_str, isfree=False, isstrict=False, ignore_comments=False)
    expected = """
      !      BEGINSOURCE <cStringIO.StringI object at 0x121aa80> mode=fix90

        SUBROUTINE m_struc_def()
          !abc test
        END SUBROUTINE m_struc_def
"""
    assert str(tree).strip().split("\n")[1:] == expected.strip().split("\n")[1:]
