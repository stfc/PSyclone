
from fparser import api

def test_reproduce_issue():
    source_str = '''\
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
'''
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=='''
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
    '''.strip().split('\n')[1:]

def test_Issue_r25_1():
    source_str = '''
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
    '''
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=='''
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
    '''.strip().split('\n')[1:]

def test_Issue_r25_1():
    source_str = """
      subroutine rsedit()
        print 311
  311   format(/
     .  t4,'read [fn]',t15,

     .  t14,'  Third argument = flip: exchange n+, n- ')
      end

    """
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=="""
      !      BEGINSOURCE <cStringIO.StringI object at 0x121bab0> mode=fix90

        SUBROUTINE rsedit()
          PRINT 311
 311      FORMAT (/  t4, 'read [fn]', t15, t14, '  Third argument = flip: exchange n+, n- ')

        END SUBROUTINE rsedit
    """.strip().split('\n')[1:]

def test_comment_4():
    source_str = """
      subroutine m_struc_def()
 !abc test
      end
    """
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=="""
      !      BEGINSOURCE <cStringIO.StringI object at 0x121aa80> mode=fix90

        SUBROUTINE m_struc_def()
          !abc test
        END SUBROUTINE m_struc_def
""".strip().split('\n')[1:]

