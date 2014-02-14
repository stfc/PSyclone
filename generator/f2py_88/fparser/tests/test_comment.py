
from fparser.api import parse

def test_comment_lines():
    source_str = '''\
  !comment line 1

!comment line 2
module foo
!comment line 3
subroutine f
!comment line 4
end subroutine f !comment line 5
end module foo
    '''
    tree = parse(source_str, isfree=True, isstrict=False,
                 ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=='''
!BEGINSOURCE <cStringIO.StringI object at 0x1518de0> mode=free90
  !comment line 1

  !comment line 2
  MODULE foo
    !comment line 3
    SUBROUTINE f()
      !comment line 4
    END SUBROUTINE f
    !comment line 5
  END MODULE foo
'''.strip().split('\n')[1:]

    assert tree.asfix().strip().split('\n')[1:]=='''
C      BEGINSOURCE <cStringIO.StringI object at 0x1630de0> mode=free90
C       comment line 1

C       comment line 2
        MODULE foo
C         comment line 3
          SUBROUTINE f()
C           comment line 4
          END SUBROUTINE f
C         comment line 5
        END MODULE foo
'''.strip().split('\n')[1:]

def test_labels():
    source_str = '''\
subroutine foo
  real a
! Valid code:
100 a = 3
  l: do
  end do l
200 &! haa
   a=4
300 l1: do
  end do l1
end subroutine foo
'''
    tree = parse(source_str, isfree=True, isstrict=False,
                 ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=='''
!BEGINSOURCE <cStringIO.StringI object at 0x2952e70> mode=free90
  SUBROUTINE foo()
    REAL a
    ! Valid code:
100 a = 3
    l: DO
    END DO l
200 a = 4
    ! haa
300 l1: DO
    END DO l1
  END SUBROUTINE foo
'''.strip().split('\n')[1:]

    source_str = '''\
      subroutine foo
      real a
      ! Valid code:
  100 a = 3
      l: do
      end do l
  200 ! haa
     &a=4
  300 l1: do
      end do l1
      end subroutine foo
'''
    tree = parse(source_str, isfree=False, isstrict=False,
                 ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=='''
!      BEGINSOURCE <cStringIO.StringI object at 0x1d3b390> mode=fix90
        SUBROUTINE foo()
          REAL a
          ! Valid code:
 100      a = 3
          l: DO
          END DO l
 200      a = 4
          ! haa
 300      l1: DO
          END DO l1
        END SUBROUTINE foo
'''.strip().split('\n')[1:]
