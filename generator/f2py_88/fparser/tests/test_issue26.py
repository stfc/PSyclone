
from fparser import api

def test_reproduce_issue():
    source_str = '''\
MODULE testa

TYPE t
   INTEGER :: x
END TYPE

CONTAINS

SUBROUTINE f(arga)
INTEGER :: arga
TYPE(t), DIMENSION(2) :: v

v(1)%x = 23
v  (2) % x = 42

END SUBROUTINE f
END MODULE testa
'''
    tree = api.parse(source_str, isfree=True, isstrict=False,
                     ignore_comments=False)
    r = str(tree).strip()
    assert r.split('\n')[1:]=='''
!BEGINSOURCE <cStringIO.StringI object at 0x2a1ee70> mode=free
  MODULE testa

    TYPE t
      INTEGER x
    END TYPE t

    CONTAINS

    SUBROUTINE f(arga)
      INTEGER arga
      TYPE(t), dimension(2) :: v

      v(1)%x = 23
      v(2)%x = 42

    END SUBROUTINE f
  END MODULE testa

    '''.strip().split('\n')[1:],`r`
