
from fparser import api

def test_reproduce_issue():
    source_str = '''\
MODULE testa

CONTAINS

SUBROUTINE f(arga)
INTEGER :: arga

CONTAINS

SUBROUTINE subf(argx)
INTEGER :: argx
END SUBROUTINE subf

END SUBROUTINE f

END MODULE testa
'''
    tree = api.parse(source_str, isfree=True, isstrict=False,
                     ignore_comments=False)
    r = str(tree).strip()
    assert r.split('\n')[1:]=='''
...!BEGINSOURCE <cStringIO.StringI object at 0x139f8a0> mode=free
  MODULE testa

    CONTAINS

    SUBROUTINE f(arga)
      INTEGER arga

      CONTAINS

      SUBROUTINE subf(argx)
        INTEGER argx
      END SUBROUTINE subf

    END SUBROUTINE f

  END MODULE testa
    '''.strip().split('\n')[1:],`r`
