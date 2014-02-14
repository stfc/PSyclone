
from fparser import api

def test_reproduce_issue():
    source_str = '''\
      subroutine foo
      do 10
 10   continue
      end subroutine
'''
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=='''
      !      BEGINSOURCE <cStringIO.StringI object at 0x1733ea0> mode=fix90
        SUBROUTINE foo()
          DO 10
 10       CONTINUE
        END SUBROUTINE foo
    '''.strip().split('\n')[1:]
