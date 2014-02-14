
from fparser import api

def test_reproduce_issue():
    source_str = '''\
module foo

interface assignment(=)
    module procedure baa
end interface assignment(=)
  
end module foo
'''
    tree = api.parse(source_str, isfree=True, isstrict=False,
                     ignore_comments=False)
    r = str(tree).strip()
    assert r.split('\n')[1:]=='''
...!BEGINSOURCE <cStringIO.StringI object at 0x302c1e0> mode=free
  MODULE foo

    INTERFACE assignment(=)
      MODULE PROCEDURE baa
    END INTERFACE assignment(=)

  END MODULE foo
    '''.strip().split('\n')[1:],`r`
