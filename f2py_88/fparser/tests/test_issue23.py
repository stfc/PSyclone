
from fparser import api

def test_reproduce_issue_1():
    source_str = '''\
      subroutine bndfp(ax,i)
      logical:: ax
      if(ax) i=1
      end
'''
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    ifstmt = tree.content[0].content[1]
    assert str(ifstmt).strip()=='''
    IF (ax) i = 1'''.strip()

def test_reproduce_issue_2():
    source_str = '''\
      subroutine bndfp(ax,i)
      logical:: ax
      if(ax) call bb(a,b)
      end
'''
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    ifstmt = tree.content[0].content[1]
    assert str(ifstmt).strip()=='''
    IF (ax) CALL bb(a, b)'''.strip()
