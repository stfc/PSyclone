
from fparser import api

def test_reproduce_issue():
    source_str = '''\
      subroutine bndfp()
      include "events.ins"
      end
'''
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    print tree
    return
    assert str(tree).strip().split('\n')[1:]=='''
    '''.strip().split('\n')[1:]
