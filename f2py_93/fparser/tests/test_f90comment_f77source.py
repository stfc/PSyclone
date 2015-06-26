from fparser import api

def test_reproduce_issue():
    source_str = '''\
      subroutine foobar()
!#here's a f90 comment starting at 0
      end
'''
    tree = api.parse(source_str, isfree=False, isstrict=True,
            analyze=False)
    assert str(tree).strip().split('\n')[1:] == '''
      !      BEGINSOURCE <cStringIO.StringI object at 0x3721710> mode=f77
        SUBROUTINE foobar()
        END SUBROUTINE foobar
        '''.strip().split('\n')[1:]
