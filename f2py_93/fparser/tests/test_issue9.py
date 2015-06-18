
from fparser import api

def test_reproduce_issue():
    source_str = '''\
      module m_rdctrl

      contains
      
      subroutine readctrl(prgn,vstrn,vn)

      end subroutine readctrl
      end module
      
'''
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=='''
      !      BEGINSOURCE <cStringIO.StringI object at 0x2405ea0> mode=fix90
        MODULE m_rdctrl

          CONTAINS

          SUBROUTINE readctrl(prgn, vstrn, vn)

          END SUBROUTINE readctrl
        END MODULE m_rdctrl
    '''.strip().split('\n')[1:]
