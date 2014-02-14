from fparser import api

def test_reproduce_issue():
    source_str = '''\
      subroutine bl(a,
     &b,

     &c)
      integer a, b, c

      a = b + c

      end subroutine bl

      subroutine blc(a,
c here's an annoying comment line
     &b,

     &c,
c another annoying comment

     &d,

c a third annoying comment
     &e)

      a = b + c + d + e

      end subroutine blc
'''
    tree = api.parse(source_str, isfree=False, isstrict=True,
            analyze=False)
    print tree
    assert str(tree).strip().split('\n')[1:] == '''
      !      BEGINSOURCE <cStringIO.StringI object at 0x3723710> mode=f77
        SUBROUTINE bl(a, b, c)
          INTEGER a, b, c
          a = b + c
        END SUBROUTINE bl
        SUBROUTINE blc(a, b, c, d, e)
          a = b + c + d + e
        END SUBROUTINE blc
    '''.strip().split('\n')[1:]

# def test_reproduce_issue():
    # source_str = '''\
      # subroutine foo
      # do 10
 # 10   continue
      # end subroutine
# '''
    # tree = api.parse(source_str, isfree=False, isstrict=False,
                     # ignore_comments=False)
    # assert str(tree).strip().split('\n')[1:]=='''
      # !      BEGINSOURCE <cStringIO.StringI object at 0x1733ea0> mode=fix90
        # SUBROUTINE foo()
          # DO 10
 # 10       CONTINUE
        # END SUBROUTINE foo
    # '''.strip().split('\n')[1:]
