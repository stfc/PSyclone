"""
Test parsing of whole fortran files; 'blackbox' tests here.
"""

from fparser import api
import sys
from os.path import abspath, join, dirname

def test_use_module():
    d = dirname(__file__)
    sources = [join(d,'modfile.f95'), join(d,'funcfile.f95')]
    file_to_parse = sources[1]
    tree = api.parse(file_to_parse, isfree=True, isstrict=False, source_only = sources)

def test_dimension_attr():
    source_str = '''
    subroutine foo
    integer, dimension( -10 : 10, -   2147483648 : 2147483648) :: a( -2 : 2, 1000000 : 1000001 )
    real, dimension(-20:20, 100:113, -  512  : 713) :: b
    end
    '''

    tree = api.parse(source_str, isfree=True, isstrict=False)
    subr = tree.a.external_subprogram['foo']
    avar = subr.a.variables['a']

    assert avar.dimension == [('-10', '10'), ('-   2147483648', '2147483648')]
    assert avar.bounds == [('-2', '2'), ('1000000', '1000001')]
    assert avar.shape == ['4', '1']

    bvar = subr.a.variables['b']

    assert bvar.dimension == [('-20', '20'), ('100', '113'), ('-  512', '713')]
    assert bvar.shape == ['40', '13', '1225']

def test_provides():
    source_str = '''
    module mod1
    implicit none
    integer, parameter :: GP = 6
    integer :: a,b,c,d,e
    ! module_provides = {GP,a,b,c,d,e}
    ! use_provides = {}
    end module mod1

    module mod2
    implicit none
    integer, parameter :: SP = 5
    real :: a,b,c
    ! module_provides = {SP,a,b,c}
    ! use_provides = {}
    end module mod2

    module mod3
    use mod1
    implicit none
    integer, parameter :: DP = 0
    ! module_provides = {DP}
    ! use_provides = {GP,a,b,c,d,e}
    end module mod3

    module mod4
    use mod2
    implicit none
    ! module_provides = {}
    ! use_provides = {SP,a,b,c}
    end module mod4

    module mod5
    use mod3, only: lGP => GP, a,b,e
    use mod4, only: a2 => a, b2 => b
    implicit none

    integer, parameter :: FP = 1000
    integer(kind=kind(0)) :: dummy
    parameter (dummy = 20)
    integer, private :: x,y,z

    ! module_provides = {FP, dummy}
    ! use_provides = {lGP, a, b, e, a2, b2}
    end module mod5

    module mod6
    use mod5, qgp => lgp
    implicit none
    ! module_provides = {}
    ! use_provides = {FP, dummy, a2, b2, qgp, a, b, e}
    end module mod6

      '''

    tree = api.parse(source_str, isfree=True, isstrict=False)
    mod5 = tree.a.module['mod5']
    mod6 = tree.a.module['mod6']
    assert mod5.a.module_provides.keys() == ['fp', 'dummy']
    assert mod5.a.use_provides.keys() == ['a', 'b', 'e', 'a2', 'b2', 'lgp']
    assert mod6.a.module_provides.keys() ==  []
    assert mod6.a.use_provides.keys() ==  ['fp', 'dummy', 'b', 'e', 'qgp', 'a2', 'a', 'b2']
    assert mod6.a.use_provides['qgp'].name == 'gp'

def test_walk():
    source_str = '''\
    ! before foo 
    subroutine foo
    integer i, r
    do i = 1,100
      r = r + 1
    end do
    ! after end do
    end subroutine foo
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False, ignore_comments=False)
    for stmt, depth in api.walk(tree, 1):
        print depth, stmt.item
