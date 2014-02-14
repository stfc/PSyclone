from fparser import api

def test_default_private():
    src = '''\
module mod1
private
integer :: i
contains
subroutine s1
end subroutine
end module mod1
'''
    mod1 = api.parse(src, isfree=True, isstrict=False).content[0]
    assert mod1.get_provides() == {}, `mod1.get_provides()`
    assert mod1.a.variables.keys() == ['i']
    assert mod1.a.module_subprogram.keys() == ['s1']

def test_access_spec():
    src = '''\
module mod1
private
integer, public :: i
integer :: j, k
public :: j, s1
contains
subroutine s1
end subroutine
subroutine s2
end subroutine
end module mod1
'''
    mod1 = api.parse(src, isfree=True, isstrict=False).content[0]
    assert sorted(mod1.get_provides().keys()) == sorted(['i', 'j', 's1'])
    assert sorted(mod1.a.variables.keys()) == sorted(['i', 'j', 'k'])
    assert sorted(mod1.a.module_subprogram.keys()) == sorted(['s2', 's1'])
