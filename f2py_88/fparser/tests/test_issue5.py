from fparser import api

def test_reproduce_issue():
    source_str = '''
    subroutine foo
    implicit none
    character a*8
    real*4 r
    character*2 b*4
    end subroutine foo
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False)
    foo = tree.content[0]
    a = foo.get_variable('a')
    b = foo.get_variable('b')
    r = foo.get_variable('r')
    assert a.typedecl.__class__.__name__=='Character'
    assert a.length == '8',`a.length`
    assert a.name =='a'
    assert a.typedecl.selector==('','')
    assert r.typedecl.__class__.__name__=='Real'
    assert r.typedecl.selector==('4','')

    assert b.typedecl.__class__.__name__=='Character'
    assert b.length == '4',`a.length`
    assert b.name =='b'
    assert b.typedecl.selector==('2','')

def test_recursive():
    source_str = '''
    recursive subroutine foo
    end subroutine foo
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False)
    foo = tree.content[0]
    assert foo.is_recursive()
