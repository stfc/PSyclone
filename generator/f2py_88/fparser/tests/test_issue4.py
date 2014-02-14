
from fparser import api

def test_reproduce_issue_private():
    source_str = '''
    module m
    contains
    subroutine a
    end subroutine a
    end module m
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False)

def test_private_subroutine():
    source_str = '''
    module m
    public
    private a
    contains
    subroutine a
    end subroutine a
    subroutine b
    end subroutine b
    end module m
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False)
    a = tree.content[0].content[3]
    b = tree.content[0].content[4]

    assert not a.is_public()
    assert a.is_private()

    assert b.is_public()
    assert not b.is_private()

def test_related_issue_type():
    source_str = '''
    module m
    type private :: a
    end type a
    type public :: b
    end type b
    type :: c
    end type c
    end module m
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False)
    a,b,c = tree.content[0].content[:3]
    assert a.is_private()
    assert not a.is_public()

    assert not b.is_private()
    assert b.is_public()

    assert not c.is_private()
    assert c.is_public()

def test_private_type():
    source_str = '''
    module m
    private
    public b
    type :: a
    end type a
    type :: b
    end type b
    type public :: c
    end type c
    end module m
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False)
    a,b,c = tree.content[0].content[2:5]
    assert a.is_private()
    assert not a.is_public()
    assert not b.is_private()
    assert b.is_public()
    assert not c.is_private()
    assert c.is_public()

def test_reproduce_issue_len():
    source_str = '''
    subroutine foo(a)
    character(lenmax) a
    character(lenmax, kind=4) b
    character(len=lenmax, kind=4) c
    character(kind=4, len=lenmax) d
    character(lenmax, 4) e
    end subroutine foo
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False)
    a, b, c, d, e=tree.content[0].content[:5]
    assert a.selector==('lenmax','')
    assert b.selector==('lenmax','4')
    assert c.selector==('lenmax','4')
    assert d.selector==('lenmax','4')
    assert e.selector==('lenmax','4')
