
from srcgen.basic import *

def test_Word():
    w = Word('hey')
    assert w.realize()=='hey'

def test_Line():
    l = Line()
    assert l.realize()==''
    l.add('hey')
    assert l.realize()=='hey'
    l.add(' you')
    assert l.realize()=='hey you'
    l.restore()
    assert l.realize()==''
    l.add(['hei ','you'])
    assert l.realize()=='hei you'
    l.add(l)
    assert l.realize()=='hei youhei you'

def test_Block():
    b = Block()
    assert b.realize()==''
    b.add('hey')
    assert b.realize()=='hey'
    b.save()
    b.add('you')
    assert b.realize()=='hey\nyou'
    b.add(b)
    assert b.realize()=='hey\nyou\nhey\nyou'
    b.restore()
    assert b.realize()=='hey'
    b.restore()
    assert b.realize()==''

    l = Line('word1')
    b.add(l)
    b.add('end')
    b.save()
    l.add('word2')
    assert b.realize()=='word1word2\nend'
    b.restore()
    assert b.realize()=='word1\nend'
