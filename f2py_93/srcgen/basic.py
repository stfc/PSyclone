
__all__ = ['Word', 'Line', 'Block', 'SourceFile']
from .component import Component

class Word(Component):
    """ Represents immutable string.

    Provides content for:
      words
    """
    templates = dict(
        string = '%(word)s'
        )

    template_options = dict()

    def __init__(self, word):
        Component.__init__(self)
        self.word = word

    def add(self, other):
        raise TypeError('%s instance is immutable' % (type(self).__name__))

    def request_data(self, subs_name, parents):
        if subs_name=='words':
            return self.word

    def realize(self):
        self.save()
        r = self.get_view('string')
        self.restore()
        return r

class Line(Component):
    """ Represents a string.

    Components must provide:
      words
    
    Provides content for:
      strings
    """
    templates = dict(
        string = '%(words)s',
        )

    template_options = dict(
        words = dict(separator='')
        )

    def add(self, other):
        if isinstance(other, str):
            other = Word(other)
        Component.add(self, other)

    def request_data(self, subs_name, parents):
        if subs_name=='strings':
            return self.get_view('string')
        if subs_name=='blocks':
            return self.get_view('string')
        
    def realize(self):
        self.save()
        r = self.get_view('string')
        self.restore()
        return r    

class Block(Component):
    """ Represents a (optionally named) block of lines.

    Components must provide:
      strings

    Provides content for:
      block, named_block
    """
    templates = dict(
        string = '%(strings)s'
        )

    template_options = dict(
        strings = dict(separator='\n',
                       ignore_empty_content = True
                       )
        )

    def __init__(self, name = None, *leafs):
        Component.__init__(self, *leafs)
        self.name = name

    def add(self, other):
        if isinstance(other, str):
            other = Line(other)
        Component.add(self, other)

    def request_data(self, subs_name, parents):
        if subs_name=='block':
            return self.get_view('string')
        if subs_name=='named_block':
            return self.name, self.get_view('string')

    def realize(self):
        self.save()
        r = self.get_view('string')
        self.restore()
        return r    

class SourceFile(Block):
    """ Represents a file.

    Components must provide:
      strings

    Provides content for:
      block, named_block. filename
    """

    templates = dict(
        filename = '%(name)s',
        )

    def request_data(self, subs_name, parents):
        if subs_name=='filename':
            d = parents.get_attr('path', '.')
            return os.path.join(d, self.name)
        return Block.request_data(self, subs_name, parents)

    def realize(self):
        content = Block.realize(self)
        f = open(self.name, 'w')
        f.write(content)
        f.close()
        return self.name
