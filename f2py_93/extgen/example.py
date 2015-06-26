
from __init__ import *

class Source(Component):

    template = 'This source has name: %(name)s\n'\
    'and the following component blocks:\n%(blocks)s'

    container_options = dict(
        blocks = dict(separator='\n')
        )
    default_container_label = 'blocks'
        
    def initialize(self, name, *components, **options):
        self.name = name
        map(self.add, components)
        return self

class Block(Component):

    template = 'Block No %(id)s'\
               '  data: %(content)s'

    def initialize(self, content, **options):
        self.content = content
        return self

