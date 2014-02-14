""" Defines Joiner class.
"""
__version__ = '1.0'
__author__ = 'Pearu Peterson <pearu.peterson@gmail.com>'
__all__ = ['Joiner']

import time

class Joiner(object):
    """ Join a list of string values using specified rules.

    The following methods are defined:

      .__init__(*args, **options) - construct Joiner values from args.
                The options define the rules how the string values
                are joined together in __str__() method.

      .add(value, key=None) - add value to a join list. Adding
                different values with the same key is not allowed.
                Default key is time.time(). If key exists with equal
                values then adding a value is skipped. A typical
                applications for this behaviour would be avoiding
                redefining the same functions.

      .copy(mapping=lambda v:v, **new_options) - return a (mapped)
                copy with new options.

      .reversed_copy() - return a reversed copy.

      .__radd__(indent) - return an indented copy.

      .__iadd__(other) - add other in-place.

      .__add__(other) - return joined Joiner instances using the
                LHS options.

      .__str__() - return a joined string.

    Examples:

    >>> print Joiner('a','b',separator=':', prefix='<', suffix='>')
    <a:b>
    >>> print Joiner('a','b',separator=':', prefix='<', suffix='>', reverse=True)
    <b:a>
    >>> print Joiner('ab','b',separator=':', prefix='<', suffix='>', replace_map={'b':'c'})
    <ac:c>
    >>> print Joiner('a','b',separator=':', prefix='<', suffix='>', use_indent=True, indent_offset=2)
      <a:b>
    >>> print Joiner(' a','b',separator=':', prefix='<', suffix='>', use_firstline_indent=True)
    < a:b>

    Note that the first value is raw-string:
    >>> print Joiner(r' a\\nb','c',separator=':', prefix='<', suffix='>', use_firstline_indent=True)
    < a: b:c>

    >>> print Joiner(default='hey', prefix='<', suffix='>')
    <hey>
    >>> print Joiner(default='hey', prefix='<', suffix='>', skip_prefix_when_empty=True)
    hey>
    >>> print Joiner(default='hey', prefix='<', suffix='>', skip_suffix_when_empty=True)
    <hey
    >>> print Joiner(default='hey', prefix='<', suffix='>', skip_prefix_suffix_when_single=True)
    hey
    >>> print Joiner('hey', prefix='<', suffix='>', skip_prefix_suffix_when_single=True)
    hey
    
    >>> c = Joiner(separator=', ', prefix='"', suffix='"')
    >>> c.add('hey',1)
    >>> c.add('hoo',2)
    >>> print c
    "hey, hoo"
    >>> c.add('hey',1)
    >>> c.add('hey2',1)
    Traceback (most recent call last):
    ...
    ValueError: Item 1 exists with different value

    >>> c2 = Joiner()
    >>> c2.add('bar')
    >>> c += c2
    >>> print c
    "hey, hoo, bar"
    >>> c = Joiner(c_block_indent=True)

    >>> c += 'line 2'
    >>> c += 'line 3 {'
    >>> c += 'line 4 {\\n  line 1'
    >>> c += '}'
    >>> c += '}'
    >>> c += 'line 5'
    >>> print c
    line 2
    line 3 {
      line 4 {
        line 1
      }
    }
    line 5
    >>> print '==' + c
    ==line 2
    ==line 3 {
      ==line 4 {
      ==  line 1
      ==}
    ==}
    ==line 5

    >>> c2 = Joiner(c_block_indent=True)
    >>> c2 += 'LINE 1 {'
    >>> c2 += c
    >>> c2 += '}'
    >>> print c2
    LINE 1 {
      line 2
      line 3 {
        line 4 {
          line 1
        }
      }
      line 5
    }

    """

    default_options = dict(
        separator='\n',
        prefix='',
        suffix='',
        skip_prefix_when_empty=False,
        skip_suffix_when_empty=False,
        default = '',
        reverse=False,
        user_defined_str = None,
        use_indent = False,
        use_firstline_indent = False, # implies use_indent, the string values are assumed to contain r'\n'
        c_block_indent = False, # implies use_indent
        indent_offset = 0,
        replace_map = {},
        ignore_empty_content = False,
        skip_prefix_suffix_when_single = False,
        check_func = None,
        )

    def __init__(self, *args, **options):
        self.keys = []
        self.values = []
        self.dict = {}
        for o in options:
            if o not in self.default_options:
                raise KeyError("Unsupported keyword arguments: %r" % (o))
        self.options = options.copy()
        map(self.add, args)

    @property
    def use_indent(self):
        if self.options.get('use_indent'):
            return True
        return self.use_firstline_indent or self.c_block_indent

    def __getattr__(self, name):
        if name in self.options:
            return self.options[name]
        if name in self.default_options:
            return self.default_options[name]
        return getattr(super(Joiner, self), name)

    def add(self, value, key=None):
        assert isinstance(value, str),`type(value)`
        if key is None:
            key = time.time()
        for old, new in self.replace_map.items():
            value = value.replace(old, new)
        if key in self.dict:
            v = self.dict[key]
            if v != value:
                raise ValueError("Item %r exists with different value" % (key))
            return
        if not value and self.ignore_empty_content:
            return
        self.keys.append(key)
        self.values.append(value)
        self.dict[key] = value

    def __iadd__(self, other):
        if isinstance(other, type(self)):
            for key, value in zip(other.keys, other.values):
                self.add(value, key)
        elif isinstance(other, list):
            for item in other:
                self += item
        elif isinstance(other, tuple):
            key, value = other
            self.add(value, key)
        else:
            self.add(other)
        return self

    def __radd__(self, indent):
        cpy = self.copy()
        for i in range(len(cpy.values)):
            cpy.values[i] = (indent+cpy.values[i].replace('\n','\n'+indent))
        return cpy

    def __add__(self, other):
        cpy = self.copy()
        cpy += other
        return cpy
    
    def __str__(self):
        if self.user_defined_str is not None:
            return self.user_defined_str(self)
        if self.check_func is not None:
            self.check_func(self)
        if self.values:
            l = self.values
            if self.reverse:
                l = l[:]
                l.reverse()
            if self.use_firstline_indent: # to indent documentation strings
                new_l = []
                for l1 in l:
                    lines = l1.split(r'\n')
                    i = len(lines[0]) - len(lines[0].lstrip())
                    indent = i * ' '
                    new_l.append(lines[0])
                    new_l.extend([indent + l2 for l2 in lines[1:]])
                l = new_l
            if self.c_block_indent:
                i = 0
                new_l = []
                for l1 in l:
                    di = l1.count('{') - l1.count('}')
                    i += di
                    if di>0:
                        indent = '  ' * (i-di)
                    else:
                        indent = '  ' * (i)
                    for l2 in l1.split('\n'):
                        new_l.append(indent + l2)
                l = new_l

            r = self.separator.join(l)
            if not (len(self.values)==1 and self.skip_prefix_suffix_when_single):
                r = self.prefix + r
                r = r + self.suffix
        else:
            r = self.default
            if not (r and self.skip_prefix_suffix_when_single):
                if not self.skip_prefix_when_empty:
                    r = self.prefix + r
                if not self.skip_suffix_when_empty:
                    r = r + self.suffix
        if r and self.use_indent:
            lines = r.splitlines(True)
            indent = self.indent_offset * ' '
            r = ''.join([indent + line for line in lines])
        return r

    def copy(self, mapping=None, **new_options):
        options = self.options.copy()
        options.update(**new_options)
        cpy = type(self)(**options)
        if mapping is None:
            cpy += self
        else:
            for key, value in zip(self.keys, self.values):
                cpy.add(mapping(value), key)
        return cpy

    def reverse_copy(self):
        cpy = self.copy(reverse=not self.reverse)
        cpy.keys.reverse()
        cpy.values.reverse()
        return cpy
    
def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    _test()
    print 'ok'
