""" Implements components for C programs.
"""
# Created: April, 2008
# Author: Pearu Peterson

__all__ = ['Include',
           'Keyword',
           'TypeDefStruct', 'TypeDefFunction', 'TypeDefTypeSpec',
           'TypeDefEnum',
           'TypeSpec', 'Declarator', 'Declaration',
           'Variable', 'Argument', 'Function',
           'SourceFile']

import sys
from .api import Component, basic

# http://en.wikipedia.org/wiki/C_standard_library
clibs_headers = '''assert.h complex.h ctype.h errno.h fenv.h float.h
inttypes.h iso646.h limits.h locale.h math.h setjmp.h signal.h stdarg.h
stdbool.h stddef.h stdint.h stdio.h stdlib.h string.h tgmath.h time.h
wchar.h wctype.h'''.split()

short_type_spec_map = dict(
    Struct='S', Function='F', TypeSpec='T', Pointer='p', Enum='E',
    void='v',
    int='i',unsignedint='ui',signedint='i',
    char='c',unsignedchar='uC', signedchar='C',
    short='s', shortint='s', unsignedshort='us', signedshort='s',
    long='l', longint='l',
    signedlong='l', signedlongint='l', longsignedint='l',
    unsignedlong='ul', unsignedlongint='ul', longunsignedint='ul',
    longlong='L', longlongint='L',
    signedlonglong='L', signedlonglongint='L',
    unsignedlonglong='uL',
    unsignedlonglongint='uL',
    float='f',
    double='d', longdouble='D',
    size_t = 't',
    PyObject='PyO',
    PyObjectType='PyT',
    PyArrayObject='PyA',
    )

class Include(Component):
    """ Represents include CPP directive.
    """

    templates = dict(
        string = '#include %(include)s',
        user_include = '"%(filename)s"',
        std_include = '<%(filename)s>',
        )

    template_options = dict(
        include = dict()
        )

    request_own_data = True

    def __init__(self, filename, is_std_header = None):
        Component.__init__(self)
        self.filename = filename
        if is_std_header is None:
            is_std_header = filename in clibs_headers
        self.is_std_header = is_std_header

    def request_data(self, subs_name, parents):
        if subs_name=='include':
            if self.is_std_header:
                return self.get_view('std_include', parents)
            else:
                return self.get_view('user_include', parents)
        if subs_name=='Includes':
            return self.filename, self.get_view('string', parents)
        return super(type(self), self).request_data(subs_name, parents)

class TypeSpec(Component):
    """ C type specification.

    Basic <type_spec>-s:
      int, char, short, long, void, float, double,
      long long, long double,
      PyObject, PyObjectType, PyArrayObject
      <name of typedef struct>
    
    Pointers:
    
      <type_spec>*, <type_spec>**, etc

    Arrays:
      <type_spec>[<dim1>], <type_spec>[<dim1>][<dim2>], etc

    Not implemented:
      pointers to arrays, arrays of pointers
      
    """

    templates = dict(
        string = '%(name)s',
        )

    def __init__(self, name):
        Component.__init__(self)
        self.name = name

    @property
    def short_type_spec(self):
        return self.request_data('short_type_spec', None)

    def request_data(self, subs_name, parents):
        if subs_name=='type_spec':
            return self.name
        if subs_name=='short_type_spec':
            t = self.name
            t = t0 = t.replace(' ','')
            while t.endswith('*'):
                t = t[:-1]
            n = len(t0) - len(t)
            p = n*short_type_spec_map['Pointer']
            l = []
            while t.endswith(']'):
                i = t.rfind('[')
                l.insert(0, t[i+1:-1])
                t = t[:i]
            s = 'j'.join(l)
            r = short_type_spec_map.get(t)
            if r is None and parents is not None:
                c = parents.find_component_with_view('type_spec', t)
                if c is not None:
                    r = c.get_view('short_type_spec', parents)
            if r is None:
                sys.stderr.write('warning: %r is not in short_type_spec map\n' % (t))
                r = t
            return p + r + s

def type_spec_first_str(joiner):
    new = type(joiner)(default='void')
    if joiner.keys:
        new.add(joiner.values[0], joiner.keys[0])
    return str(new)

def type_spec_rest_str(joiner):
    new = type(joiner)(default='void', separator=', ')
    for k, v in zip(joiner.keys[1:], joiner.values[1:]):
        new.add(v, k)
    return str(new)

class TypeDefBase(Component):
    
    def __init__(self, name, *leafs):
        Component.__init__(self, *leafs)
        self._name = name

    @property
    def name(self):
        return self.get_name(None)

    def get_name(self, parents):
        if self._name is None:
            return '%s_type' % (self.get_view('short_type_spec', parents))
        return self._name

    def request_data(self, subs_name, parents):
        if subs_name=='TypeDefs':
            return self.get_name(parents), self.get_view('string', parents)
        if subs_name=='type_spec':
            return self.get_name(parents)
        if subs_name=='short_type_spec':
            self.get_view('short_type_spec', parents)
        return Component.request_data(self, subs_name, parents)

class TypeDefTypeSpec(TypeDefBase):
    """ Typedef of a type spec.
    """
    templates = dict(
        string = 'typedef %(type_spec)s %(name)s',
        type_spec = '%(name)s',
        short_type_spec = short_type_spec_map['TypeSpec'] + '%(short_type_spec)s_',
        )

    template_options = dict(
        type_spec = dict(default='int'),
        short_type_spec = dict(default='i', separator=''),
        )

    def add(self, other):
        if isinstance(other, str):
            other = TypeSpec(other)
        TypeDefBase.add(self, other)

class TypeDefStruct(TypeDefBase):
    """ Typedef of a struct.
    """

    templates = dict(
        string = 'typedef struct {\n%(variable_declarations)s} %(name)s',
        type_spec = '%(name)s',
        short_type_spec = short_type_spec_map['Struct'] + '%(variable_short_type_specs)s_',
        )

    template_options = dict(
        variable_declarations = dict(separator=';\n',
                                     use_indent=True, indent_offset=2,
                                     prefix='', skip_prefix_when_empty=True,
                                     suffix=';\n', skip_suffix_when_empty=True),
        variable_short_type_specs = dict(separator='')
        )

    def add(self, other):
        if isinstance(other, str):
            other = Declaration(other)
        TypeDefBase.add(self, other)

class TypeDefEnum(TypeDefBase):

    templates = dict(
        string = 'typedef enum {\n%(declarators)s} %(name)s',
        type_spec = '%(name)s',
        short_type_spec = short_type_spec_map['Enum'] + '%(name)s_',
        )

    template_options = dict(
        declarators = dict(separator=',\n',
                           use_indent=True, indent_offset=2,
                           prefix='', skip_prefix_when_empty=True,
                           suffix='\n', skip_suffix_when_empty=True),
        short_type_spec = dict(default='i', separator=''),
        )

    def add(self, other):
        if isinstance(other, str):
            other = Declarator(other)
        TypeDefBase.add(self, other)

class TypeDefFunction(TypeDefBase):
    """ Typedef of a function.
    """

    templates = dict(
        string = 'typedef %(type_spec_first..type_spec)s (*%(name)s)(%(type_spec_rest..type_spec)s)',
        type_spec = '%(name)s',
        short_type_spec = short_type_spec_map['Function'] + '%(short_type_spec)s_',
        )

    template_options = dict(
        type_spec_first = dict(user_defined_str=type_spec_first_str),
        type_spec_rest = dict(user_defined_str=type_spec_rest_str),
        short_type_spec = dict(separator=''),
        )

    def add(self, other):
        if isinstance(other, str):
            other = TypeSpec(other)
        TypeDefBase.add(self, other)

class Declarator(Component):
    """ Represents a declarator with optional initialization.

    Declarator(<name>, <initial values>,..., is_string=, is_scalar=)

    Initial values can be either be string or Line instances.
    """

    templates = dict(
        string = '%(name)s%(init)s',
        stringinit = '%(stringvalue..strings)s',
        scalarinit = '%(scalarvalue..strings)s',
        sequenceinit = '%(sequencevalue..strings)s',
        )

    template_options = dict(
        scalarvalue = dict(prefix=' = ',
                          skip_prefix_when_empty=True,
                          skip_suffix_when_empty=True,
                          ignore_empty_content = True, default=''),
        stringvalue = dict(prefix='[] = "', suffix='"',
                          skip_prefix_when_empty=True,
                          skip_suffix_when_empty=True,
                          ignore_empty_content = True, default='',
                          separator='\\n"\n"', replace_map = {'\n':'\\n'},
                          use_firstline_indent = True,
                          ),
        sequencevalue = dict(prefix=' = {', suffix='}',
                             skip_prefix_when_empty=True,
                             skip_suffix_when_empty=True,
                             ignore_empty_content = True, default='',
                             separator=', ',
                             ),
        init = dict() #
        )

    request_own_data = True

    def __init__(self, name, *leafs, **options):
        Component.__init__(self, *leafs)
        self._check_options(options, 'is_scalar', 'is_string')
        is_string = options.get('is_string')
        is_scalar = options.get('is_scalar')
        self.name = name
        if is_string:
            self.is_string = True
            self.is_scalar = False
            self.is_sequence = False
        else:
            if is_scalar is None:
                is_scalar = True
            self.is_string = False
            self.is_scalar = is_scalar
            self.is_sequence = not is_scalar

    def add(self, other):
        if isinstance(other, str):
            other = basic.Line(other)
        super(type(self), self).add(other)

    def request_data(self, subs_name, parents):
        if subs_name=='declarators':
            return self.get_view('string', parents)
        if subs_name=='declarator_name':
            return self.name
        if subs_name=='init':
            if self.is_sequence:
                return self.get_view('sequenceinit')
            if self.is_string:
                return self.get_view('stringinit')
            return self.get_view('scalarinit')        

class Keyword(Component):
    """ Represents keyword.

    Basic keywords:
      static, extern, const, etc
    """

    templates = dict(
        string = '%(name)s'
        )

    def __init__(self, name, *leafs):
        Component.__init__(self, *leafs)
        self.name = name

    def request_data(self, subs_name, parents):
        if subs_name == 'keyword':
            return self.name, self.name

def check_declarator_name(joiner):
    if len(joiner.values)!=1:
        raise ValueError('declarator_name must have exactly one value, got %s' % (len(joiner.values)))

class Declaration(Component):
    """ Represents declarations.
    """

    templates = dict(
        string = '%(type_spec)s %(declarators)s',
        argument= '%(declarator_name)s',
        variable= '%(declarator_name)s',
        argument_declaration = '%(type_spec)s %(declarator_name)s',
        variable_declaration = '%(type_spec)s %(declarator_name)s',
        prototype_argument_declaration = '%(type_spec)s',
        variable_short_type_spec = '%(short_type_spec)s',
        argument_short_type_spec = '%(short_type_spec)s',
        static_definition = '%(keyword)s%(type_spec)s %(declarator_name)s;',
        )

    template_options = dict(
        type_spec = dict(default='int'),
        short_type_spec = dict(default='i'),
        declarators = dict(separator=', ', default='<KILLLINE>'),
        declarator_name = dict(check_func = check_declarator_name),
        keyword = dict(default='', separator=' ',
                       suffix=' ', skip_suffix_when_empty=True
                       )
        )

    def add(self, other):
        if isinstance(other, str):
            other = Declarator(other)
        Component.add(self, other)

    def request_data(self, subs_name, parents):
        if subs_name=='variable_declarations':
            return self.get_view('string', parents)
        if subs_name=='arguments':
            return self.get_view('argument', parents)
        if subs_name=='argument_declarations':
            return self.get_view('argument_declaration', parents)
        if subs_name=='variable_declarations':
            return self.get_view('variable_declaration', parents)
        if subs_name=='prototype_argument_declarations':
            return self.get_view('prototype_argument_declaration', parents)
        if subs_name=='variable_short_type_specs':
            return self.get_view('variable_short_type_spec', parents)
        if subs_name=='argument_short_type_specs':
            return self.get_view('argument_short_type_spec', parents)
        if subs_name=='Definitions':
            return self.get_view('variable', parents), self.get_view('static_definition', parents)

class Variable(Declaration):

    def request_data(self, subs_name, parents):
        if 'argument' in subs_name:
            return
        return super(type(self), self).request_data(subs_name, parents)

    
class Argument(Declaration):

    def request_data(self, subs_name, parents):
        if 'variable' in subs_name:
            return
        return super(type(self), self).request_data(subs_name, parents)

def check_type_spec(joiner):
    if len(joiner.values)>1:
        raise ValueError('type_spec must have at most one value, got %s' % (len(joiner.values)))

class Function(basic.Block):

    templates = dict(
        string = '''\
%(type_spec)s
%(name)s(%(argument_declarations)s) {
%(variable_declarations)s
%(exec_statements..strings)s
}''',
        prototype = '%(type_spec)s %(name)s(%(prototype_argument_declarations)s)',
        typedef = 'typedef %(type_spec)s (*%(name)s)(%(prototype_argument_declarations)s)',
        )

    template_options = dict(
        type_spec = dict(default='void', check_func=check_type_spec),
        argument_declarations = dict(separator=', ', default='void'),
        prototype_argument_declarations = dict(separator=', ', default='void'),
        variable_declarations = dict(separator=';\n', default='<KILLLINE>',
                                     use_indent=True, indent_offset=2,
                                     suffix=';', skip_suffix_when_empty=True),
        exec_statements = dict(use_indent=True, c_block_indent=True, indent_offset=2, default='<KILLLINE>'),
        )

    request_own_data = True

    def request_data(self, subs_name, parents):
        if subs_name=='Definitions':
            return self.name, self.get_view('string', parents)
        if subs_name=='ProtoTypes':
            return self.name, self.get_view('prototype', parents)
        return super(type(self), self).request_data(subs_name, parents)

class SourceFile(basic.SourceFile):
    templates = dict(
        string = '''\
%(FileHeader)s
%(Includes)s
%(TypeDefs)s
%(ProtoTypes)s
%(Definitions)s
%(MainProgram)s
%(FileFooter)s
''',
        c_header = '''\
#ifdef __cplusplus
extern \"C\" {
#endif''',
        c_footer = '''
#ifdef __cplusplus
}
#endif'''
        )
    template_options = dict(
        FileHeader = dict(separator='\n', default='<KILLLINE>'),
        Includes = dict(separator='\n', default='<KILLLINE>',
                       prefix='\n/* Includes */\n', skip_prefix_when_empty=True),
        TypeDefs = dict(separator=';\n', default='<KILLLINE>',
                        prefix='\n/* TypeDefs */\n', skip_prefix_when_empty=True,
                        suffix=';', skip_suffix_when_empty=True,
                        ),
        ProtoTypes = dict(separator=';\n', default='<KILLLINE>',
                          prefix='\n/* ProtoTypes */\n', skip_prefix_when_empty=True,
                          suffix=';', skip_suffix_when_empty=True,
                          ),
        Definitions = dict(separator='\n\n', default='<KILLLINE>',
                           prefix='\n/* Definitions */\n', skip_prefix_when_empty=True),
        MainProgram = dict(separator='\n', default='<KILLLINE>',
                           prefix='/* MainProgram */\n', skip_prefix_when_empty=True),
        FileFooter = dict(separator='\n', default='<KILLLINE>'),
        )

    request_own_data = True

    def request_data(self, subs_name, parents):
        if subs_name=='FileHeader':
            return self.get_view('c_header', parents)
        if subs_name=='FileFooter':
            return self.get_view('c_footer', parents)
        return super(type(self), self).request_data(subs_name, parents)
