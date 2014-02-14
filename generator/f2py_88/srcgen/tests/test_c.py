import sys
import difflib
def equal_strings(text1, text2, _cache=set()):
    if text1==text2:
        return True
    d = difflib.Differ()
    l = list(d.compare(text1.splitlines(1), text2.splitlines(1)))
    d = {}
    for i in range(len(l)):
        if l[i][0]!=' ':
            for j in range(i-2, i+3):
                if not 0<=j<len(l):
                    continue
                if j in d:
                    continue
                d[j] = '%03d: %r\n' % (j, l[j])
    if (text1, text2) in _cache:
        return False
    _cache.add((text1, text2))
    l = [v for i,v in sorted(d.items())]
    sys.stdout.flush()
    sys.stderr.write('\n')
    sys.stderr.writelines(l)
    sys.stderr.flush()
    return False


from srcgen.basic import Line, Block
from srcgen.c import *

def test_include():
    h = Include('hey.h')
    h1 = Include('hey.h', is_std_header=True)
    assert equal_strings(h.realize(), '#include "hey.h"')
    assert h1.realize()=='#include <hey.h>'

def test_typedef_enum():
    s = TypeDefEnum('foo')
    assert equal_strings(s.realize(), '''typedef enum {\n} foo''')
    s.add('A')
    assert equal_strings(s.realize(), '''\
typedef enum {
  A
} foo''')

    s.add(Declarator('B', '2'))
    s.add('C')
    assert equal_strings(s.realize(), '''\
typedef enum {
  A,
  B = 2,
  C
} foo''')

def test_typedef_struct():
    s = TypeDefStruct('foo')
    assert equal_strings(s.realize(), '''typedef struct {\n} foo''')
    s.add(Declaration('a', TypeSpec('long')))
    assert equal_strings(s.realize(), '''\
typedef struct {
  long a;
} foo''')
    s.add('b')
    assert equal_strings(s.realize(), '''\
typedef struct {
  long a;
  int b;
} foo''')
    assert equal_strings(s.get_view('type_spec'),'foo')
    assert equal_strings(s.get_view('short_type_spec'),'Sli_')

    s = TypeDefStruct(None)
    assert equal_strings(s.realize(), '''typedef struct {\n} S__type''')
    s.add(Declaration('a', TypeSpec('long')))
    assert equal_strings(s.realize(), '''\
typedef struct {
  long a;
} Sl__type''')

def test_typedef_function():
    s = TypeDefFunction('foo')
    assert equal_strings(s.realize(), 'typedef void (*foo)(void)')
    assert equal_strings(s.get_view('short_type_spec'), 'F_')
    s.add('int')
    assert equal_strings(s.realize(), 'typedef int (*foo)(void)')
    assert equal_strings(s.get_view('short_type_spec'), 'Fi_')
    s.add('long')
    assert equal_strings(s.realize(), 'typedef int (*foo)(long)')
    assert equal_strings(s.get_view('short_type_spec'), 'Fil_')
    s.add('float')
    assert equal_strings(s.realize(), 'typedef int (*foo)(long, float)')
    assert equal_strings(s.get_view('short_type_spec'), 'Filf_')

    s = TypeDefFunction(None)
    assert equal_strings(s.realize(), 'typedef void (*F__type)(void)')
    s.add('int')
    assert equal_strings(s.realize(), 'typedef int (*Fi__type)(void)')

def test_typedef_typespec():
    s = TypeDefTypeSpec('foo')
    assert equal_strings(s.realize(), 'typedef int foo')
    assert equal_strings(s.get_view('short_type_spec'), 'Ti_')
    s.save()
    s.add('long')
    assert equal_strings(s.realize(), 'typedef long foo')
    assert equal_strings(s.get_view('short_type_spec'), 'Tl_')
    s.restore()
    s.add('long**')
    assert equal_strings(s.realize(), 'typedef long** foo')
    assert equal_strings(s.get_view('short_type_spec'), 'Tppl_')
    s.restore()
    s.add('double[2][3]')
    assert equal_strings(s.realize(), 'typedef double[2][3] foo')
    assert equal_strings(s.get_view('short_type_spec'), 'Td2j3_')

    s = TypeDefTypeSpec(None)
    assert equal_strings(s.realize(), 'typedef int Ti__type')
    assert equal_strings(s.get_view('short_type_spec'), 'Ti_')

    s.save()
    s.add('long*')
    assert equal_strings(s.realize(), 'typedef long* Tpl__type')

def test_scalar_declarator():
    d = Declarator('a')
    assert d.realize()=='a'
    d.add('2')
    assert d.realize()=='a = 2'

def test_string_declarator():
    d = Declarator('a', is_string=True)
    l = Line('2')
    assert d.realize()=='a'
    d.add(l)
    assert d.realize()=='a[] = "2"'
    l.add('b')
    assert d.realize()=='a[] = "2b"'
    d.add('hey')
    assert d.realize()=='a[] = "2b\\n"\n"hey"'

def test_seq_declarator():
    d = Declarator('a', is_scalar=False)
    assert d.realize()=='a'
    d.add('2')
    d.add('3')
    assert d.realize()=='a = {2, 3}'

def test_type_spec():
    t = TypeSpec('int')
    assert t.realize()=='int'
    assert equal_strings(t.short_type_spec,'i')
    assert equal_strings(TypeSpec('long**').short_type_spec,'ppl')
    assert equal_strings(TypeSpec('float[2]').short_type_spec,'f2')
    assert equal_strings(TypeSpec('float[2][34]').short_type_spec,'f2j34')

def test_declaration():
    d = Declaration('a')
    assert d.realize()=='int a'
    d.add('b')
    assert d.realize()=='int a, b'
    d.add(Declarator('c', '3'))
    assert equal_strings(d.realize(), 'int a, b, c = 3')

def test_function():
    f = Function('foo')
    assert equal_strings(f.realize(),"""\
void
foo(void) {
}""")
    f.add(Argument('a'))
    assert equal_strings(f.realize(),"""\
void
foo(int a) {
}""")
    f.add(Argument('b', TypeSpec('long')))
    assert equal_strings(f.realize(),"""\
void
foo(int a, long b) {
}""")
    f.add(Variable('c', TypeSpec('double'), 'd'))
    assert equal_strings(f.realize(),"""\
void
foo(int a, long b) {
  double c, d;
}""")
    f.add('c = 2;')
    assert equal_strings(f.realize(),"""\
void
foo(int a, long b) {
  double c, d;
  c = 2;
}""")
    f.add('{\n  d = 3;\n  ;\n}')
    assert equal_strings(f.realize(),"""\
void
foo(int a, long b) {
  double c, d;
  c = 2;
  {
    d = 3;
    ;
  }
}""")

def test_csource():
    s = SourceFile('foo.c')
    s.save()
    assert equal_strings(s.get_view('string'),'''\
#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
}
#endif
''')
    s.add(Include('foo.h'))
    s.add(Include('foo.h'))
    s.add(Include('string.h'))
    assert equal_strings(s.get_view('string'), '''\
#ifdef __cplusplus
extern "C" {
#endif

/* Includes */
#include "foo.h"
#include <string.h>

#ifdef __cplusplus
}
#endif
''')
    s.restore()

    f = Function('foo')
    s.add(f)
    assert equal_strings(s.get_view('string'), '''\
#ifdef __cplusplus
extern "C" {
#endif

/* ProtoTypes */
void foo(void);

/* Definitions */
void
foo(void) {
}

#ifdef __cplusplus
}
#endif
''')
    s.add(f)
    assert equal_strings(s.get_view('string'),'''\
#ifdef __cplusplus
extern "C" {
#endif

/* ProtoTypes */
void foo(void);

/* Definitions */
void
foo(void) {
}

#ifdef __cplusplus
}
#endif
''')
    s.restore()

    s.add(f)
    f2 = Function('bar',TypeSpec('int'), Argument('a', TypeSpec('float')))
    f2.add(Variable(Declarator('c','2'), TypeSpec('double')))
    s.add(f2)
    assert equal_strings(s.get_view('string'),'''\
#ifdef __cplusplus
extern "C" {
#endif

/* ProtoTypes */
void foo(void);
int bar(float);

/* Definitions */
void
foo(void) {
}

int
bar(float a) {
  double c = 2;
}

#ifdef __cplusplus
}
#endif
''')

    s.restore()
    foo = TypeDefStruct('foo',
                        Declaration('a', TypeSpec('double')),
                        Declaration('b', TypeSpec('double')))

    s.add(foo)
    bar = TypeDefStruct('bar',
                        Declaration('c', TypeSpec('foo*')),
                        Declaration('d', TypeSpec('int')),
                        )
    s.add(bar)
    assert equal_strings(s.get_view('string'),'''\
#ifdef __cplusplus
extern "C" {
#endif

/* TypeDefs */
typedef struct {
  double a;
  double b;
} foo;
typedef struct {
  foo* c;
  int d;
} bar;

#ifdef __cplusplus
}
#endif
''')

    assert equal_strings(bar.get_view('type_spec'), 'bar')
    assert equal_strings(bar.get_view('short_type_spec', s), 'SpSdd_i_')

    s.restore()
    s.add(Declaration('a', Keyword('static'), TypeSpec('double*'), Keyword('static')))

    assert equal_strings(s.get_view('string'),'''\
#ifdef __cplusplus
extern "C" {
#endif

/* Definitions */
static double* a;

#ifdef __cplusplus
}
#endif
''')
