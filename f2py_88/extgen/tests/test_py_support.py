
import numpy
from numpy.testing import *
from extgen import *
import unittest
NumpyTestCase = unittest.TestCase

class _test_PyCArrayTypeSpec(NumpyTestCase):

    def check_str(self):
        s = PyCArrayTypeSpec(numpy.int8, [10])
        assert_equal(str(s),"PyCArrayTypeSpec('numpy_int8', [10])")
        assert_equal(s.generate(),'npy_int8*')

    def check_simple(self):
        f = PyCFunction('func')
        f += PyCArgument('i8', PyCArrayTypeSpec(numpy.int8,[3]))
        print f.generate()


class test_PyCModule(NumpyTestCase):

    def check_doc(self):
        m = PyCModule('PyCModule_test',
                      title='This is first line.\\nSecond line.',
                      description='This is a module.\\nYes, it is.')
        mod = m.build()
        assert_string_equal(mod.__doc__,'''\
This module 'PyCModule_test' is generated with ExtGen from NumPy version .*

This is first line.
Second line.

This is a module.
Yes, it is.''')

class test_PyCFunction(NumpyTestCase):

    def check_generate(self):
        f = PyCFunction('foo')
        assert_string_equal(f.generate(),'''\
static
char pyc_function_foo_doc[] = 
\"  foo() -> None\"
\"\\n\\n:Returns:\\n  None\"
;
static
PyObject*
pyc_function_foo(PyObject *pyc_self, PyObject *pyc_args, PyObject *pyc_keywds) {
  PyObject * volatile pyc_buildvalue = NULL;
  volatile int capi_success = 1;
  static char *capi_kwlist[] = {NULL};
  if (PyArg_ParseTupleAndKeywords(pyc_args, pyc_keywds,"",
                                  capi_kwlist)) {
    capi_success = !PyErr_Occurred();
    if (capi_success) {
      pyc_buildvalue = Py_BuildValue("");
    }
  }
  return pyc_buildvalue;
}''')

    def check_doc(self):
        f = PyCFunction('foo',
                        title='  Function title.\\nSecond line.',
                        description=' This is a function.\\n2nd line.')
        e = PyCModule('PyCFunction_test', f)
        mod = e.build()
        assert_string_equal(mod.foo.__doc__,'''\
  foo() -> None

  Function title.
  Second line.

 This is a function.
 2nd line.

:Returns:
  None''')

    def check_py_scalar_local_integer(self):
        f = PyCFunction('foo')
        f += Variable('a1', int, init='int(a2)+1', depends=['a2'])
        f += Variable('a2', float, init=2.3)
        f += Variable('r', object, 'out')
        f += CCode('r = a1; Py_INCREF(r);')
        #print PyCModule('aa',f).build()
        #print
        print f.generate()
        #foo = f.build()
        #foo()

    def check_c_scalar_argument_return_integer(self):
        f = PyCFunction('foo')
        f += Variable('a1', 'c_char', 'in, out')
        f += Variable('a2', 'c_short', 'in, out')
        f += Variable('a3', 'c_int', 'in, out')
        f += Variable('a4', 'c_long', 'in, out')
        f += Variable('a5', 'c_long_long', 'in, out')
        f += Variable('a6', 'c_Py_ssize_t', 'in, out')
        print f.generate()
        foo = f.build()
        args = (2,2,2,2,2,2)
        assert_equal(foo(*args),args)

        f = PyCFunction('foo')
        f += Variable('a1', 'c_unsigned_char', 'in, out')
        f += Variable('a2', 'c_unsigned_short', 'in, out')
        f += Variable('a3', 'c_unsigned_int', 'in, out')
        f += Variable('a4', 'c_unsigned_long', 'in, out')
        f += Variable('a5', 'c_unsigned_long_long', 'in, out')
        foo = f.build()
        args = (2,2,2,2,2)
        assert_equal(foo(*args),args)

    def check_c_scalar_return_integer(self):
        f = PyCFunction('foo')
        f += Variable('a1', 'c_char', 'out')
        f += Variable('a2', 'c_short', 'out')
        f += Variable('a3', 'c_int', 'out')
        f += Variable('a4', 'c_long', 'out')
        f += Variable('a5', 'c_long_long', 'out')
        f += Variable('a6', 'c_Py_ssize_t', 'out')
        f += 'a1 = 2; a2 = 3; a3 = 4; a4 = 5; a5 = 6; a6 = 7;'
        foo = f.build()
        assert_equal(foo(),(2,3,4,5,6,7))

    def check_c_scalar_optional_integer(self):
        f = PyCFunction('foo')
        f += Variable('a1', 'c_char', 'optional', init = '1')
        f += Variable('b1', 'c_char', 'out')
        f += 'b1 = a1;'
        foo = f.build()
        assert_equal(foo(),1)
        assert_equal(foo(2),2)

        f = PyCFunction('foo')
        f += Variable('a1', 'c_char', 'optional')
        f += Variable('b1', 'c_char', 'out')
        f += 'b1 = a1;'
        foo = f.build()
        assert_equal(foo(),0)
        assert_equal(foo(2),2)

    def check_c_scalar_optional_return_integer(self):
        f = PyCFunction('foo')
        f += Variable('a1', 'c_char', 'optional, out', init='1')
        f += 'a1++;'
        foo = f.build()
        assert_equal(foo(),2)
        assert_equal(foo(3),4)

    def check_c_scalar_argument_return_float(self):
        f = PyCFunction('foo')
        f += Variable('a1', 'c_float', 'in, out')
        f += Variable('a2', 'c_double', 'in, out')
        f += Variable('a3', 'c_Py_complex', 'in, out')
        foo = f.build()
        args = (1.2, 1.2, 1+2j)
        results = (numpy.float32(1.2), 1.2, 1+2j)
        assert_equal(foo(*args),results)

    def check_c_scalar_argument_return_string(self):
        f = PyCFunction('foo')
        f += Variable('a1', 'c_char1', 'in, out')
        f += Variable('a2', 'c_const_char_ptr', 'in, out')
        f += Variable('a3', 'c_Py_UNICODE_ptr', 'in, out')
        foo = f.build()
        args = ('a','hey', u'hoo')
        assert_equal(foo(*args),args)
        args = ('b',None, u'soo')
        assert_equal(foo(*args),args)

    def check_c_scalar_argument_return_string_2(self):
        f = PyCFunction('foo')
        f += Variable('a', 'c_char_ptr', 'in, out')
        f += 'a[0] = \'H\';'
        foo = f.build()
        s = 'hey'
        assert_equal(foo(s),'Hey')
        assert_equal(s,'hey')
        
    def check_py_argument_return_immutable(self):
        f = PyCFunction('foo')
        f += Variable('a1', int, 'in, out')
        f += Variable('a2', long, 'in, out')
        f += Variable('a3', float, 'in, out')
        f += Variable('a4', complex, 'in, out')
        f += Variable('a5', str, 'in, out')
        f += Variable('a6', unicode, 'in, out')
        f += Variable('a7', bool, 'in, out')
        foo = f.build()

        args = 2, 2L, 1.2, 1+2j,'hei',u'tere', True
        assert_equal(foo(*args),args)

    def check_py_argument_return_containers(self):
        f = PyCFunction('foo')
        f += Variable('a1', list, 'in, out')
        f += Variable('a2', tuple, 'in, out')
        f += Variable('a3', dict, 'in, out')
        f += Variable('a4', set, 'in, out')
        f += Variable('a5', object, 'in, out')
        foo = f.build()
        args = [1,2], (1,2), {1:2}, set([1,2]), f
        assert_equal(foo(*args),args)
        
    def check_numpy_scalar_argument_return_int(self):
        f = PyCFunction('foo')
        f += Variable('i1', numpy.int8, 'in, out')
        f += Variable('i2', numpy.int16, 'in, out')
        f += Variable('i3', numpy.int32, 'in, out')
        f += Variable('i4', numpy.int64, 'in, out')

        foo = f.build()
        args = -2, -3, -4, -5
        assert_equal(foo(*args),args)

        f = PyCFunction('foo')
        f += Variable('i1', 'npy_int8', 'in, out')
        f += Variable('i2', 'npy_int16', 'in, out')
        f += Variable('i3', 'npy_int32', 'in, out')
        f += Variable('i4', 'npy_int64', 'in, out')

        foo = f.build()
        args = (-1,-1,-1,-1)
        results = (-1,-1,-1,-1)
        assert_equal(foo(*args),results)
        args = (2**8,2**16,2**32,2**63-1)
        results = (0,0,0,2**63-1)
        assert_equal(foo(*args),results)

    def check_numpy_scalar_argument_return_uint(self):
        f = PyCFunction('foo')
        f += Variable('i1', numpy.uint8, 'in, out')
        f += Variable('i2', numpy.uint16, 'in, out')
        f += Variable('i3', numpy.uint32, 'in, out')
        f += Variable('i4', numpy.uint64, 'in, out')
        foo = f.build()
        args = -2, -3, -4, -5
        results = numpy.uint8(-2), numpy.uint16(-3), numpy.uint32(-4), numpy.uint64(-5)
        assert_equal(foo(*args), results)

        f = PyCFunction('foo')
        f += Variable('i1', 'npy_uint8', 'in, out')
        f += Variable('i2', 'npy_uint16', 'in, out')
        f += Variable('i3', 'npy_uint32', 'in, out')
        f += Variable('i4', 'npy_uint64', 'in, out')
        
        foo = f.build()
        args = (-1,-1,-1,-1)
        results = (2**8-1,2**16-1,2**32-1,2**64-1)
        assert_equal(foo(*args),results)

    def check_numpy_scalar_argument_return_float(self):
        f = PyCFunction('foo')
        f += Variable('a1', numpy.float32, 'in, out')
        f += Variable('a2', numpy.float64, 'in, out')
        f += Variable('a3', numpy.float128, 'in, out')
        foo = f.build()
        args = 1.2, 1.2, 1.2
        results = (numpy.float32(1.2), numpy.float64(1.2), numpy.float128(1.2))
        assert_equal(foo(*args), results)

        f = PyCFunction('foo')
        f += Variable('a1', 'npy_float32', 'in, out')
        f += Variable('a2', 'npy_float64', 'in, out')
        f += Variable('a3', 'npy_float128', 'in, out')
        foo = f.build()
        args = (1.2, 1.2, 1.2)
        results = (numpy.float32(1.2), numpy.float64(1.2), numpy.float128(1.2))
        assert_equal(foo(*args),results)

    def check_numpy_scalar_argument_return_complex(self):
        f = PyCFunction('foo')
        f += Variable('a1', numpy.complex64, 'in, out')
        f += Variable('a2', numpy.complex128, 'in, out')
        f += Variable('a3', numpy.complex256, 'in, out')
        foo = f.build()
        args = 1+2j,1+2j,1+2j
        results = (numpy.complex64(1+2j),numpy.complex128(1+2j),numpy.complex256(1+2j))
        assert_equal(foo(*args),results)

        f = PyCFunction('foo')
        f += Variable('a1', 'npy_complex64', 'in, out')
        f += Variable('a2', 'npy_complex128', 'in, out')
        f += Variable('a3', 'npy_complex256', 'in, out')

        foo = f.build()
        args = (1+2j, 1+2j, 1+2j)
        results = (numpy.complex64(1+2j),numpy.complex128(1+2j),numpy.complex256(1+2j))
        assert_equal(foo(*args),results)

    def check_numpy_scalar_argument_return_string_1(self):
        f = PyCFunction('foo')
        f += Variable('a1', numpy.string_, 'in, out')
        f += Variable('a2', numpy.string0, 'in, out')
        foo = f.build()
        args = ('hey', [1,2])
        results = (numpy.string_('hey'), numpy.string_('[1, 2]'))
        assert_equal(foo(*args), results)

        f = PyCFunction('foo')
        f += Variable('a1', 'npy_str', 'in, out')
        f += Variable('a2', 'npy_string', 'in, out')
        foo = f.build()
        assert_equal(foo(*args), results)

    def check_numpy_scalar_argument_return_string_2(self):
        f = PyCFunction('foo')
        f += Variable('a', 'npy_str', 'in, out')
        f += 'a.data[0] = \'H\';'
        foo = f.build()
        s = numpy.str_('hey')
        assert_equal(foo(s),'Hey')
        assert_equal(s,'hey')

    def check_numpy_scalar_argument_return_unicode_1(self):
        f = PyCFunction('foo')
        f += Variable('a1', numpy.unicode_, 'in, out')
        f += Variable('a2', numpy.unicode0, 'in, out')
        foo = f.build()
        args = (u'hey', [1,2])
        results = (numpy.unicode_('hey'), numpy.unicode_('[1, 2]'))
        assert_equal(foo(*args), results)

    def check_numpy_scalar_argument_return_unicode_2(self):
        f = PyCFunction('foo')
        f += Variable('a', 'npy_unicode', 'in, out')
        f += 'a.data[0] = \'H\';'
        foo = f.build()
        s = numpy.unicode_('hey')
        assert_equal(foo(s),u'Hey')
        assert_equal(s, u'hey')

    def check_numpy_scalar_argument_return_generic(self):
        f = PyCFunction('foo')
        f += Variable('a1', numpy.int_, 'in, out')
        f += Variable('a2', numpy.float_, 'in, out')
        f += Variable('a3', numpy.complex_, 'in, out')
        foo = f.build()
        args = 2, 1.2, 1+2j
        results = numpy.int_(2), numpy.float_(1.2), numpy.complex(1+2j)
        assert_equal(foo(*args),results)
        args = [2], [1.2], [1+2j]
        assert_equal(foo(*args),results)
        args = [2], [1.2], [1,2]
        assert_equal(foo(*args),results)

        f = PyCFunction('foo')
        f += Variable('a1', 'npy_int', 'in, out')
        f += Variable('a2', 'npy_float', 'in, out')
        f += Variable('a3', 'npy_complex', 'in, out')
        foo = f.build()
        args = 2, 1.2, 1+2j
        results = numpy.int_(2), numpy.float_(1.2), numpy.complex(1+2j)
        assert_equal(foo(*args),results)
        args = [2], [1.2], [1+2j]
        assert_equal(foo(*args),results)
        args = [2], [1.2], [1,2]
        assert_equal(foo(*args),results)

    def check_numpy_scalar_argument_return_bool(self):
        f = PyCFunction('foo')
        f += Variable('a1', 'npy_bool', 'in, out')
        f += Variable('a2', numpy.bool_, 'in, out')
        foo = f.build()
        args = (2,0)
        results = (True, False)
        assert_equal(foo(*args),results)
        
    def check_numeric_array_argument_return(self):
        import Numeric as N
        f = PyCFunction('foo')
        f += Variable('a', N.ArrayType, 'in, out')
        foo = f.build()
        assert_equal(foo(N.array([1,2])), N.array([1, 2]))

    def check_numpy_array_argument_return(self):
        import numpy as N
        f = PyCFunction('foo')
        f += Variable('a', N.ndarray, 'in, out')
        foo = f.build()
        assert_equal(foo(N.array([1,2])), N.array([1, 2]))

    def check_numpy_scalar_argument_return_void(self):
        f = PyCFunction('foo')
        f += Variable('a1', numpy.void, 'in, out')
        f += Variable('a2', numpy.void, 'in, out')
        foo = f.build()
        args = ('he', 4)
        results = (numpy.void('he'), numpy.void(4))
        assert_equal(foo(*args), results)

    def check_numpy_array_local_int(self):
        f = PyCFunction('foo')
        f += Variable('n', 'npy_intp', 'in')
        f += Variable('a1', numpy.int_, ('n',))
        f += Variable('m', 'npy_intp', 'out')
        f += 'm = PyArray_SIZE(a1);'
        foo = f.build()
        assert_equal(foo(2), 2)

    def check_numpy_array_argument_return_int(self):
        f = PyCFunction('foo')
        f += Variable('a1', numpy.int_, 'in, out', (3))
        foo = f.build()
        print foo([1,2])

if __name__ == "__main__":
    print NumpyTest().run()
