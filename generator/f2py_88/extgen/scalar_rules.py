"""
TODO: npy_void
"""

import numpy
from capi import sctypebits

c_char = dict(\
    ctype = 'signed char',
    init = ' = 0',
    argument_format = 'b',
    return_format = 'b',
    argument_title = 'a python integer (converting to C signed char)',
    return_title = 'a python integer (converting from C signed char)',
    init_container = 'CDeclaration',
    )

c_short = dict(\
    ctype = 'short int',
    init = ' = 0',
    argument_format = 'h',
    return_format = 'h',
    argument_title = 'a python integer (converting to C short int)',
    return_title = 'a python integer (converting from C short int)',
    init_container = 'CDeclaration',
    )

c_int = dict(\
    ctype = 'int',
    init = ' = 0',
    argument_format = 'i',
    return_format = 'i',
    argument_title = 'a python integer (converting to C int)',
    return_title = 'a python integer (converting from C int)',
    init_container = 'CDeclaration',
    )
c_long = dict(\
    ctype = 'long',
    init = ' = 0',
    argument_format = 'l',
    return_format = 'l',
    argument_title = 'a python integer (converting to C long int)',
    return_title = 'a python integer (converting from C long int)',
    init_container = 'CDeclaration',
    )
c_long_long = dict(\
    ctype = 'PY_LONG_LONG',
    init = ' = 0',
    argument_format = 'L',
    return_format = 'L',
    argument_title = 'a python integer (converting to C PY_LONG_LONG)',
    return_title = 'a python integer (converting from C PY_LONG_LONG)',
    init_container = 'CDeclaration',
    )

c_unsigned_char = dict(\
    ctype = 'unsigned char',
    init = ' = 0',
    argument_format = 'B',
    return_format = 'B',
    argument_title = 'a python integer (converting to C unsigned char)',
    return_title = 'a python integer (converting from C unsigned char)',
    init_container = 'CDeclaration',
    )
c_unsigned_short = dict(\
    ctype = 'unsigned short int',
    init = ' = 0',
    argument_format = 'H',
    return_format = 'H',
    argument_title = 'a python integer (converting to C unsigned short int)',
    return_title = 'a python integer (converting from C unsigned short int)',
    init_container = 'CDeclaration',
    )
c_unsigned_int = dict(\
    ctype = 'unsigned int',
    init = ' = 0',
    argument_format = 'I',
    return_format = 'I',
    argument_title = 'a python integer (converting to C unsigned int)',
    return_title = 'a python integer (converting from C unsigned int)',
    init_container = 'CDeclaration',
    )
c_unsigned_long = dict(\
    ctype = 'unsigned long',
    init = ' = 0',
    argument_format = 'k',
    return_format = 'k',
    argument_title = 'a python integer (converting to C unsigned long int)',
    return_title = 'a python integer (converting from C unsigned long int)',
    init_container = 'CDeclaration',
    )
c_unsigned_long_long = dict(\
    ctype = 'unsigned PY_LONG_LONG',
    init = ' = 0',
    argument_format = 'K',
    return_format = 'K',
    argument_title = 'a python integer (converting to C unsigned PY_LONG_LONG)',
    return_title = 'a python integer (converting from C unsigned PY_LONG_LONG)',
    init_container = 'CDeclaration',
    )

c_float = dict(\
    ctype = 'float',
    init = ' = 0.0',
    argument_format = 'f',
    return_format = 'f',
    argument_title = 'a python floating point number (converting to C float)',
    return_title = 'a python floating point number (converting from C float)',
    init_container = 'CDeclaration',
    )

c_double = dict(\
    ctype = 'double',
    init = ' = 0.0',
    argument_format = 'd',
    return_format = 'd',
    argument_title = 'a python floating point number (converting to C double)',
    return_title = 'a python floating point number (converting from C double)',
    init_container = 'CDeclaration',
    )

c_Py_complex = dict(\
    ctype = 'Py_complex',
    argument_format = 'D',
    return_format = 'D',
    init = ' = {0.0, 0.0}',
    argument_title = 'a python complex number (converting to C Py_complex structure)',
    return_title = 'a python complex number (converting from C Py_complex structure)',
    init_container = 'CDeclaration',
    )

c_Py_ssize_t = dict(\
    ctype = 'Py_ssize_t',
    argument_format = 'n',
    return_format = 'n',
    init = ' = 0',
    argument_title = 'a python integer (converting to C Py_ssize_t)',
    return_title = 'a python integer (converting from C Py_ssize_t)',
    init_container = 'CDeclaration',
    )

c_char1 = dict(\
    ctype = 'char',
    argument_format = 'c',
    return_format = 'c',
    init = " = '\\0'",
    argument_title = 'a python character (converting to C char)',
    return_title = 'a python character (converting from C char)',
    init_container = 'CDeclaration',
    )

c_const_char_ptr = dict(\
    ctype = 'const char *',
    argument_format = 'z',
    return_format = 'z',
    init = ' = NULL',
    argument_title = 'a python string or Unicode or None object (converting to C const char *)',
    return_title = 'a python string or None (converting from C char *)',
    )

c_char_ptr = dict(\
    ctype = 'char *',
    argument_format = 'O&',
    argument_converter = 'pyobj_to_char_ptr',
    clean_argument_converter = 'clean_pyobj_to_char_ptr',
    return_format = 'z',
    init = ' = NULL',
    argument_title = 'a python string (converting to C char *)',
    return_title = 'a python string or None (converting from C char *)',
    )

c_Py_UNICODE_ptr = dict(\
    ctype = 'Py_UNICODE*',
    argument_format ='u',
    return_format = 'u',
    init = ' = NULL',
    argument_title = 'a python Unicode object (converting to C Py_UNICODE*)',
    return_title = 'a python Unicode object or None (converting from C Py_UNICODE*)'
    )

py_bool = dict(\
    ctype = 'PyBoolObject*',
    init = ' = NULL',
    pyctype = 'PyBool_Type',
    argument_format = 'O!',
    return_format = 'N',
    title = 'a python bool',
    )

py_int = dict(\
    ctype = 'PyObject*',
    ctype_exact = 'PyIntObject*',
    init = ' = NULL',
    pyctype = 'PyInt_Type',
    argument_format = 'O!',
    return_format = 'N',
    title = 'a python integer',
    init_container = 'FromPyObj',
    refcounted = True,
    )

py_long = dict(\
    ctype = 'PyLongObject*',
    init = ' = NULL',
    pyctype = 'PyLong_Type',
    argument_format = 'O!',
    return_format = 'N',
    title = 'a python long integer'
    )

py_float = dict(\
    ctype = 'PyObject*',
    init = ' = NULL',
    pyctype = 'PyFloat_Type',
    argument_format = 'O!',
    return_format = 'N',
    title = 'a python floating point number',
    init_container = 'FromPyObj',
    refcounted = True,
    )

py_complex = dict(\
    ctype = 'PyComplexObject*',
    init = ' = NULL',
    pyctype = 'PyComplex_Type',
    argument_format = 'O!',
    return_format = 'N',
    title = 'a python complex number'
    )

py_str = dict(\
    ctype = 'PyStringObject*',
    init = ' = NULL',
    argument_format = 'S',
    return_format = 'N',
    title = 'a python string'
    )

py_unicode = dict(\
    ctype = 'PyUnicodeObject*',
    init = ' = NULL',
    argument_format = 'U',
    return_format = 'N',
    title = 'a python Unicode object'
    )

py_buffer = dict(\
    pyctype = 'PyBuffer_Type',
    ctype = 'PyBufferObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python buffer')

py_tuple = dict(\
    pyctype = 'PyTuple_Type',
    ctype = 'PyTupleObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python tuple')

py_list = dict(\
    pyctype = 'PyList_Type',
    ctype = 'PyListObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python list')

py_dict = dict(\
    pyctype = 'PyDict_Type',
    ctype = 'PyDictObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python dictionary')

py_file = dict(\
    pyctype = 'PyFile_Type',
    ctype = 'PyFileObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python file object')

py_instance = dict(\
    pyctype = 'PyInstance_Type',
    ctype = 'PyObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python instance object')

py_function = dict(\
    pyctype = 'PyFunction_Type',
    ctype = 'PyFunctionObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python function object')

py_method = dict(\
    pyctype = 'PyMethod_Type',
    ctype = 'PyObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python instance method object')

py_module = dict(\
    pyctype = 'PyModule_Type',
    ctype = 'PyObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python module object')

py_iter = dict(\
    pyctype = 'PySeqIter_Type',
    ctype = 'PyObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python iterator')

py_property = dict(\
    pyctype = 'PyProperty_Type',
    ctype = 'PyObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python property attribute')

py_slice = dict(\
    pyctype = 'PySlice_Type',
    ctype = 'PyObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python slice object')

py_cell = dict(\
    pyctype = 'PyCell_Type',
    ctype = 'PyCellObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL')

py_generator = dict(\
    pyctype = 'PyGen_Type',
    ctype = 'PyGenObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL')

py_set = dict(\
    pyctype = 'PySet_Type',
    ctype = 'PySetObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python set object')

py_frozenset = dict(\
    pyctype = 'PyFrozenSet_Type',
    ctype = 'PySetObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python frozenset object')

py_cobject = dict(\
    ctype = 'PyCObject*',
    argument_format = 'O',
    return_format = 'N',
    init = ' = NULL',
    title = 'a PyCObject object')

py_type = dict(\
    pyctype = 'PyType_Type',
    ctype = 'PyTypeObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python type object')

py_object = dict(\
    ctype = 'PyObject*',
    argument_format = 'O',
    return_format = 'N',
    init = ' = NULL',
    title = 'a python object')

numeric_array = dict(\
    pyctype = 'PyArray_Type',
    ctype = 'PyArrayObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a Numeric array',
    require_numeric = True,
    )

numpy_ndarray = dict(\
    pyctype = 'PyArray_Type',
    ctype = 'PyArrayObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a numpy array',
    require_numpy = True,
    )

numpy_descr = dict(\
    pyctype = 'PyArrayDescr_Type',
    ctype = 'PyArray_Descr*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    require_numpy = True,
    )

numpy_ufunc = dict(\
    pyctype = 'PyUFunc_Type',
    ctype = 'PyUFuncObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    title = 'a numpy universal function',
    require_numpy = True,
    )

numpy_iter = dict(\
    pyctype = 'PyArrayIter_Type',
    ctype = 'PyArrayIterObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    require_numpy = True,
    )

numpy_multiiter = dict(\
    pyctype = 'PyArrayMultiIter_Type',
    ctype = 'PyArrayMultiIterObject*',
    argument_format = 'O!',
    return_format = 'N',
    init = ' = NULL',
    require_numpy = True,
    )

npy_bool = dict(\
    ctype = 'npy_bool',
    init = ' = 0',
    argument_format = 'O&',
    argument_converter = 'pyobj_to_npy_bool',
    return_format = 'O&',
    return_converter = 'pyobj_from_npy_bool',
    argument_title = 'a python truth value (converting to C npy_bool)',
    return_title = 'a numpy bool',
    require_numpy = True,
    init_container = 'CDeclaration',
    )

numpy_bool = dict(\
    ctype = 'PyBoolScalarObject*',
    init = ' = NULL',
    argument_format = 'O&',
    argument_converter = 'pyobj_to_numpy_bool',
    return_format = 'N',
    require_numpy = True,
    argument_title = 'a python bool (converting to C PyBoolScalarObject*)',
    return_title = 'a numpy bool',
    )

numpy_string = dict(\
    ctype = 'PyStringScalarObject*',
    init = ' = NULL',
    argument_format = 'O&',
    argument_converter = 'pyobj_to_numpy_string',
    return_format = 'N',
    require_numpy = True,
    argument_title = 'a python string (converting to C PyStringScalarObject*)',
    return_title = 'a numpy string',
    )

numpy_unicode = dict(\
    ctype = 'PyUnicodeScalarObject*',
    init = ' = NULL',
    argument_format = 'O&',
    argument_converter = 'pyobj_to_numpy_unicode',
    return_format = 'N',
    require_numpy = True,
    argument_title = 'a python string (converting to C PyUnicodeScalarObject*)',
    return_title = 'a numpy unicode',
    )

npy_string = dict(\
    typedef = 'npy_string',
    ctype = 'npy_string',
    init = ' = {NULL, 0}',
    argument_format = 'O&',
    argument_converter = 'pyobj_to_npy_string',
    clean_argument_converter = 'clean_pyobj_to_npy_string',
    return_format = 'O&',
    return_converter = 'pyobj_from_npy_string',
    require_numpy = True,
    argument_title = 'a python string (converting to C npy_string)',
    return_title = 'a numpy string',
    )

npy_unicode = dict(\
    typedef = 'npy_unicode',
    ctype = 'npy_unicode',
    init = ' = {NULL, 0}',
    argument_format = 'O&',
    argument_converter = 'pyobj_to_npy_unicode',
    clean_argument_converter = 'clean_pyobj_to_npy_unicode',
    return_format = 'O&',
    return_converter = 'pyobj_from_npy_unicode',
    require_numpy = True,
    argument_title = 'a python string (converting to C npy_unicode)',
    return_title = 'a numpy unicode',
    )

numpy_void = dict(\
    ctype = 'PyVoidScalarObject*',
    init = ' = NULL',
    argument_format = 'O&',
    argument_converter = 'pyobj_to_numpy_void',
    return_format = 'N',
    require_numpy = True,
    argument_title = 'a python string (converting to C PyVoidScalarObject*)',
    return_title = 'a numpy void',
    )

c_PY_LONG_LONG = c_long_long
c_unsigned_PY_LONG_LONG = c_unsigned_long_long
numpy_bool_ = numpy_bool
numpy_str_ = numpy_str = numpy_string0 \
                       = numpy_string_ = numpy_string
numpy_unicode0 = numpy_unicode_ = numpy_unicode
npy_str = npy_string
numpy_void0 = numpy_void

def _generate():
  scalars = {}  
  for Cls_name, bits_list in sctypebits.items():
    if Cls_name=='Complex':
        init = ' = {0.0, 0.0}'
        t = 'complex'
    elif Cls_name=='Float':
        init = ' = 0.0'
        t = 'floating point number'
    else:
        init = ' = 0'
        t = 'integer'
    for bits in bits_list:
        n = Cls_name.lower() + str(bits)
        Cls = Cls_name + str(bits)
        ctype = 'npy_' + n
        scalars[ctype] = dict(
            ctype = ctype,
            pycype = None,
            init = init,
            argument_format = 'O&',
            argument_converter = 'pyobj_to_'+ctype,
            return_format = 'O&',
            return_converter = 'pyobj_from_'+ctype,
            require_numpy = True,
            argument_title = 'a python %s (converting to C %s)' % (t,ctype),
            return_title = 'a numpy %s-bit %s' % (bits, t),
            init_container = 'CDeclaration',
            )

        ctype = 'Py%sScalarObject*' % (Cls)
        ctype_name = 'numpy_' + n
        scalars[ctype_name] = dict(
            ctype = ctype,
            pyctype = None,
            init = ' = NULL',
            argument_format = 'O&',
            argument_converter = 'pyobj_to_'+ctype_name,
            return_format = 'N',
            require_numpy = True,
            argument_title = 'a python %s (converting to C %s)' % (t,ctype),
            return_title = 'a numpy %s-bit %s' % (bits, t)
            )
  return scalars

for _k, _d in _generate().items():
    exec _k + ' = _d'

npy_intp = eval('npy_'+numpy.intp.__name__)
npy_int_ = eval('npy_'+numpy.int_.__name__)
npy_float = eval('npy_'+numpy.float_.__name__)
npy_complex = eval('npy_'+numpy.complex_.__name__)


if 0:
    array = dict(
        c_int = dict(\
    ctype='int*',
    init=' = NULL',
    title='a C int array',
    input_title = 'a python integer sequence (converting to C int*)',
    input_format = 'O',
    input_object = '&%(varname)s_py',
    input_frompyobj = dict(\
      required = '%(varname)s_arr = PyArray_FROMANY(%(varname)s_py, NPY_INT, %(rank)s, %(rank)s, %(requirements)s);\n'
      'if (%(varname)s_arr != NULL) {\n'
      '  %(varname)s = PyArray_DATA(%(varname)s_arr);',
      ),
    input_cleanfrompyobj = dict(\
      required = '} /*if (%(varname)s_arr != NULL)*/'
      ),
    output_title = 'a python integer sequence (converting from C int*)',
    output_format = 'N',
    output_object = '%(varname)s_arr'
    ),

        numpy_int8 = dict(\
    ctype='npy_int8*',
    init=' = NULL',
    title='a C npy_int8 array'
    )

        )
