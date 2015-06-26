
import numpy

implementation = dict()
typedefs = dict()
macros = dict()

typedefs['npy_unicode'] = dict(\
    code = 'typedef struct { Py_UNICODE* data; Py_ssize_t size; } npy_unicode;')
typedefs['npy_string'] = dict(\
    code = 'typedef struct { char* data; Py_ssize_t size; } npy_string;')

macros['IS_VALID_INIT'] = dict(\
    code = '''\
#define IS_VALID_INIT(pyctype, name, name_str) \\
(name != NULL) {\\
  if ((PyObject_IsInstance(name, (PyObject*)pyctype)==1) \\
      && (!PyDict_SetItemString(capi_locals, name_str, name)))'''
    )

macros['SET_EXCEPTION_INIT'] = dict(\
    code = '''\
#define SET_EXCEPTION_INIT(pyctype, name, name_str, init_str) \\
  if (!PyErr_Occurred()) { \\
    PyObject* r = PyString_FromString("expected "); \\
    PyString_ConcatAndDel(&r, PyObject_Repr((PyObject*)pyctype)); \\
    PyString_ConcatAndDel(&r, PyString_FromString(" object while initializing " name_str ", got ")); \\
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(name))); \\
    PyString_ConcatAndDel(&r, PyString_FromString(" object from `" init_str "`")); \\
    PyErr_SetObject(PyExc_TypeError,r);\\
  } \\
Py_DECREF(name); \\
}'''
    )

sctypebits = dict(
    Int = [8, 16, 32, 64, 128, 256],
    UInt = [8, 16, 32, 64, 128, 256],
    Float = [16, 32, 64, 80, 96, 128, 256],
    Complex = [32, 64, 128, 160, 192, 256, 512],
    )

template_to_npy_scalar = '''
static int pyobj_to_%(ctype)s(PyObject* obj, %(ctype)s* ptr) {
  int return_value = 0;
  if (PyArray_IsScalar(obj, %(Cls)s)) {
    *ptr = (%(ctype)s)PyArrayScalar_VAL(obj,%(Cls)s);
    return_value = 1;
  } else if (PySequence_Check(obj)) {
    if (PySequence_Size(obj)==1)
      return_value = pyobj_to_%(ctype)s(PySequence_GetItem(obj,0), ptr);
  } else {
    PyObject* sc = Py%(Cls)sArrType_Type.tp_new(
      &Py%(Cls)sArrType_Type,Py_BuildValue("(O)",obj),NULL);
    if (sc==NULL) ;
    else if (PyArray_IsScalar(sc, Generic))
      return_value = pyobj_to_%(ctype)s(sc, ptr);
    else
      return_value = pyobj_to_%(ctype)s(PyArray_ScalarFromObject(sc), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C %(ctype)s"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}
'''

template_from_npy_scalar = '''
static PyObject* pyobj_from_%(ctype)s(%(ctype)s* value) {
  PyObject* obj = PyArrayScalar_New(%(Cls)s);
  if (obj==NULL) /* TODO: set exception */ return NULL;
  PyArrayScalar_ASSIGN(obj,%(Cls)s,*value);
  return obj;
}
'''

template_to_npy_complex_scalar = '''
static int pyobj_to_%(ctype)s(PyObject* obj, %(ctype)s* ptr) {
  int return_value = 0;
  if (obj==NULL) ;
  else if (PyArray_IsScalar(obj,%(Cls)s)) {
    ptr->real = PyArrayScalar_VAL(obj,%(Cls)s).real;
    ptr->imag = PyArrayScalar_VAL(obj,%(Cls)s).imag;
    return_value = 1;
  }
  else if (PySequence_Check(obj)) {
    if (PySequence_Size(obj)==1)
      return_value = pyobj_to_%(ctype)s(PySequence_GetItem(obj,0),ptr);
    else if (PySequence_Size(obj)==2) {
      return_value = pyobj_to_%(fctype)s(PySequence_GetItem(obj,0),&(ptr->real))
                     && pyobj_to_%(fctype)s(PySequence_GetItem(obj,1),&(ptr->imag));
    }
  } else {
    PyObject* sc = Py%(Cls)sArrType_Type.tp_new(
      &Py%(Cls)sArrType_Type,Py_BuildValue("(O)",obj),NULL);
    if (sc==NULL) ;
    else if (PyArray_IsScalar(sc, Generic))
      return_value = pyobj_to_%(ctype)s(sc, ptr);
    else
      return_value = pyobj_to_%(ctype)s(PyArray_ScalarFromObject(sc), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C %(ctype)s"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}
'''

template_from_npy_complex_scalar = '''
static PyObject* pyobj_from_%(ctype)s(%(ctype)s* value) {
  PyObject* obj = PyArrayScalar_New(%(Cls)s);
  if (obj==NULL) /* TODO: set exception */ return NULL;
  PyArrayScalar_ASSIGN(obj,%(Cls)s,*value);
  return obj;
}
'''

template_to_numpy_scalar = '''
static int pyobj_to_%(ctype_name)s(PyObject* obj, %(ctype)s* ptr) {
  int return_value = 0;
  if (PyArray_IsScalar(obj, %(Cls)s)) {
    *ptr = (%(ctype)s)obj;
    return_value = 1;
  } else if (PySequence_Check(obj)) {
    if (PySequence_Size(obj)==1)
      return_value = pyobj_to_%(ctype_name)s(PySequence_GetItem(obj,0), ptr);
  } else {
    PyObject* sc = Py%(Cls)sArrType_Type.tp_new(
      &Py%(Cls)sArrType_Type,Py_BuildValue("(O)",obj),NULL);
    if (sc==NULL) ;
    else if (PyArray_IsScalar(sc, Generic))
      return_value = pyobj_to_%(ctype_name)s(sc, ptr);
    else
      return_value = pyobj_to_%(ctype_name)s(PyArray_ScalarFromObject(sc), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C %(ctype)s"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}
'''

template_to_numpy_complex_scalar = '''
static int pyobj_to_%(ctype_name)s(PyObject* obj, %(ctype)s* ptr) {
  int return_value = 0;
  if (obj==NULL) ;
  else if (PyArray_IsScalar(obj,%(Cls)s)) {
    *ptr = (%(ctype)s)obj;
    return_value = 1;
  }
  else if (PySequence_Check(obj)) {
    if (PySequence_Size(obj)==1)
      return_value = pyobj_to_%(ctype_name)s(PySequence_GetItem(obj,0),ptr);
    else if (PySequence_Size(obj)==2) {
      %(fctype)s r = (%(fctype)s)PyArrayScalar_New(%(FCls)s);
      if (r!=NULL) {
        %(fctype)s i = (%(fctype)s)PyArrayScalar_New(%(FCls)s);
        if (i!=NULL) {
          return_value = pyobj_to_%(fctype_name)s(PySequence_GetItem(obj,0),&r)
                         && pyobj_to_%(fctype_name)s(PySequence_GetItem(obj,1),&i);
          if (return_value) {
            *ptr  = (%(ctype)s)PyArrayScalar_New(%(Cls)s);
            if (*ptr!=NULL) {
              PyArrayScalar_VAL(*ptr, %(Cls)s).real = PyArrayScalar_VAL(r, %(FCls)s);
              PyArrayScalar_VAL(*ptr, %(Cls)s).imag = PyArrayScalar_VAL(i, %(FCls)s);
            } else {
              return_value = 0;
            }
          }
        Py_DECREF(i);
        }
      Py_DECREF(r);
      }
    }
  } else {
    PyObject* sc = Py%(Cls)sArrType_Type.tp_new(
      &Py%(Cls)sArrType_Type,Py_BuildValue("(O)",obj),NULL);
    if (sc==NULL) ;
    else if (PyArray_IsScalar(sc, Generic))
      return_value = pyobj_to_%(ctype_name)s(sc, ptr);
    else
      return_value = pyobj_to_%(ctype_name)s(PyArray_ScalarFromObject(sc), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C %(ctype)s"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}
'''

for Cls_name, bits_list in sctypebits.items():
    for bits in bits_list:
        n = Cls_name.lower() + str(bits)
        Cls = Cls_name + str(bits)
        ctype = 'npy_' + n
        fn = 'float' + str(bits/2)
        FCls = 'Float' + str(bits/2)
        fctype = 'npy_' + fn
        m = dict(ctype=ctype, Cls=Cls, fctype=fctype)
        for k in ['to', 'from']:
            depends = []
            if Cls_name=='Complex':
                code = eval('template_'+k+'_npy_complex_scalar') % m
                if k=='to':
                    depends = ['pyobj_'+k+'_npy_'+fn]
            else:
                code = eval('template_'+k+'_npy_scalar') % m
            implementation['pyobj_'+k+'_'+ctype] = dict(
                code = code,
                depends = depends,
                )

        ctype = 'Py%sScalarObject*' % (Cls)
        ctype_name = 'numpy_' + n
        fn = 'float' + str(bits/2)
        FCls = 'Float' + str(bits/2)
        fctype = 'Py%sScalarObject*' % (FCls)
        fctype_name = 'numpy_' + fn

        m = dict(ctype=ctype, ctype_name=ctype_name,
                 Cls=Cls, fctype=fctype, FCls=FCls,
                 fctype_name=fctype_name)
        if Cls_name=='Complex':
            code = template_to_numpy_complex_scalar % m
            depends = ['pyobj_to_'+fctype_name]
        else:
            code = template_to_numpy_scalar % m
            depends = []
        implementation['pyobj_to_'+ctype_name] = dict(
            code = code,
            depends = depends,
            )

implementation['pyobj_to_npy_bool'] = dict(\
    code = '''\
static int pyobj_to_npy_bool(PyObject *obj, npy_bool* ptr) {
  int return_value = 0;
  if (obj==NULL) ;
  else if (PyArray_IsScalar(obj, Bool)) {
    *ptr = PyArrayScalar_VAL(obj, Bool);
    return_value = 1;
  } else {
    switch (PyObject_IsTrue(obj)) {
      case 0: *ptr = 0; return_value = 1; break;
      case -1: break;
      default: *ptr = 1; return_value = 1;
    }
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C npy_bool"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}''')

implementation['pyobj_from_npy_bool'] = dict(\
    code = '''\
static PyObject* pyobj_from_npy_bool(npy_bool* ptr) {
  if (*ptr) {
    PyArrayScalar_RETURN_TRUE;
  } else {
    PyArrayScalar_RETURN_FALSE;
  }
}''')

implementation['pyobj_to_numpy_bool'] = dict(\
    code = '''\
static int pyobj_to_numpy_bool(PyObject *obj, PyBoolScalarObject** ptr) {
  int return_value = 0;
  if (obj==NULL) ;
  else if (PyArray_IsScalar(obj, Bool)) {
    *ptr = (PyBoolScalarObject*)obj;
    return_value = 1;
  } else {
    switch (PyObject_IsTrue(obj)) {
      case 0: *ptr = (PyBoolScalarObject*)PyArrayScalar_False; return_value = 1; break;
      case -1: break;
      default: *ptr = (PyBoolScalarObject*)PyArrayScalar_True; return_value = 1;
    }
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C PyBoolScalarObject*"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}''')


implementation['pyobj_to_char_ptr'] = dict(\
    code = '''\
static int pyobj_to_char_ptr(PyObject *obj, char* * ptr) {
  int return_value = 0;
  if (obj==NULL) ;
  else if (PyString_Check(obj)) {
    Py_ssize_t l = 1+PyString_GET_SIZE(obj);
    *ptr = malloc(l*sizeof(char));
    return_value = !! strncpy(*ptr,PyString_AS_STRING(obj),l);
  } else {
    return_value = pyobj_to_char_ptr(PyObject_Str(obj), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C char*"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}''')

implementation['clean_pyobj_to_char_ptr'] = dict(\
    code = '''\
static void clean_pyobj_to_char_ptr(char* * ptr) {
  if ((*ptr) != NULL) {
    free(*ptr);
  }
}''')

implementation['pyobj_to_npy_string'] = dict(\
    code = '''\
static int pyobj_to_npy_string(PyObject *obj, npy_string *ptr) {
  int return_value = 0;
  if (obj==NULL) ;
  else if (PyArray_IsScalar(obj, String)) {
    Py_ssize_t l = 1+PyString_GET_SIZE(obj);
    ptr->data = malloc(l*sizeof(char));
    ptr->size = l-1;
    return_value = !! strncpy(ptr->data,PyString_AS_STRING(obj),l);
  } else {
    PyObject* sc = PyStringArrType_Type.tp_new(
      &PyStringArrType_Type,Py_BuildValue("(O)",obj),NULL);
    if (sc==NULL) ;
    else if (PyArray_IsScalar(sc, Generic))
      return_value = pyobj_to_npy_string(sc, ptr);
    else
      return_value = pyobj_to_npy_string(PyArray_ScalarFromObject(sc), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C char*"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}''')

implementation['clean_pyobj_to_npy_string'] = dict(\
    code = '''\
static void clean_pyobj_to_npy_string(npy_string *ptr) {
  if ((ptr->data) != NULL) {
    free(ptr->data);
    ptr->size = 0;
  }
}
''')

implementation['pyobj_from_npy_string'] = dict(\
    code = '''\
static PyObject* pyobj_from_npy_string(npy_string *ptr) {
  PyObject* sc = PyStringArrType_Type.tp_new(
   &PyStringArrType_Type,Py_BuildValue("(O)",
     PyString_FromStringAndSize(ptr->data, ptr->size)),NULL);
  return sc;
}''')


implementation['pyobj_to_numpy_string'] = dict(\
    code = '''\
static int pyobj_to_numpy_string(PyObject *obj, PyStringScalarObject** ptr) {
  int return_value = 0;
  if (PyArray_IsScalar(obj, String)) {
    *ptr = (PyStringScalarObject*)obj;
    return_value = 1;
  } else {
    PyObject* sc = PyStringArrType_Type.tp_new(
      &PyStringArrType_Type,Py_BuildValue("(O)",obj),NULL);
    if (sc==NULL) ;
    else if (PyArray_IsScalar(sc, Generic))
      return_value = pyobj_to_numpy_string(sc, ptr);
    else
      return_value = pyobj_to_numpy_string(PyArray_ScalarFromObject(sc), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C PyStringScalarObject*"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}
''')

implementation['pyobj_to_numpy_unicode'] = dict(\
    code = '''\
static int pyobj_to_numpy_unicode(PyObject *obj, PyUnicodeScalarObject** ptr) {
  int return_value = 0;
  if (PyArray_IsScalar(obj, Unicode)) {
    *ptr = (PyUnicodeScalarObject*)obj;
    return_value = 1;
  } else {
    PyObject* sc = PyUnicodeArrType_Type.tp_new(
      &PyUnicodeArrType_Type,Py_BuildValue("(O)",obj),NULL);
    if (sc==NULL) ;
    else if (PyArray_IsScalar(sc, Generic))
      return_value = pyobj_to_numpy_unicode(sc, ptr);
    else
      return_value = pyobj_to_numpy_unicode(PyArray_ScalarFromObject(sc), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C PyUnicodeScalarObject*"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}
''')

implementation['pyobj_to_npy_unicode'] = dict(\
    code = '''\
static int pyobj_to_npy_unicode(PyObject *obj, npy_unicode * ptr) {
  int return_value = 0;
  if (obj==NULL) ;
  else if (PyArray_IsScalar(obj, Unicode)) {
    Py_ssize_t l = PyUnicode_GET_DATA_SIZE(obj);
    ptr->data = malloc(l);
    ptr->size = PyUnicode_GET_SIZE(obj);
    return_value = !! memcpy(ptr->data,PyUnicode_AS_UNICODE(obj),l);
  } else {
    PyObject* sc = PyUnicodeArrType_Type.tp_new(
      &PyUnicodeArrType_Type,Py_BuildValue("(O)",obj),NULL);    

    if (sc==NULL) ;
    else if (PyArray_IsScalar(sc, Generic))
      return_value = pyobj_to_npy_unicode(sc, ptr);
    else
      return_value = pyobj_to_npy_unicode(PyArray_ScalarFromObject(sc), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C Py_UNICODE*"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}''')

implementation['clean_pyobj_to_npy_unicode'] = dict(\
    code = '''\
static void clean_pyobj_to_npy_unicode(npy_unicode * ptr) {
  if ((ptr->data) != NULL) {
    free(ptr->data);
    ptr->size = 0;
  }
}
''')

implementation['pyobj_from_npy_unicode'] = dict(\
    code = '''\
static PyObject* pyobj_from_npy_unicode(npy_unicode *ptr) {
  PyObject* sc = PyUnicodeArrType_Type.tp_new(
   &PyUnicodeArrType_Type,Py_BuildValue("(O)",PyUnicode_FromUnicode(ptr->data, ptr->size)),NULL);
  return sc;
}''')

implementation['pyobj_to_numpy_void'] = dict(\
    code = '''\
static int pyobj_to_numpy_void(PyObject *obj, PyVoidScalarObject** ptr) {
  int return_value = 0;
  if (PyArray_IsScalar(obj, Void)) {
    *ptr = (PyVoidScalarObject*)obj;
    return_value = 1;
  } else {
    PyObject* sc = PyVoidArrType_Type.tp_new(
      &PyVoidArrType_Type,Py_BuildValue("(O)",obj),NULL);
    if (sc==NULL) ;
    else if (PyArray_IsScalar(sc, Generic))
      return_value = pyobj_to_numpy_void(sc, ptr);
    else
      return_value = pyobj_to_numpy_void(PyArray_ScalarFromObject(sc), ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C PyVoidScalarObject*"));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}
''')

implementation['pyobj_to_numpy_array_int64'] = dict(\
    code = '''\
static int pyobj_to_numpy_array_int64(PyObject *obj, PyArrayObject** ptr) {
  int return_value = 0;
  if (PyArray_Check(obj)) {
    if (PyArray_TYPE(obj)==PyArray_INT64) {
      *ptr = (PyArrayObject*)obj;
      return_value = 1;
    }
  } else {
    PyObject* arr = PyArray_FROM_OT(obj, PyArray_INT64);
    return_value = pyobj_to_numpy_array_int64(arr, ptr);
  }
  if (!return_value && !PyErr_Occurred()) {
    PyObject* r = PyString_FromString("Failed to convert ");
    PyString_ConcatAndDel(&r, PyObject_Repr(PyObject_Type(obj)));
    PyString_ConcatAndDel(&r, PyString_FromString(" to C "));
    PyErr_SetObject(PyExc_TypeError,r);
  }
  return return_value;
}
'''
    )
