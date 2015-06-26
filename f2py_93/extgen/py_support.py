"""
Defines classes

  PySource - represents pure Python module
  PyCModule - represents Python extension module
  PyCFunction - represents Python extension function
    PyCArgument - represents argument to Python extension function
    PyCReturn - represents return value of Python extension function
      PyCTypeSpec - represents scalar type of arguments and return values
      PyCArrayTypeSpec - represents numpy array type of arguments and return values
"""


__all__ = ['PySource', 'PyCFunction', 'PyCModule', 'PyCTypeSpec', 'PyCArgument',
           'PyCReturn', 'PyCArrayTypeSpec', 'PyCVariable',
           'Variable','Bound','Shape','Intent']

import os
import sys
import time
import tempfile

from base import Component
from utils import *
from c_support import *

import scalar_rules
import array_rules

class PySource(FileSource):

    template_py_header = '''\
#!/usr/bin/env python
# This file %(path)r is generated using ExtGen tool
# from NumPy version %(numpy_version)s.
# ExtGen is developed by Pearu Peterson <pearu.peterson@gmail.com>.
# For more information see http://www.scipy.org/ExtGen/ .'''

    container_options = dict(
        Content = dict(default='',
                       prefix = template_py_header + '\n',
                       suffix = '\n',
                       use_indent=True)
        )


class PyCModule(CSource):

    template = CSource.template_c_header + '''
#ifdef __cplusplus
extern \"C\" {
#endif
#include "Python.h"
%(CHeader)s
%(CTypeDef)s
%(CProto)s
%(CDefinition)s

static PyObject* extgen_module;

%(CAPIDefinition)s
%(CDeclaration)s
%(PyCModuleCDeclaration)s
%(CMainProgram)s
#ifdef __cplusplus
}
#endif
'''

    container_options = CSource.container_options.copy()
    container_options.update(CAPIDefinition=container_options['CDefinition'],
                             PyCModuleCDeclaration=dict(default='<KILLLINE>',
                                                    ignore_empty_content=True),
                             )

    component_container_map = dict(
        PyCModuleInitFunction = 'CMainProgram',
        PyCModuleCDeclaration = 'PyCModuleCDeclaration',
        PyCFunction = 'CAPIDefinition',
        )

    def initialize(self, pyname, *components, **options):
        self.pyname = pyname
        self.title = options.pop('title', None)
        self.description = options.pop('description', None)

        self = CSource.initialize(self, '%smodule.c' % (pyname), **options)
        self.need_numpy_support = False
        self.need_numeric_support = False

        self.cdecl = PyCModuleCDeclaration(pyname)
        self += self.cdecl

        self.main = PyCModuleInitFunction(pyname)
        self += self.main
        map(self.add, components)
        return self

    def update_SetupPy(self):
        parent = self.parent
        parent.setup_py += '    config.add_extension(%r,'\
                                'sources = ["%s"])' % (self.pyname, self.path)
        parent.init_py += 'import %s' % (self.pyname)

    def finalize(self):
        assert not(self.need_numpy_support and self.need_numeric_support)
        if self.need_numpy_support:
            self.add(CCode('''
#define PY_ARRAY_UNIQUE_SYMBOL PyArray_API
#include "numpy/arrayobject.h"
#include "numpy/arrayscalars.h"
'''), 'CHeader')
            self.main.add(CCode('''
import_array();
if (PyErr_Occurred()) {
  PyErr_SetString(PyExc_ImportError, "failed to load NumPy array module.");
  goto capi_error;
}
'''),'CBody')
        if self.need_numeric_support:
            self.add(CCode('''
#define PY_ARRAY_UNIQUE_SYMBOL PyArray_API
#include "Numeric/arrayobject.h"
'''), 'CHeader')
            self.main.add(CCode('''
import_array();
if (PyErr_Occurred()) {
  PyErr_SetString(PyExc_ImportError, "failed to load Numeric module.");
  goto capi_error;
}
'''),'CBody')
        CSource.finalize(self)

    def build(self, build_dir=None, clean_at_exit=None):
        """ build(build_dir=None, clean_at_exit=None)

        A convenience function to build, import, an return
        an extension module object.
        """
        if build_dir is None:
            packagename = 'extgen_' + str(hex(int(time.time()*10000000)))[2:]
            build_dir = os.path.join(tempfile.gettempdir(), packagename)
            clean_at_exit = True

        setup = Component.SetupPy(build_dir)
        setup += self
        s,o = setup.execute('build_ext','--inplace')
        if s:
            #self.info('return status=%s' % (s))
            #self.info(o)
            raise RuntimeError('failed to build extension module %r,'\
                               ' the build is located in %r directory'\
                               % (self.pyname, build_dir))

        if clean_at_exit:
            import atexit
            import shutil
            atexit.register(lambda d=build_dir: shutil.rmtree(d))
            self.debug('directory %r will be removed at exit from python.' % (build_dir))

        sys.path.insert(0, os.path.dirname(build_dir))
        packagename = os.path.basename(build_dir)
        try:
            p = __import__(packagename)
            m = getattr(p, self.pyname)
        except:
            del sys.path[0]
            raise
        else:
            del sys.path[0]
        return m


class PyCModuleCDeclaration(Component):

    template = '''\
static
PyMethodDef extgen_module_methods[] = {
  %(PyMethodDef)s
  {NULL,NULL,0,NULL}
};
static
char extgen_module_doc[] =
"This module %(pyname)r is generated with ExtGen from NumPy version %(numpy_version)s."
%(Title)s
%(Description)s
%(FunctionSignature)s
;'''
    container_options = dict(
        PyMethodDef = dict(suffix=',', skip_suffix_when_empty=True,separator=',\n',
                           default='<KILLLINE>', use_indent=True, ignore_empty_content=True),
        FunctionSignature = dict(prefix='"\\n\\n:Functions:\\n"\n"  ', skip_prefix_when_empty=True, use_indent=True,
                                 ignore_empty_content=True, default='<KILLLINE>',
                                 separator = '"\n"  ', suffix='"', skip_suffix_when_empty=True,
                                 ),
        Title = dict(default='<KILLLINE>',prefix='"\\n\\n',suffix='"',separator='\\n"\n"',
                         skip_prefix_when_empty=True, skip_suffix_when_empty=True,
                         use_firstline_indent=True, replace_map={'\n':'\\n'}),
        Description = dict(default='<KILLLINE>',prefix='"\\n\\n"\n"',
                         suffix='"',separator='\\n"\n"',
                         skip_prefix_when_empty=True, skip_suffix_when_empty=True,
                         use_firstline_indent=True, replace_map={'\n':'\\n'}),
        )

    default_component_class_name = 'Line'

    def initialize(self, pyname):
        self.pyname = pyname
        return self


    def update_PyCModule(self):
        parent = self.parent
        if parent.title:
            self.add(parent.title, 'Title')
        if parent.description:
            self.add(parent.description, 'Description')


class PyCModuleInitFunction(CFunction):

    _s = """
    >>> f = PyCModuleInitFunction('test_PyCModuleInitFunction')
    >>> print f.generate()
    PyMODINIT_FUNC
    inittest_PyCModuleInitFunction(void) {
      PyObject* extgen_module_dict = NULL;
      PyObject* extgen_str_obj = NULL;
      extgen_module = Py_InitModule(\"test_PyCModuleInitFunction\", extgen_module_methods);
      if ((extgen_module_dict = PyModule_GetDict(extgen_module))==NULL) goto capi_error;
      if ((extgen_str_obj = PyString_FromString(extgen_module_doc))==NULL) goto capi_error;
      PyDict_SetItemString(extgen_module_dict, \"__doc__\", extgen_str_obj);
      Py_DECREF(extgen_str_obj);
      if ((extgen_str_obj = PyString_FromString(\"restructuredtext\"))==NULL) goto capi_error;
      PyDict_SetItemString(extgen_module_dict, \"__docformat__\", extgen_str_obj);
      Py_DECREF(extgen_str_obj);
      return;
    capi_error:
      if (!PyErr_Occurred()) {
        PyErr_SetString(PyExc_RuntimeError, \"failed to initialize 'test_PyCModuleInitFunction' module.\");
      }
      return;
    }
    """

    template = '''\
%(CSpecifier)s
%(CTypeSpec)s
%(name)s(void) {
  PyObject* extgen_module_dict = NULL;
  PyObject* extgen_str_obj = NULL;
  %(CDeclaration)s
  extgen_module = Py_InitModule3("%(pyname)s", extgen_module_methods, extgen_module_doc);
  if ((extgen_module_dict = PyModule_GetDict(extgen_module))==NULL) goto capi_error;
  if ((extgen_str_obj = PyString_FromString("restructuredtext"))==NULL) goto capi_error;
  PyDict_SetItemString(extgen_module_dict, "__docformat__", extgen_str_obj);
  Py_DECREF(extgen_str_obj);
  //builtins = PyImport_ImportModuleLevel("__builtin__", NULL, NULL, NULL, 0);
  %(CBody)s
  return;
capi_error:
  if (!PyErr_Occurred()) {
    PyErr_SetString(PyExc_RuntimeError, "failed to initialize %(pyname)r module.");
  }
  return;
}'''

    def initialize(self, pyname, *components, **options):
        self.pyname = pyname
        self.title = options.pop('title', None)
        self.description = options.pop('description', None)
        self = CFunction.initialize(self, 'init'+pyname, 'PyMODINIT_FUNC', *components, **options)
        return self

#helper classes for PyCFunction
class KWListBase(Word): parent_container_options = dict(separator=', ', suffix=', ', skip_suffix_when_empty=True)
class ReqKWList(KWListBase): pass
class OptKWList(KWListBase): pass
class ExtKWList(KWListBase): pass
class ArgBase(Word): parent_container_options = dict(separator=', ')
class ReqArg(ArgBase): pass
class OptArg(ArgBase): pass
class ExtArg(ArgBase): pass
class RetArg(ArgBase):
    parent_container_options = dict(separator=', ', prefix='(', suffix=')', default = 'None',
                                    skip_prefix_when_empty=True, skip_suffix_when_empty=True,
                                    skip_prefix_suffix_when_single=True)
class OptExtArg(ArgBase):
    parent_container_options = dict(separator=', ', prefix=' [, ', skip_prefix_when_empty=True,
                                    suffix=']', skip_suffix_when_empty=True)
class ArgDocBase(Word):
    parent_container_options = dict(default='<KILLLINE>', prefix='"\\n\\nArguments:\\n"\n"  ',
                                    separator='\\n"\n"  ', suffix='"',
                                    skip_prefix_when_empty=True, skip_suffix_when_empty=True,
                                    use_firstline_indent=True, replace_map={'\n':'\\n'})
class ReqArgDoc(ArgDocBase):
    parent_container_options = ArgDocBase.parent_container_options.copy()
    parent_container_options.update(prefix='"\\n\\n:Parameters:\\n"\n"  ')
class OptArgDoc(ArgDocBase):
    parent_container_options = ArgDocBase.parent_container_options.copy()
    parent_container_options.update(prefix='"\\n\\n:Optional parameters:\\n"\n"  ')
class ExtArgDoc(ArgDocBase):
    parent_container_options = ArgDocBase.parent_container_options.copy()
    parent_container_options.update(prefix='"\\n\\n:Extra parameters:\\n"\n"  ')
class RetArgDoc(ArgDocBase):
    parent_container_options = ArgDocBase.parent_container_options.copy()
    parent_container_options.update(prefix='"\\n\\n:Returns:\\n"\n"  ',
                                    default='"\\n\\n:Returns:\\n  None"')
class ArgFmtBase(Word): parent_container_options = dict(separator='')
class ReqArgFmt(ArgFmtBase): pass
class OptArgFmt(ArgFmtBase): pass
class ExtArgFmt(ArgFmtBase): pass
class RetArgFmt(ArgFmtBase): pass
class OptExtArgFmt(ArgFmtBase):
    parent_container_options = dict(separator='', prefix='|', skip_prefix_when_empty=True)
class ArgObjBase(Word): parent_container_options = dict(separator=', ', prefix=', ', skip_prefix_when_empty=True)
class ReqArgObj(ArgObjBase): pass
class OptArgObj(ArgObjBase): pass
class ExtArgObj(ArgObjBase): pass
class RetArgObj(ArgObjBase): pass

class FunctionSignature(Component):
    template = '%(name)s(%(ReqArg)s%(OptExtArg)s) -> %(RetArg)s'
    parent_container_options = dict()
    container_options = dict(
        ReqArg = ReqArg.parent_container_options,
        OptArg = OptArg.parent_container_options,
        ExtArg = ExtArg.parent_container_options,
        RetArg = RetArg.parent_container_options,
        OptExtArg = OptExtArg.parent_container_options,
        )
    def initialize(self, name, *components, **options):
        self.name = name
        map(self.add, components)
        return self
    def update_containers(self):
        self.container_OptExtArg += self.container_OptArg + self.container_ExtArg

class PyCFunction(CFunction):

    template = '''\
static
char %(name)s_doc[] =
"  %(FunctionSignature)s"
%(Title)s
%(Description)s
%(ReqArgDoc)s
%(RetArgDoc)s
%(OptArgDoc)s
%(ExtArgDoc)s
;
static
PyObject*
%(name)s(PyObject *pyc_self, PyObject *pyc_args, PyObject *pyc_keywds) {
  PyObject * volatile pyc_buildvalue = NULL;
  volatile int capi_success = 1;
  PyObject * capi_globals = PyEval_GetGlobals();
  PyObject * capi_locals = PyEval_GetLocals();
  %(CDeclaration)s
  static char *capi_kwlist[] = {%(ReqKWList)s%(OptKWList)s%(ExtKWList)sNULL};
  if (capi_globals == NULL) {
    PyErr_SetString(PyExc_TypeError, 
      "failed to set globals in extension function %(name)s");
    return NULL;
  }
  if (capi_locals == NULL) {
    PyErr_SetString(PyExc_TypeError, 
      "failed to set locals in extension function %(name)s");
    return NULL;
  }
  %(InitFunc)s
  if (PyArg_ParseTupleAndKeywords(pyc_args, pyc_keywds,"%(ReqArgFmt)s%(OptExtArgFmt)s",
                                  capi_kwlist%(ReqArgObj)s%(OptArgObj)s%(ExtArgObj)s)) {
    %(PyArgParseBody)s
  }
  %(CleanInitFunc)s
  //Py_DECREF(capi_locals);
  return pyc_buildvalue;
}'''

    container_options = CFunction.container_options.copy()

    container_options.update(\

        TMP = dict(),

        ReqArg = ReqArg.parent_container_options,
        OptArg = OptArg.parent_container_options,
        ExtArg = ExtArg.parent_container_options,
        RetArg = RetArg.parent_container_options,

        FunctionSignature = FunctionSignature.parent_container_options,

        OptExtArg = OptExtArg.parent_container_options,

        Title = dict(default='<KILLLINE>',prefix='"\\n\\n',suffix='"',separator='\\n"\n"',
                     skip_prefix_when_empty=True, skip_suffix_when_empty=True,
                     use_firstline_indent=True, replace_map={'\n':'\\n'}),
        Description = dict(default='<KILLLINE>',prefix='"\\n\\n"\n"',
                           suffix='"',separator='\\n"\n"',
                           skip_prefix_when_empty=True, skip_suffix_when_empty=True,
                           use_firstline_indent=True, replace_map={'\n':'\\n'}),

        ReqArgDoc = ReqArgDoc.parent_container_options,
        OptArgDoc = OptArgDoc.parent_container_options,
        ExtArgDoc = ExtArgDoc.parent_container_options,
        RetArgDoc = RetArgDoc.parent_container_options,

        ReqKWList = ReqKWList.parent_container_options,
        OptKWList = OptKWList.parent_container_options,
        ExtKWList = ExtKWList.parent_container_options,

        ReqArgFmt = ReqArgFmt.parent_container_options,
        OptArgFmt = OptArgFmt.parent_container_options,
        ExtArgFmt = ExtArgFmt.parent_container_options,
        OptExtArgFmt = OptExtArgFmt.parent_container_options,
        RetArgFmt = ExtArgFmt.parent_container_options,

        ReqArgObj = ReqArgObj.parent_container_options,
        OptArgObj = OptArgObj.parent_container_options,
        ExtArgObj = ExtArgObj.parent_container_options,
        RetArgObj = RetArgObj.parent_container_options,

        PyArgParseBody = CCode.parent_container_options,

        FromPyObj = CCode.parent_container_options,
        PyObjFrom = CCode.parent_container_options,

        CleanPyObjFrom = dict(default='<KILLLINE>', reverse=True, use_indent=True, ignore_empty_content=True),
        CleanCBody = dict(default='<KILLLINE>', reverse=True, use_indent=True, ignore_empty_content=True),
        CleanFromPyObj = dict(default='<KILLLINE>', reverse=True, use_indent=True, ignore_empty_content=True),

        CBody = CCode.parent_container_options,
        InitFunc = CCode.parent_container_options,
        CleanInitFunc = dict(default='<KILLLINE>', reverse=True, use_indent=True, ignore_empty_content=True),

        )

    default_component_class_name = 'CCode'

    component_container_map = CFunction.component_container_map.copy()
    component_container_map.update(
        PyCArgument = 'TMP',
        PyCVariable = 'CDeclaration',
        CCode = 'CBody',
        Variable = 'TMP',
        )

    def initialize(self, pyname, *components, **options):
        self.pyname = pyname
        self.title = options.pop('title', None)
        self.description = options.pop('description', None)
        self = CFunction.initialize(self, 'pyc_function_'+pyname, 'PyObject*', **options)
        self.signature = FunctionSignature(pyname)
        self += self.signature
        if self.title:
            self.add(self.title, 'Title')
        if self.description:
            self.add(self.description, 'Description')
        map(self.add, components)
        return self

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, ', '.join(map(repr,[self.pyname]+[c for (c,l) in self.components])))

    def update_PyCModule(self):
        parent = self.parent
        t = '  {"%(pyname)s", (PyCFunction)%(name)s, METH_VARARGS | METH_KEYWORDS, %(name)s_doc}' % (self.__dict__)
        parent.cdecl.add(t,'PyMethodDef')
        parent.cdecl.add(self.signature,'FunctionSignature')

    def update_containers(self):

        for (var, label) in self.components:
            if not isinstance(var, Variable):
                continue
            n = var.get_implementation_name()
            mth = getattr(self, n, None)
            if mth is None:
                self.warning('Method %s.%s not implemented' %(self.__class__.__name__, n))
                continue
            mth(var)

        for (var, label) in self.sorted_components:
            if not isinstance(var, Variable):
                continue
            n = var.get_implementation_name() + '_sorted'
            mth = getattr(self, n, None)
            if mth is None:
                self.warning('Method %s.%s not implemented' %(self.__class__.__name__, n))
                continue
            mth(var)

        self.container_OptExtArg += self.container_OptArg + self.container_ExtArg
        self.container_OptExtArgFmt += self.container_OptArgFmt + self.container_ExtArgFmt

        self.container_PyArgParseBody += self.container_FromPyObj
        self.container_PyArgParseBody += self.container_CBody
        self.container_PyArgParseBody += 'if (!!(capi_success = !PyErr_Occurred())) {'
        self.container_PyArgParseBody += '  ' + self.container_PyObjFrom
        self.container_PyArgParseBody += '  pyc_buildvalue = Py_BuildValue("%(RetArgFmt)s"%(RetArgObj)s);'
        self.container_PyArgParseBody += '  ' + self.container_CleanPyObjFrom.reverse_copy()
        self.container_PyArgParseBody += '}'
        self.container_PyArgParseBody += self.container_CleanCBody.reverse_copy()
        self.container_PyArgParseBody += self.container_CleanFromPyObj.reverse_copy()

    def add_implementation(self, resource):
        if resource is None: return
        import capi
        if not capi.implementation.has_key(resource):
            raise NotImplementedError('capi.implementation[%r]' % (resource))
        d = capi.implementation[resource]
        for r in d.get('depends',[]):
            self.add_implementation(r)
        self.container_CDefinition.add(d['code'], resource)

    def add_typedef(self, resource):
        if resource is None: return
        import capi
        if not capi.typedefs.has_key(resource):
            raise NotImplementedError('capi.typedefs[%r]' % (resource))
        d = capi.typedefs[resource]
        for r in d.get('depends',[]):
            self.add_typedef(r)
        self.container_CTypeDef.add(d['code'], resource)

    def add_macro(self, resource):
        if resource is None: return
        import capi
        if not capi.macros.has_key(resource):
            raise NotImplementedError('capi.macros[%r]' % (resource))
        d = capi.macros[resource]
        for r in d.get('depends',[]):
            self.add_macro(r)
        self.container_CHeader.add(d['code'], resource)

    ###################

    def _scalar_return(self, var):
        name, typeinfo = var.name, var.type.scalar_typeinfo
        self.container_RetArg += name
        self.container_RetArgFmt += typeinfo['return_format']
        if typeinfo['return_format']=='N':
            self.container_RetArgObj += '%s' % (name)
        elif typeinfo['return_format']=='O&':
            self.container_RetArgObj += '&%s, &%s' % (typeinfo['return_converter'], name)
            self.add_implementation(typeinfo['return_converter'])
        elif typeinfo['return_format']=='D':
            self.container_RetArgObj += '&%s' % (name)
        else:
            self.container_RetArgObj += '%s' % (name)

    def _scalar_required(self, var):
        name, typeinfo = var.name, var.type.scalar_typeinfo
        self.container_ReqArg += name
        self.container_ReqKWList += '"%s"' % (name)
        self.container_ReqArgFmt += typeinfo['argument_format']
        self.add_typedef(typeinfo.get('typedef'))
        if typeinfo['argument_format']=='O!':
            self.container_ReqArgObj += '&%s, &%s' % (typeinfo['pyctype'], name)
        elif typeinfo['argument_format']=='O&':
            self.add_implementation(typeinfo['argument_converter'])
            self.container_ReqArgObj += '&%s, &%s' % (typeinfo['argument_converter'], name)
            clean_converter = typeinfo.get('clean_argument_converter')
            if clean_converter is not None:
                self.add_implementation(clean_converter)
                self.container_CleanFromPyObj += '%s(&%s);' % (clean_converter, name)
        else:
            self.container_ReqArgObj += '&%s' % (name)

    def _scalar_optional(self, var):
        name, typeinfo = var.name, var.type.scalar_typeinfo
        self.container_OptArg += name
        self.container_OptKWList += '"%s"' % (name)
        self.container_OptArgFmt += typeinfo['argument_format']
        self.add_typedef(typeinfo.get('typedef'))
        if typeinfo['argument_format']=='O!':
            self.container_OptArgObj += '&%s, &%s' % (typeinfo['pyctype'], name)
        elif typeinfo['argument_format']=='O&':
            self.add_implementation(typeinfo['argument_converter'])
            self.container_OptArgObj += '&%s, &%s' % (typeinfo['argument_converter'], name)
            clean_converter = typeinfo.get('clean_argument_converter')
            if clean_converter is not None:
                self.add_implementation(clean_converter)
                self.container_CleanFromPyObj += '%s(&%s);' % (clean_converter, name)
        else:
            self.container_OptArgObj += '&%s' % (name)

    def scalar_local_create_c(self, var):
        name, typeinfo = var.name, var.type.scalar_typeinfo
        if var.initialize_in_CDeclaration:
            init = ' = %s' % (var.init)
        else:
            init =  typeinfo.get('init', '')
        d = '%s %s%s' % (typeinfo['ctype'], name, init)
        self.container_CDeclaration += d

    def scalar_local_create_c_sorted(self, var):
        name, typeinfo = var.name, var.type.scalar_typeinfo
        if var.initialize_in_FromPyObj:
            ctype = var.typeinfo.get('ctype', None)
            pyctype = var.typeinfo.get('pyctype', None)
            if ctype is not None and pyctype is not None:
                if isinstance(var.init, str):
                    init = var.init
                else:
                    init = repr(var.init)
                self.container_FromPyObj += '%s = PyRun_String("%s",\n  Py_eval_input, capi_globals, capi_locals);' % (name, init)
                if typeinfo.get('refcounted', False):
                    self.container_FromPyObj += 'if IS_VALID_INIT(&%(pyctype)s, %(name)s, "%(name)s") {' % (
                        dict(name=name, pyctype=pyctype)
                        )
                    self.container_CleanFromPyObj += '} else SET_EXCEPTION_INIT(&%(pyctype)s, %(name)s, "%(name)s", "%(init)s");' %\
                                                    (dict(pyctype=pyctype, name=name, init = var.init))
                    self.add_macro('IS_VALID_INIT')
                    self.add_macro('SET_EXCEPTION_INIT')
                else:
                    raise NotImplementedError
            else:
                raise NotImplementedError

    def scalar_return_create_c(self, var):
        self.scalar_local_create_c(var)
        self._scalar_return(var)

    def scalar_return_create_c_sorted(self, var):
        pass

    def scalar_argument_copy_c(self, var):
        self.scalar_local_create_c(var)
        self._scalar_required(var)

    def scalar_argument_copy_c_sorted(self, var):
        pass

    def scalar_optional_copy_c(self, var):
        self.scalar_local_create_c(var)
        self._scalar_optional(var)

    def scalar_optional_copy_c_sorted(self, var):
        pass

    def scalar_argument_return_copy_c(self, var):
        self.scalar_argument_copy_c(var)
        self._scalar_return(var)

    def scalar_argument_return_copy_c_sorted(self, var):
        name, typeinfo = var.name, var.type.scalar_typeinfo
        if typeinfo['return_format']=='N':
            self.container_FromPyObj += '''\
/* make `%(name)s_original` a new reference */
%(name)s_original = %(name)s;
Py_INCREF((PyObject*)%(name)s_original);
''' % (dict(name=name))
            self.container_CleanFromPyObj += '''\
if (!(%(name)s_original==%(name)s)) {
  /* a new `%(name)s` was created, undoing `%(name)s_original` new reference.
    `%(name)s` must be a new reference or expect a core dump.                 */
  Py_DECREF((PyObject*)%(name)s_original);
}
''' % (dict(name=name))

    def scalar_optional_return_copy_c(self, var):
        self.scalar_optional_copy_c(var)
        self._scalar_return(var)

    def scalar_optional_return_copy_c_sorted(self, var):
        pass

    def array_local_create_c(self, var):
        name, typeinfo, shape = var.name, var.type.array_typeinfo, var.shape
        d = '%s %s%s' % (typeinfo['ctype'], name, typeinfo.get('init',''))
        self.container_CDeclaration += d
        self.container_CDeclaration += 'npy_intp %s_dims[] = {%s}' \
                                       % (name, ', '.join([str(b.init_value) for b in shape.bounds]))

    def array_local_create_c_sorted(self, var):
        name, typeinfo, shape = var.name, var.type.array_typeinfo, var.shape
        for i in range(shape.nd):
            b = shape.bounds[i]
            if b.is_variable:
                self.container_FromPyObj += '%s_dims[%s] = %s;' % (name, i, b.value)
        self.container_FromPyObj += 'capi_success = (%s = (PyArrayObject*)PyArray_SimpleNew(%s, %s_dims, %s))!=NULL;' \
                                   % (name, shape.nd, name, typeinfo['typenum'])
        self.container_FromPyObj += 'if (capi_success) {'
        self.container_CleanFromPyObj += '  Py_DECREF(%s);\n} /* %s = ... */' % (name, name)

    def array_argument_copy_c(self, var):
        name, typeinfo, shape = var.name, var.type.array_typeinfo, var.shape
        self.array_local_create_c(var)
        self.container_ReqArg += name
        self.container_ReqKWList += '"%s"' % (name)
        self.container_ReqArgFmt += typeinfo['argument_format']
        self.add_typedef(typeinfo.get('typedef'))
        if typeinfo['argument_format']=='O!':
            self.container_ReqArgObj += '&%s, &%s' % (typeinfo['pyctype'], name)
        elif typeinfo['argument_format']=='O&':
            self.add_implementation(typeinfo['argument_converter'])
            self.container_ReqArgObj += '&%s, &%s' % (typeinfo['argument_converter'], name)
            clean_converter = typeinfo.get('clean_argument_converter')
            if clean_converter is not None:
                self.add_implementation(clean_converter)
                self.container_CleanFromPyObj += '%s(&%s);' % (clean_converter, name)
        else:
            self.container_ReqArgObj += '&%s' % (name)
            
    def array_argument_return_copy_c(self, var):
        name, typeinfo, shape = var.name, var.type.array_typeinfo, var.shape
        self.array_argument_copy_c(var)
        self.container_RetArg += name
        self.container_RetArgFmt += typeinfo['return_format']
        if typeinfo['return_format']=='N':
            d = '%s %s_original%s' % (typeinfo['ctype'], name, typeinfo.get('init',''))
            self.container_CDeclaration += d
            self.container_RetArgObj += '%s' % (name)
        elif typeinfo['return_format']=='O&':
            self.container_RetArgObj += '&%s, &%s' % (typeinfo['return_converter'], name)
            self.add_implementation(typeinfo['return_converter'])
        else:
            self.container_RetArgObj += '%s' % (name)

    def array_argument_return_copy_c_sorted(self, var):
        name, typeinfo = var.name, var.type.scalar_typeinfo
        if typeinfo['return_format']=='N':
            self.container_FromPyObj += '''\
/* make `%(name)s_original` a new reference */
%(name)s_original = %(name)s;
Py_INCREF((PyObject*)%(name)s_original);
''' % (dict(name=name))
            self.container_CleanFromPyObj += '''\
if (!(%(name)s_original==%(name)s)) {
  /* a new `%(name)s` was created, undoing `%(name)s_original` new reference.
    `%(name)s` must be a new reference or expect a core dump.                 */
  Py_DECREF((PyObject*)%(name)s_original);
}
''' % (dict(name=name))

    def build(self):
        modulename = 'extgen_' + str(hex(int(time.time()*10000000)))[2:]
        m = PyCModule(modulename, self).build()
        return getattr(m, self.pyname)
    
class Bound(Component):

    template = '%(init_value)s'

    def initialize(self, value=-1):
        if isinstance(value, self.__class__):
            return value
        if isinstance(value, str):
            try:
                value = eval(value, {}, {})
            except NameError:
                pass
        assert isinstance(value, (str,int)),`value`
        self.is_defined = isinstance(value, int) and value >= 0
        self.is_undefined = isinstance(value, int) and value < 0
        self.is_variable = isinstance(value, str)
        if self.is_variable:
            self.init_value = -2
        else:
            self.init_value = value
        self.value = value
        return self

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.value)

class Shape(Component):

    """
    >>> d = Shape('2+3', -1, 'n')
    >>> d
    Shape(Bound(5), Bound(-1), Bound('n'))
    """
    
    template = ''
    container_options = dict(Bound = dict())
    default_component_class_name = 'Bound'

    def initialize(self, *bounds):
        map(self.add, bounds)
        return self

    @property
    def bounds(self):
        return [c for c,l in self.components if isinstance(c, Bound)]

    @property
    def nd(self):
        return len(self.bounds)

class Type(Component):

    template = '%(name)s'

    def initialize(self, typeobj):
        if isinstance(typeobj, self.__class__):
            return typeobj
        self.name = typeobj2key(typeobj)
        return self

    def __repr__(self):
        return '%s(%r)' % (self.__class__.__name__, self.name)

    @property
    def scalar_typeinfo(self):
        default_typeinfo = dict(ctype=self.name)
        if self.name.endswith('*'):
            default_typeinfo['init'] = ' = NULL'
        typeinfo = getattr(scalar_rules, self.name, default_typeinfo)
        if typeinfo is default_typeinfo:
            raise NotImplementedError('scalar_rules.%s' % (self.name))
        return typeinfo

    @property
    def array_typeinfo(self):
        default_typeinfo = dict(ctype=self.name)
        if self.name.endswith('*'): default_typeinfo['init'] = ' = NULL'
        typeinfo = type_rules.array.get(self.name, default_typeinfo)
        typeinfo = getattr(array_rules, self.name, default_typeinfo)
        if typeinfo is default_typeinfo:
            raise NotImplementedError('array_rules.%s' % (self.name))
        return typeinfo

    def finalize(self):
        parent = self.parent
        if isinstance(parent, Variable):
            self.title = parent.typeinfo.get('title','')
            if parent.is_array:
                self.title = self.array_typeinfo.get('title','')
                self.argument_title = self.array_typeinfo.get('argument_title',self.title)
                self.return_title = self.array_typeinfo.get('return_title',self.title)
            else:
                self.title = self.scalar_typeinfo.get('title','')
                self.argument_title = self.scalar_typeinfo.get('argument_title',self.title)
                self.return_title = self.scalar_typeinfo.get('return_title',self.title)

class Variable(Component):

    """
    >>> v = Variable('a', 'c_int', 2)
    >>> v
    Variable('a', Type('c_int'), Shape(Bound(2)))
    """

    template = '%(name)s'

    container_options = dict(
        TMP = dict()
        )

    component_container_map = dict(
        Intent = 'TMP',
        Type = 'TMP',
        Shape = 'TMP',
        Initialize = 'TMP',
        )

    def initialize(self, name, type, *components, **options):
        """
        Variable(name, type, [<intent>, <dimension>, depends=[]])
        """
        self.name = name
        self.type = Type(type)
        self += self.type
        self.depends = options.pop('depends', [])
        self.title = options.pop('title','')
        self.argument_title = options.pop('argument_title', self.title)
        self.return_title = options.pop('return_title', self.title)
        self.description = options.pop('description','')
        self.argument_description = options.pop('argument_description', self.description)
        self.return_description = options.pop('return_description', self.description)
        self.init = options.pop('init', None)
        if options: self.warning('%s unused options: %s\n' \
                                 % (self.__class__.__name__, options))
        map(self.add, components)
        return self

    def add(self, component, label=None):
        if isinstance(component, str):
            component = Intent(component)
        elif isinstance(component, (tuple, list)):
            component = Shape(*component)
        elif isinstance(component, int):
            component = Shape(component)
        Component.add(self, component, label)

    @property
    def intent(self):
        return ([c for (c,l) in self.components if isinstance(c, Intent)]\
                +[Intent()])[0]


    @property
    def shape(self):
        return ([c for (c,l) in self.components if isinstance(c, Shape)]\
                +[None])[0]

    @property
    def is_array(self):
        return isinstance(self.shape, Shape)

    @property
    def typeinfo(self):
        if self.is_array:
            return self.type.array_typeinfo
        return self.type.scalar_typeinfo

    @property
    def initialize_in_CDeclaration(self):
        return self.init is not None \
               and self.typeinfo.get('init_container', 'CDeclaration')=='CDeclaration'

    @property
    def initialize_in_FromPyObj(self):
        return self.init is not None \
               and self.typeinfo.get('init_container', 'CDeclaration')=='FromPyObj'

    @property
    def initialize_in_InitFunc(self):
        return self.init is not None \
               and self.type.typeinfo.get('init_container', 'CDeclaration')=='InitFunc'

    @property
    def need_numpy(self):
        if self.is_array:
            return self.type.array_typeinfo.get('require_numpy', False)
        return self.type.scalar_typeinfo.get('require_numpy', False)

    @property
    def need_numeric(self):
        if self.is_array:
            return self.type.array_typeinfo.get('require_numeric', False)
        return self.type.scalar_typeinfo.get('require_numeric', False)
    

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                           ', '.join([`self.name`]+\
                                     [`c` for (c,l) in self.components]))

    def get_implementation_name(self):
        prefix = 'array_'
        if self.shape is None:
            prefix = 'scalar_'
        return prefix + self.intent.get_intent_str()

    def finalize(self):

        self.component_PyCModule.need_numpy_support = self.need_numpy
        self.component_PyCModule.need_numeric_support = self.need_numeric

        argument_title = self.argument_title
        if argument_title:
            if self.type.argument_title:
                argument_title += ', ' + self.type.argument_title
        else:
            argument_title = self.type.argument_title
        return_title = self.return_title
        if return_title:
            if self.type.return_title:
                return_title += ', ' + self.type.return_title
        else:
            return_title = self.type.return_title
        
        parent = self.parent
        if isinstance(parent, PyCFunction):
            input_doc_title = '%s : %s' % (self.name, argument_title)
            output_doc_title = '%s : %s' % (self.name, return_title)
            if self.argument_description:
                input_doc_descr = '  %s' % (self.argument_description)
            else: input_doc_descr = None
            if self.return_description:
                output_doc_descr = '  %s' % (self.return_description)
            else: output_doc_descr = None
            if self.intent.intent in ['argument', 'argument_return']:
                parent.signature += ReqArg(self.name)
                parent += ReqArgDoc(input_doc_title)
                parent += ReqArgDoc(input_doc_descr)
            elif self.intent.intent in ['optional', 'optional_return']:
                parent.signature += OptArg(self.name)
                parent += OptArgDoc(input_doc_title)
                parent += OptArgDoc(input_doc_descr)
            if self.intent.intent in ['return', 'argument_return', 'optional_return']:
                parent.signature += RetArg(self.name)
                parent += RetArgDoc(output_doc_title)
                parent += RetArgDoc(output_doc_descr)

class Intent(Flags):

    """
    >>> Intent()
    Intent()
    >>> Intent().get_intent_str()
    'local_create_c'
    >>> Intent('in','out')
    Intent('argument_return')
    >>> Intent('out').get_intent_str()
    'return_create_c'
    """

    _intent_words = 'in out inout outin inplace copy cache required optional'\
                    ' return extra hide aux c fortran overwrite callback local'\
                    ' argument argument_return optional_return update'.split()

    _intent_list = ['local', 'argument', 'optional', 'argument_return', 'optional_return', 'return']
    _mutability_list = ['create', 'copy', 'overwrite', 'update']
    _storage_list = ['c', 'fortran']

    @property
    def intent(self):
        return ([w for w in self.words if w in self._intent_list] + ['local'])[0]

    @property
    def mutability(self):
        if self.intent in ['local','return']:
            return ([w for w in self.words if w in self._mutability_list] + ['create'])[0]
        else:
            return ([w for w in self.words if w in self._mutability_list] + ['copy'])[0]

    @property
    def storage(self):
        return ([w for w in self.words if w in self._storage_list] + ['c'])[0]
    
    def get_intent_str(self):
        return '_'.join([self.intent, self.mutability, self.storage])

    def is_valid(self, word):
        if word in self.words: return
        if word not in self._intent_words:
            raise ValueError('%s: unknown intent %r' \
                             % (self.__class__.__name__, word))
        if word in ['in', 'required']:
            word = 'argument'
        if word in ['inout','outin']:
            self.add('overwrite')
            word = 'argument'
        if word=='inplace':
            self.add('update')
            word = 'argument'
        if word in ['optional', 'extra']:
            word = 'optional'
        if word=='hide':
            return []
        if word=='aux':
            word = 'local'
        if word=='out':
            word = 'return'
        for k in ['argument', 'optional']:
            if word==k and 'return' in self.words:
                self.words.remove('return')
                return k+'_return'
            if word=='return' and k in self.words:
                self.words.remove(k)
                return k+'_return'
        for l in [self._intent_list, self._mutability_list, self._storage_list]:
            if word not in l:
                continue
            i = l.index(word)
            for w in l[:i]+l[i+1:]:
                if w in self.words:
                    self.words.remove(w)
            break
        assert word in self._intent_list+self._mutability_list+self._storage_list,`word`
        return word

class PyCType(Component):

    template = '%(name)s'
    
    def initialize(self, typeobj, dims = None, init = None):
        """
        >>> t = PyCType('numpy_int8',[2])
        >>> t
        PyCType('numpy_int8', dims = (2,), init = ' = NULL')
        >>> PyCType(t)
        PyCType('numpy_int8', dims = (2,), init = ' = NULL')
        >>> PyCType(t, [3,2])
        PyCType('numpy_int8', dims = (3, 2), init = ' = NULL')
        >>> PyCType(('numpy_int8',[1]))
        PyCType('numpy_int8', dims = (1,), init = ' = NULL')
        >>> print t.generate()
        numpy_int8
        """
        if isinstance(typeobj, self.__class__):
            return PyCType(typeobj.name,
                           dims = dims or typeobj.dims,
                           init = init or typeobj.init)
        if isinstance(typeobj, tuple):
            assert dims is None,`typeobj,dims`
            assert init is None,`typeobj,init`
            return PyCType(*typeobj)

        self.name = typeobj2key(typeobj)
        self.init = init
        if dims:
            if isinstance(dims, (list,tuple)):
                dims = tuple(dims)
            else:
                dims = (dims,)
            self.dims = dims
            self.rank = len(dims)
            typeinfo = type_rules.array_type_map.get(self.name)
        else:
            self.dims = ()
            self.rank = 0
            typeinfo = type_rules.type_map.get(self.name)

        if typeinfo is None:
            raise NotImplementedError('%s: %s typeobj argument value %r' \
                                      % (self.__class__.__name__,
                                         self.name, typeobj))
        for k,v in typeinfo.items():
            setattr(self, k, v)

        if init is not None: # restore init attribute when specified
            self.init = init

        title = typeinfo.get('title', 'a C %s%s' % \
                             (self.name,{True:' array',False:''}[bool(self.dims)]))
        self.input_title = typeinfo.get('input_title', title)
        self.output_title = typeinfo.get('output_title', title)

        return self

    def __repr__(self):
        l = [`self.name`]
        if self.dims:
            l.append('dims = %s' % (`self.dims`))
        if self.init:
            l.append('init = %s' % (`self.init`))
        return '%s(%s)' % (self.__class__.__name__, ', '.join(l))


class PyCVariable(Component):

    """
    >>> v = PyCVariable('v', 'c_int')
    >>> v
    PyCVariable('v', PyCType('c_int', init = ' = 0'))
    >>> print v.generate()
    int v = 0
    >>> v = PyCVariable('v', ('c_int',[2]))
    >>> v
    PyCVariable('v', PyCType('c_int', dims = (2,), init = ' = NULL'))
    >>> print v.generate()
    int* v = NULL
    """
    
    template = '%(ctype)s %(name)s%(init)s'

    def initialize(self, name, pyctype, intent=None, depends=[],
                   title=None, description=None):
        """
        name - variable name in C
        pyctype - PyCType instance defining variable type in C
        intent - Intent instance
        depends - list of variables that given variable depends
        title - short title of the variable
        description - longer description about the variable
        """
        self.name = name
        self.pyctype = PyCType(pyctype)
        self.intent = Intent(intent)
        self.depends = depends
        self.title = title
        self.description = description
        self.init = self.pyctype.init or ''
        self.ctype = self.pyctype.ctype
        self.rank = str(self.pyctype.rank)
        #self.requirements = self.intent.get_requirements()
        return self

    def is_array(self):
        return not not self.pyctype.dims

    def __repr__(self):
        l = [`self.name`, `self.pyctype`]
        if self.intent.words:
            l.append('intent=%r' % (self.intent))
        if self.depends:
            l.append('depends=%r' % (self.depends))
        if self.title:
            l.append('title=%r' % (self.title))
        if self.description:
            l.append('description=%r' % (self.description))
        return '%s(%s)' % (self.__class__.__name__, ', '.join(l))

    def update_PyCFunction(self):
        parent = self.parent
        if self.description is not None:
            doc_descr = '  %s' % (self.description)
        else:
            doc_descr = None

        Arg = None
        if self.intent.is_required():
            intent = 'required'
            Arg, KWList, ArgFmt, ArgObj, ArgDoc =\
                 ReqArg, ReqKWList, ReqArgFmt, ReqArgObj, ReqArgDoc
        elif self.intent.is_optional():
            intent = 'optional'
            Arg, KWList, ArgFmt, ArgObj, ArgDoc =\
                 OptArg, OptKWList, OptArgFmt, OptArgObj, OptArgDoc
        elif self.intent.is_extra():
            intent = 'optional'
            Arg, KWList, ArgFmt, ArgObj, ArgDoc =\
                 ExtArg, ExtKWList, ExtArgFmt, ExtArgObj, ExtArgDoc

        if Arg is not None:
            parent += Arg(self.name)
            parent.signature += Arg(self.name)
            parent += KWList('"' + self.name + '"')
            parent += ArgFmt(self.pyctype.input_format)
            parent += ArgObj(self.evaluate(self.pyctype.input_object, varname=self.name))
            if self.title:
                parent += ArgDoc('%s : %s, %s' % (self.name,self.pyctype.input_title, self.title))
            else:
                parent += ArgDoc('%s : %s' % (self.name, self.pyctype.input_title))
            parent += ArgDoc(doc_descr)
            parent.add(CCode(self.evaluate(self.pyctype.input_frompyobj[intent],
                                           varname=self.name)), 'FromPyObj')
            parent.add(CCode(self.evaluate(self.pyctype.input_cleanfrompyobj[intent],
                                           varname=self.name)), 'CleanFromPyObj')
        if self.intent.is_return():
            parent += RetArg(self.name)
            parent.signature += RetArg(self.name)
            parent += RetArgFmt(self.pyctype.output_format)
            parent += RetArgObj(self.evaluate(self.pyctype.output_object, varname=self.name))
            if self.title:
                parent += RetArgDoc('%s : %s, %s' % (self.name, self.pyctype.output_title, self.title))
            else:
                parent += RetArgDoc('%s : %s' % (self.name, self.pyctype.output_title))
            parent += RetArgDoc(doc_descr)            

        if self.is_array():
            parent += PyCVariable(self.name + '_py', 'c_PyObject_ptr')
            parent += PyCVariable(self.name + '_arr', 'c_PyArrayObject_ptr')
            parent += PyCVariable(self.name + '_dims',
                                  PyCType('npy_dims',
                                          init='[] = {%s}' % (', '.join(map(str,self.pyctype.dims)))))
        

class PyCArgument(PyCVariable):

    _skip = """
    >>> from __init__ import *
    >>> a = PyCArgument('a')
    >>> print a
    PyCArgument('a', PyCTypeSpec('py_object'))
    >>> print a.generate()
    a
    >>> f = PyCFunction('foo')
    >>> f += a
    >>> f += PyCArgument('b')
    >>> m = PyCModule('PyCArgument_test')
    >>> m += f
    >>> #print m.generate()
    >>> mod = m.build()
    >>> print mod.__doc__ #doctest: +ELLIPSIS
    This module 'PyCArgument_test' is generated with ExtGen from NumPy version ...
    <BLANKLINE>
    :Functions:
      foo(a, b) -> None

    """

    container_options = dict(
        TMP = dict()
        )

    component_container_map = dict(
        PyCTypeSpec = 'TMP',
        PyCArrayTypeSpec = 'TMP'
        )

    template = '%(name)s'

    def initialize(self, name, ctype = object, intent=None, *components, **options):
        self.input_intent = options.pop('input_intent','required') # 'optional', 'extra', 'hide'
        self.output_intent = options.pop('output_intent','hide')   # 'return'
        self.input_title = options.pop('input_title', None)
        self.output_title = options.pop('output_title', None)
        self.input_description = options.pop('input_description', None)
        self.output_description = options.pop('output_description', None)
        self.depends = options.pop('depends', [])
        title = options.pop('title', None)
        description = options.pop('description', None)
        if title is not None:
            if self.input_intent!='hide':
                if self.input_title is None:
                    self.input_title = title
            elif self.output_intent!='hide':
                if self.output_title is None:
                    self.output_title = title
        if description is not None:
            if self.input_intent!='hide':
                if self.input_description is None:
                    self.input_description = description
            elif self.output_intent!='hide':
                if self.output_description is None:
                    self.output_description = description
        if options: self.warning('%s unused options: %s\n' % (self.__class__.__name__, options))

        self.name = name
        self.ctype = ctype = PyCTypeSpec(ctype)
        self += ctype

        if intent is None:
            self.intent = Intent('required')
        else:
            assert isinstance(intent, Intent),`intent.__class__`
            self.intent = intent
        self += self.intent

        self.cvar = name
        self.pycvar = None
        self.retpycvar = None

        retfmt = ctype.get_pyret_fmt(self)
        if isinstance(ctype, PyCTypeSpec):
            if retfmt and retfmt in 'SON':
                if self.output_intent == 'return':
                    if self.input_intent=='hide':
                        self.retpycvar = name
                    else:
                        self.pycvar = name
                        self.retpycvar = name + '_return'
                elif self.input_intent!='hide':
                    self.pycvar = name
            else:
                self.pycvar = name
                self.retpycvar = name
        elif isinstance(ctype, PyCArrayTypeSpec):
            self.pycvar = name + '_pyc'
            self.retpycvar = name
            if 1:
                pass
            elif retfmt and retfmt in 'SON':
                if self.output_intent == 'return':
                    if self.input_intent=='hide':
                        self.retpycvar = name
                    else:
                        self.pycvar = name
                        self.retpycvar = name + '_return'
                elif self.input_intent!='hide':
                    self.pycvar = name
            else:
                self.pycvar = name
                self.retpycvar = name
        else:
            self.pycvar = name + '_pyc'
            self.retpycvar = name + '_pyc_r'

        ctype.set_titles(self)

        map(self.add, components)
        return self

    def add(self, component, container_label=None):
        if isinstance(component, Intent):
            self.intent += component
        else:
            Component.add(self, component, container_label)

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, ', '.join(map(repr,[self.name]+[c for (c,l) in self.components])))

    def update_PyCFunction(self):
        parent = self.parent
        ctype = self.ctype

        input_doc_title = '%s : %s' % (self.name, self.input_title)
        output_doc_title = '%s : %s' % (self.name, self.output_title)
        if self.input_description is not None:
            input_doc_descr = '  %s' % (self.input_description)
        else:
            input_doc_descr = None
        if self.output_description is not None:
            output_doc_descr = '  %s' % (self.output_description)
        else:
            output_doc_descr = None

        # add components to parent:
        parent += ctype.get_decl(self, parent)
        if self.input_intent=='required':
            parent += ReqArg(self.name)
            parent.signature += ReqArg(self.name)
            parent += ReqKWList('"' + self.name + '"')
            parent += ReqArgFmt(ctype.get_pyarg_fmt(self))
            parent += ReqArgObj(ctype.get_pyarg_obj(self))
            parent += ReqArgDoc(input_doc_title)
            parent += ReqArgDoc(input_doc_descr)
        elif self.input_intent=='optional':
            parent += OptArg(self.name)
            parent.signature += OptArg(self.name)
            parent += OptKWList('"' + self.name + '"')
            parent += OptArgFmt(ctype.get_pyarg_fmt(self))
            parent += OptArgObj(ctype.get_pyarg_obj(self))
            parent += OptArgDoc(input_doc_title)
            parent += OptArgDoc(input_doc_descr)
        elif self.input_intent=='extra':
            parent += ExtArg(self.name)
            parent.signature += ExtArg(self.name)
            parent += ExtKWList('"' + self.name + '"')
            parent += ExtArgFmt(ctype.get_pyarg_fmt(self))
            parent += ExtArgObj(ctype.get_pyarg_obj(self))
            parent += ExtArgDoc(input_doc_title)
            parent += ExtArgDoc(input_doc_descr)
        elif self.input_intent=='hide':
            pass
        else:
            raise NotImplementedError('input_intent=%r' % (self.input_intent))

        if self.output_intent=='return':
            parent += RetArg(self.name)
            parent.signature += RetArg(self.name)
            parent += RetArgFmt(ctype.get_pyret_fmt(self))
            parent += RetArgObj(ctype.get_pyret_obj(self))
            parent += RetArgDoc(output_doc_title)
            parent += RetArgDoc(output_doc_descr)
        elif self.output_intent=='hide':
            pass
        else:
            raise NotImplementedError('output_intent=%r' % (self.output_intent))

class PyCReturn(PyCArgument):

    def initialize(self, name, ctype = object, *components, **options):
        return PyCArgument(name, ctype, intent=Intent('out','hide'),
                           input_intent='hide', output_intent='return', *components, **options)

def typeobj2key(typeobj):
    key = None
    if isinstance(typeobj, type):
        if typeobj.__module__=='__builtin__':
            key = typeobj.__name__
            if key=='array':
                key = 'numeric_array'
            else:
                key = 'py_' + key
        elif typeobj.__module__=='numpy':
            key = 'numpy_' + typeobj.__name__
    elif isinstance(typeobj, str):
        key = typeobj
        if key.startswith('numpy_'):
            k = key[6:]
            named_scalars = ['byte','short','int','long','longlong',
                             'ubyte','ushort','uint','ulong','ulonglong',
                             'intp','uintp',
                             'float_','double',
                             'longfloat','longdouble',
                             'complex_',
                             ]
            if k in named_scalars:
                import numpy
                key = 'numpy_' + getattr(numpy, k).__name__    
    return key


class PyCTypeSpec(CTypeSpec):

    """
    >>> s = PyCTypeSpec(object)
    >>> print s
    PyCTypeSpec('py_object')
    >>> print s.generate()
    PyObject*

    See more examples in tests/test_py_support.py.

    """

    typeinfo_map = dict(
        py_int = ('PyInt_Type', 'PyIntObject*', 'O!', 'N', 'NULL',
                  'a python integer'),
        py_long = ('PyLong_Type', 'PyLongObject*', 'O!', 'N', 'NULL',
                   'a python long integer'),
        py_float = ('PyFloat_Type', 'PyFloatObject*', 'O!', 'N', 'NULL',
                    'a python floating point number'),
        py_complex = ('PyComplex_Type', 'PyComplexObject*', 'O!', 'N', 'NULL',
                      'a python complex number'),
        py_str = ('PyString_Type', 'PyStringObject*', 'S', 'N', 'NULL',
                  'a python string'),
        py_unicode = ('PyUnicode_Type', 'PyUnicodeObject*', 'U', 'N', 'NULL',
                      'a python Unicode object'),
        py_buffer = ('PyBuffer_Type', 'PyBufferObject*', 'O!', 'N', 'NULL',
                     'a python buffer'),
        py_tuple = ('PyTuple_Type', 'PyTupleObject*', 'O!', 'N', 'NULL',
                    'a python tuple'),
        py_list = ('PyList_Type', 'PyListObject*', 'O!', 'N', 'NULL',
                   'a python list'),
        py_dict = ('PyDict_Type', 'PyDictObject*', 'O!', 'N', 'NULL',
                   'a python dictionary'),
        py_file = ('PyFile_Type', 'PyFileObject*', 'O!', 'N', 'NULL',
                   'a python file object'),
        py_instance = ('PyInstance_Type', 'PyObject*', 'O!', 'N', 'NULL',
                       'a python instance object'),
        py_function = ('PyFunction_Type', 'PyFunctionObject*', 'O!', 'N', 'NULL',
                       'a python function object'),
        py_method = ('PyMethod_Type', 'PyObject*', 'O!', 'N', 'NULL',
                     'a python instance method object'),
        py_module = ('PyModule_Type', 'PyObject*', 'O!', 'N', 'NULL',
                     'a python module object'),
        py_iter = ('PySeqIter_Type', 'PyObject*', 'O!', 'N', 'NULL',
                   'a python iterator'),
        py_property = ('PyProperty_Type', 'PyObject*', 'O!', 'N', 'NULL',
                       'a python property attribute'),
        py_slice = ('PySlice_Type', 'PyObject*', 'O!', 'N', 'NULL',
                    'a python slice object'),
        py_cell = ('PyCell_Type', 'PyCellObject*', 'O!', 'N', 'NULL'),
        py_generator = ('PyGen_Type', 'PyGenObject*', 'O!', 'N', 'NULL'),
        py_set = ('PySet_Type', 'PySetObject*', 'O!', 'N', 'NULL',
                  'a python set object'),
        py_frozenset = ('PyFrozenSet_Type', 'PySetObject*', 'O!', 'N', 'NULL',
                        'a python frozenset object'),
        py_cobject = (None, 'PyCObject*', 'O', 'N', 'NULL',
                      'a PyCObject object'),
        py_type = ('PyType_Type', 'PyTypeObject*', 'O!', 'N', 'NULL',
                   'a python type object'),
        py_object = (None, 'PyObject*', 'O', 'N', 'NULL',
                     'a python object'),
        numpy_ndarray = ('PyArray_Type', 'PyArrayObject*', 'O!', 'N', 'NULL',
                         'a numpy array'),
        numpy_descr = ('PyArrayDescr_Type','PyArray_Descr', 'O!', 'N', 'NULL'),

        numpy_ufunc = ('PyUFunc_Type', 'PyUFuncObject*', 'O!', 'N', 'NULL',
                       'a numpy universal function'),
        numpy_iter = ('PyArrayIter_Type', 'PyArrayIterObject*', 'O!', 'N',
                      'NULL'),
        numpy_multiiter = ('PyArrayMultiIter_Type', 'PyArrayMultiIterObject*',
                           'O!', 'N', 'NULL'),
        numpy_int8 = ('PyInt8ArrType_Type', 'PyInt8ScalarObject*', 'O!', 'N',
                      'NULL', 'a numpy 8-bit integer'),
        numpy_int16 = ('PyInt16ArrType_Type', 'PyInt16ScalarObject*', 'O!', 'N',
                       'NULL', 'a numpy 16-bit integer'),
        numpy_int32 = ('PyInt32ArrType_Type', 'PyInt32ScalarObject*', 'O!', 'N',
                       'NULL', 'a numpy 32-bit integer'),
        numpy_int64 = ('PyInt64ArrType_Type', 'PyInt64ScalarObject*', 'O!', 'N',
                       'NULL', 'a numpy 64-bit integer'),
        numpy_int128 = ('PyInt128ArrType_Type', 'PyInt128ScalarObject*', 'O!',
                        'N', 'NULL', 'a numpy 128-bit integer'),
        numpy_uint8 = ('PyUInt8ArrType_Type', 'PyUInt8ScalarObject*', 'O!', 'N',
                       'NULL', 'a numpy 8-bit unsigned integer'),
        numpy_uint16 = ('PyUInt16ArrType_Type', 'PyUInt16ScalarObject*', 'O!',
                        'N', 'NULL', 'a numpy 16-bit unsigned integer'),
        numpy_uint32 = ('PyUInt32ArrType_Type', 'PyUInt32ScalarObject*', 'O!',
                        'N', 'NULL', 'a numpy 32-bit unsigned integer'),
        numpy_uint64 = ('PyUInt64ArrType_Type', 'PyUInt64ScalarObject*', 'O!',
                        'N', 'NULL', 'a numpy 64-bit unsigned integer'),
        numpy_uint128 = ('PyUInt128ArrType_Type', 'PyUInt128ScalarObject*',
                         'O!', 'N', 'NULL', 'a numpy 128-bit unsigned integer'),
        numpy_float16 = ('PyFloat16ArrType_Type', 'PyFloat16ScalarObject*',
                         'O!', 'N', 'NULL',
                         'a numpy 16-bit floating point number'),
        numpy_float32 = ('PyFloat32ArrType_Type', 'PyFloat32ScalarObject*',
                         'O!', 'N', 'NULL',
                         'a numpy 32-bit floating point number'),
        numpy_float64 = ('PyFloat64ArrType_Type', 'PyFloat64ScalarObject*',
                         'O!', 'N', 'NULL',
                         'a numpy 64-bit floating point number'),
        numpy_float80 = ('PyFloat80ArrType_Type', 'PyFloat80ScalarObject*',
                         'O!', 'N', 'NULL',
                         'a numpy 80-bit floating point number'),
        numpy_float96 = ('PyFloat96ArrType_Type', 'PyFloat96ScalarObject*',
                         'O!', 'N', 'NULL',
                         'a numpy 96-bit floating point number'),
        numpy_float128 = ('PyFloat128ArrType_Type', 'PyFloat128ScalarObject*',
                          'O!', 'N', 'NULL',
                          'a numpy 128-bit floating point number'),
        numpy_complex32 = ('PyComplex32ArrType_Type','PyComplex32ScalarObject*',
                           'O!', 'N', 'NULL',
                           'a numpy 32-bit complex number'),
        numpy_complex64 = ('PyComplex64ArrType_Type','PyComplex64ScalarObject*',
                           'O!', 'N', 'NULL',
                           'a numpy 64-bit complex number'),
        numpy_complex128 = ('PyComplex128ArrType_Type',
                            'PyComplex128ScalarObject*', 'O!', 'N', 'NULL',
                            'a numpy 128-bit complex number'),
        numpy_complex160 = ('PyComplex160ArrType_Type',
                            'PyComplex160ScalarObject*', 'O!', 'N', 'NULL',
                            'a numpy 160-bit complex number'),
        numpy_complex192 = ('PyComplex192ArrType_Type',
                            'PyComplex192ScalarObject*', 'O!', 'N', 'NULL',
                            'a numpy 192-bit complex number'),
        numpy_complex256 = ('PyComplex256ArrType_Type',
                            'PyComplex256ScalarObject*', 'O!', 'N', 'NULL',
                            'a numpy 256-bit complex number'),
        numeric_array = ('PyArray_Type', 'PyArrayObject*', 'O!', 'N', 'NULL',
                         'a Numeric array'),
        c_char = (None, 'char', 'b', 'b', '0',
                  'a python integer (converting to C char)',
                  'a python integer (converting from C char)',
                  ),
        c_unsigned_char = (None, 'unsigned char', 'B', 'B', '0',
                           'a python integer (converting to C unsigned char)',
                           'a python integer (converting from C unsigned char)',
                           ),
        c_short = (None, 'short int', 'h', 'h', '0',
                  'a python integer (converting to C short int)',
                  'a python integer (converting from C short int)',
                   ),
        c_unsigned_short = (None, 'unsigned short int', 'H', 'H', '0',
                            'a python integer (converting to C unsigned short int)',
                            'a python integer (converting from C unsigned short int)',
                            ),
        c_int = (None,'int', 'i', 'i', '0',
                  'a python integer (converting to C int)',
                  'a python integer (converting from C int)',
                 ),
        c_unsigned_int = (None,'unsigned int', 'I', 'I', '0',
                  'a python integer (converting to C unsigned int)',
                  'a python integer (converting from C unsigned int)',
                          ),
        c_long = (None,'long', 'l', 'l', '0',
                  'a python integer (converting to C long int)',
                  'a python integer (converting from C long int)',
                  ),
        c_unsigned_long = (None,'unsigned long', 'k', 'k', '0',
                           'a python integer (converting to C unsigned long int)',
                           'a python integer (converting from C insigned long int)',
                           ),
        c_long_long = (None,'PY_LONG_LONG', 'L', 'L', '0',
                  'a python integer (converting to C long long)',
                  'a python integer (converting from C long long)',
                       ),
        c_unsigned_long_long = (None,'unsigned PY_LONG_LONG', 'K', 'K', '0',
                                'a python integer (converting to C unsigned long long)',
                                'a python integer (converting from C unsigned long long)',
                                ),     
        c_Py_ssize_t = (None,'Py_ssize_t', 'n', 'n', '0',
                        'a python integer (converting to C Py_ssize_t)',
                        'a python integer (converting from C Py_ssize_t)',
                        ),
        c_char1 = (None,'char', 'c', 'c', '"\\0"',
                   'a python character (converting to C char)',
                   'a python character (converting from C char)',
                   ),
        c_float = (None,'float', 'f', 'f', '0.0',
                   'a python floating point number (converting to C float)',
                   'a python floating point number (converting from C float)',
                   ),
        c_double = (None,'double', 'd', 'd', '0.0',
                   'a python floating point number (converting to C double)',
                   'a python floating point number (converting from C double)',
                    ),
        c_Py_complex = (None,'Py_complex', 'D', 'D', '{0.0, 0.0}',
                   'a python complex number (converting to C Py_complex structure)',
                   'a python complex number (converting from C Py_complex structure)',
                        ),
        c_const_char_ptr = (None,'const char *', 'z', 'z', 'NULL',
                            'a python string or Unicode or None object (converting to C const char *)',
                            'a python string or None (converting from C char *)',
                            ),
        c_Py_UNICODE = (None,'Py_UNICODE*','u','u', 'NULL',
                        'a python Unicode object (converting to C Py_UNICODE*)',
                        'a python Unicode object or None (converting from C Py_UNICODE*)'
                        ),
        )

    def initialize(self, typeobj):
        if isinstance(typeobj, CTypeSpec):
            return typeobj

        m = self.typeinfo_map
        key = typeobj2key(typeobj)
        try: item = m[key]
        except KeyError:
            raise NotImplementedError('%s: need %s support' % (self.__class__.__name__, typeobj))

        self.typeobj_name = key
        self.ctypeobj = item[0]
        self.line = item[1]
        self.arg_fmt = item[2]
        self.ret_fmt = item[3]
        self.cinit_value = item[4]
        try:
            self.typeobj_input_name = item[5]
        except IndexError:
            self.typeobj_input_name = key
        try:
            self.typeobj_output_name = item[6]
        except IndexError:
            self.typeobj_output_name = self.typeobj_input_name

        self.need_numpy_support = key.startswith('numpy_')
        self.need_numeric_support = key.startswith('numeric_')
        return self

    def finalize(self):
        if self.need_numpy_support:
            self.component_PyCModule.need_numpy_support = True
        if self.need_numeric_support:
            self.component_PyCModule.need_numeric_support = True
        return

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, ', '.join([repr(self.typeobj_name)]+[repr(c) for (c,l) in self.components]))

    def get_pyarg_fmt(self, arg):
        if arg.input_intent=='hide': return None
        return self.arg_fmt

    def get_pyarg_obj(self, arg):
        if arg.input_intent=='hide': return None
        if self.arg_fmt=='O!':
            return '&%s, &%s' % (self.ctypeobj, arg.pycvar)
        return '&' + arg.pycvar

    def get_pyret_fmt(self, arg):
        if arg.output_intent=='hide': return None
        return self.ret_fmt

    def get_pyret_obj(self, arg):
        if arg.output_intent=='return':
            if self.get_pyret_fmt(arg)=='D':
                return '&' + arg.retpycvar
            return arg.retpycvar
        return

    def get_init_value(self, arg):
        return self.cinit_value

    def set_titles(self, arg):
        if arg.input_intent!='hide':
            r = ''
            if arg.input_title: r = ', ' + arg.input_title
            arg.input_title = self.typeobj_input_name + r
        if arg.output_intent!='hide':
            r = ''
            if arg.output_title: r = ', ' + arg.output_title
            arg.output_title = self.typeobj_output_name + r

    def get_decl(self, arg, func):
        init_value = self.get_init_value(arg)
        if init_value:
            init =  ' = %s' % (init_value)
        else:
            init = ''
        if arg.pycvar and arg.pycvar==arg.retpycvar:
            func += CDeclaration(self, '%s%s' % (arg.pycvar, init))
        else:
            if self.get_pyret_obj(arg) is None:
                if self.get_pyret_obj(arg) is not None:
                    func += CDeclaration(self, '%s%s' % (arg.pycvar, init))
            elif self.get_pyarg_obj(arg) is not None:
                func += CDeclaration(self, '%s%s' % (arg.pycvar, init))
                func += CDeclaration(self,'%s%s' % (arg.retpycvar, init))
            else:
                func += CDeclaration(self, '%s%s' % (arg.retpycvar, init))
        return

    def set_converters(self, arg):
        """
        Notes for user:
          if arg is intent(optional, in, out) and not specified
          as function argument then function may created but
          it must then have *new reference* (ie use Py_INCREF
          unless it is a new reference already).
        """
        # this method is called from PyCFunction.update_containers(),
        # note that self.parent is None put arg.parent is PyCFunction
        # instance.
        eval_a = arg.evaluate
        FromPyObj = arg.container_FromPyObj
        PyObjFrom = arg.container_PyObjFrom

        argfmt = self.get_pyarg_fmt(arg)
        retfmt = self.get_pyret_fmt(arg)
        if arg.output_intent=='return':
            if arg.input_intent in ['optional', 'extra']:
                if retfmt in 'SON':
                    FromPyObj += eval_a('''\
if (!(%(pycvar)s==NULL)) {
  /* make %(pycvar)r a new reference */
  %(retpycvar)s = %(pycvar)s;
  Py_INCREF((PyObject*)%(retpycvar)s);
}
''')
                    PyObjFrom += eval_a('''\
if (%(retpycvar)s==NULL) {
  /* %(pycvar)r was not specified */
  if (%(pycvar)s==NULL) {
    %(retpycvar)s = Py_None;
    Py_INCREF((PyObject*)%(retpycvar)s);
  } else {
    %(retpycvar)s = %(pycvar)s;
    /* %(pycvar)r must be a new reference or expect a core dump. */
  }
} elif (!(%(retpycvar)s == %(pycvar)s)) {
  /* a new %(retpycvar)r was created, undoing %(pycvar)s new reference */
  Py_DECREF((PyObject*)%(pycvar)s);
}
''')
            elif arg.input_intent=='hide':
                if retfmt in 'SON':
                    PyObjFrom += eval_a('''\
if (%(retpycvar)s==NULL) {
  %(retpycvar)s = Py_None;
  Py_INCREF((PyObject*)%(retpycvar)s);
} /* else %(retpycvar)r must be a new reference or expect a core dump. */
''')
            elif arg.input_intent=='required':
                if retfmt in 'SON':
                    FromPyObj += eval_a('''\
/* make %(pycvar)r a new reference */
%(retpycvar)s = %(pycvar)s;
Py_INCREF((PyObject*)%(retpycvar)s);
''')
                    PyObjFrom += eval_a('''\
if (!(%(retpycvar)s==%(pycvar)s)) {
  /* a new %(retpycvar)r was created, undoing %(pycvar)r new reference */
  /* %(retpycvar)r must be a new reference or expect a core dump. */
  Py_DECREF((PyObject*)%(pycvar)s);
}
''')


class PyCArrayTypeSpec(CTypeSpec):

    typeinfo_map = dict(
        numpy_int8 = ('PyInt8ArrType_Type', 'npy_int8*', 'O', 'N')
        )

    def initialize(self, typeobj, dimensions):
        """
        """
        if isinstance(typeobj, self.__class__):
            return typeobj
        
        m = self.typeinfo_map
        key = typeobj2key(typeobj)
        try: item = m[key]
        except KeyError:
            raise NotImplementedError('%s: need %s support' % (self.__class__.__name__, typeobj))
        self.typeobj_name = key
        self.line = item[1]
        self.dims = dimensions
        try:
            self.typeobj_input_name = item[5]
        except IndexError:
            self.typeobj_input_name = key
        try:
            self.typeobj_output_name = item[6]
        except IndexError:
            self.typeobj_output_name = self.typeobj_input_name
        self.need_numpy_support = True
        return self

    def finalize(self):
        if self.need_numpy_support:
            self.component_PyCModule.need_numpy_support = True

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, ', '.join(\
            [repr(self.typeobj_name), repr(self.dims)]+[repr(c) for (c,l) in self.components]))

    def get_pyarg_fmt(self, arg):
        if arg.input_intent=='hide': return None
        return 'O'

    def get_pyarg_obj(self, arg):
        if arg.input_intent=='hide': return None
        return '&' + arg.pycvar

    def get_pyret_fmt(self, arg):
        if arg.output_intent=='hide': return None
        return 'N'

    def get_pyret_obj(self, arg):
        if arg.output_intent=='return':
            return arg.retpycvar
        return

    def get_init_value(self, arg):
        return 'NULL'

    def set_titles(self, arg):
        if arg.input_intent!='hide':
            r = ''
            if arg.input_title: r = ', ' + arg.input_title
            arg.input_title = self.typeobj_input_name + r
        if arg.output_intent!='hide':
            r = ''
            if arg.output_title: r = ', ' + arg.output_title
            arg.output_title = self.typeobj_output_name + r

    def get_decl(self, arg, func):
        init_value = self.get_init_value(arg)
        if init_value:
            init =  ' = %s' % (init_value)
        else:
            init = ''

        if arg.pycvar and arg.pycvar==arg.retpycvar:
            func += CDeclaration(self, '%s%s' % (arg.pycvar, init))
        else:
            if self.get_pyret_obj(arg) is None:
                if self.get_pyret_obj(arg) is not None:
                    func += CDeclaration(self, '%s%s' % (arg.pycvar, init))
            elif self.get_pyarg_obj(arg) is not None:
                func += CDeclaration(self, '%s%s' % (arg.pycvar, init))
                func += CDeclaration(self,'%s%s' % (arg.retpycvar, init))
            else:
                func += CDeclaration(self, '%s%s' % (arg.retpycvar, init))
        return

    def set_converters(self, arg):
        """
        Notes for user:
          if arg is intent(optional, in, out) and not specified
          as function argument then function may created but
          it must then have *new reference* (ie use Py_INCREF
          unless it is a new reference already).
        """
        # this method is called from PyCFunction.update_containers(),
        # note that self.parent is None put arg.parent is PyCFunction
        # instance.
        eval_a = arg.evaluate
        FromPyObj = arg.container_FromPyObj
        PyObjFrom = arg.container_PyObjFrom

        argfmt = self.get_pyarg_fmt(arg)
        retfmt = self.get_pyret_fmt(arg)
        if arg.output_intent=='return':
            if arg.input_intent in ['optional', 'extra']:
                if retfmt in 'SON':
                    FromPyObj += eval_a('''\
if (!(%(pycvar)s==NULL)) {
  /* make %(pycvar)r a new reference */
  %(retpycvar)s = %(pycvar)s;
  Py_INCREF((PyObject*)%(retpycvar)s);
}
''')
                    PyObjFrom += eval_a('''\
if (%(retpycvar)s==NULL) {
  /* %(pycvar)r was not specified */
  if (%(pycvar)s==NULL) {
    %(retpycvar)s = Py_None;
    Py_INCREF((PyObject*)%(retpycvar)s);
  } else {
    %(retpycvar)s = %(pycvar)s;
    /* %(pycvar)r must be a new reference or expect a core dump. */
  }
} elif (!(%(retpycvar)s == %(pycvar)s)) {
  /* a new %(retpycvar)r was created, undoing %(pycvar)s new reference */
  Py_DECREF((PyObject*)%(pycvar)s);
}
''')
            elif arg.input_intent=='hide':
                if retfmt in 'SON':
                    PyObjFrom += eval_a('''\
if (%(retpycvar)s==NULL) {
  %(retpycvar)s = Py_None;
  Py_INCREF((PyObject*)%(retpycvar)s);
} /* else %(retpycvar)r must be a new reference or expect a core dump. */
''')
            elif arg.input_intent=='required':
                if retfmt in 'SON':
                    FromPyObj += eval_a('''\
/* make %(pycvar)r a new reference */
%(retpycvar)s = %(pycvar)s;
Py_INCREF((PyObject*)%(retpycvar)s);''')
                    PyObjFrom += eval_a('''\
if (!(%(retpycvar)s==%(pycvar)s)) {
  /* a new %(retpycvar)r was created, undoing %(pycvar)r new reference */
  /* %(retpycvar)r must be a new reference or expect a core dump. */
  Py_DECREF((PyObject*)%(pycvar)s);
}''')

def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    _test()
