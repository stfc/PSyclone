#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author L. Mitchell Imperial College
# Modified by R. Ford STFC Daresbury Lab
#     "       A. Porter STFC Daresbury Lab

import fparser
from fparser import parsefortran
from fparser import api as fpapi
import expression as expr
import logging
import os
from line_length import FortLineLength

class ParseError(Exception):
    def __init__(self, value):
        self.value = "Parse Error: "+value
    def __str__(self):
        return repr(self.value)

class Descriptor(object):
    """A description of how a kernel argument is accessed"""
    def __init__(self,access,space,stencil):
        self._access=access
        self._space=space
        self._stencil=stencil

    @property
    def access(self):
        return self._access

    @property
    def function_space(self):
        return self._space

    @property
    def stencil(self):
        return self._stencil

    def __repr__(self):
        return 'Descriptor(%s, %s)' % (self.stencil, self.access)

class GODescriptor(Descriptor):
    def __init__(self, access, space, stencil):
        Descriptor.__init__(self,access,space,stencil)

class DynDescriptor(Descriptor):
    def __init__(self,access,funcspace,stencil,basis,diff_basis,gauss_quad):
        Descriptor.__init__(self,access,funcspace,stencil)
        self._basis=basis
        self._diff_basis=diff_basis
        self._gauss_quad=gauss_quad
    @property
    def basis(self):
        return self._basis
    @property
    def diff_basis(self):
        return self._diff_basis
    @property
    def gauss_quad(self):
        return self._gauss_quad

class GHProtoDescriptor(Descriptor):
    def __init__(self, access, space, stencil):
        self._space = FunctionSpace.unpack(space)
        Descriptor.__init__(self,access,self._space,stencil)

    @property
    def element(self):
        return self._space.element

    def __repr__(self):
        return 'Descriptor(%s, %s, %s)' % (self.stencil, self.element, self.access)


class FunctionSpace(object):
    @staticmethod
    def unpack(string):
        p = expr.expression.parseString(string)[0]
        dim = 1
        if isinstance(p, expr.BinaryOperator) and p.symbols[0] == '**':
            dim = int(p.operands[1])
            p = p.operands[0]
        ele = Element.unpack(p)
        return FunctionSpace(ele, dim)

    def __init__(self, element, dimension):
        self._element = element
        self._dimension = dimension

    @property
    def element(self):
        return self._element

    @property
    def dimension(self):
        return self._dimension


class Element(object):
    @staticmethod
    def unpack(string_or_expr):
        if isinstance(string_or_expr, str):
            p = expr.expression.parseString(string_or_expr)[0]
        else:
            p = string_or_expr
        if isinstance(p, expr.Grouping):
            p = p.expr
        if isinstance(p, expr.BinaryOperator):
            assert all(a == '*' for a in p.symbols)
            eles = p.operands
        else:
            assert isinstance(p, expr.FunctionVar)
            eles = [p]
        order = eles[0].args[0] if eles[0].args else None
        ele = Element(eles[0].name, order)
        for e in eles[1:]:
            order = e.args[0] if e.args else None
            ele *= Element(e.name, order)
        return ele

    def __init__(self, name=None, order=None):
        self._name = name
        if isinstance(order, str):
            order = int(order)
        self._order = order

    def __repr__(self):
        if self._order:
            return "%s%d" % (self._name, self._order)
        return "%s" % self._name

    def __mul__(self, other):
        assert isinstance(other, Element), \
            'Can only take tensor products with Elements'
        return TensorProductElement(self, other)


class TensorProductElement(Element):
    def __init__(self, *elements):
        assert all(isinstance(e, Element) for e in elements), 'All arguments to build a TensorProductElement should be Elements'
        self._elements = elements

    @property
    def elements(self):
        return self._elements

    def __repr__(self):
        s = " * ".join("%s" % e for e in self.elements)
        return s

    def __mul__(self, other):
        assert isinstance(other, Element), 'Can only take tensor products with Elements not %s' % type(other)
        if isinstance(other, TensorProductElement):
            return TensorProductElement(*(self.elements + other.elements))
        else:
            return TensorProductElement(*(self.elements + (other, )))


class KernelProcedure(object):
    """An elemental kernel procedure"""
    def __init__(self, ktype_ast, ktype_name, modast):
        a, n = KernelProcedure.get_procedure(ktype_ast, ktype_name, modast)
        self._ast = a
        self._name = n

    @staticmethod
    def get_procedure(ast, name, modast):
        bname = None
        for statement in ast.content:
            if isinstance(statement, fparser.statements.SpecificBinding):
                if statement.name=="code" and statement.bname!="":
                    # prototype gungho style
                    bname = statement.bname
                elif statement.name.lower()!="code" and statement.bname!="":
                    raise ParseError("Kernel type %s binds to a specific procedure but does not use 'code' as the generic name." % \
                                     name)
                else:
                    # psyclone style
                    bname=statement.name
        if bname is None:
            raise RuntimeError("Kernel type %s does not bind a specific procedure" % \
                               name)
        if bname=='':
            raise ParseError("Internal error: empty kernel name returned for Kernel type %s." % \
                               name)
        code = None
        default_public=True
        declared_private=False
        declared_public=False
        for statement, depth in fpapi.walk(modast, -1):
            if isinstance(statement, fparser.statements.Private):
                if len(statement.items)==0:
                    default_public=False
                elif bname in statement.items:
                    declared_private=True
            if isinstance(statement, fparser.statements.Public):
                if len(statement.items)==0:
                    default_public=True
                elif bname in statement.items:
                    declared_public=True
            if isinstance(statement, fparser.block_statements.Subroutine) and \
               statement.name == bname:
                if statement.is_public():
                    declared_public=True
                code = statement
        if code is None:
            raise RuntimeError("Kernel subroutine %s not implemented" % bname)
        if declared_private or (not default_public and not declared_public):
            raise ParseError("Kernel subroutine '%s' is not public" % bname)
        return code, bname


    @property
    def name(self):
        return self._name

    @property
    def ast(self):
        return self._ast

    def __repr__(self):
        return 'KernelProcedure(%s, %s)' % (self.name, self.ast)

    def __str__(self):
        return self._ast.__str__()

class KernelTypeFactory(object):
    def __init__(self,api=""):
        if api=="":
            from config import DEFAULTAPI
            self._type=DEFAULTAPI
        else:
            from config import SUPPORTEDAPIS as supportedTypes
            self._type=api
            if self._type not in supportedTypes:
                raise ParseError("KernelTypeFactory: Unsupported API '{0}' "
                                 "specified. Supported types are {1}.".\
                                 format(self._type, supportedTypes))

    def create(self, ast, name=None):
        if self._type=="gunghoproto":
            return GHProtoKernelType(ast, name=name)
        elif self._type=="dynamo0.1":
            return DynKernelType(ast, name=name)
        elif self._type=="dynamo0.3":
            from dynamo0p3 import DynKernMetadata
            return DynKernMetadata(ast, name=name)
        elif self._type=="gocean0.1":
            return GOKernelType(ast, name=name)
        elif self._type=="gocean1.0":
            from gocean1p0 import GOKernelType1p0
            return GOKernelType1p0(ast, name=name)
        else:
            raise ParseError("KernelTypeFactory: Internal Error: Unsupported "
                             "kernel type '{0}' found. Should not be possible.".\
                             format(self._myType))

class KernelType(object):
    """ Kernel Metadata baseclass

    This contains the elemental procedure and metadata associated with
    how that procedure is mapped over mesh entities."""

    def __init__(self, ast, name=None):

        if name is None:
            # if no name is supplied then use the module name to
            # determine the type name. The assumed convention is that
            # the module is called <name/>_mod and the type is called
            # <name/>_type
            found = False
            for statement, depth  in fpapi.walk(ast, -1):
                if isinstance(statement, fparser.block_statements.Module):
                    module_name = statement.name
                    found = True
                    break
            if not found:
                raise ParseError("Error KernelType, the file does not contain a module. Is it a Kernel file?")

            mn_len = len(module_name)
            if mn_len<5:
                raise ParseError("Error, module name '{0}' is too short to have '_mod' as an extension. This convention is assumed.".format(module_name))
            base_name = module_name.lower()[:mn_len-4]
            extension_name = module_name.lower()[mn_len-4:mn_len]
            if extension_name != "_mod":
                raise ParseError("Error, module name '{0}' does not have '_mod' as an extension. This convention is assumed.".format(module_name))
            name = base_name + "_type"

        self._name = name
        self._ast = ast
        self.checkMetadataPublic(name,ast)
        self._ktype=self.getKernelMetadata(name,ast)
        self._iterates_over = self._ktype.get_variable('iterates_over').init
        self._procedure = KernelProcedure(self._ktype, name, ast)
        self._inits=self.getkerneldescriptors(self._ktype)
        self._arg_descriptors=None # this is set up by the subclasses
        
    def getkerneldescriptors(self,ast, var_name='meta_args'):
        descs = ast.get_variable(var_name)
        if descs is None:
            raise ParseError("kernel call does not contain a {0} type".format(var_name))
        try:
            nargs=int(descs.shape[0])
        except AttributeError as e:
            raise ParseError("kernel metadata {0}: {1} variable must be an array".format(self._name, var_name))
        if len(descs.shape) is not 1:
            raise ParseError("kernel metadata {0}: {1} variable must be a 1 dimensional array".format(self._name, var_name))
        if descs.init.find("[") is not -1 and descs.init.find("]") is not -1:
            # there is a bug in f2py
            raise ParseError("Parser does not currently support [...] initialisation for {0}, please use (/.../) instead".format(var_name))
        inits = expr.expression.parseString(descs.init)[0]
        nargs=int(descs.shape[0])
        if len(inits) != nargs:
            raise ParseError("Error, in {0} specification, the number of args {1} and number of dimensions {2} do not match".format(var_name, nargs, len(inits)))
        return inits

    @property
    def name(self):
        return self._name

    @property
    def iterates_over(self):
        return self._iterates_over

    @property
    def procedure(self):
        return self._procedure

    @property
    def nargs(self):
        return len(self._arg_descriptors)

    @property
    def arg_descriptors(self):
        return self._arg_descriptors

    def __repr__(self):
        return 'KernelType(%s, %s)' % (self.name, self.iterates_over)

    def checkMetadataPublic(self,name,ast):
        default_public=True
        declared_private=False
        declared_public=False
        for statement, depth  in fpapi.walk(ast, -1):
            if isinstance(statement, fparser.statements.Private):
                if len(statement.items)==0:
                    default_public=False
                elif name in statement.items:
                    declared_private=True
            if isinstance(statement, fparser.statements.Public):
                if len(statement.items)==0:
                    default_public=True
                elif name in statement.items:
                    declared_public=True
            if isinstance(statement, fparser.block_statements.Type) \
               and statement.name == name and statement.is_public():
                    declared_public=True
        if declared_private or (not default_public and not declared_public):
            raise ParseError("Kernel type '%s' is not public" % name)

    def getKernelMetadata(self,name, ast):
        ktype = None
        for statement, depth  in fpapi.walk(ast, -1):
            if isinstance(statement, fparser.block_statements.Type) \
               and statement.name == name:
                ktype = statement
        if ktype is None:
            raise RuntimeError("Kernel type %s does not exist" % name)
        return ktype

class DynKernelType(KernelType):
    def __init__(self, ast, name=None):
        KernelType.__init__(self,ast, name=name)
        self._arg_descriptors=[]
        for init in self._inits:
            if init.name != 'arg_type':
                raise ParseError("Each meta_arg value must be of type 'arg_type' for the dynamo0.1 api, but found '{0}'".format(init.name))
            access=init.args[0].name
            funcspace=init.args[1].name
            stencil=init.args[2].name
            x1=init.args[3].name
            x2=init.args[4].name
            x3=init.args[5].name
            self._arg_descriptors.append(DynDescriptor(access,funcspace,stencil,x1,x2,x3))

class GOKernelType(KernelType):
    def __init__(self, ast, name=None):
        KernelType.__init__(self, ast, name=name)
        self._arg_descriptors=[]
        for init in self._inits:
            if init.name != 'arg':
                raise ParseError("Each meta_arg value must be of type 'arg' for the gocean0.1 api, but found '{0}'".format(init.name))
            access=init.args[0].name
            funcspace=init.args[1].name
            stencil=init.args[2].name
            if len(init.args) != 3:
                raise ParseError("'arg' type expects 3 arguments but found '{}' in '{}'".format(str(len(init.args)), init.args))
            self._arg_descriptors.append(GODescriptor(access,funcspace,stencil))


class GHProtoKernelType(KernelType):

    def __init__(self, ast, name=None):
        KernelType.__init__(self, ast, name=name)
        self._arg_descriptors = []
        for init in self._inits:
            if init.name != 'arg':
                raise ParseError("Each meta_arg value must be of type 'arg' for the GungHo prototype API, but found '"+init.name+"'")
            if len(init.args) != 3:
                raise ParseError("'arg' type expects 3 arguments but found '{}' in '{}'".format(str(len(init.args)), init.args))
            self._arg_descriptors.append(GHProtoDescriptor(init.args[0].name,
                                         str(init.args[1]),
                                         init.args[2].name))

class InfCall(object):
    """An infrastructure call (appearing in
    `call invoke(kernel_name(field_name, ...))`"""
    def __init__(self, module_name,func_name, args):
        self._module_name = module_name
        self._args = args
        self._func_name=func_name

    @property
    def args(self):
        return self._args

    @property
    def module_name(self):
        return self._module_name

    @property
    def func_name(self):
        return self._func_name

    @property
    def type(self):
        return "InfrastructureCall"

    def __repr__(self):
        return 'InfrastructureCall(%s, %s)' % (self.module_name, self.args)


class KernelCall(object):
    """A kernel call (appearing in
    `call invoke(kernel_name(field_name, ...))`"""

    def __init__(self, module_name, ktype, args):
        self._module_name = module_name
        self._ktype = ktype
        self._args = args
        if len(self._args) < self._ktype.nargs:
            # we cannot test for equality here as API's may have extra arguments
            # passed in from the algorithm layer (e.g. 'QR' in dynamo0.3), but
            # we do expect there to be at least the same number of real
            # arguments as arguments specified in the metadata.
            raise ParseError("Kernel '{0}' called from the algorithm layer with an insufficient number of arguments as specified by the metadata. Expected at least '{1}' but found '{2}'.".format(self._ktype.name, self._ktype.nargs, len(self._args)))

    @property
    def ktype(self):
        return self._ktype

    @property
    def args(self):
        return self._args

    @property
    def module_name(self):
        return self._module_name

    @property
    def type(self):
        return "kernelCall"

    def __repr__(self):
        return 'KernelCall(%s, %s)' % (self.ktype, self.args)
        
class Arg(object):
    ''' Descriptions of an argument '''
    def __init__(self,form,text,varName=None):
        formOptions=["literal","variable","indexed_variable"]
        self._form=form
        self._text=text
        self._varName=varName
        if form not in formOptions:
            raise ParseError("Unknown arg type provided. Expected one of {0} but found {1}".format(str(formOptions),form))
    def __str__(self):
        return "Arg(form='{0}',text='{1}',varName='{2}'".format(self._form, self._text, str(self._varName))
    @property
    def form(self):
        return self._form
    @property
    def text(self):
        return self._text
    @property
    def varName(self):
        return self._varName
    def is_literal(self):
        if self._form=="literal":
            return True
        return False

class InvokeCall(object):
    def __init__(self, kcalls, name=None, myid=1, invoke_name="invoke"):
        self._kcalls = kcalls
        self._name=name

    @property
    def name(self):
        """Return the name of this invoke call"""
        return self._name

    @property
    def kcalls(self):
        """Return the list of kernel calls in this invoke call"""
        return self._kcalls

class FileInfo(object):
    def __init__(self,name,calls):
        self._name=name
        self._calls=calls
    @property
    def name(self):
        return self._name
    @property
    def calls(self):
        return self._calls

def parse(alg_filename, api="", invoke_name="invoke", inf_name="inf", 
          kernel_path="", line_length=False):
    '''Takes a GungHo algorithm specification as input and outputs an AST of this specification and an object containing information about the invocation calls in the algorithm specification and any associated kernel implementations.

    :param str alg_filename: The file containing the algorithm specification.
    :param str invoke_name: The expected name of the invocation calls in the algorithm specification
    :param str inf_name: The expected module name of any required infrastructure routines.
    :param str kernel_path: The path to search for kernel source files (if different from the location of the algorithm source).
    :param bool line_length: A logical flag specifying whether we
                             care about line lengths being longer
                             than 132 characters. If so, the input
                             (algorithm and kernel) code is checked
                             to make sure that it conforms and an
                             error raised if not. The default is
                             False.
    :rtype: ast,invoke_info
    :raises IOError: if the filename or search path does not exist
    :raises ParseError: if there is an error in the parsing
    :raises RuntimeError: if there is an error in the parsing

    For example:

    >>> from parse import parse
    >>> ast,info=parse("argspec.F90")

    '''
    if api=="":
        from config import DEFAULTAPI
        api=DEFAULTAPI
    else:
        from config import SUPPORTEDAPIS
        if api not in SUPPORTEDAPIS:
            raise ParseError("parse: Unsupported API '{0}' specified. Supported types are {1}.".format(api, SUPPORTEDAPIS))

    from pyparsing import ParseException

    # drop cache
    fparser.parsefortran.FortranParser.cache.clear()
    fparser.logging.disable('CRITICAL')
    if not os.path.isfile(alg_filename):
        raise IOError("File %s not found" % alg_filename)
    try:
        ast = fpapi.parse(alg_filename, ignore_comments = False, analyze = False)
        # ast includes an extra comment line which contains file
        # details. This line can be long which can cause line length
        # issues. Therefore set the information (name) to be empty.
        ast.name = ""
    except:
        import traceback
        traceback.print_exc()
	raise ParseError("Fatal error in external fparser tool")
    if line_length:
        fll = FortLineLength()
        with open (alg_filename, "r") as myfile:
            code_str=myfile.read()
        if fll.long_lines(code_str):
            raise ParseError(
                "parse: the algorithm file does not conform to the specified"
                " {0} line length limit".format(str(fll.length)))
        
    name_to_module = {}
    try:
        from collections import OrderedDict
    except:
        try:
            from ordereddict import OrderedDict
        except:
            import sys
            python_version=sys.version_info
            if python_version[0]<=2 and python_version[1]<7:
                raise ParseError("OrderedDict not provided natively pre python 2.7 (you are running {0}. Try installing with 'sudo pip install ordereddict'".format(python_version))
            else:
                raise ParseError("OrderedDict not found which is unexpected as it is meant to be part of the Python library from 2.7 onwards")
    invokecalls = OrderedDict()

    container_name=None
    for child in ast.content:
        if isinstance(child,fparser.block_statements.Program) or \
           isinstance(child,fparser.block_statements.Module) or \
           isinstance(child,fparser.block_statements.Subroutine):
            container_name=child.name
            break
    if container_name is None:
        raise ParseError("Error, program, module or subroutine not found in ast")

    for statement, depth in fpapi.walk(ast, -1):
        if isinstance(statement, fparser.statements.Use):
            for name in statement.items:
                name_to_module[name] = statement.name
        if isinstance(statement, fparser.statements.Call) \
           and statement.designator == invoke_name:
            statement_kcalls = []
            for arg in statement.items:
                try:
                    parsed = expr.expression.parseString(arg)[0]
                except ParseException:
                    raise ParseError("Failed to parse string: {0}".format(arg))

                argname = parsed.name
                argargs=[]
                for a in parsed.args:
                    if type(a) is str: # a literal is being passed by argument
                        argargs.append(Arg('literal',a))
                    else: # assume argument parsed as a FunctionVar
                        variableName = a.name
                        if a.args is not None:
                            # argument is an indexed array so extract the full text
                            fullText = ""
                            for tok in a.walk_skipping_name():
                                fullText+=str(tok)
                            argargs.append(Arg('indexed_variable',fullText,variableName))
                        else:
                            # argument is a standard variable
                            argargs.append(Arg('variable',variableName,variableName))
                if argname in ['set']: # this is an infrastructure call
                    statement_kcalls.append(InfCall(inf_name,argname,argargs))
                else:
                    try:
                        modulename = name_to_module[argname]
                    except KeyError:
                        raise ParseError("kernel call '%s' must be named in a use statement" % argname)

                    # Search for the file containing the kernel source
                    import fnmatch

                    # We only consider files with the suffixes .f90 and .F90
                    # when searching for the kernel source.
                    search_string = "{0}.[fF]90".format(modulename)

                    # Our list of matching files (should have length == 1)
                    matches = []

                    # If a search path has been specified then we look there.
                    # Otherwise we look in the directory containing the 
                    # algorithm definition file
                    if len(kernel_path) > 0:
                        cdir = os.path.abspath(kernel_path)

                        if not os.access(cdir, os.R_OK):
                            raise IOError("Supplied kernel search path does not "
                                          "exist or cannot be read: {0}".\
                                          format(cdir))

                        # We recursively search down through the directory
                        # tree starting at the specified path
                        if os.path.exists(cdir):
                            for root, dirnames, filenames in os.walk(cdir):
                                for filename in fnmatch.filter(filenames, 
                                                               search_string):
                                    matches.append(os.path.join(root, filename))

                    else:
                        # We look *only* in the directory that contained the 
                        # algorithm file
                        cdir = os.path.abspath(os.path.dirname(alg_filename))
                        filenames = os.listdir(cdir)
                        for filename in fnmatch.filter(filenames, 
                                                       search_string):
                                    matches.append(os.path.join(cdir, filename))

                    # Check that we only found one match
                    if len(matches) != 1:
                        if len(matches) == 0:
                            raise IOError("Kernel file '{0}.[fF]90' not found in {1}".\
                                          format(modulename, cdir))
                        else:
                            raise IOError("More than one match for kernel file "
                                          "'{0}.[fF]90' found!".
                                          format(modulename))
                    else:
                        try:
                            modast = fpapi.parse(matches[0])
                            # ast includes an extra comment line which
                            # contains file details. This line can be
                            # long which can cause line length
                            # issues. Therefore set the information
                            # (name) to be empty.
                            modast.name = ""
                        except:
                            raise ParseError("Failed to parse kernel code "
                                             "'{0}'. Is the Fortran correct?".
                                             format(matches[0]))
                        if line_length:
                            fll = FortLineLength()
                            with open (matches[0], "r") as myfile:
                                code_str=myfile.read()
                            if fll.long_lines(code_str):
                                raise ParseError(
                                    "parse: the kernel file '{0}' does not"
                                    " conform to the specified {1} line length"
                                    " limit".format(modulename, str(fll.length)))

                    statement_kcalls.append(KernelCall(modulename, 
                                                       KernelTypeFactory(api=api).\
                                                       create(modast, name=argname),
                                                       argargs))
            invokecalls[statement] = InvokeCall(statement_kcalls)
    return ast, FileInfo(container_name,invokecalls)
