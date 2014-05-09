# Copyright 2013 Imperial College London, all rights reserved
# Copyright 2013 STFC, all rights reserved
import fparser
from fparser import api, parsefortran
import expression as expr
import logging
import os

class ParseError(Exception):
    def __init__(self, value):
        self.value = "Parse Error: "+value
    def __str__(self):
        return repr(self.value)

class Descriptor(object):
    """A description of how a kernel argument is accessed"""
    def __init__(self, stencil, space, access):
        self._stencil = stencil # topological relationships
        self._space = FunctionSpace.unpack(space)
        self._access = access

    @property
    def stencil(self):
        return self._stencil

    @property
    def element(self):
        return self._space.element

    @property
    def function_space(self):
        return self._space

    @property
    def access(self):
        return self._access

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
        for statement, depth in api.walk(modast, -1):
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


class KernelType(object):
    """A description of a kernel type.

    This contains the elemental procedure and metadata associated with
    how that procedure is mapped over mesh entities."""
    def __init__(self, name, ast):
        self._name = name
        self._ast = ast
        i, p, d = KernelType.parse_kernel_type(name, ast)
        self._procedure = p
        self._arg_descriptors = d
        self._iterates_over = i

    @staticmethod
    def get_kernel_descriptors(ast):
        descs = ast.get_variable('meta_args') # name to be decided
        if descs is None:
            raise ParseError("kernel call does not contain a meta_args type")
        try:
            nargs=int(descs.shape[0])
        except AttributeError as e:
            raise ParseError("kernel call meta_args variable must be an array")
        if len(descs.shape) is not 1:
            raise ParseError("kernel call meta_args variable must be a 1 dimensional array")
        if descs.init.find("[") is not -1 and descs.init.find("]") is -1:
            # there is a bug in f2py
            raise ParseError("Parser does not currently support [...] initialisation for meta_args, please use (/.../) instead")
        inits = expr.expression.parseString(descs.init)[0]
        if len(inits) != nargs:
            raise ParseError("Error, in meta_args specification, the number of args %s and number of dimensions %s do not match" % (nargs,len(inits)))
        ret = []
        for init in inits:
            if init.name != 'arg':
                raise ParseError("Each meta_arg value must be of type 'arg', but found '{}'".format(init.name))
            if len(init.args) != 3:
                raise ParseError("'arg' type expects 3 arguments but found '{}' in '{}'".format(str(len(init.args)), init.args))
            ret.append(Descriptor(init.args[0].name,
                                  str(init.args[1]),
                                  init.args[2].name))
        return ret

    @staticmethod
    def parse_kernel_type(name, ast):
        ktype = None
        default_public=True
        declared_private=False
        declared_public=False
        for statement, depth  in api.walk(ast, -1):
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
               and statement.name == name:
                ktype = statement
                if statement.is_public():
                    declared_public=True
        if ktype is None:
            raise RuntimeError("Kernel type %s not implemented" % name)
        if declared_private or (not default_public and not declared_public):
            raise ParseError("Kernel type '%s' is not public" % name)
        iterates = ktype.get_variable('iterates_over').init
        proc = KernelProcedure(ktype, name, ast)
        try:
            descs = KernelType.get_kernel_descriptors(ktype)
        except Exception as e:
            print "Location: '"+name+"'"
            raise e
        return iterates, proc, descs

    @property
    def nargs(self):
        return len(self.arg_descriptors)

    @property
    def procedure(self):
        return self._procedure

    @property
    def arg_descriptors(self):
        return self._arg_descriptors

    @property
    def name(self):
        return self._name

    @property
    def iterates_over(self):
        return self._iterates_over

    def __repr__(self):
        return 'KernelType(%s, %s)' % (self.name, self.iterates_over)

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
        if self._ktype.nargs != len(self._args):
            raise ParseError("Kernel %s called with incorrect number of arguments (%d not %d)" % (self._ktype, len(self._args), self._ktype.nargs))

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
    def __init__(self,form,value):
        formOptions=["literal","variable"]
        self._form=form
        self._value=value
        if form not in formOptions:
            raise ParseError("Unknown arg type provided. Expected one of {0} but found {1}".format(str(formOptions),form))
    @property
    def form(self):
        return self._form
    @property
    def value(self):
        return self._value
    def isLiteral(self):
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

def parse(filename, invoke_name="invoke", inf_name="inf"):
    '''
    Takes a GungHo algorithm specification as input and outputs an AST of this specification and an object containing information about the invocation calls in the algorithm specification and any associated kernel implementations.

    :param str filename: The file containing the algorithm specification.
    :param str invoke_name: The expected name of the invocation calls in the algorithm specification
    :param str inf_name: The expected module name of any required infrastructure routines.
    :rtype: ast,invoke_info
    :raises IOError: if the filename does not exist
    :raises ParseError: if there is an error in the parsing
    :raises RuntimeError: if there is an error in the parsing

    For example:

    >>> from parse import parse
    >>> ast,info=parse("argspec.F90")

    '''
    # drop cache
    fparser.parsefortran.FortranParser.cache.clear()
    fparser.logging.disable('CRITICAL')
    if not os.path.isfile(filename):
        raise IOError("File %s not found" % filename)
    ast = api.parse(filename, ignore_comments=False)
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

    for statement, depth in api.walk(ast, -1):
        if isinstance(statement, fparser.statements.Use):
            for name in statement.items:
                name_to_module[name] = statement.name
        if isinstance(statement, fparser.statements.Call) \
           and statement.designator == invoke_name:
            statement_kcalls = []
            for arg in statement.items:
                parsed = expr.expression.parseString(arg)[0]
                argname = parsed.name
                argargs=[]
                for a in parsed.args:
                    if type(a) is str: # a literal is being passed by argument
                        argargs.append(Arg('literal',a))
                    else: # assume argument parsed as a FunctionVar
                        argargs.append(Arg('variable',a.name))
                if argname in ['set']: # this is an infrastructure call
                    statement_kcalls.append(InfCall(inf_name,argname,argargs))
                else:
                    try:
                        modulename = name_to_module[argname]
                    except KeyError:
                        raise ParseError("kernel call '%s' must be named in a use statement" % argname)
                    if not os.path.isfile('%s.F90' % modulename):
                        if not os.path.isfile('%s.f90' % modulename):
                            raise IOError("Kernel file '%s.[fF]90' not found" % modulename)
                        else:
                            modast = api.parse('%s.f90' % modulename)
                    else:
                        modast = api.parse('%s.F90' % modulename)
                    statement_kcalls.append(KernelCall(modulename, KernelType(argname, modast),
                                                   argargs))
            invokecalls[statement] = InvokeCall(statement_kcalls)
    return ast, FileInfo(container_name,invokecalls)
