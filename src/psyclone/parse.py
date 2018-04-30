# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Modifications copyright (c) 2017, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author L. Mitchell Imperial College
# Modified by R. W. Ford STFC Daresbury Lab
#     "       A. R. Porter STFC Daresbury Lab

''' Module implementing classes populated by parsing either kernel
    meta-data or invoke()'s in the Algorithm layer '''

import os
from pyparsing import ParseException
import fparser
from fparser.one import parsefortran
from fparser import one as fparser1
from fparser import api as fpapi
import psyclone.expression as expr
from psyclone.line_length import FortLineLength
from psyclone import config


def check_api(api):
    ''' Check that the supplied API is valid '''
    from config import SUPPORTEDAPIS
    if api not in SUPPORTEDAPIS:
        raise ParseError(
            "check_api: Unsupported API '{0}' specified. "
            "Supported types are {1}.".format(api,
                                              SUPPORTEDAPIS))


def get_builtin_defs(api):
    '''Get the names of the supported built-in operations
    and the file containing the associated meta-data for the supplied API '''

    # Check that the supplied API is valid
    check_api(api)

    if api == "dynamo0.3":
        from psyclone.dynamo0p3_builtins import BUILTIN_MAP as builtins
        from psyclone.dynamo0p3_builtins import BUILTIN_DEFINITIONS_FILE as \
            fname
    else:
        # We don't support any built-ins for this API
        builtins = []
        fname = None
    return builtins, fname


def get_mesh(metadata, valid_mesh_types):
    '''
    Returns the mesh-type described by the supplied meta-data
    :param  metadata: node in parser ast
    :type metadata: py:class:`psyclone.expression.NamedArg`
    :param valid_mesh_types: List of valid mesh types
    :type valid_mesh_types: list of strings
    :return: the name of the mesh
    :rtype: string
    :raises ParseError: if the supplied meta-data is not a recognised
                        mesh identifier
    '''
    if not isinstance(metadata, expr.NamedArg) or \
       metadata.name.lower() != "mesh_arg":
        raise ParseError(
            "{0} is not a valid mesh identifier (expected "
            "mesh_arg=MESH_TYPE where MESH_TYPE is one of {1}))".
            format(str(metadata), valid_mesh_types))
    mesh = metadata.value.lower()
    if mesh not in valid_mesh_types:
        raise ParseError("mesh_arg must be one of {0} but got {1}".
                         format(valid_mesh_types, mesh))
    return mesh


def get_stencil(metadata, valid_types):
    '''
    Returns stencil_type and stencil_extent as a dictionary
    object from stencil metadata if the metadata conforms to the
    stencil(type[,extent]) format

    :param metadata: Component of kernel meta-data stored as a node in the
                     parser AST
    :type metadata: :py:class:`psyclone.expression.FunctionVar`
    :param list valid_types: List of valid stencil types (strings)
    :return: The stencil type and extent described in the meta-data
    :rtype: dict with keys 'type' (str) and 'extent' (int)

    :raises ParseError: if the supplied meta-data is not a recognised
                        stencil specification
    '''

    if not isinstance(metadata, expr.FunctionVar):
        raise ParseError(
            "Expecting format stencil(<type>[,<extent>]) but found the "
            "literal {0}".format(metadata))
    if metadata.name.lower() != "stencil" or not metadata.args:
        raise ParseError(
            "Expecting format stencil(<type>[,<extent>]) but found {0}".
            format(metadata))
    if len(metadata.args) > 2:
        raise ParseError(
            "Expecting format stencil(<type>[,<extent>]) but there must "
            "be at most two arguments inside the brackets {0}".
            format(metadata))
    if not isinstance(metadata.args[0], expr.FunctionVar):
        if isinstance(metadata.args[0], str):
            raise ParseError(
                "Expecting format stencil(<type>[,<extent>]). However, "
                "the specified <type> '{0}' is a literal and therefore is "
                "not one of the valid types '{1}'".
                format(metadata.args[0], valid_types))
        else:
            raise ParseError(
                "Internal error, expecting either FunctionVar or "
                "str from the expression analyser but found {0}".
                format(type(metadata.args[0])))
    if metadata.args[0].args:
        raise ParseError(
            "Expected format stencil(<type>[,<extent>]). However, the "
            "specified <type> '{0}' includes brackets")
    stencil_type = metadata.args[0].name
    if stencil_type not in valid_types:
        raise ParseError(
            "Expected format stencil(<type>[,<extent>]). However, the "
            "specified <type> '{0}' is not one of the valid types '{1}'".
            format(stencil_type, valid_types))

    stencil_extent = None
    if len(metadata.args) == 2:
        if not isinstance(metadata.args[1], str):
            raise ParseError(
                "Expected format stencil(<type>[,<extent>]). However, the "
                "specified <extent> '{0}' is not an integer".
                format(metadata.args[1]))
        stencil_extent = int(metadata.args[1])
        if stencil_extent < 1:
            raise ParseError(
                "Expected format stencil(<type>[,<extent>]). However, the "
                "specified <extent> '{0}' is less than 1".
                format(str(stencil_extent)))
        raise ParseError(
            "Kernels with fixed stencil extents are not currently "
            "supported")
    return {"type": stencil_type, "extent": stencil_extent}


class ParseError(Exception):
    def __init__(self, value):
        self.value = "Parse Error: " + value

    def __str__(self):
        return repr(self.value)


class Descriptor(object):
    """A description of how a kernel argument is accessed"""
    def __init__(self, access, space, stencil=None, mesh=None):
        '''
        :param string access: whether argument is read/write etc.
        :param string space: which function space/grid-point type
                             argument is on
        :param dict stencil: type of stencil access for this argument
        :param string mesh: which mesh this argument is on
        '''
        self._access = access
        self._space = space
        self._stencil = stencil
        self._mesh = mesh

    @property
    def access(self):
        return self._access

    @property
    def function_space(self):
        return self._space

    @property
    def stencil(self):
        return self._stencil

    @property
    def mesh(self):
        '''
        :return: the mesh the argument is on (or None)
        :rtype: string
        '''
        return self._mesh

    def __repr__(self):
        return 'Descriptor(%s, %s)' % (self.stencil, self.access)


class GODescriptor(Descriptor):
    def __init__(self, access, space, stencil):
        Descriptor.__init__(self, access, space, stencil)


class DynDescriptor(Descriptor):
    def __init__(self, access, funcspace, stencil, basis, diff_basis,
                 gauss_quad):
        Descriptor.__init__(self, access, funcspace, stencil)
        self._basis = basis
        self._diff_basis = diff_basis
        self._gauss_quad = gauss_quad

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
        Descriptor.__init__(self, access, self._space, stencil)

    @property
    def element(self):
        return self._space.element

    def __repr__(self):
        return 'Descriptor(%s, %s, %s)' % (self.stencil, self.element,
                                           self.access)


class FunctionSpace(object):
    @staticmethod
    def unpack(string):
        p = expr.FORT_EXPRESSION.parseString(string)[0]
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
            p = expr.FORT_EXPRESSION.parseString(string_or_expr)[0]
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
        assert all(isinstance(e, Element) for e in elements), \
            "All arguments to build a TensorProductElement should be Elements"
        self._elements = elements

    @property
    def elements(self):
        return self._elements

    def __repr__(self):
        s = " * ".join("%s" % e for e in self.elements)
        return s

    def __mul__(self, other):
        assert isinstance(other, Element), "Can only take tensor products "
        "with Elements not %s" % type(other)
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
            if isinstance(statement, fparser1.statements.SpecificBinding):
                if statement.name == "code" and statement.bname != "":
                    # prototype gungho style
                    bname = statement.bname
                elif statement.name.lower() != "code" \
                        and statement.bname != "":
                    raise ParseError(
                        "Kernel type %s binds to a specific procedure but "
                        "does not use 'code' as the generic name." % name)
                else:
                    # psyclone style
                    bname = statement.name
        if bname is None:
            raise RuntimeError(
                "Kernel type %s does not bind a specific procedure" % name)
        if bname == '':
            raise ParseError(
                "Internal error: empty kernel name returned for Kernel type "
                "%s." % name)
        code = None
        default_public = True
        declared_private = False
        declared_public = False
        for statement, depth in fpapi.walk(modast, -1):
            if isinstance(statement, fparser1.statements.Private):
                if len(statement.items) == 0:
                    default_public = False
                elif bname in statement.items:
                    declared_private = True
            if isinstance(statement, fparser1.statements.Public):
                if len(statement.items) == 0:
                    default_public = True
                elif bname in statement.items:
                    declared_public = True
            if isinstance(statement, fparser1.block_statements.Subroutine) \
               and statement.name == bname:
                if statement.is_public():
                    declared_public = True
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
    ''' Factory for calls to user-supplied Kernels '''

    def __init__(self, api=""):
        if api == "":
            from config import DEFAULTAPI
            self._type = DEFAULTAPI
        else:
            check_api(api)
            self._type = api

    def create(self, ast, name=None):

        if self._type == "gunghoproto":
            return GHProtoKernelType(ast, name=name)
        elif self._type == "dynamo0.1":
            return DynKernelType(ast, name=name)
        elif self._type == "dynamo0.3":
            from psyclone.dynamo0p3 import DynKernMetadata
            return DynKernMetadata(ast, name=name)
        elif self._type == "gocean0.1":
            return GOKernelType(ast, name=name)
        elif self._type == "gocean1.0":
            from psyclone.gocean1p0 import GOKernelType1p0
            return GOKernelType1p0(ast, name=name)
        else:
            raise ParseError(
                "KernelTypeFactory: Internal Error: Unsupported "
                "kernel type '{0}' found. Should not be possible.".
                format(self._type))


class BuiltInKernelTypeFactory(KernelTypeFactory):
    ''' Factory class for calls to built-ins '''

    def create(self, builtin_names, builtin_defs_file, name=None):
        ''' Create a built-in call object '''
        if name not in builtin_names:
            raise ParseError(
                "BuiltInKernelTypeFactory: unrecognised built-in name. "
                "Got '{0}' but expected one of {1}".format(name,
                                                           builtin_names))
        # The meta-data for these lives in a Fortran module file
        # passed in to this method.
        fname = os.path.join(
            os.path.dirname(os.path.abspath(__file__)),
            builtin_defs_file)
        if not os.path.isfile(fname):
            raise ParseError(
                "Kernel '{0}' is a recognised Built-in but cannot "
                "find file '{1}' containing the meta-data describing "
                "the Built-in operations for API '{2}'".format(name,
                                                               fname,
                                                               self._type))
        # Attempt to parse the meta-data
        try:
            ast = fpapi.parse(fname)
        except:
            raise ParseError(
                "Failed to parse the meta-data for PSyclone "
                "built-ins in {0}".format(fname))

        # Now we have the AST, call our parent class to create the object
        return KernelTypeFactory.create(self, ast, name)


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
            for statement, depth in fpapi.walk(ast, -1):
                if isinstance(statement, fparser1.block_statements.Module):
                    module_name = statement.name
                    found = True
                    break
            if not found:
                raise ParseError(
                    "Error KernelType, the file does not contain a module. "
                    "Is it a Kernel file?")

            mn_len = len(module_name)
            if mn_len < 5:
                raise ParseError(
                    "Error, module name '{0}' is too short to have '_mod' as "
                    "an extension. This convention is assumed.".
                    format(module_name))
            base_name = module_name.lower()[:mn_len-4]
            extension_name = module_name.lower()[mn_len-4:mn_len]
            if extension_name != "_mod":
                raise ParseError(
                    "Error, module name '{0}' does not have '_mod' as an "
                    "extension. This convention is assumed.".
                    format(module_name))
            name = base_name + "_type"

        self._name = name
        self._ast = ast
        self.checkMetadataPublic(name, ast)
        self._ktype = self.getKernelMetadata(name, ast)
        self._iterates_over = self.get_integer_variable("iterates_over")
        self._procedure = KernelProcedure(self._ktype, name, ast)
        self._inits = self.getkerneldescriptors(self._ktype)
        self._arg_descriptors = None  # this is set up by the subclasses

    def getkerneldescriptors(self, ast, var_name='meta_args'):
        descs = ast.get_variable(var_name)
        if descs is None:
            raise ParseError(
                "kernel call does not contain a {0} type".format(var_name))
        try:
            nargs = int(descs.shape[0])
        except AttributeError:
            raise ParseError(
                "kernel metadata {0}: {1} variable must be an array".
                format(self._name, var_name))
        if len(descs.shape) is not 1:
            raise ParseError(
                "kernel metadata {0}: {1} variable must be a 1 dimensional "
                "array".format(self._name, var_name))
        if descs.init.find("[") is not -1 and descs.init.find("]") is not -1:
            # there is a bug in f2py
            raise ParseError(
                "Parser does not currently support [...] initialisation for "
                "{0}, please use (/.../) instead".format(var_name))
        try:
            inits = expr.FORT_EXPRESSION.parseString(descs.init)[0]
        except ParseException:
            raise ParseError("kernel metadata has an invalid format {0}".
                             format(descs.init))
        nargs = int(descs.shape[0])
        if len(inits) != nargs:
            raise ParseError(
                "Error, in {0} specification, the number of args {1} and "
                "number of dimensions {2} do not match".
                format(var_name, nargs, len(inits)))
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

    def checkMetadataPublic(self, name, ast):
        default_public = True
        declared_private = False
        declared_public = False
        for statement, depth in fpapi.walk(ast, -1):
            if isinstance(statement, fparser1.statements.Private):
                if len(statement.items) == 0:
                    default_public = False
                elif name in statement.items:
                    declared_private = True
            if isinstance(statement, fparser1.statements.Public):
                if len(statement.items) == 0:
                    default_public = True
                elif name in statement.items:
                    declared_public = True
            if isinstance(statement, fparser1.block_statements.Type) \
               and statement.name == name and statement.is_public():
                    declared_public = True
        if declared_private or (not default_public and not declared_public):
            raise ParseError("Kernel type '%s' is not public" % name)

    def getKernelMetadata(self, name, ast):
        ktype = None
        for statement, depth in fpapi.walk(ast, -1):
            if isinstance(statement, fparser1.block_statements.Type) \
               and statement.name == name:
                ktype = statement
        if ktype is None:
            raise RuntimeError("Kernel type %s does not exist" % name)
        return ktype

    def get_integer_variable(self, name):
        ''' Parse the kernel meta-data and find the value of the
        integer variable with the supplied name. Return None if no
        matching variable is found.'''
        for statement, _ in fpapi.walk(self._ktype, -1):
            if isinstance(statement, fparser1.typedecl_statements.Integer):
                # fparser only goes down to the statement level. We use
                # the expression parser (expression.py) to parse the
                # statement itself.
                assign = expr.FORT_EXPRESSION.parseString(
                    statement.entity_decls[0])
                if assign[0].name == name:
                    return assign[0].value
        return None


class DynKernelType(KernelType):
    def __init__(self, ast, name=None):
        KernelType.__init__(self, ast, name=name)
        self._arg_descriptors = []
        for init in self._inits:
            if init.name != 'arg_type':
                raise ParseError(
                    "Each meta_arg value must be of type 'arg_type' for the "
                    "dynamo0.1 api, but found '{0}'".format(init.name))
            access = init.args[0].name
            funcspace = init.args[1].name
            stencil = init.args[2].name
            x1 = init.args[3].name
            x2 = init.args[4].name
            x3 = init.args[5].name
            self._arg_descriptors.append(DynDescriptor(access, funcspace,
                                                       stencil, x1, x2, x3))


class GOKernelType(KernelType):
    def __init__(self, ast, name=None):
        KernelType.__init__(self, ast, name=name)
        self._arg_descriptors = []
        for init in self._inits:
            if init.name != 'arg':
                raise ParseError(
                    "Each meta_arg value must be of type 'arg' for the "
                    "gocean0.1 api, but found '{0}'".format(init.name))
            access = init.args[0].name
            funcspace = init.args[1].name
            stencil = init.args[2].name
            if len(init.args) != 3:
                raise ParseError(
                    "'arg' type expects 3 arguments but found '{}' in '{}'".
                    format(str(len(init.args)), init.args))
            self._arg_descriptors.append(GODescriptor(access, funcspace,
                                                      stencil))


class GHProtoKernelType(KernelType):

    def __init__(self, ast, name=None):
        KernelType.__init__(self, ast, name=name)
        self._arg_descriptors = []
        for init in self._inits:
            if init.name != 'arg':
                raise ParseError(
                    "Each meta_arg value must be of type 'arg' for the GungHo "
                    "prototype API, but found '" + init.name + "'")
            if len(init.args) != 3:
                raise ParseError(
                    "'arg' type expects 3 arguments but found '{}' in '{}'".
                    format(str(len(init.args)), init.args))
            self._arg_descriptors.append(GHProtoDescriptor(init.args[0].name,
                                         str(init.args[1]),
                                         init.args[2].name))


class ParsedCall(object):
    ''' A call to either a user-supplied kernel or a built-in appearing
    in an invoke. '''

    def __init__(self, ktype, args):
        self._ktype = ktype
        self._args = args
        if len(self._args) < self._ktype.nargs:
            # we cannot test for equality here as API's may have extra
            # arguments passed in from the algorithm layer (e.g. 'QR'
            # in dynamo0.3), but we do expect there to be at least the
            # same number of real arguments as arguments specified in
            # the metadata.
            raise ParseError(
                "Kernel '{0}' called from the algorithm layer with an "
                "insufficient number of arguments as specified by the "
                "metadata. Expected at least '{1}' but found '{2}'.".
                format(self._ktype.name, self._ktype.nargs, len(self._args)))

    @property
    def ktype(self):
        return self._ktype

    @property
    def args(self):
        return self._args

    @property
    def module_name(self):
        return self._module_name


class KernelCall(ParsedCall):
    """A call to a user-supplied kernel (appearing in
    `call invoke(kernel_name(field_name, ...))`"""

    def __init__(self, module_name, ktype, args):
        ParsedCall.__init__(self, ktype, args)
        self._module_name = module_name

    @property
    def type(self):
        return "kernelCall"

    def __repr__(self):
        return 'KernelCall(%s, %s)' % (self.ktype, self.args)


class BuiltInCall(ParsedCall):
    ''' A built-in call (appearing in
    `call invoke(kernel_name(field_name, ...))` '''

    def __init__(self, ktype, args):
        ParsedCall.__init__(self, ktype, args)
        self._func_name = ktype.name

    @property
    def func_name(self):
        return self._func_name

    @property
    def type(self):
        return "BuiltInCall"

    def __repr__(self):
        return 'BuiltInCall(%s, %s)' % (self.args)


class Arg(object):
    ''' Description of an argument as obtained from parsing the Fortran code
        where a kernel is invoke'd '''
    def __init__(self, form, text, varName=None):
        formOptions = ["literal", "variable", "indexed_variable"]
        self._form = form
        self._text = text
        # Replace any '%' chars in the supplied name with underscores so
        # as to have a valid Fortran variable name (in the PSy layer).
        if varName:
            self._varName = varName.replace("%", "_")
        else:
            self._varName = None
        if form not in formOptions:
            raise ParseError(
                "Unknown arg type provided. Expected one of {0} but found "
                "{1}".format(str(formOptions), form))

    def __str__(self):
        return "Arg(form='{0}',text='{1}',varName='{2}'". \
            format(self._form, self._text, str(self._varName))

    @property
    def form(self):
        return self._form

    @property
    def text(self):
        return self._text

    @property
    def varName(self):
        return self._varName

    @varName.setter
    def varName(self, value):
        ''' sets the varName value '''
        self._varName = value

    def is_literal(self):
        if self._form == "literal":
            return True
        return False


class InvokeCall(object):
    def __init__(self, kcalls, name=None, myid=1, invoke_name="invoke"):
        self._kcalls = kcalls
        if name:
            # Prefix the name with "invoke_" unless it already starts
            # with that...
            if not name.lower().startswith("invoke_"):
                self._name = "invoke_" + name.lower()
            else:
                self._name = name.lower()
        else:
            self._name = None

    @property
    def name(self):
        """Return the name of this invoke call"""
        return self._name

    @property
    def kcalls(self):
        """Return the list of kernel calls in this invoke call"""
        return self._kcalls


class FileInfo(object):
    def __init__(self, name, calls):
        self._name = name
        self._calls = calls

    @property
    def name(self):
        return self._name

    @property
    def calls(self):
        return self._calls


def parse(alg_filename, api="", invoke_name="invoke", inf_name="inf",
          kernel_path="", line_length=False,
          distributed_memory=config.DISTRIBUTED_MEMORY):
    '''Takes a GungHo algorithm specification as input and outputs an AST of
    this specification and an object containing information about the
    invocation calls in the algorithm specification and any associated kernel
    implementations.

    :param str alg_filename: The file containing the algorithm specification.
    :param str invoke_name: The expected name of the invocation calls in the
                            algorithm specification
    :param str inf_name: The expected module name of any required
                         infrastructure routines.
    :param str kernel_path: The path to search for kernel source files (if
                            different from the location of the algorithm
                            source).
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

    if distributed_memory not in [True, False]:
        raise ParseError(
            "The distributed_memory flag in parse() must be set to"
            " 'True' or 'False'")
    config.DISTRIBUTED_MEMORY = distributed_memory
    if api == "":
        from psyclone.config import DEFAULTAPI
        api = DEFAULTAPI
    else:
        check_api(api)

    # Get the names of the supported Built-in operations for this API
    builtin_names, builtin_defs_file = get_builtin_defs(api)

    # drop cache
    fparser1.parsefortran.FortranParser.cache.clear()
    fparser.logging.disable('CRITICAL')
    if not os.path.isfile(alg_filename):
        raise IOError("File %s not found" % alg_filename)
    try:
        ast = fpapi.parse(alg_filename, ignore_comments=False,
                          analyze=False)
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
        with open(alg_filename, "r") as myfile:
            code_str = myfile.read()
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
            python_version = sys.version_info
            if python_version[0] <= 2 and python_version[1] < 7:
                raise ParseError(
                    "OrderedDict not provided natively pre python 2.7 "
                    "(you are running {0}. Try installing with 'sudo "
                    "pip install ordereddict'".format(python_version))
            else:
                raise ParseError(
                    "OrderedDict not found which is unexpected as it is "
                    "meant to be part of the Python library from 2.7 onwards")
    invokecalls = OrderedDict()
    # Keep a list of the named invokes so that we can check that the same
    # name isn't used more than once
    unique_invoke_labels = []
    container_name = None
    for child in ast.content:
        if isinstance(child, fparser1.block_statements.Program) or \
           isinstance(child, fparser1.block_statements.Module) or \
           isinstance(child, fparser1.block_statements.Subroutine):
            container_name = child.name
            break
    if container_name is None:
        raise ParseError(
            "Error, program, module or subroutine not found in ast")

    for statement, depth in fpapi.walk(ast, -1):
        if isinstance(statement, fparser1.statements.Use):
            for name in statement.items:
                name_to_module[name] = statement.name
        if isinstance(statement, fparser1.statements.Call) \
           and statement.designator == invoke_name:
            statement_kcalls = []
            invoke_label = None
            for arg in statement.items:
                # We expect each item in an invoke call to be either a
                # call to a kernel or the name of the invoke (specifed
                # as name="my_name")
                try:
                    parsed = expr.FORT_EXPRESSION.parseString(arg)[0]
                except ParseException:
                    raise ParseError("Failed to parse string: {0}".format(arg))

                if isinstance(parsed, expr.NamedArg):
                    if parsed.name.lower() != "name":
                        raise ParseError(
                            "The arguments to an invoke() must be either "
                            "kernel calls or an (optional) name='invoke-name' "
                            "but got '{0}' in file {1}".format(str(parsed),
                                                               alg_filename))
                    if invoke_label:
                        raise ParseError(
                            "An invoke must contain one or zero 'name=xxx' "
                            "arguments but found more than one in: {0} in "
                            "file {1}".
                            format(str(statement), alg_filename))
                    if not parsed.is_string:
                        raise ParseError(
                            "The (optional) name of an invoke must be "
                            "specified as a string but got {0} in file {1}.".
                            format(str(parsed), alg_filename))
                    # Store the supplied label. Use lower-case (as Fortran
                    # is case insensitive)
                    invoke_label = parsed.value.lower()
                    # Remove any spaces.
                    # TODO check with LFRic team - should we raise an error if
                    # it contains spaces?
                    invoke_label = invoke_label.replace(" ", "_")
                    # Check that the resulting label is a valid Fortran Name
                    from fparser.two import pattern_tools
                    if not pattern_tools.abs_name.match(invoke_label):
                        raise ParseError(
                            "The (optional) name of an invoke must be a "
                            "string containing a valid Fortran name (with "
                            "any spaces replaced by underscores) but "
                            "got '{0}' in file {1}".format(invoke_label,
                                                           alg_filename))
                    # Check that it's not already been used in this Algorithm
                    if invoke_label in unique_invoke_labels:
                        raise ParseError(
                            "Found multiple named invoke()'s with the same "
                            "name ('{0}') when parsing {1}".
                            format(invoke_label, alg_filename))
                    unique_invoke_labels.append(invoke_label)
                    # Continue parsing the other arguments to this invoke call
                    continue

                argname = parsed.name
                argargs = []
                for a in parsed.args:
                    if type(a) is str:  # a literal is being passed by argument
                        argargs.append(Arg('literal', a))
                    else:  # assume argument parsed as a FunctionVar
                        variableName = a.name
                        if a.args is not None:
                            # argument is an indexed array so extract the
                            # full text
                            fullText = ""
                            for tok in a.walk_skipping_name():
                                fullText += str(tok)
                            argargs.append(Arg('indexed_variable', fullText,
                                               variableName))
                        else:
                            # argument is a standard variable
                            argargs.append(Arg('variable',
                                               variableName,
                                               variableName))
                if argname in builtin_names:
                    if argname in name_to_module:
                        raise ParseError("A built-in cannot be named in a use "
                                         "statement but '{0}' is used from "
                                         "module '{1}' in file {2}".
                                         format(argname,
                                                name_to_module[argname],
                                                alg_filename))
                    # this is a call to a built-in operation. The
                    # KernelTypeFactory will generate appropriate meta-data
                    statement_kcalls.append(
                        BuiltInCall(
                            BuiltInKernelTypeFactory(api=api).create(
                                builtin_names, builtin_defs_file,
                                name=argname),
                            argargs))
                else:
                    try:
                        modulename = name_to_module[argname]
                    except KeyError:
                        raise ParseError(
                            "kernel call '{0}' must either be named in a use "
                            "statement or be a recognised built-in "
                            "(one of '{1}' for this API)".
                            format(argname, builtin_names))

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
                            raise IOError(
                                "Supplied kernel search path does not exist "
                                "or cannot be read: {0}".format(cdir))

                        # We recursively search down through the directory
                        # tree starting at the specified path
                        if os.path.exists(cdir):
                            for root, dirnames, filenames in os.walk(cdir):
                                for filename in fnmatch.filter(filenames,
                                                               search_string):
                                    matches.append(os.path.join(root,
                                                                filename))

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
                            raise IOError(
                                "Kernel file '{0}.[fF]90' not found in {1}".
                                format(modulename, cdir))
                        else:
                            raise IOError(
                                "More than one match for kernel file "
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
                            with open(matches[0], "r") as myfile:
                                code_str = myfile.read()
                            if fll.long_lines(code_str):
                                raise ParseError(
                                    "parse: the kernel file '{0}' does not"
                                    " conform to the specified {1} line length"
                                    " limit".format(modulename,
                                                    str(fll.length)))

                    statement_kcalls.append(
                        KernelCall(modulename, KernelTypeFactory(api=api).
                                   create(modast, name=argname),
                                   argargs))
            invokecalls[statement] = InvokeCall(statement_kcalls,
                                                name=invoke_label)
    return ast, FileInfo(container_name, invokecalls)
