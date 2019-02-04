# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter STFC Daresbury Lab

'''Module that uses the Fortran parser fparser2 to parse
PSyclone-conformant Algorithm code.

'''

from psyclone.configuration import Config
from psyclone.parse_orig import Arg

def check_api(api):
    '''
    Check that the supplied API is valid.
    :param str api: The API to check.
    :raises ParseError: if the supplied API is not recognised.

    '''
    _config = Config.get()

    if api not in _config.supported_apis:
        raise ParseError(
            "check_api: Unsupported API '{0}' specified. "
            "Supported types are {1}.".format(api,
                                              _config.supported_apis))


def parse_fp2(filename):
    '''
    Parse a Fortran source file using fparser2.

    :param str filename: source file (including path) to read.
    :returns: fparser2 AST for the source file.
    :rtype: :py:class:`fparser.two.Fortran2003.Program`
    '''
    from fparser.common.readfortran import FortranFileReader
    from fparser.two.parser import ParserFactory
    from fparser.two.utils import FortranSyntaxError

    parser = ParserFactory().create()
    # We get the directories to search for any Fortran include files from
    # our configuration object.
    config = Config.get()
    try:
        reader = FortranFileReader(filename, include_dirs=config.include_paths)
    except IOError as error:
        print (error)
        exit(1)
    try:
        ast = parser(reader)
    except FortranSyntaxError as msg:
        print ("Syntax error: {0}".format(str(msg)))
        exit(1)
    return ast


def check_ll(alg_filename):
    ''' xxx '''
    from psyclone.line_length import FortLineLength
    fll = FortLineLength()
    with open(alg_filename, "r") as myfile:
        code_str = myfile.read()
    if fll.long_lines(code_str):
        raise ParseError(
            "parse: the algorithm file does not conform to the specified"
            " {0} line length limit".format(str(fll.length)))


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
        builtins = {}
        fname = None
    return builtins, fname


class ParseError(Exception):
    ''' xxx '''
    
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
    """
    An elemental Kernel procedure.

    :param ktype_ast: the fparser1 parse tree for the Kernel meta-data.
    :type ktype_ast: :py:class:`fparser.one.block_statements.Type`
    :param str ktype_name: name of the Fortran type holding the Kernel \
                           meta-data.
    :param modast: the fparser1 parse tree for the module containing the \
                   Kernel routine.
    :type modast: :py:class:`fparser.one.block_statements.BeginSource`

    """
    def __init__(self, ktype_ast, ktype_name, modast):
        self._ast, self._name = KernelProcedure.get_procedure(
            ktype_ast, ktype_name, modast)

    @staticmethod
    def get_procedure(ast, name, modast):
        '''
        Get the name of the subroutine associated with the Kernel. This is
        a type-bound procedure in the meta-data which may take one of two
        forms:
                PROCEDURE, nopass :: code => <proc_name>
        or
                PROCEDURE, nopass :: <proc_name>

        :param ast: the fparser1 parse tree for the Kernel meta-data.
        :type ast: :py:class:`fparser.one.block_statements.Type`
        :param str name: the name of the Fortran type holding the Kernel \
                         meta-data.
        :param modast: the fparser1 parse tree for the module containing the \
                       Kernel routine.
        :type modast: :py:class:`fparser.one.block_statements.BeginSource`

        :returns: 2-tuple of the fparser1 parse tree of the Subroutine \
                  statement and the name of that Subroutine.
        :rtype: (:py:class:`fparser1.block_statements.Subroutine`, str)

        :raises RuntimeError: if the supplied Kernel meta-data does not \
                              have a type-bound procedure.
        :raises RuntimeError: if no implementation is found for the \
                              type-bound procedure.
        :raises ParseError: if the type-bound procedure specifies a binding \
                            name but the generic name is not "code".
        :raises ParseError: if the type-bound procedure is not public in the \
                            Fortran module.
        :raises InternalError: if we get an empty string for the name of the \
                               type-bound procedure.
        '''
        bname = None
        # Search the the meta-data for a SpecificBinding
        for statement in ast.content:
            if isinstance(statement, fparser1.statements.SpecificBinding):
                # We support either:
                # PROCEDURE, nopass :: code => <proc_name> or
                # PROCEDURE, nopass :: <proc_name>
                if statement.bname:
                    if statement.name.lower() != "code":
                        raise ParseError(
                            "Kernel type {0} binds to a specific procedure but"
                            " does not use 'code' as the generic name.".
                            format(name))
                    bname = statement.bname
                else:
                    bname = statement.name
                break
        if bname is None:
            raise RuntimeError(
                "Kernel type {0} does not bind a specific procedure".
                format(name))
        if bname == '':
            raise InternalError(
                "Empty Kernel name returned for Kernel type {0}.".format(name))
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
    '''
    Factory for objects in the PSyIR representing calls to user-supplied
    Kernels.

    :param str api: The API for which this factory is to create Kernels.
    '''
    def __init__(self, api=""):
        if api == "":
            _config = Config.get()
            self._type = _config.default_api
        else:
            check_api(api)
            self._type = api

    def create(self, ast, name=None):
        '''
        Create a Kernel object for the API supplied to the constructor
        of this factory.

        :param ast: The fparser1 AST for the Kernel code.
        :type ast: :py:class:`fparser.one.block_statements.BeginSource`
        :param str name: the name of the Kernel or None.
        '''
        if self._type == "dynamo0.1":
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


#class BuiltInKernelTypeFactory(KernelTypeFactory):
#    ''' Factory class for calls to built-ins '''
#
#    def create(self, builtin_names, builtin_defs_file, name=None):
#        ''' Create a built-in call object '''
#        print "***"
#        print (name)
#        print (builtin_names)
#        if name not in builtin_names:
#            raise ParseError(
#                "BuiltInKernelTypeFactory: unrecognised built-in name. "
#                "Got '{0}' but expected one of {1}".format(name,
#                                                           builtin_names))
#        # The meta-data for these lives in a Fortran module file
#        # passed in to this method.
#        fname = os.path.join(
#            os.path.dirname(os.path.abspath(__file__)),
#            builtin_defs_file)
#        if not os.path.isfile(fname):
#            raise ParseError(
#                "Kernel '{0}' is a recognised Built-in but cannot "
#                "find file '{1}' containing the meta-data describing "
#                "the Built-in operations for API '{2}'".format(name,
#                                                               fname,
#                                                               self._type))
#        # Attempt to parse the meta-data
#        try:
#            ast = fpapi.parse(fname)
#        except:
#            raise ParseError(
#                "Failed to parse the meta-data for PSyclone "
#                "built-ins in {0}".format(fname))
#
#        # Now we have the AST, call our parent class to create the object
#        return KernelTypeFactory.create(self, ast, name)


class KernelType(object):
    """
    Base class for describing Kernel Metadata.

    This contains the name of the elemental procedure and metadata associated
    with how that procedure is mapped over mesh entities.

    :param ast: fparser1 AST for the parsed kernel meta-data.
    :type ast: :py:class:`fparser.one.block_statements.BeginSource`
    :param str name: name of the Fortran derived type describing the kernel.

    :raises ParseError: if the supplied name does not follow the convention \
                        of ending in "_mod" or the AST does not contain a \
                        module definition.
    """
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
        self._arg_descriptors = []  # this is set up by the subclasses

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
        matching variable is found.
        :param str name: the name of the integer variable to find.
        :returns: value of the specified integer variable or None.
        :rtype: str
        :raises ParseError: if the RHS of the assignment is not a Name.
        '''
        # Ensure the Fortran2003 parser is initialised
        _ = ParserFactory().create()

        for statement, _ in fpapi.walk(self._ktype, -1):
            if isinstance(statement, fparser1.typedecl_statements.Integer):
                # fparser only goes down to the statement level. We use
                # fparser2 to parse the statement itself (eventually we'll
                # use fparser2 to parse the whole thing).
                assign = Fortran2003.Assignment_Stmt(
                    statement.entity_decls[0])
                if str(assign.items[0]) == name:
                    if not isinstance(assign.items[2], Fortran2003.Name):
                        raise ParseError(
                            "get_integer_variable: RHS of assignment is not "
                            "a variable name: '{0}'".format(str(assign)))
                    return str(assign.items[2])
        return None

    def get_integer_array(self, name):
        ''' Parse the kernel meta-data and find the value of the
        integer array variable with the supplied name. Returns an empty list
        if no matching variable is found.

        :param str name: the name of the integer array to find.
        :returns: list of values.
        :rtype: list of str.
        :raises InternalError: if we fail to parse the LHS of the array \
                               declaration or the array constructor.
        :raises ParseError: if the RHS of the declaration is not an array \
                            constructor.
        '''
        # Ensure the classes are setup for the Fortran2003 parser
        _ = ParserFactory().create()

        for statement, _ in fpapi.walk(self._ktype, -1):
            if not isinstance(statement, fparser1.typedecl_statements.Integer):
                # This isn't an integer declaration so skip it
                continue
            # fparser only goes down to the statement level. We use fparser2 to
            # parse the statement itself.
            assign = Fortran2003.Assignment_Stmt(statement.entity_decls[0])
            names = walk_ast(assign.items, [Fortran2003.Name])
            if not names:
                raise InternalError("Unsupported assignment statement: '{0}'".
                                    format(str(assign)))
            if str(names[0]) == name:
                # This is the variable declaration we're looking for
                if not isinstance(assign.items[2],
                                  Fortran2003.Array_Constructor):
                    raise ParseError(
                        "get_integer_array: RHS of assignment is not "
                        "an array constructor: '{0}'".format(str(assign)))
                # fparser2 AST for Array_Constructor is:
                # Array_Constructor('[', Ac_Value_List(',', (Name('w0'),
                #                                      Name('w1'))), ']')
                # Construct a list of the names in the array constructor
                names = walk_ast(assign.items[2].items, [Fortran2003.Name])
                if not names:
                    raise InternalError("Failed to parse array constructor: "
                                        "'{0}'".format(str(assign.items[2])))
                return [str(name) for name in names]
        return []


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


#class BuiltInCall(ParsedCall):
#    ''' A built-in call (appearing in
#    `call invoke(kernel_name(field_name, ...))` '''
#
#    def __init__(self, ktype, args):
#        ParsedCall.__init__(self, ktype, args)
#        self._func_name = ktype.name
#
#    @property
#    def func_name(self):
#        return self._func_name
#
#    @property
#    def type(self):
#        return "BuiltInCall"
#
#    def __repr__(self):
#        return 'BuiltInCall(%s, %s)' % (self.args)


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
          distributed_memory=None):
    '''Takes a GungHo algorithm specification as input and outputs an AST of
    this specification and an object containing information about the
    invocation calls in the algorithm specification and any associated kernel
    implementations.

    :param str alg_filename: The file containing the algorithm specification.
    :param str invoke_name: The expected name of the invocation calls in the
                            algorithm code.
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
    *** dist_mem arg ***
    :returns: 2-tuple consisting of the fparser2 parse tree of the \
              Algorithm file and an object holding details of the \
              invokes found.
    :rtype: :py:class:`******************`, \
            :py:class:`psyclone.parse.FileInfo`
    :raises IOError: if the filename or search path does not exist.
    :raises ParseError: if there is an error in the parsing.
    :raises RuntimeError: if there is an error in the parsing.

    For example:

    >>> from parse import parse
    >>> ast, info = parse("argspec.F90")

    '''

    _config = Config.get()
    if not api:
        api = _config.default_api
    else:
        check_api(api)

    if line_length:
        check_ll(alg_filename)

    if api == "nemo":
        # For this API we just parse the NEMO code and return the resulting
        # fparser2 AST with None for the Algorithm AST.
        ast = parse_fp2(alg_filename)
        return None, ast

    alg_parse_tree = parse_fp2(alg_filename)

    from fparser.two.Fortran2003 import Main_Program, Module, \
        Subroutine_Subprogram, Function_Subprogram, Use_Stmt, \
        Call_Stmt, Actual_Arg_Spec_List, Actual_Arg_Spec, Part_Ref

    # Find the first program, module, subroutine or function in the
    # parse tree. The assumption here is that the first is the one
    # that is required.
    container_name = None
    for child in alg_parse_tree.content:
        if isinstance(child, (Main_Program, Module, Subroutine_Subprogram,
                              Function_Subprogram)):
            container_name = str(child.content[0].items[1])
            break
            
    if not container_name:
        # Nothing relevant found.
        raise ParseError(
            "Error, program, module, function or subroutine not found in "
            "parse tree")

    builtin_name_map, builtin_defs_file = get_builtin_defs(api)

    unique_invoke_labels = []
    arg_name_to_module_name = {}
    invoke_calls = []

    from fparser.two.utils import walk_ast
    for statement in walk_ast(alg_parse_tree.content):

        if isinstance(statement, Use_Stmt):
            # found a Fortran use statement
            #print str(statement)
            #print type(statement.items[4])
            statement_kcalls = []
            use_name = str(statement.items[2])
            from fparser.two.Fortran2003 import Only_List
            if isinstance(statement.items[4], Only_List):
                only_list = statement.items[4].items
            else:
                only_list = [statement.items[4]]
            for item in only_list:
                arg_name_to_module_name[str(item)] = use_name
        if isinstance(statement, Call_Stmt):
            # found a Fortran call statement
            call_name = str(statement.items[0])
            if call_name.lower() == invoke_name.lower():
                statement_kcalls = []
                # The call statement is an invoke

                invoke_label = None
                # Extract argument list. This can be removed when
                # fparser#170 is implemented
                argument_list = []
                if isinstance(statement.items[1], Actual_Arg_Spec_List):
                    argument_list = statement.items[1].items
                else:
                    # Expecting a single entry rather than a list
                    argument_list = [statement.items[1]]

                for argument in argument_list:
                    
                    if isinstance(argument, Actual_Arg_Spec):
                        # This should be the invoke label.
                        if invoke_label:
                            raise ParseError(
                                "An invoke must contain one or zero 'name=xxx' "
                                "arguments but found more than one in: {0} in "
                                "file {1}".
                                format(str(statement), alg_filename))

                        invoke_label = get_invoke_label(argument, alg_filename)
                        if invoke_label in unique_invoke_labels:
                            raise ParseError(
                                "Found multiple named invoke()'s with the same "
                                "label ('{0}') when parsing {1}".
                                format(invoke_label, alg_filename))
                        unique_invoke_labels.append(invoke_label)

                    elif isinstance(argument, Part_Ref):
                        # This should be a kernel call.
                        kernel_name, args = get_kernel(argument, alg_filename)
                        if kernel_name.lower() in builtin_name_map.keys():
                            if kernel_name in arg_name_to_module_name:
                                raise ParseError(
                                    "A built-in cannot be named in a use "
                                    "statement but '{0}' is used from "
                                    "module '{1}' in file {2}".
                                    format(kernel_name,
                                           arg_name_to_module_name[kernel_name],
                                           alg_filename))
                            from psyclone.parse_orig import BuiltInKernelTypeFactory, BuiltInCall
                            statement_kcalls.append(
                                BuiltInCall(
                                    BuiltInKernelTypeFactory(api=api).create(
                                        builtin_name_map.keys(), builtin_defs_file,
                                        name=kernel_name.lower()), args))
                        else:
                            try:
                                module_name = arg_name_to_module_name[kernel_name]
                            except KeyError:
                                raise ParseError(
                                    "kernel call '{0}' must either be named "
                                    "in a use "
                                    "statement (found {1}) or be a recognised built-in "
                                    "(one of '{2}' for this API)".
                                    format(kernel_name, arg_name_to_module_name.values(), list(builtin_name_map.keys())))
                            # coded kernel
                            modast = get_kernel_ast(module_name, alg_filename, kernel_path, line_length)
                            from psyclone.parse_orig import KernelCall, KernelTypeFactory
                            statement_kcalls.append(
                                KernelCall(module_name, KernelTypeFactory(api=api).create(modast, name=kernel_name), args))
                    else:
                        # I don't support this.
                        print ("  unexpected input")
                        print ("  arg: {0}".format(argument))
                        print ("  type: {0}".format(type(argument)))
                from psyclone.parse_orig import InvokeCall
                invoke_calls.append(InvokeCall(statement_kcalls,
                                               name=invoke_label))
    from psyclone.parse_orig import FileInfo
    return alg_parse_tree, FileInfo(container_name, invoke_calls)


def get_invoke_label(parse_tree, alg_filename, identifier="name"):
    ''' xxx '''
    from fparser.two.Fortran2003 import Actual_Arg_Spec, Char_Literal_Constant

    if not isinstance(parse_tree, Actual_Arg_Spec):
        raise ParseError("xxx")
    
    if len(parse_tree.items) != 2:
        raise ParseError("xxx")

    ident = str(parse_tree.items[0])
    if ident.lower() != identifier:
        raise ParseError("Expecting named identifier to be '{0}' but "
                         "found '{1}'".format(identifier, ident.lower()))

    if not isinstance(parse_tree.items[1], Char_Literal_Constant):
        raise ParseError("The (optional) name of an invoke must be "
                         "specified as a string, but found "
                         "{0} in {1}".format(str(parse_tree.items[1]),
                                                 alg_filename))

    invoke_label = parse_tree.items[1].items[0]
    invoke_label = invoke_label.lower()
    invoke_label = invoke_label.strip()
    if invoke_label[0] == '"' and invoke_label[-1] == '"' or \
       invoke_label[0] == "'" and invoke_label[-1] == "'":
        invoke_label = invoke_label[1:-1]
    invoke_label = invoke_label.replace(" ", "_")

    from fparser.two import pattern_tools
    if not pattern_tools.abs_name.match(invoke_label):
        raise ParseError(
            "The (optional) name of an invoke must be a "
            "string containing a valid Fortran name (with "
            "any spaces replaced by underscores) but "
            "got '{0}' in file {1}".format(invoke_label,
                                           alg_filename))

    return invoke_label

def get_kernel(parse_tree, alg_filename):
    ''' xxx '''
    from fparser.two.Fortran2003 import Part_Ref, Section_Subscript_List, \
        Name, Real_Literal_Constant, Data_Ref, Int_Literal_Constant, \
        Function_Reference

    if not isinstance(parse_tree, Part_Ref):
        raise ParseError("xxx")
    
    kernel_name = str(parse_tree.items[0])

    # Extract argument list. This can be removed when
    # fparser#170 is implemented
    argument_list = []
    if isinstance(parse_tree.items[1], Section_Subscript_List):
        argument_list = parse_tree.items[1].items
    else:
        # Expecting a single entry rather than a list
        argument_list = [parse_tree.items[1]]

    arguments = []
    for argument in argument_list:
        if isinstance(argument, Name):
            full_text = str(argument).lower()
            var_name = full_text
            arguments.append(Arg('variable', full_text, var_name))
        elif isinstance(argument, Real_Literal_Constant):
            arguments.append(Arg('literal', argument.tostr().lower()))
        elif isinstance(argument, Int_Literal_Constant):
            arguments.append(Arg('literal', argument.tostr().lower()))
        elif isinstance(argument, Part_Ref):
            full_text = argument.tostr().lower()
            var_name = str(argument.items[0]).lower()
            arguments.append(Arg('indexed_variable', full_text, var_name))
        elif isinstance(argument, Function_Reference):
            full_text = argument.tostr().lower()
            designator = argument.items[0]
            lhs = designator.items[0]
            lhs = create_var_name(lhs)
            rhs = str(designator.items[2])
            var_name = "{0}_{1}".format(lhs, rhs)
            var_name = var_name.lower()
            arguments.append(Arg('indexed_variable', full_text, var_name))
        elif isinstance(argument, Data_Ref):
            full_text = argument.tostr().lower()
            var_name = create_var_name(argument).lower()
            arguments.append(Arg('variable', full_text, var_name))
        else:
            print ("Unsupported argument structure '{0}', value '{1}', "
            "kernel '{2}' in file '{3}'.".format(type(argument), str(argument),
                                                 parse_tree, alg_filename))
            exit(1)
    return kernel_name, arguments

def create_var_name(arg_parse_tree):
    ''' remove brackets, return % '''
    from fparser.two.Fortran2003 import Data_Ref, Name, Part_Ref
    var_name = ""
    tree = arg_parse_tree
    while isinstance(tree, Data_Ref):
        var_name += str(tree.items[0]) + "_"
        tree = tree.items[1]
    if isinstance(tree, Name):
        var_name += str(tree)
    elif isinstance(tree, Part_Ref):
        var_name += str(tree.items[0])
    else:
        raise ParseError("unsupported structure '{0}'".format(type(tree)))
    return var_name


def get_kernel_ast(module_name, alg_filename, kernel_path, line_length):
    ''' xxx '''

    import os
    # Search for the file containing the kernel source
    import fnmatch

    # We only consider files with the suffixes .f90 and .F90
    # when searching for the kernel source.
    search_string = "{0}.[fF]90".format(module_name)

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
                format(module_name, cdir))
        else:
            raise IOError(
                "More than one match for kernel file "
                "'{0}.[fF]90' found!".
                format(module_name))
    else:
        from fparser.one import parsefortran
        import fparser
        from fparser import api as fpapi
        parsefortran.FortranParser.cache.clear()
        fparser.logging.disable(fparser.logging.CRITICAL)
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
            from psyclone.line_length import FortLineLength
            fll = FortLineLength()
            with open(matches[0], "r") as myfile:
                code_str = myfile.read()
            if fll.long_lines(code_str):
                raise ParseError(
                    "parse: the kernel file '{0}' does not"
                    " conform to the specified {1} line length"
                    " limit".format(matches[0],
                                    str(fll.length)))
    return modast
