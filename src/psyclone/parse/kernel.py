# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
# Authors L. Mitchell Imperial College, R. W. Ford and A. R. Porter STFC
# Daresbury Lab

'''Module that uses the Fortran parser fparser1 to parse
PSyclone-conformant Kernel code.

'''

import os
import fparser

from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.two.utils import walk_ast

from fparser import one as fparser1
from fparser import api as fpapi
from fparser.one import parsefortran

import psyclone.expression as expr
from psyclone.psyGen import InternalError
from psyclone.configuration import Config
from psyclone.parse.utils import check_api, check_ll, ParseError

from pyparsing import ParseException


def get_kernel_filepath(module_name, kernel_path, alg_filename):
    '''Search for a kernel module file containing a module with
    'module_name'. The assumed convention is that the name of the
    kernel file is the name of the module with .f90 or .F90 appended.

    Look in the directories and all subdirectories associated with the
    supplied kernel paths or in the same directory as the algorithm
    file if the kernel path is empty.

    Return the filepath if the file is found.

    :param str module_name: the name of the module to search for. The \
    assumption is that the file containing the module will have the \
    same name as the module name with .f90 or .F90 appended.
    :param str kernel_path: directory in which to search for the module \
    file. If nothing is supplied then look in the same directory as the \
    algorithm file. Directories below the specified directory are \
    recursively searched.
    :param str alg_filename: the name of the algorithm file. This is \
    used to determine its directory location if required.

    :returns: a filepath to the file containing the specified module \
    name.
    :rtype: str

    :except ParseError: if the supplied kernel directory does not exist
    :except ParseError: if the file can not be found
    :except ParseError: if more than one file with the specified name is found

    '''
    import fnmatch

    # Only consider files with the suffixes .f90 and .F90
    # when searching for the kernel source.
    search_string = "{0}.[fF]90".format(module_name)

    # The list of matching files (should have length == 1).
    matches = []

    # If a search path has been specified then look there.  Otherwise
    # look in the directory containing the algorithm definition file.
    if kernel_path:
        # Look for the file in the supplied directory and recursively
        # in any subdirectories.
        cdir = os.path.abspath(kernel_path)

        if not os.access(cdir, os.R_OK):
            raise ParseError(
                "Supplied kernel search path does not exist "
                "or cannot be read: {0}".format(cdir))

        # Recursively search down through the directory tree starting
        # at the specified path
        if os.path.exists(cdir):
            for root, _, filenames in os.walk(cdir):
                for filename in fnmatch.filter(filenames, search_string):
                    matches.append(os.path.join(root, filename))
    else:
        # Look *only* in the directory that contained the algorithm
        # file.
        cdir = os.path.abspath(os.path.dirname(alg_filename))
        filenames = os.listdir(cdir)
        for filename in fnmatch.filter(filenames,
                                       search_string):
            matches.append(os.path.join(cdir, filename))

    if not matches:
        # There were no matches.
        raise ParseError(
            "Kernel file '{0}.[fF]90' not found in {1}".
            format(module_name, cdir))
    if len(matches) > 1:
        # There was more than one match
        raise ParseError(
            "More than one match for kernel file '{0}.[fF]90' found!".
            format(module_name))
    # There is a single match
    return matches[0]


def get_kernel_parse_tree(filepath):
    '''Parse the file in filepath with fparser1 and return a parse tree.

    :param str filepath: path to a file (hopefully) containing \
    PSyclone kernel code.

    :returns: Parse tree of the kernel code contained in the specified \
    file.
    :rtype: :py:class:`fparser.one.block_statements.BeginSource`

    :raises ParseError: if fparser fails to parse the file

    '''
    parsefortran.FortranParser.cache.clear()
    fparser.logging.disable(fparser.logging.CRITICAL)
    try:
        parse_tree = fpapi.parse(filepath)
        # parse_tree includes an extra comment line which contains
        # file details. This line can be long which can cause line
        # length issues. Therefore set the information (name) to be
        # empty.
        parse_tree.name = ""
    except:
        raise ParseError(
            "Failed to parse kernel code '{0}'. Is the Fortran correct?".
            format(filepath))
    return parse_tree


def get_kernel_ast(module_name, alg_filename, kernel_path, line_length):
    '''Search for the kernel source code containing a module with the name
    'module_name' looking in the directory and subdirectories
    associated with the supplied 'kernel_path' or in the same
    directory as the 'algorithm_filename' if the kernel path is
    empty. If the file is found then check it conforms to the
    'line_length' restriction if this is set and then parse this file
    and return the parsed file.

    :param str module_name: the name of the module to search for.
    :param str alg_filename: the name of the algorithm file.
    :param str kernel_path: directory in which to search for the module \
    file.
    :param bool line_length: whether to check that the kernel code \
    conforms to the 132 character line length limit (True) or not \
    (False).

    :returns: Parse tree of the kernel module with the name 'module_name'
    :rtype: :py:class:`fparser.one.block_statements.BeginSource`

    '''
    filepath = get_kernel_filepath(module_name, kernel_path, alg_filename)
    if line_length:
        check_ll(filepath)
    parse_tree = get_kernel_parse_tree(filepath)
    return parse_tree


class KernelTypeFactory(object):
    '''Factory to create the required API-specific information about
    coded-kernel metadata and a reference to its code.

    :param str api: The API for which this factory is to create Kernel \
    information. If it is not supplied then the default API, as \
    specified in the PSyclone config file is used.

    '''
    def __init__(self, api=""):
        if not api:
            _config = Config.get()
            self._type = _config.default_api
        else:
            check_api(api)
            self._type = api

    def create(self, parse_tree, name=None):
        '''Create API-specific information about the kernel metadata and a
        reference to its code. The API is set when the factory is
        created.

        :param parse_tree: The fparser1 parse tree for the Kernel code.
        :type parse_tree: :py:class:`fparser.one.block_statements.BeginSource`
        :param str name: the name of the Kernel or None ***** ??? WHY

        '''
        # ***** MOVE DynKernelType to dynamo0p1.py ****???
        if self._type == "dynamo0.1":
            return DynKernelType(parse_tree, name=name)
        elif self._type == "dynamo0.3":
            from psyclone.dynamo0p3 import DynKernMetadata
            return DynKernMetadata(parse_tree, name=name)
        elif self._type == "gocean0.1":
            return GOKernelType(parse_tree, name=name)
        elif self._type == "gocean1.0":
            from psyclone.gocean1p0 import GOKernelType1p0
            return GOKernelType1p0(parse_tree, name=name)
        else:
            raise ParseError(
                "KernelTypeFactory:create: Unsupported kernel type '{0}' "
                "found.".format(self._type))


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
            parsefortran.FortranParser.cache.clear()
            fparser.logging.disable(fparser.logging.CRITICAL)
            ast = fpapi.parse(fname)
        except:
            raise ParseError(
                "Failed to parse the meta-data for PSyclone "
                "built-ins in {0}".format(fname))

        # Now we have the AST, call our parent class to create the object
        return KernelTypeFactory.create(self, ast, name)


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

    def __init__(self, element, dimension):
        self._element = element
        self._dimension = dimension

    @property
    def element(self):
        return self._element

    @property
    def dimension(self):
        return self._dimension


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
