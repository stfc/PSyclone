# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors: L. Mitchell Imperial College
#          R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab
# Modified: C.M. Maynard, Met Office / University of Reading,
#           I. Kavcic and L. Turner, Met Office
#           J. Henrichs, Bureau of Meteorology

'''Module that uses the Fortran parser fparser1 to parse
PSyclone-conformant kernel code.

'''

import os
import sys

from pyparsing import ParseException

import fparser
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.two.utils import walk

from fparser import one as fparser1
from fparser import api as fpapi
from fparser.one import parsefortran

import psyclone.expression as expr
from psyclone.errors import InternalError
from psyclone.configuration import Config
from psyclone.parse.utils import check_api, check_line_length, ParseError


def get_kernel_filepath(module_name, kernel_paths, alg_filename):
    '''Search for a kernel module file containing a module with
    'module_name'. The assumed convention is that the name of the
    kernel file is the name of the module with .f90 or .F90 appended.

    Look in the directories and all subdirectories associated with the
    supplied kernel paths or in the same directory as the algorithm
    file if not found within the kernel paths.

    Return the filepath if the file is found.

    :param str module_name: the name of the module to search for. The \
        assumption is that the file containing the module will have the \
        same name as the module name with .f90 or .F90 appended.
    :param kernel_paths: directories in which to search for the module \
        file. If nothing is supplied then look in the same directory \
        as the algorithm file. Directories below the specified \
        directories are recursively searched.
    :type list_kernels: list of str
    :param str alg_filename: the name of the algorithm file. This is \
        used to determine its directory location if required.

    :returns: a filepath to the file containing the specified module \
        name.
    :rtype: str

    :raises ParseError: if the supplied kernel directory does not \
        exist.
    :raises ParseError: if the file can not be found.
    :raises ParseError: if more than one file with the specified name \
        is found.

    '''
    # Only consider files with the suffixes .f90 and .F90 when
    # searching for the kernel source (we perform a case insensitive
    # match).
    search_string = f"{module_name}.F90"
    matches = []

    # If a search path has been specified then look there. Otherwise
    # look in the directory containing the algorithm definition file.
    for kernel_path in kernel_paths:
        # Look for the file in the supplied directory and recursively
        # in any subdirectories.
        cdir = os.path.abspath(kernel_path)

        if not os.access(cdir, os.R_OK):
            raise ParseError(
                f"kernel.py:get_kernel_filepath: Supplied kernel search path "
                f"does not exist or cannot be read: {cdir}")

        # Recursively search down through the directory tree starting
        # at the specified path.
        for root, _, filenames in os.walk(cdir):
            for filename in filenames:
                # perform a case insensitive match
                if filename.lower() == search_string.lower():
                    matches.append(os.path.join(root, filename))
    if not kernel_paths:
        # Look *only* in the directory that contained the algorithm
        # file.
        cdir = os.path.abspath(os.path.dirname(alg_filename))
        filenames = os.listdir(cdir)
        for filename in filenames:
            # perform a case insensitive match
            if filename.lower() == search_string.lower():
                matches.append(os.path.join(cdir, filename))

    if not matches:
        # There were no matches.
        raise ParseError(
            f"Kernel file '{module_name}.[fF]90' not found in {cdir}")
    if len(matches) > 1:
        # There was more than one match
        raise ParseError(
            f"kernel.py:get_kernel_filepath: More than one match for kernel "
            f"file '{module_name}.[fF]90' found!")
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

    # If logging is disable during a sphinx doctest run, doctest will just
    # stop working. So only disable logging if we are not running doctest.
    if 'sphinx.ext.doctest' not in sys.modules:
        fparser.logging.disable(fparser.logging.CRITICAL)

    try:
        parse_tree = fpapi.parse(filepath)
        # parse_tree includes an extra comment line which contains
        # file details. This line can be long which can cause line
        # length issues. Therefore set the information (name) to be
        # empty.
        parse_tree.name = ""
    except Exception as err:
        raise ParseError(
            f"Failed to parse kernel code '{filepath}'. Is the Fortran "
            f"correct?") from err
    return parse_tree


def get_kernel_ast(module_name, alg_filename, kernel_paths, line_length):
    '''Search for the kernel source code containing a module with the name
    'module_name' looking in the directory and subdirectories
    associated with the supplied 'kernel_paths' or in the same
    directory as the 'algorithm_filename' if the kernel path is
    empty. If the file is found then check it conforms to the
    'line_length' restriction if this is set and then parse this file
    and return the parsed file.

    :param str module_name: the name of the module to search for.
    :param str alg_filename: the name of the algorithm file.
    :param kernel_paths: directory in which to search for the module \
        file.
    :type kernel_paths: list of str
    :param bool line_length: whether to check that the kernel code \
        conforms to the 132 character line length limit (True) or not \
        (False).

    :returns: Parse tree of the kernel module with the name 'module_name'.
    :rtype: :py:class:`fparser.one.block_statements.BeginSource`

    '''
    filepath = get_kernel_filepath(module_name, kernel_paths, alg_filename)
    if line_length:
        check_line_length(filepath)
    parse_tree = get_kernel_parse_tree(filepath)
    return parse_tree


# pylint: disable=too-few-public-methods
class KernelTypeFactory():
    '''Factory to create the required API-specific information about
    coded-kernel metadata and a reference to its code.

    :param str api: The API for which this factory is to create Kernel \
    information. If it is not supplied then the default API, as \
    specified in the PSyclone config file, is used.

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

        :param name: the name of the Kernel. Defaults to None if \
        one is not provided.
        :type name: str or NoneType

        :raises ParseError: if the supplied API is not supported.

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        if self._type == "dynamo0.3":
            from psyclone.domain.lfric import LFRicKernMetadata
            return LFRicKernMetadata(parse_tree, name=name)
        if self._type == "gocean1.0":
            from psyclone.gocean1p0 import GOKernelType1p0
            return GOKernelType1p0(parse_tree, name=name)
        raise ParseError(
            f"KernelTypeFactory:create: Unsupported kernel type '{self._type}'"
            f" found.")


class BuiltInKernelTypeFactory(KernelTypeFactory):
    '''Create API-specific information about the builtin metadata. The API
    is set when the factory is created. Subclasses KernelTypeFactory
    and makes use of its init method.

    '''
    # pylint: disable=arguments-differ
    def create(self, builtin_names, builtin_defs_file, name=None):
        '''Create API-specific information about the builtin metadata. This
        method finds and parses the metadata then makes use of the
        KernelTypeFactory parent class to return the api-specific
        information about the builtin.

        :param builtin_names: a list of valid builtin names
        :type builtin_names: list of str
        :param str builtin_defs_file: the file containing builtin \
        metadata
        :param name: the name of the builtin. Defaults to None if \
        one is not provided.
        :type name: str or NoneType

        :raises ParseError: if the supplied name is not one of the \
        builtin names
        :raises ParseError: if the supplied name is recognised as a \
        builtin but the associated file containing the required \
        metadata can not be found.
        :raises ParseError: if the metadata for the supplied builtin \
        can not be parsed.

        '''
        if name not in builtin_names:
            raise ParseError(
                f"BuiltInKernelTypeFactory:create unrecognised built-in name. "
                f"Got '{name}' but expected one of {builtin_names}")
        # The meta-data for these lives in a Fortran module file
        # passed in to this method.
        fname = os.path.join(
            os.path.dirname(os.path.abspath(__file__)),
            builtin_defs_file)
        if not os.path.isfile(fname):
            raise ParseError(
                f"BuiltInKernelTypeFactory:create Kernel '{name}' is a "
                f"recognised Built-in but cannot find file '{fname}' "
                f"containing the meta-data describing the Built-in operations "
                f"for API '{self._type}'")
        # Attempt to parse the meta-data
        try:
            parsefortran.FortranParser.cache.clear()
            fparser.logging.disable(fparser.logging.CRITICAL)
            parse_tree = fpapi.parse(fname)
        except Exception as err:
            raise ParseError(
                f"BuiltInKernelTypeFactory:create: Failed to parse the meta-"
                f"data for PSyclone built-ins in file '{fname}'.") from err

        # Now we have the parse tree, call our parent class to create \
        # the object
        return KernelTypeFactory.create(self, parse_tree, name)
# pylint: enable=too-few-public-methods


def get_mesh(metadata, valid_mesh_types):
    '''
    Returns the mesh-type described by the supplied meta-data

    :param  metadata: node in parser ast
    :type metadata: py:class:`psyclone.expression.NamedArg`
    :param valid_mesh_types: List of valid mesh types
    :type valid_mesh_types: list of strings

    :return: the name of the mesh
    :rtype: string

    :raises ParseError: if the supplied meta-data is not a recognised \
                        mesh identifier.
    :raises ParseError: if the mesh type is unsupported.

    '''
    if not isinstance(metadata, expr.NamedArg) or \
       metadata.name.lower() != "mesh_arg":
        raise ParseError(
            f"{metadata} is not a valid mesh identifier (expected "
            f"mesh_arg=MESH_TYPE where MESH_TYPE is one of "
            f"{valid_mesh_types}))")
    mesh = metadata.value.lower()
    if mesh not in valid_mesh_types:
        raise ParseError(f"mesh_arg must be one of {valid_mesh_types} but got "
                         f"{mesh}")
    return mesh


def get_stencil(metadata, valid_types):
    '''Returns stencil_type and stencil_extent as a dictionary
    object from stencil metadata if the metadata conforms to the
    stencil(type[,extent]) format

    :param metadata: Component of kernel meta-data stored as a node in \
                     the fparser1 AST
    :type metadata: :py:class:`psyclone.expression.FunctionVar`
    :param list valid_types: List of valid stencil types (strings)

    :return: The stencil type and extent described in the meta-data
    :rtype: dict with keys 'type' (str) and 'extent' (int)

    :raises ParseError: if the supplied meta-data is not a recognised \
                        stencil specification

    '''

    if not isinstance(metadata, expr.FunctionVar):
        raise ParseError(
            f"Expecting format stencil(<type>[,<extent>]) but found the "
            f"literal {metadata}")
    if metadata.name.lower() != "stencil" or not metadata.args:
        raise ParseError(
            f"Expecting format stencil(<type>[,<extent>]) but found {metadata}"
            )
    if len(metadata.args) > 2:
        raise ParseError(
            f"Expecting format stencil(<type>[,<extent>]) so there must "
            f"be at most two arguments inside the brackets {metadata}")
    if not isinstance(metadata.args[0], expr.FunctionVar):
        if isinstance(metadata.args[0], str):
            raise ParseError(
                f"Expecting format stencil(<type>[,<extent>]). However, "
                f"the specified <type> '{metadata.args[0]}' is a literal and "
                f"therefore is not one of the valid types '{valid_types}'")
        raise ParseError(
            f"Internal error, expecting either FunctionVar or str from the "
            f"expression analyser but found {type(metadata.args[0])}")
    if metadata.args[0].args:
        raise ParseError(
            "Expected format stencil(<type>[,<extent>]). However, the "
            "specified <type> '{0}' includes brackets")
    stencil_type = metadata.args[0].name
    if stencil_type not in valid_types:
        raise ParseError(
            f"Expected format stencil(<type>[,<extent>]). However, the "
            f"specified <type> '{stencil_type}' is not one of the valid types "
            f"'{valid_types}'")

    stencil_extent = None
    if len(metadata.args) == 2:
        if not isinstance(metadata.args[1], str):
            raise ParseError(
                f"Expected format stencil(<type>[,<extent>]). However, the "
                f"specified <extent> '{metadata.args[1]}' is not an integer")
        stencil_extent = int(metadata.args[1])
        if stencil_extent < 1:
            raise ParseError(
                f"Expected format stencil(<type>[,<extent>]). However, the "
                f"specified <extent> '{stencil_extent}' is less than 1")
        raise NotImplementedError(
            "Kernels with fixed stencil extents are not currently "
            "supported")
    return {"type": stencil_type, "extent": stencil_extent}


class Descriptor():
    '''
    A description of how a kernel argument is accessed, constructed from
    the kernel metadata.

    :param str access: whether argument is read/write etc.
    :param str space: which function space/grid-point type argument is on.
    :param int metadata_index: position of this argument in the list of \
                               arguments specified in the metadata.
    :param dict stencil: type of stencil access for this argument. \
                         Defaults to None if the argument is not supplied.
    :param str mesh: which mesh this argument is on. Defaults to None \
                     if the argument is not supplied.
    :param str argument_type: the type of this argument. Defaults to \
                              None if the argument is not supplied.

    :raises InternalError: if the metadata_index argument is not an int or is \
                           less than zero.

    '''
    # pylint: disable=too-many-arguments
    def __init__(self, access, space, metadata_index, stencil=None, mesh=None,
                 argument_type=None):
        self._access = access
        self._space = space
        if not isinstance(metadata_index, int) or metadata_index < 0:
            raise InternalError(
                f"The metadata index must be an integer and greater than or "
                f"equal to zero but got: {metadata_index}")
        self._metadata_index = metadata_index
        self._stencil = stencil
        self._mesh = mesh
        self._argument_type = argument_type

    @property
    def access(self):
        '''
        :returns: whether argument is read/write etc.
        :rtype: str

        '''
        return self._access

    @property
    def function_space(self):
        '''
        :returns: which function space/grid-point type argument is on.
        :rtype: str

        '''
        return self._space

    @property
    def metadata_index(self):
        '''
        :returns: the position of the corresponding argument descriptor in \
                  the kernel metadata.
        :rtype: int
        '''
        return self._metadata_index

    @property
    def stencil(self):
        '''
        :returns: type of stencil access for this argument.
        :rtype: dict or NoneType

        '''
        return self._stencil

    @property
    def mesh(self):
        '''
        :returns: the mesh the argument is on.
        :rtype: str or NoneType

        '''
        return self._mesh

    @property
    def argument_type(self):
        '''
        :returns: the type of the argument depending on the specific \
                  API (e.g. scalar, field, grid property, operator).
        :rtype: str or NoneType

        '''
        return self._argument_type

    def __repr__(self):
        return (f"Descriptor({self.access}, {self.function_space}, "
                f"{self.metadata_index})")


class KernelProcedure():
    '''
    Captures the parse tree and name of a kernel subroutine.

    :param ktype_ast: the fparser1 parse tree for the Kernel meta-data.
    :type ktype_ast: :py:class:`fparser.one.block_statements.Type`
    :param str ktype_name: name of the Fortran type holding the Kernel \
                           meta-data.
    :param modast: the fparser1 parse tree for the module containing the \
                   Kernel routine.
    :type modast: :py:class:`fparser.one.block_statements.BeginSource`

    '''
    def __init__(self, ktype_ast, ktype_name, modast):
        self._ast, self._name = KernelProcedure.get_procedure(
            ktype_ast, ktype_name, modast)

    # pylint: disable=too-many-branches
    @staticmethod
    def get_procedure(ast, name, modast):
        '''
        Get the name of the subroutine associated with the Kernel. This is
        a type-bound procedure in the meta-data which may take one of three
        forms:
                PROCEDURE, nopass :: code => <proc_name>
        or
                PROCEDURE, nopass :: <proc_name>
        or if there is no type-bound procedure, an interface may be used:
                INTERFACE <proc_name>

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

        :raises ParseError: if the supplied Kernel meta-data does not \
                            have a type-bound procedure or interface.
        :raises ParseError: if no implementation is found for the \
                             type-bound procedure or interface module \
                             procedures.
        :raises ParseError: if the type-bound procedure specifies a binding \
                            name but the generic name is not "code".
        :raises InternalError: if we get an empty string for the name of the \
                               type-bound procedure.
        '''
        bname = None
        # Search the the meta-data for a SpecificBinding
        for statement in ast.content:
            if isinstance(statement, fparser1.statements.SpecificBinding):
                # We support:
                # PROCEDURE, nopass :: code => <proc_name> or
                # PROCEDURE, nopass :: <proc_name>
                if statement.bname:
                    if statement.name.lower() != "code":
                        raise ParseError(
                            f"Kernel type {name} binds to a specific procedure"
                            f" but does not use 'code' as the generic name.")
                    bname = statement.bname.lower()
                else:
                    bname = statement.name.lower()
                break
        if bname is None:
            # If no type-bound procedure found, search for an explicit
            # interface that has module procedures.
            bname, subnames = get_kernel_interface(name, modast)
            if bname is None:
                # no interface found either
                raise ParseError(
                    f"Kernel type {name} does not bind a specific procedure "
                    f"or provide an explicit interface")
        elif bname == '':
            raise InternalError(
                f"Empty Kernel name returned for Kernel type {name}.")
        else:
            # add the name of the tbp to the list of strings to search for.
            subnames = [bname]
        # walk the AST to check the subroutine names exist.
        procedure_count = 0
        for subname in subnames:
            for statement, _ in fpapi.walk(modast):
                if isinstance(statement,
                              fparser1.block_statements.Subroutine) \
                              and statement.name.lower() \
                              == subname:
                    procedure_count = procedure_count + 1
                    if procedure_count == 1:
                        # set code to statement if there is one procedure.
                        code = statement
                    else:
                        code = None  # set to None if there is more than one.
                    break
            else:
                raise ParseError(
                    f"kernel.py:KernelProcedure:get_procedure: Kernel "
                    f"subroutine '{subname}' not found.")
        return code, bname

    @property
    def name(self):
        '''
        :returns: the name of the kernel subroutine
        :rtype: str

        '''
        return self._name

    @property
    def ast(self):
        '''
        :returns: the parse tree of the kernel subroutine
        :rtype: :py:class:`fparser.one.block_statements.Subroutine`

        '''
        return self._ast

    def __repr__(self):
        return f"KernelProcedure({self.name})"

    def __str__(self):
        return str(self._ast)


def get_kernel_metadata(name, ast):
    '''Takes the kernel module parse tree and returns the metadata part
    of the parse tree (a Fortran type) with the name 'name'.

    :param str name: the metadata name (of a Fortran type). Also \
    the name referencing the kernel in the algorithm layer. The name \
    provided and the name of the kernel in the parse tree are case \
    insensitive in this function.
    :param ast: parse tree of the kernel module code
    :type ast: :py:class:`fparser.one.block_statements.BeginSource`

    :returns: Parse tree of the metadata (a Fortran type with name \
    'name')
    :rtype: :py:class:`fparser.one.block_statements.Type`

    :raises ParseError: if the metadata type name is not found in \
    the kernel code parse tree

    '''
    ktype = None
    for statement, _ in fpapi.walk(ast):
        if isinstance(statement, fparser1.block_statements.Type) \
           and statement.name.lower() == name.lower():
            ktype = statement
            break
    if ktype is None:
        raise ParseError(f"Kernel type {name} does not exist")
    return ktype


def get_kernel_interface(name, ast):
    '''Takes the kernel module parse tree and returns the interface part
    of the parse tree.

    :param str name: The kernel name
    :param ast: parse tree of the kernel module code
    :type ast: :py:class:`fparser.one.block_statements.BeginSource`

    :returns: Name of the interface block and the names of the module \
              procedures (lower case). Or None, None if there is no \
              interface or the interface has no nodule procedures.
    :rtype: : `str`, list of str`.

    :raises ParseError: if more than one interface is found.
    '''

    iname = None
    sname = None
    count = 0
    for statement, _ in fpapi.walk(ast):
        if isinstance(statement, fparser1.block_statements.Interface):
            # count the interfaces, then can be only one!
            count = count + 1
            if count >= 2:
                raise ParseError(f"Module containing kernel {name} has more "
                                 f"than one interface, this is forbidden in "
                                 f"the LFRic API.")
            # Check the interfaces assigns one or more module procedures.
            if statement.a.module_procedures:
                iname = statement.name.lower()
                # If implicit interface (no name) set to none as there is no
                # procedure name for PSyclone to use.
                if iname == '':
                    iname = None
                else:
                    sname = [str(sname).lower() for sname
                             in statement.a.module_procedures]
    return iname, sname


def getkerneldescriptors(name, ast, var_name='meta_args', var_type=None):
    '''Get the required argument metadata information for a kernel.

    :param str name: the name of the kernel (for error messages).
    :param ast: metadata describing kernel arguments.
    :type ast: :py:class:`fparser.one.block_statements.Type`
    :param str var_name: the name of the variable storing the \
           argument metadata. this argument is optional and defaults to \
           'meta_args'.
    :param str var_type: the type of the structure constructor used to \
                         define the meta-data or None.

    :returns: argument metadata parsed using the expression parser \
              (as fparser1 will not parse expressions and arguments).
    :rtype: :py:class:`psyclone.expression.LiteralArray`

    :raises ParseError: if 'var_name' is not found in the metadata.
    :raises ParseError: if 'var_name' is not an array.
    :raises ParseError: if 'var_name' is not a 1D array.
    :raises ParseError: if the structure constructor uses '[...]' \
                        as only '(/.../)' is supported.
    :raises ParseError: if the argument metadata is invalid and cannot \
                        be parsed.
    :raises ParseError: if the dimensions specified do not tally with \
                        the number of metadata arguments.
    :raises ParseError: if var_type is specified and a structure \
                        constructor for a different type is found.
    '''
    descs = ast.get_variable(var_name)
    if "INTEGER" in str(descs):
        # INTEGER in above 'if' test is an fparser1 hack as get_variable()
        # returns an integer if the variable is not found.
        raise ParseError(
            f"No variable named '{var_name}' found in the metadata for kernel "
            f"'{name}': '{str(ast).strip()}'.")
    try:
        nargs = int(descs.shape[0])
    except AttributeError as err:
        raise ParseError(
            f"In kernel metadata '{name}': '{var_name}' variable must be an "
            f"array.") from err
    if len(descs.shape) != 1:
        raise ParseError(
            f"In kernel metadata '{name}': '{var_name}' variable must be a 1 "
            f"dimensional array.")
    if descs.init.find("[") != -1 and descs.init.find("]") != -1:
        # there is a bug in fparser1
        raise ParseError(
            f"Parser does not currently support '[...]' initialisation for "
            f"'{var_name}', please use '(/.../)' instead.")
    try:
        inits = expr.FORT_EXPRESSION.parseString(descs.init)[0]
    except ParseException as err:
        raise ParseError(f"Kernel metadata has an invalid format "
                         f"{descs.init}.") from err
    nargs = int(descs.shape[0])
    if len(inits) != nargs:
        raise ParseError(
            f"In the '{var_name}' metadata, the number of items in the array "
            f"constructor ({len(inits)}) does not match the extent of the "
            f"array ({nargs}).")
    if var_type:
        # Check that each element in the list is of the correct type
        if not all(init.name == var_type for init in inits):
            raise ParseError(
                f"The '{var_name}' metadata must consist of an array of "
                f"structure constructors, all of type '{var_type}' but found: "
                f"{[str(init.name) for init in inits]}.")
    return inits


class KernelType():
    '''Base class for describing Kernel Metadata.

    This contains the name of the elemental procedure and metadata associated
    with how that procedure is mapped over mesh entities.

    :param ast: fparser1 AST for the parsed kernel meta-data.
    :type ast: :py:class:`fparser.one.block_statements.BeginSource`
    :param str name: name of the Fortran derived type describing the kernel.

    :raises ParseError: if the supplied AST does not contain a Fortran \
    module.
    :raises ParseError: if the module name is too short to contain \
    '_mod' at the end.
    :raises ParseError: if the module name does not end in '_mod'.

    '''
    def __init__(self, ast, name=None):

        if name is None:
            # if no name is supplied then use the module name to
            # determine the type name. The assumed convention is that
            # the module is called <name/>_mod and the type is called
            # <name/>_type
            found = False
            for statement, _ in fpapi.walk(ast):
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
                    f"Error, module name '{module_name}' is too short to have "
                    f"'_mod' as an extension. This convention is assumed.")
            base_name = module_name.lower()[:mn_len-4]
            extension_name = module_name.lower()[mn_len-4:mn_len]
            if extension_name != "_mod":
                raise ParseError(
                    f"Error, module name '{module_name}' does not have '_mod' "
                    f"as an extension. This convention is assumed.")
            name = base_name + "_type"

        self._name = name
        self._ast = ast
        self._ktype = get_kernel_metadata(name, ast)
        # TODO #1204 since the valid form of the metadata beyond this point
        # depends on the API, the code beyond this point should be refactored
        # into LFRicKernMetadata and GOKernelType1p0.
        operates_on = self.get_integer_variable("operates_on")
        # The GOcean API still uses the 'iterates_over' metadata entry
        # although this is deprecated in the LFRic API.
        # Validation is left to the API-specific code in either dynamo0p3.py
        # or gocean1p0.py.
        iterates_over = self.get_integer_variable("iterates_over")
        if operates_on:
            self._iterates_over = operates_on
        elif iterates_over:
            self._iterates_over = iterates_over
        else:
            # We don't raise an error here - we leave it to the API-specific
            # validation code.
            self._iterates_over = None
        # Although validation of the value given to operates_on or
        # iterates_over is API-specific, we can check that the metadata doesn't
        # specify both of them because that doesn't make sense.
        if operates_on and iterates_over:
            raise ParseError(f"The metadata for kernel '{name}' contains both "
                             f"'operates_on' and 'iterates_over'. Only one of "
                             f"these is permitted.")
        self._procedure = KernelProcedure(self._ktype, name, ast)
        self._inits = getkerneldescriptors(name, self._ktype)
        self._arg_descriptors = []  # this is set up by the subclasses

    @property
    def name(self):
        '''
        :returns: the name of the kernel subroutine.
        :rtype: str

        '''
        return self._name

    @property
    def iterates_over(self):
        '''
        :returns: the name of the iteration space supported by this kernel
        :rtype: str

        '''
        return self._iterates_over

    @property
    def procedure(self):
        '''
        :returns: a kernelprocedure instance which contains a parse tree \
        of the kernel subroutine and its name.
        :rtype: :py:class:`psyclone.parse.kernel.KernelProcedure`

        '''
        return self._procedure

    @property
    def nargs(self):
        '''
        :returns: the number of arguments specified in the metadata.
        :rtype: int

        '''
        return len(self._arg_descriptors)

    @property
    def arg_descriptors(self):
        '''
        :returns: a list of API-specific argument descriptors.
        :rtype: list of API-specific specialisation of \
        :py:class:`psyclone.kernel.Descriptor`

        '''
        return self._arg_descriptors

    def __repr__(self):
        return f"KernelType({self.name}, {self.iterates_over})"

    def get_integer_variable(self, name):
        ''' Parse the kernel meta-data and find the value of the
        integer variable with the supplied name. Return None if no
        matching variable is found. The search is not case sensitive.

        :param str name: the name of the integer variable to find.

        :return: value of the specified integer variable (lower case) or None.
        :rtype: str

        :raises ParseError: if the RHS of the assignment is not a Name.

        '''
        # Ensure the Fortran2008 parser is initialised
        _ = ParserFactory().create(std="f2008")
        # Fortran is not case sensitive so nor is our matching
        lower_name = name.lower()

        for statement, _ in fpapi.walk(self._ktype):
            if isinstance(statement, fparser1.typedecl_statements.Integer):
                # fparser only goes down to the statement level. We use
                # fparser2 to parse the statement itself (eventually we'll
                # use fparser2 to parse the whole thing).
                assign = Fortran2003.Assignment_Stmt(
                    statement.entity_decls[0])
                if str(assign.items[0]).lower() == lower_name:
                    if not isinstance(assign.items[2], Fortran2003.Name):
                        raise ParseError(
                            f"get_integer_variable: RHS of assignment is not "
                            f"a variable name: '{assign}'")
                    return str(assign.items[2]).lower()
        return None

    def get_integer_array(self, name):
        ''' Parse the kernel meta-data and find the values of the
        integer array variable with the supplied name. Returns an empty list
        if no matching variable is found. The search is not case sensitive.

        :param str name: the name of the integer array to find.

        :return: list of values (lower-case).
        :rtype: list of str.

        :raises InternalError: if we fail to parse the LHS of the array \
                               declaration or the array constructor.
        :raises ParseError: if the array is not of rank 1.
        :raises ParseError: if the array extent is not specified using an \
                            integer literal.
        :raises ParseError: if the RHS of the declaration is not an array \
                            constructor.
        :raises InternalError: if the parse tree for the array constructor \
                               does not have the expected structure.
        :raises ParseError: if the number of items in the array constructor \
                            does not match the extent of the array.

        '''
        # Ensure the classes are setup for the Fortran2008 parser
        _ = ParserFactory().create(std="f2008")
        # Fortran is not case sensitive so nor is our matching
        lower_name = name.lower()

        for statement, _ in fpapi.walk(self._ktype):
            if not isinstance(statement, fparser1.typedecl_statements.Integer):
                # This isn't an integer declaration so skip it
                continue
            # fparser only goes down to the statement level. We use fparser2 to
            # parse the statement itself.
            assign = Fortran2003.Assignment_Stmt(statement.entity_decls[0])
            names = walk(assign.children, Fortran2003.Name)
            if not names:
                raise InternalError(f"Unsupported assignment statement: "
                                    f"'{assign}'")

            if str(names[0]).lower() != lower_name:
                # This is not the variable declaration we're looking for
                continue

            if not isinstance(assign.children[0], Fortran2003.Part_Ref):
                # Not an array declaration
                return []

            if not isinstance(assign.children[0].children[1],
                              Fortran2003.Section_Subscript_List):
                raise InternalError(
                    f"get_integer_array: expected array declaration to have a "
                    f"Section_Subscript_List but found "
                    f"'{type(assign.children[0].children[1]).__name__}' "
                    f"for: '{assign}'")

            dim_stmt = assign.children[0].children[1]
            if len(dim_stmt.children) != 1:
                raise ParseError(
                    f"get_integer_array: array must be 1D but found an array "
                    f"with {len(dim_stmt.children)} dimensions for name '"
                    f"{name}'")
            if not isinstance(dim_stmt.children[0],
                              Fortran2003.Int_Literal_Constant):
                raise ParseError(
                    f"get_integer_array: array extent must be specified using "
                    f"an integer literal but found '{dim_stmt.children[0]}' "
                    f"for array '{name}'")
            # Get the declared size of the array
            array_extent = int(str(dim_stmt.children[0]))

            if not isinstance(assign.children[2],
                              Fortran2003.Array_Constructor):
                raise ParseError(
                    f"get_integer_array: RHS of assignment is not "
                    f"an array constructor: '{assign}'")
            # fparser2 AST for Array_Constructor is:
            # Array_Constructor('[', Ac_Value_List(',', (Name('w0'),
            #                                      Name('w1'))), ']')
            # Construct a list of the names in the array constructor
            names = walk(assign.children[2].children, Fortran2003.Name)
            if not names:
                raise InternalError(f"Failed to parse array constructor: "
                                    f"'{assign.items[2]}'")
            if len(names) != array_extent:
                # Ideally fparser would catch this but it isn't yet mature
                # enough.
                raise ParseError(
                    f"get_integer_array: declared length of array '{name}' is "
                    f"{array_extent} but constructor only contains "
                    f"{len(names)} names: '{assign}'")
            return [str(name).lower() for name in names]
        # No matching declaration for the provided name was found
        return []
