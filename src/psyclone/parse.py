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


def parse(alg_filename, api=None, invoke_name="invoke", inf_name="inf",
          kernel_path="", line_length=False, distributed_memory=None):
    '''Takes a PSyclone-conformant algorithm file as input and outputs an
    parse tree of this file and an object containing information about
    the invocation calls in the algorithm specification and any
    associated kernel implementations.

    :param str alg_filename: The file containing the algorithm code.
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

    builtin_names, builtin_defs_file = get_builtin_defs(api)
    
    unique_invoke_labels = []
    arg_name_to_module_name = {}

    from collections import OrderedDict
    invokecalls = OrderedDict()
    
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
                        if kernel_name.lower() in builtin_names:
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
                                        builtin_names, builtin_defs_file,
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
                                    format(kernel_name, arg_name_to_module_name.values(), builtin_names.keys()))
                            # coded kernel
                            modast = get_kernel_ast(module_name, alg_filename, kernel_path, line_length)
                            from psyclone.parse_orig import KernelCall, KernelTypeFactory
                            statement_kcalls.append(
                                KernelCall(module_name, KernelTypeFactory(api=api).create(modast, name=kernel_name), args))
                    else:
                        # I don't support this.
                        print "  unexpected input"
                        print "  arg: {0}".format(argument)
                        print "  type: {0}".format(type(argument))
                from psyclone.parse_orig import InvokeCall
                invokecalls[statement] = InvokeCall(statement_kcalls,
                                                    name=invoke_label)
    from psyclone.parse_orig import FileInfo
    return alg_parse_tree, FileInfo(container_name, invokecalls)


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
