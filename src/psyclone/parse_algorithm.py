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
from psyclone.parse_utils import ParseError

# Capture algorithm information


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

# parse algorithm


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

    my_parser = Parser(api, invoke_name, inf_name, kernel_path, line_length,
                       distributed_memory)
    return my_parser.parse(alg_filename)


class Parser(object):
    ''' xxx '''

    def __init__(self, api="", invoke_name="invoke", inf_name="inf",
                 kernel_path="", line_length=False, distributed_memory=None):

        self._invoke_name = invoke_name
        self._inf_name = inf_name
        self._kernel_path = kernel_path
        self._line_length = line_length
        self._distributed_memory = distributed_memory

        _config = Config.get()
        if not api:
            api = _config.default_api
        else:
            check_api(api)
        self._api = api

        self._arg_name_to_module_name = {}
        self._unique_invoke_labels = []

        self._builtin_name_map, \
            self._builtin_defs_file = get_builtin_defs(self._api)

    def parse(self, alg_filename):
        ''' xxx '''

        from fparser.two.Fortran2003 import Main_Program, Module, \
            Subroutine_Subprogram, Function_Subprogram, Use_Stmt, \
            Call_Stmt
        from fparser.two.utils import walk_ast

        if self._line_length:
            check_ll(alg_filename)

        if self._api == "nemo":
            # For this API we just parse the NEMO code and return the resulting
            # fparser2 AST with None for the Algorithm AST.
            ast = parse_fp2(alg_filename)
            return None, ast

        alg_parse_tree = parse_fp2(alg_filename)

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

        self._unique_invoke_labels = []
        self._arg_name_to_module_name = {}
        invoke_calls = []

        for statement in walk_ast(alg_parse_tree.content):

            if isinstance(statement, Use_Stmt):
                # found a Fortran use statement
                self.update_arg_to_module_map(statement)

            if isinstance(statement, Call_Stmt):
                # found a Fortran call statement
                call_name = str(statement.items[0])
                if call_name.lower() == self._invoke_name.lower():
                    # The call statement is an invoke
                    invoke_call = self.create_invoke_call(statement,
                                                          alg_filename)
                    invoke_calls.append(invoke_call)

        return alg_parse_tree, FileInfo(container_name, invoke_calls)

    def create_invoke_call(self, statement, alg_filename):
        ''' xxx '''

        from fparser.two.Fortran2003 import Actual_Arg_Spec_List, \
            Actual_Arg_Spec, Part_Ref

        # Extract argument list. This can be removed when
        # fparser#170 is implemented
        argument_list = []
        if isinstance(statement.items[1], Actual_Arg_Spec_List):
            argument_list = statement.items[1].items
        else:
            # Expecting a single entry rather than a list
            argument_list = [statement.items[1]]

        invoke_label = None
        statement_kcalls = []

        for argument in argument_list:

            if isinstance(argument, Actual_Arg_Spec):
                # This should be the invoke label.
                if invoke_label:
                    raise ParseError(
                        "An invoke must contain one or zero 'name=xxx' "
                        "arguments but found more than one in: {0} in "
                        "file {1}".
                        format(str(statement), alg_filename))
                invoke_label = self.check_invoke_label(argument, alg_filename)

            elif isinstance(argument, Part_Ref):
                # This should be a kernel call.

                kernel_call = self.create_kernel_call(argument, alg_filename)
                statement_kcalls.append(kernel_call)

            else:
                # I don't support this.
                print ("  unexpected input")
                print ("  arg: {0}".format(argument))
                print ("  type: {0}".format(type(argument)))
                raise Exception("xxx")

        return InvokeCall(statement_kcalls, name=invoke_label)

    def create_kernel_call(self, argument, alg_filename):
        ''' xxx '''
        kernel_name, args = get_kernel(argument, alg_filename)

        if kernel_name.lower() in self._builtin_name_map.keys():
            kernel_call = self.create_builtin_kernel_call(
                kernel_name, alg_filename, args)
        else:
            kernel_call = self.create_coded_kernel_call(
                kernel_name, alg_filename, args)
        return kernel_call

    def create_builtin_kernel_call(self, kernel_name, alg_filename, args):
        if kernel_name in self._arg_name_to_module_name:
            raise ParseError(
                "A built-in cannot be named in a use "
                "statement but '{0}' is used from "
                "module '{1}' in file {2}".
                format(kernel_name,
                       self._arg_name_to_module_name[kernel_name],
                       alg_filename))
        from psyclone.parse_kernel import BuiltInKernelTypeFactory
        return BuiltInCall(BuiltInKernelTypeFactory(api=self._api).create(
            self._builtin_name_map.keys(), self._builtin_defs_file,
            name=kernel_name.lower()), args)

    def create_coded_kernel_call(self, kernel_name, alg_filename, args):

        try:
            module_name = self._arg_name_to_module_name[kernel_name]
        except KeyError:
            raise ParseError(
                "kernel call '{0}' must either be named "
                "in a use "
                "statement (found {1}) or be a recognised built-in "
                "(one of '{2}' for this API)".
                format(kernel_name,
                       list(self._arg_name_to_module_name.values()),
                       list(self._builtin_name_map.keys())))
        # coded kernel
        from psyclone.parse_kernel import get_kernel_ast
        modast = get_kernel_ast(module_name, alg_filename, self._kernel_path,
                                self._line_length)
        from psyclone.parse_kernel import KernelTypeFactory
        return KernelCall(module_name,
                          KernelTypeFactory(api=self._api).create(
                              modast, name=kernel_name), args)

    def update_arg_to_module_map(self, statement):
        '''Takes a use statement and adds its contents to the
        arg_name_to_module_name map.

        ********
        '''
        from fparser.two.Fortran2003 import Only_List

        # make sure statement is a use

        statement_kcalls = []
        use_name = str(statement.items[2])

        # Extract only list. This can be removed when
        # fparser#170 is implemented
        if isinstance(statement.items[4], Only_List):
            only_list = statement.items[4].items
        else:
            only_list = [statement.items[4]]

        for item in only_list:
            self._arg_name_to_module_name[str(item)] = use_name

    def check_invoke_label(self, argument, alg_filename):
        ''' xxx '''

        invoke_label = get_invoke_label(argument, alg_filename)
        if invoke_label in self._unique_invoke_labels:
            raise ParseError(
                "Found multiple named invoke()'s with the same "
                "label ('{0}') when parsing {1}".
                format(invoke_label, alg_filename))
        self._unique_invoke_labels.append(invoke_label)
        return invoke_label


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
        raise ParseError(
            "The (optional) name of an invoke must be specified as a string, "
            "but found {0} in {1}".format(str(parse_tree.items[1]),
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
                   "kernel '{2}' in file '{3}'.".format(
                       type(argument), str(argument), parse_tree,
                       alg_filename))
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
