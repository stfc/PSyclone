# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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

from __future__ import absolute_import
from collections import OrderedDict
import six

from fparser.two import pattern_tools
from fparser.two.utils import walk
# pylint: disable=no-name-in-module
from fparser.two.Fortran2003 import Main_Program, Module, \
    Subroutine_Subprogram, Function_Subprogram, Use_Stmt, Call_Stmt, \
    Actual_Arg_Spec, Data_Ref, Part_Ref, Char_Literal_Constant, \
    Section_Subscript_List, Name, Real_Literal_Constant, \
    Int_Literal_Constant, Function_Reference, Level_2_Unary_Expr, \
    Add_Operand, Parenthesis, Structure_Constructor, Component_Spec_List, \
    Proc_Component_Ref, Kind_Selector, Type_Declaration_Stmt, \
    Declaration_Type_Spec, Entity_Decl, Intrinsic_Type_Spec, \
    Data_Component_Def_Stmt, Component_Decl
# pylint: enable=no-name-in-module

from psyclone.configuration import Config
from psyclone.errors import InternalError
from psyclone.parse.kernel import BuiltInKernelTypeFactory, get_kernel_ast, \
    KernelTypeFactory
from psyclone.parse.utils import check_api, check_line_length, ParseError, \
    parse_fp2

# pylint: disable=too-many-statements
# pylint: disable=too-many-branches

# Section 1: parse the algorithm file


def parse(alg_filename, api="", invoke_name="invoke", kernel_paths=None,
          line_length=False):
    '''Takes a PSyclone conformant algorithm file as input and outputs a
    parse tree of the code contained therein and an object containing
    information about the 'invoke' calls in the algorithm file and any
    associated kernels within the invoke calls.

    :param str alg_filename: the file containing the algorithm \
        specification.
    :param str api: the PSyclone API to use when parsing the \
        code. Defaults to empty string.
    :param str invoke_name: the expected name of the invocation calls \
        in the algorithm code. Defaults to "invoke".
    :param kernel_paths: the paths to search for kernel source files \
        (if different from the location of the algorithm source). \
        Defaults to None.
    :type kernel_paths: list of str or NoneType
    :param bool line_length: a logical flag specifying whether we care \
        about line lengths being longer than 132 characters. If so, \
        the input (algorithm and kernel) code is checked to make sure \
        that it conforms and an error raised if not. The default is \
        False.

    :returns: 2-tuple consisting of the fparser2 parse tree of the \
        Algorithm file and an object holding details of the invokes \
        found.
    :rtype: (:py:class:`fparser.two.Fortran2003.Program`, \
             :py:class:`psyclone.parse.FileInfo`)

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> ast, info = parse(SOURCE_FILE)

    '''
    if kernel_paths is None:
        kernel_paths = []
    # Parsing is encapsulated in the Parser class. We keep this
    # function for compatibility.
    my_parser = Parser(api, invoke_name, kernel_paths, line_length)
    return my_parser.parse(alg_filename)


# pylint: disable=too-many-instance-attributes
class Parser(object):
    '''Supports the parsing of PSyclone conformant algorithm code within a
    file and extraction of relevant information for any 'invoke' calls
    contained within the code.

    :param str api: the PSyclone API to use when parsing the code.
    :param str invoke_name: the expected name of the invocation calls in the \
        algorithm code.
    :param kernel_paths: the paths to search for kernel source files \
        (if different from the location of the algorithm source). \
        Defaults to None.
    :type kernel_paths: list of str or NoneType
    :param bool line_length: a logical flag specifying whether we care \
        about line lengths being longer than 132 characters. If so, \
        the input (algorithm and kernel) code is checked to make sure \
        that it conforms and an error raised if not. The default is \
        False.

    For example:

    >>> from psyclone.parse.algorithm import Parser
    >>> parser = Parser(api="gocean1.0")
    >>> ast, info = parser.parse(SOURCE_FILE)

    '''

    def __init__(self, api="", invoke_name="invoke", kernel_paths=None,
                 line_length=False):

        self._invoke_name = invoke_name
        if kernel_paths is None:
            self._kernel_paths = []
        else:
            self._kernel_paths = kernel_paths
        self._line_length = line_length

        _config = Config.get()
        if not api:
            api = _config.default_api
        else:
            check_api(api)
        self._api = api

        self._arg_name_to_module_name = {}
        # Dict holding a 2-tuple consisting of type and precision
        # information for each variable declared in the algorithm
        # file, indexed by variable name.
        self._arg_type_defns = {}
        self._unique_invoke_labels = []

        # Use the get_builtin_defs helper function to access
        # information about the builtins supported by this API. The
        # first argument contains the names of the builtins and the
        # second is the file where these names are defined.
        self._builtin_name_map, \
            self._builtin_defs_file = get_builtin_defs(self._api)

        self._alg_filename = None

    def parse(self, alg_filename):
        '''Takes a PSyclone conformant algorithm file as input and outputs a
        parse tree of the code contained therein and an object
        containing information about the 'invoke' calls in the
        algorithm file and any associated kernels within the invoke
        calls. If the NEMO API is being used then the parsed code is
        returned without any additional information about the code.

        :param str alg_filename: The file containing the algorithm code.

        :returns: 2-tuple consisting of the fparser2 parse tree of the \
            algorithm code and an object holding details of the \
            algorithm code and the invokes found within it, unless it \
            is the NEMO API, where the first entry of the tuple is \
            None and the second is the fparser2 parse tree of the \
            code.
        :rtype: (:py:class:`fparser.two.Fortran2003.Program`, \
            :py:class:`psyclone.parse.FileInfo`) or (NoneType, \
            :py:class:`fparser.two.Fortran2003.Program`)

        '''
        self._alg_filename = alg_filename
        if self._line_length:
            # Make sure the code conforms to the line length limit.
            check_line_length(alg_filename)
        alg_parse_tree = parse_fp2(alg_filename)

        if self._api == "nemo":
            # For this API we just parse the NEMO code and return the resulting
            # fparser2 AST with None for the Algorithm AST.
            return None, alg_parse_tree

        return alg_parse_tree, self.invoke_info(alg_parse_tree)

    def invoke_info(self, alg_parse_tree):
        '''Takes an fparser2 representation of a PSyclone-conformant algorithm
        code as input and returns an object containing information
        about the 'invoke' calls in the algorithm file and any
        associated kernels within the invoke calls. Also captures the
        type and precision of every variable declaration within the
        parse tree.

        :param alg_parse_tree: the fparser2 representation of the \
            algorithm code.
        :type: :py:class:`fparser.two.Fortran2003.Program`

        :returns: an object holding details of the algorithm \
            code and the invokes found within it.
        :rtype: :py:class:`psyclone.parse.FileInfo`

        :raises ParseError: if a program, module, subroutine or \
            function is not found in the fparser2 tree.
        :raises InternalError: if the fparser2 tree representing the \
            type declaration statements is not in the expected form.
        :raises NotImplementedError: if the algorithm code contains \
            two different datatypes with the same name.

        '''
        # Find the first program, module, subroutine or function in the
        # parse tree. The assumption here is that the first is the one
        # that is required. See issue #307.
        container_name = None
        for child in alg_parse_tree.content:
            if isinstance(child, (Main_Program, Module, Subroutine_Subprogram,
                                  Function_Subprogram)):
                container_name = str(child.content[0].items[1])
                break

        if not container_name:
            # Nothing relevant found.
            raise ParseError(
                "algorithm.py:parser:parse: Program, module, function or "
                "subroutine not found in fparser2 parse tree.")

        self._unique_invoke_labels = []
        self._arg_name_to_module_name = OrderedDict()
        # Dict holding a 2-tuple consisting of type and precision
        # information for each variable declared in the algorithm
        # file, indexed by variable name.
        self._arg_type_defns = {}
        invoke_calls = []

        # Find all invoke calls and capture information about
        # them. Also find information about use statements and find
        # all declarations within the supplied parse
        # tree. Declarations will include the definitions of any
        # components of derived types that are defined within the
        # code.
        for statement in walk(alg_parse_tree.content,
                              types=(Type_Declaration_Stmt,
                                     Data_Component_Def_Stmt,
                                     Use_Stmt, Call_Stmt)):
            if isinstance(statement,
                          (Type_Declaration_Stmt, Data_Component_Def_Stmt)):
                # Capture datatype information for the variable
                spec = statement.children[0]
                if isinstance(spec, Declaration_Type_Spec):
                    # This is a type declaration
                    my_type = spec.children[1].string.lower()
                    my_precision = None
                elif isinstance(spec, Intrinsic_Type_Spec):
                    # This is an intrinsic declaration
                    my_type = spec.children[0].lower()
                    my_precision = None
                    if isinstance(spec.children[1], Kind_Selector):
                        my_precision = \
                            spec.children[1].children[1].string.lower()
                else:
                    raise InternalError(
                        "Expected first child of Type_Declaration_Stmt or "
                        "Data_Component_Def_Stmt to be Declaration_Type_Spec "
                        "or Intrinsic_Type_Spec but found '{0}'"
                        "".format(type(spec).__name__))
                for decl in walk(statement.children[2], (
                        Entity_Decl, Component_Decl)):
                    # Determine the variables names. Note that if a
                    # variable declaration is a component of a derived
                    # type, its name is stored 'as is'. This means
                    # that e.g. real :: a will clash with a
                    # derived-type definition if the latter has a
                    # component named 'a' and their datatypes differ.
                    my_var_name = decl.children[0].string.lower()
                    if my_var_name in self._arg_type_defns and (
                            self._arg_type_defns[my_var_name][0] != my_type or
                            self._arg_type_defns[my_var_name][1] !=
                            my_precision):
                        raise NotImplementedError(
                            "The same symbol '{0}' is used for different "
                            "datatypes, '{1}, {2}' and '{3}, {4}'. This is "
                            "not currently supported.".format(
                                my_var_name,
                                self._arg_type_defns[my_var_name][0],
                                self._arg_type_defns[my_var_name][1],
                                my_type, my_precision))
                    # Store the variable name and information about its type
                    self._arg_type_defns[my_var_name] = (my_type, my_precision)

            if isinstance(statement, Use_Stmt):
                # found a Fortran use statement
                self.update_arg_to_module_map(statement)

            if isinstance(statement, Call_Stmt):
                # found a Fortran call statement
                call_name = str(statement.items[0])
                if call_name.lower() == self._invoke_name.lower():
                    # The call statement is an invoke
                    invoke_call = self.create_invoke_call(statement)
                    invoke_calls.append(invoke_call)

        return FileInfo(container_name, invoke_calls)

    def create_invoke_call(self, statement):
        '''Takes the part of a parse tree containing an invoke call and
        returns an InvokeCall object which captures the required
        information about the invoke.

        :param statement: Parse tree of the invoke call.
        :type statement: :py:class:`fparser.two.Fortran2003.Call_Stmt`
        :returns: An InvokeCall object which contains relevant \
        information about the invoke call.
        :rtype: :py:class:`psyclone.parse.algorithm.InvokeCall`
        :raises ParseError: if more than one invoke argument contains \
        'name=xxx'.
        :raises ParseError: if an unknown or unsupported invoke \
        argument is found.

        '''
        # Extract argument list.
        argument_list = statement.items[1].items

        invoke_label = None
        kernel_calls = []

        for argument in argument_list:

            if isinstance(argument, Actual_Arg_Spec):
                # This should be the invoke label.
                if invoke_label:
                    raise ParseError(
                        "algorithm.py:Parser():create_invoke_call: An invoke "
                        "must contain one or zero 'name=xxx' arguments but "
                        "found more than one in: {0} in file {1}".
                        format(str(statement), self._alg_filename))
                invoke_label = self.check_invoke_label(argument)

            elif isinstance(
                    argument, (Data_Ref, Part_Ref, Structure_Constructor)):
                # This should be a kernel call.
                kernel_call = self.create_kernel_call(argument)
                kernel_calls.append(kernel_call)

            else:
                # Unknown and/or unsupported argument type
                raise ParseError(
                    "algorithm.py:Parser():create_invoke_call: Expecting "
                    "argument to be of the form 'name=xxx' or a "
                    "Kernel call but found '{0}' in file "
                    "'{1}'.".format(argument, self._alg_filename))

        return InvokeCall(kernel_calls, name=invoke_label)

    def create_kernel_call(self, argument):
        '''Takes the parse tree of an invoke argument containing a
        reference to a kernel or a builtin and returns the kernel or
        builtin object respectively which contains the required
        information.

        :param argument: Parse tree of an invoke argument. This \
            should contain a kernel name and associated arguments.
        :type argument: :py:class:`fparser.two.Fortran2003.Part_Ref` or \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: A builtin or coded kernel call object which contains \
            relevant information about the Kernel.
        :rtype: :py:class:`psyclone.parse.algorithm.KernelCall` or \
            :py:class:`psyclone.parse.algorithm.BuiltInCall`

        '''
        kernel_name, args = get_kernel(argument, self._alg_filename,
                                       self._arg_type_defns)
        if kernel_name.lower() in self._builtin_name_map.keys():
            # This is a builtin kernel
            kernel_call = self.create_builtin_kernel_call(
                kernel_name, args)
        else:
            # This is a coded kernel
            kernel_call = self.create_coded_kernel_call(
                kernel_name, args)
        return kernel_call

    def create_builtin_kernel_call(self, kernel_name, args):
        '''Takes the builtin kernel name and a list of Arg objects which
        capture information about the builtin call arguments and
        returns a BuiltinCall instance with content specific to the
        particular API (as specified in self._api).

        :param str kernel_name: the name of the builtin kernel being \
        called
        :param args: a list of 'Arg' instances containing the required \
        information for the arguments being passed from the algorithm \
        layer. The list order is the same as the argument order.
        :type arg: list of :py:class:`psyclone.parse.algorithm.Arg`
        :returns: a BuiltInCall instance with information specific to \
        the API.
        :rtype: :py:class:`psyclone.parse.algorithm.BuiltInCall`
        :raises ParseError: if the builtin is specified in a use \
        statement in the algorithm layer

        '''
        if kernel_name.lower() in self._arg_name_to_module_name:
            raise ParseError(
                "A built-in cannot be named in a use "
                "statement but '{0}' is used from "
                "module '{1}' in file {2}".
                format(kernel_name,
                       self._arg_name_to_module_name[kernel_name.lower()],
                       self._alg_filename))

        return BuiltInCall(BuiltInKernelTypeFactory(api=self._api).create(
            self._builtin_name_map.keys(), self._builtin_defs_file,
            name=kernel_name.lower()), args)

    def create_coded_kernel_call(self, kernel_name, args):
        '''Takes a coded kernel name and a list of Arg objects which
        capture information about the coded call arguments and
        returns a KernelCall instance with content specific to the
        particular API (as specified in self._api).

        :param str kernel_name: the name of the coded kernel being \
        called
        :param args: a list of 'Arg' instances containing the required \
        information for the arguments being passed from the algorithm \
        layer. The list order is the same as the argument order.
        :type arg: list of :py:class:`psyclone.parse.algorithm.Arg`
        :returns: a KernelCall instance with information specific to \
        the API.
        :rtype: :py:class:`psyclone.parse.algorithm.KernelCall`
        :raises ParseError: if the kernel is not specified in a use \
        statement in the algorithm layer

        '''
        try:
            module_name = self._arg_name_to_module_name[kernel_name.lower()]
        except KeyError as info:
            message = (
                "kernel call '{0}' must either be named in a use statement "
                "(found {1}) or be a recognised built-in (one of '{2}' for "
                "this API)".format(
                    kernel_name.lower(),
                    list(self._arg_name_to_module_name.values()),
                    list(self._builtin_name_map.keys())))
            six.raise_from(ParseError(message), info)

        modast = get_kernel_ast(module_name, self._alg_filename,
                                self._kernel_paths, self._line_length)
        return KernelCall(module_name,
                          KernelTypeFactory(api=self._api).create(
                              modast, name=kernel_name), args)

    def update_arg_to_module_map(self, statement):
        '''Takes a use statement and adds its contents to the internal
        arg_name_to_module_name map. This map associates names
        specified in the 'only' list with the corresponding use name.

        :param statement: A use statement
        :type statement: :py:class:`fparser.two.Fortran2003.Use_Stmt`
        :raises InternalError: if the statement being passed is not an \
        fparser use statement.

        '''
        # make sure statement is a use
        if not isinstance(statement, Use_Stmt):
            raise InternalError(
                "algorithm.py:Parser:update_arg_to_module_map: Expected "
                "a use statement but found instance of "
                "'{0}'.".format(type(statement)))

        use_name = str(statement.items[2])

        # Extract 'only' list.
        if statement.items[4]:
            only_list = statement.items[4].items
        else:
            only_list = []

        for item in only_list:
            self._arg_name_to_module_name[str(item).lower()] = use_name

    def check_invoke_label(self, argument):
        '''Takes the parse tree of an invoke argument containing an invoke
        label. Raises an exception if this label has already been used
        by another invoke in the same algorithm code. If all is well
        it returns the label as a string.

        :param argument: Parse tree of an invoke argument. This \
        should contain a "name=xxx" argument.
        :type argument: :py:class:`fparser.two.Actual_Arg_Spec`
        :returns: the label as a string.
        :rtype: str
        :raises ParseError: if this label has already been used by \
        another invoke in this algorithm code.

        '''
        invoke_label = get_invoke_label(argument, self._alg_filename)
        if invoke_label in self._unique_invoke_labels:
            raise ParseError(
                "Found multiple named invoke()'s with the same "
                "label ('{0}') when parsing {1}".
                format(invoke_label, self._alg_filename))
        self._unique_invoke_labels.append(invoke_label)
        return invoke_label

# pylint: enable=too-many-arguments
# pylint: enable=too-many-instance-attributes

# Section 2: Support functions


def get_builtin_defs(api):
    '''
    Get the names of the supported built-in operations and the file
    containing the associated meta-data for the supplied API

    :param str api: the specified PSyclone API.
    :returns: a 2-tuple containing a dictionary of the supported \
              built-ins and the filename where these built-ins are specified.
    :rtype: (dict, str)

    '''

    # Check that the supplied API is valid
    check_api(api)

    # pylint: disable=import-outside-toplevel
    if api == "dynamo0.3":
        from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP \
            as builtins
        from psyclone.domain.lfric.lfric_builtins import \
            BUILTIN_DEFINITIONS_FILE as fname
    else:
        # We don't support any built-ins for this API
        builtins = {}
        fname = None
    return builtins, fname


def get_invoke_label(parse_tree, alg_filename, identifier="name"):
    '''Takes an invoke argument contained in the parse_tree argument and
    returns the label specified within it.

    :param parse_tree: Parse tree of an invoke argument. This should \
    contains a "name=xxx" argument.
    :type parse_tree: :py:class:`fparser.two.Actual_Arg_Spec`
    :param str alg_filename: The file containing the algorithm code.
    :param str identifier: An optional string specifying the name \
    used to specify a named arguement. Defaults to 'name'.
    :returns: the label as a string.
    :rtype: str
    :except InternalError: if the form of the argument is not what was \
    expected.
    :except InternalError: if the number of items contained in the \
    argument is not what was expected.
    :except ParseError: if the name used for the named argument does \
    not match what was expected.
    :except ParseError: if the label is not specified as a string.
    :except ParseError: if the label is not a valid Fortran name \
    (after any white space has been replaced with '_').

    '''
    if not isinstance(parse_tree, Actual_Arg_Spec):
        raise InternalError(
            "algorithm.py:Parser:get_invoke_label: Expected a Fortran "
            "argument of the form name=xxx but found instance of "
            "'{0}'.".format(type(parse_tree)))

    if len(parse_tree.items) != 2:
        raise InternalError(
            "algorithm.py:Parser:get_invoke_label: Expected the Fortran "
            "argument to have two items but found "
            "'{0}'.".format(len(parse_tree.items)))

    ident = str(parse_tree.items[0])
    if ident.lower() != identifier.lower():
        raise ParseError(
            "algorithm.py:Parser:get_invoke_label Expected named identifier "
            "to be '{0}' but found '{1}'".format(identifier.lower(),
                                                 ident.lower()))

    if not isinstance(parse_tree.items[1], Char_Literal_Constant):
        raise ParseError(
            "algorithm.py:Parser:get_invoke_label The (optional) name of an "
            "invoke must be specified as a string, but found {0} in "
            "{1}".format(str(parse_tree.items[1]), alg_filename))

    invoke_label = parse_tree.items[1].items[0]
    invoke_label = invoke_label.lower()
    invoke_label = invoke_label.strip()
    if invoke_label[0] == '"' and invoke_label[-1] == '"' or \
       invoke_label[0] == "'" and invoke_label[-1] == "'":
        invoke_label = invoke_label[1:-1]
    invoke_label = invoke_label.replace(" ", "_")

    if not pattern_tools.abs_name.match(invoke_label):
        raise ParseError(
            "algorithm.py:Parser:get_invoke_label the (optional) name of an "
            "invoke must be a string containing a valid Fortran name (with "
            "any spaces replaced by underscores) but got '{0}' in file "
            "{1}".format(invoke_label, alg_filename))

    return invoke_label


def get_kernel(parse_tree, alg_filename, arg_type_defns):
    '''Takes the parse tree of an invoke kernel argument and returns the
    name of the kernel and a list of Arg instances which capture the
    relevant information about the arguments associated with the
    kernel.

    :param parse_tree: parse tree of an invoke argument. This \
        should contain a kernel name and associated arguments.
    :type parse_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` or \
        :py:class:`fparser.two.Fortran2003.Structure_Constructor`
    :param str alg_filename: The file containing the algorithm code.

    :param arg_type_defns: dictionary holding a 2-tuple consisting of \
        type and precision information for each variable declared in \
        the algorithm layer, indexed by variable name.
    :type arg_type_defns: dict[str] = (str, str or NoneType)

    :returns: a 2-tuple with the name of the kernel being called and a \
        list of 'Arg' instances containing the required information for \
        the arguments being passed from the algorithm layer. The list \
        order is the same as the argument order.
    :rtype: (str, list of :py:class:`psyclone.parse.algorithm.Arg`)

    :raises InternalError: if the parse tree is of the wrong type.
    :raises InternalError: if Part_Ref or Structure_Constructor do not \
        have two children.
    :raises InternalError: if Proc_Component_Ref has a child with an \
        unexpected type.
    :raises InternalError: if Data_Ref has a child with an unexpected \
        type.
    :raises NotImplementedError: if an expression contains a variable.
    :raises InternalError: if an unsupported argument format is found.

    '''
    # pylint: disable=too-many-branches
    if not isinstance(parse_tree, (Part_Ref, Structure_Constructor)):
        raise InternalError(
            f"algorithm.py:get_kernel: Expected a parse tree (type Part_Ref "
            f"or Structure_Constructor) but found instance of "
            f"'{type(parse_tree)}'.")

    if len(parse_tree.items) != 2:
        raise InternalError(
            f"algorithm.py:get_kernel: Expected Part_Ref or "
            f"Structure_Constructor to have 2 children "
            f"but found {len(parse_tree.items)}.")

    kernel_name = str(parse_tree.items[0])

    # Extract argument list. This can be removed when fparser#211 is fixed.
    argument_list = []
    if isinstance(parse_tree.items[1],
                  (Section_Subscript_List, Component_Spec_List)):
        argument_list = parse_tree.items[1].items
    else:
        # Expecting a single entry rather than a list
        argument_list = [parse_tree.items[1]]

    arguments = []
    for argument in argument_list:
        if isinstance(argument, (Real_Literal_Constant, Int_Literal_Constant)):
            # A simple constant e.g. 1.0, or 1_i_def
            precision = argument.children[1]
            if precision:
                precision = str(precision)
            if isinstance(argument, Real_Literal_Constant):
                datatype = ("real", precision)
            else:
                datatype = ("integer", precision)
            arguments.append(Arg('literal', argument.tostr().lower(),
                                 datatype=datatype))
        elif isinstance(argument, Name):
            # A simple variable e.g. arg
            full_text = str(argument).lower()
            var_name = full_text
            datatype = arg_type_defns.get(var_name)
            arguments.append(Arg('variable', full_text, varname=var_name,
                                 datatype=datatype))
        elif isinstance(argument, Part_Ref):
            # An indexed variable e.g. arg(n)
            full_text = argument.tostr().lower()
            var_name = str(argument.items[0]).lower()
            datatype = arg_type_defns.get(var_name)
            arguments.append(Arg('indexed_variable', full_text,
                                 varname=var_name, datatype=datatype))
        elif isinstance(argument, Function_Reference):
            # A function reference e.g. func(). The datatype of this
            # function is not determined so datatype in Arg is None.
            full_text = argument.tostr().lower()
            designator = argument.items[0]
            lhs = designator.items[0]
            lhs = create_var_name(lhs)
            rhs = str(designator.items[2])
            var_name = "{0}_{1}".format(lhs, rhs)
            var_name = var_name.lower()
            arguments.append(Arg('indexed_variable', full_text,
                                 varname=var_name))
        elif isinstance(argument, (Data_Ref, Proc_Component_Ref)):
            # A structure access e.g. a % b or self % c
            if isinstance(argument, Proc_Component_Ref):
                if isinstance(argument.children[2], Name):
                    arg = argument.children[2].string.lower()
                else:
                    # It does not appear to be possible to get to here
                    # as an array (e.g. self%a(10)) is not treated as
                    # being a Proc_Component_Ref by fparser2 and
                    # Data_Ref otherwise always has a Name on the rhs
                    # (3rd argument).
                    raise InternalError(
                        f"The third argument to to a Proc_Component_Ref is "
                        f"expected to be a Name, but found "
                        f"'{type(argument.children[2]).__name__}'.")
            elif isinstance(argument, Data_Ref):
                rhs_node = argument.children[-1]
                if isinstance(rhs_node, Part_Ref):
                    rhs_node = rhs_node.children[0]
                if not isinstance(rhs_node, Name):
                    raise InternalError(
                        f"The last child of a Data_Ref is expected to be "
                        f"a Name or a Part_Ref whose first child is a "
                        f"Name, but found '{type(rhs_node).__name__}'.")
                arg = rhs_node.string.lower()
            datatype = arg_type_defns.get(arg)
            full_text = argument.tostr().lower()
            var_name = create_var_name(argument).lower()
            collection_type = None
            if not datatype and isinstance(argument, Data_Ref):
                # This could be a collection of some sort.
                # Find the name of the parent type.
                collection = argument.children[-2]
                if isinstance(collection, Part_Ref):
                    collection = collection.children[0]
                if isinstance(collection, Name):
                    collection_type = arg_type_defns.get(
                        collection.tostr().lower())
            if collection_type:
                arguments.append(
                    Arg('collection', full_text,
                        varname=var_name, datatype=collection_type))
            else:
                arguments.append(Arg('variable', full_text,
                                     varname=var_name, datatype=datatype))
        elif isinstance(argument, (Level_2_Unary_Expr, Add_Operand,
                                   Parenthesis)):
            # An expression e.g. -1, 1*n, ((1*n)/m). Note, for some
            # reason Add_Operation represents binary expressions in
            # fparser2.  Walk the tree to look for an argument.
            if walk(argument, Name):
                raise NotImplementedError(
                    f"algorithm.py:get_kernel: Expressions containing "
                    f"variables are not yet supported '{type(argument)}', "
                    f"value '{str(argument)}', kernel '{parse_tree}' in "
                    f"file '{alg_filename}'.")
            # This is a literal so store the full expression as a
            # string.
            candidate_datatype = None
            for literal in walk(
                # Determine datatype and precision.
                    argument, (Real_Literal_Constant,
                               Int_Literal_Constant)):
                precision = literal.children[1]
                if precision:
                    precision = str(precision)
                if isinstance(literal, Real_Literal_Constant):
                    datatype = ("real", precision)
                else:  # it's an Int_Literal_Constant
                    datatype = ("integer", precision)

                if not candidate_datatype:
                    # This is the first candidate
                    candidate_datatype = datatype
                elif candidate_datatype != datatype:
                    raise NotImplementedError(
                        f"Found two non-matching literals within an "
                        f"expression ('{str(argument)}') passed into an "
                        f"invoke from the algorithm layer. '{datatype}' and "
                        f"'{candidate_datatype}' do not match. This is not "
                        f"supported in PSyclone.")
            arguments.append(Arg('literal', argument.tostr().lower(),
                                 datatype=datatype))
        else:
            raise InternalError(
                f"algorithm.py:get_kernel: Unsupported argument structure "
                f"'{type(argument)}', value '{str(argument)}', kernel "
                f"'{parse_tree}' in file '{alg_filename}'.")

    return kernel_name, arguments


def create_var_name(arg_parse_tree):
    '''Creates a valid variable name from an argument that optionally
    includes brackets and potentially dereferences using '%'.

    :param arg_parse_tree: the input argument. Contains braces and \
        potentially dereferencing. e.g. a%b(c).
    :type arg_parse_tree: :py:class:`fparser.two.Fortran2003.Name` or \
        :py:class:`fparser.two.Fortran2003.Data_Ref` or \
        :py:class:`fparser.two.Fortran2003.Part_Ref` or \
        :py:class:`fparser.two.Fortran2003.Proc_Component_Ref`

    :returns: a valid variable name.
    :rtype: str

    :raises InternalError: if unrecognised fparser content is found.

    '''
    tree = arg_parse_tree
    if isinstance(tree, Name):
        return str(tree)
    if isinstance(tree, Part_Ref):
        return str(tree.items[0])
    if isinstance(tree, Proc_Component_Ref):
        # Proc_Component_Ref is of the form 'variable %
        # proc-name'. Its RHS (proc-name) is always a Name but its
        # LHS (variable) could be more complex, so call the function
        # again for the LHS.
        return "{0}_{1}".format(
            create_var_name(tree.items[0]), tree.items[2])
    if isinstance(tree, Data_Ref):
        component_names = []
        for item in tree.items:
            if isinstance(item, (Data_Ref, Part_Ref)):
                component_names.append(str(item.items[0]))
            elif isinstance(item, Name):
                component_names.append(str(item))
            else:
                raise InternalError(
                    "algorithm.py:create_var_name unrecognised structure "
                    "'{0}' in '{1}'.".format(type(item), type(tree)))
        return "_".join(component_names)
    raise InternalError(
        "algorithm.py:create_var_name unrecognised structure "
        "'{0}'".format(type(tree)))

# Section 3: Classes holding algorithm information.


class FileInfo(object):
    '''Captures information about the algorithm file and the invoke calls
    found within the contents of the file.

    :param str name: the name of the algorithm program unit (program, \
    module, subroutine or function)
    :param calls: information about the invoke calls in the algorithm code.
    :type calls: list of :py:class:`psyclone.parse.algorithm.InvokeCall`

    '''

    def __init__(self, name, calls):
        self._name = name
        self._calls = calls

    @property
    def name(self):
        '''
        :returns: the name of the algorithm program unit
        :rtype: str

        '''
        return self._name

    @property
    def calls(self):
        '''
        :returns: information about invoke calls
        :rtype: list of :py:class:`psyclone.parse.algorithm.InvokeCall`

        '''
        return self._calls


class InvokeCall(object):
    '''Keeps information about an individual invoke call.

    :param kcalls: Information about the kernels specified in the \
    invoke.
    :type kcalls: list of \
    :py:class:`psyclone.parse.algorithm.KernelCall` or \
    :py:class:`psyclone.parse.algorithm.BuiltInCall`
    :param str name: An optional name to call the transformed invoke \
    call. This defaults to None.
    :param str invoke_name: the name that is used to indicate an invoke \
    call. This defaults to 'invoke'.

    '''

    def __init__(self, kcalls, name=None, invoke_name="invoke"):
        self._kcalls = kcalls
        if name:
            # Prefix the name with invoke_name + '_" unless it already
            # starts with that ...
            if not name.lower().startswith("{0}_".format(invoke_name)):
                self._name = "{0}_".format(invoke_name) + name.lower()
            else:
                self._name = name.lower()
        else:
            self._name = None

    @property
    def name(self):
        '''
        :returns: the name of this invoke call
        :rtype: str

        '''
        return self._name

    @property
    def kcalls(self):
        '''
        :returns: the list of kernel calls in this invoke call
        :rtype: list of \
        :py:class:`psyclone.parse.algorithm.KernelCall` or \
        :py:class:`psyclone.parse.algorithm.BuiltInCall`

        '''
        return self._kcalls


class ParsedCall(object):
    '''Base class for information about a user-supplied or built-in
    kernel.

    :param ktype: information about a kernel or builtin. Provides \
        access to the PSyclone description metadata and the code if it \
        exists.
    :type ktype: API-specific specialisation of \
        :py:class:`psyclone.parse.kernel.KernelType`
    :param args: a list of Arg instances which capture the relevant \
        information about the arguments associated with the call to the \
        kernel or builtin.
    :type args: list of :py:class:`psyclone.parse.algorithm.Arg`

    '''
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
        self._module_name = None

    @property
    def ktype(self):
        '''
        :returns: information about a kernel or builtin. Provides \
            access to the PSyclone description metadata and the code if it \
            exists.
        :rtype: API-specific specialisation of \
            :py:class:`psyclone.parse.kernel.KernelType`

        '''
        return self._ktype

    @property
    def args(self):
        '''
        :returns: a list of Arg instances which capture the relevant \
        information about the arguments associated with the call to the \
        kernel or builtin
        :rtype: list of :py:class:`psyclone.parse.algorithm.Arg`

        '''
        return self._args

    @property
    def module_name(self):
        '''This name is assumed to be set by the subclasses.

        :returns: the name of the module containing the kernel code.
        :rtype: str

        '''
        return self._module_name


class KernelCall(ParsedCall):
    '''Store information about a user-supplied (coded) kernel. Specialises
    the generic ParsedCall class adding a module name value and a
    type for distinguishing this class.

    :param str module_name: the name of the kernel module.
    :param ktype: information about the kernel. Provides access to the \
    PSyclone description metadata and the code.
    :type ktype: API-specific specialisation of \
    :py:class:`psyclone.parse.kernel.KernelType`
    :param args: a list of Arg instances which capture the relevant \
    information about the arguments associated with the call to the \
    kernel.
    :type arg: list of :py:class:`psyclone.parse.algorithm.Arg`

    '''
    def __init__(self, module_name, ktype, args):
        ParsedCall.__init__(self, ktype, args)
        self._module_name = module_name

    @property
    def type(self):
        '''Specifies that this is a kernel call.

        :returns: the type of call as a string.
        :rtype: str

        '''
        return "kernelCall"

    def __repr__(self):
        return "KernelCall('{0}', {1})".format(self.ktype.name, self.args)


class BuiltInCall(ParsedCall):
    '''Store information about a system-supplied (builtin)
    kernel. Specialises the generic ParsedCall class adding a function
    name method (the name of the builtin) and a type for
    distinguishing this class.

    :param ktype: information about this builtin. Provides \
    access to the PSyclone description metadata.
    :type ktype: API-specific specialisation of \
    :py:class:`psyclone.parse.kernel.KernelType`
    :param args: a list of Arg instances which capture the relevant \
    information about the arguments associated with the call to the \
    kernel or builtin
    :type args: list of :py:class:`psyclone.parse.algorithm.Arg`

    '''
    def __init__(self, ktype, args):
        ParsedCall.__init__(self, ktype, args)
        self._func_name = ktype.name

    @property
    def func_name(self):
        '''
        :returns: the name of this builtin.
        :rtype: str

        '''
        return self._func_name

    @property
    def type(self):
        '''Specifies that this is a builtin call.

        :returns: the type of call as a string.
        :rtype: str

        '''
        return "BuiltInCall"

    def __repr__(self):
        return "BuiltInCall('{0}', {1})".format(self.ktype.name, self.args)


class Arg(object):
    '''Description of an argument as obtained from parsing kernel or
    builtin arguments within invokes in a PSyclone algorithm code.

    :param str form: describes whether the argument is a literal \
    value, standard variable or indexed variable. Supported options \
    are specified in the local form_options list.
    :param str text: the original Fortran text of the argument.
    :param varname: the extracted variable name from the text if the \
    form is not literal otherwise it is set to None. This is optional \
    and defaults to None.
    :value varname: str or NoneType
    :param datatype: a tuple containing information about the datatype \
        and precision of the argument, or None if no information is \
        available. Defaults to None.
    :type datatype: (str, str or NoneType) or NoneType

    :raises InternalError: if the form argument is not one one of the \
    supported types as specified in the local form_options list.

    '''
    form_options = ["literal", "variable", "indexed_variable", "collection"]

    def __init__(self, form, text, varname=None, datatype=None):
        self._form = form
        self._text = text
        self._varname = varname
        if form not in Arg.form_options:
            raise InternalError(
                f"algorithm.py:Alg:__init__: Unknown arg type provided. "
                f"Expected one of {str(Arg.form_options)} but found "
                f"'{form}'.")
        # A tuple containing information about the datatype and
        # precision of this argument, or None if there is none.
        self._datatype = datatype

    def __str__(self):
        return (f"Arg(form='{self._form}',text='{self._text}',"
                f"varname='{self._varname}')")

    @property
    def form(self):
        '''
        :returns: a string indicating what type of variable this \
        is. Supported options are specified in the local form_options \
        list.
        :rtype: str

        '''
        return self._form

    @property
    def text(self):
        '''
        :returns: the original Fortran text of the argument.
        :rtype: str

        '''
        return self._text

    @property
    def varname(self):
        '''
        :returns: the extracted variable name from the text if the \
        form is not literal and None otherwise
        :rtype: str or NoneType

        '''
        return self._varname

    @varname.setter
    def varname(self, value):
        '''Allows the setting or re-setting of the variable name value.

        :param str value: the new variable name

        '''
        self._varname = value

    def is_literal(self):
        ''' Indicates whether this argument is a literal or not.

        :returns: True if this argument is a literal and False otherwise.
        :rtype: bool

        '''
        return self._form == "literal"


__all__ = ["parse", "Parser", "get_builtin_defs", "get_invoke_label",
           "get_kernel", "create_var_name", "FileInfo", "InvokeCall",
           "ParsedCall", "KernelCall", "BuiltInCall", "Arg"]
