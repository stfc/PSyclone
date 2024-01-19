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
# Authors: R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab
# Modified A. J. Voysey, Met Office
# Modified J. Henrichs, Bureau of Meteorology

'''
    This module provides the PSyclone 'main' routine which is intended
    to be driven from the bin/psyclone executable script. 'main'
    takes an algorithm file as input and produces modified algorithm
    code and generated PSy code. A function, 'generate', is also provided
    which has the same functionality as 'main' but can be called
    from within another Python program.
'''

import argparse
import os
import sys
import traceback

from fparser.api import get_reader
from fparser.two import Fortran2003

from psyclone import configuration
from psyclone.alg_gen import Alg, NoInvokesError
from psyclone.configuration import Config, ConfigurationError
from psyclone.domain.common.algorithm.psyir import (
    AlgorithmInvokeCall, KernelFunctor)
from psyclone.domain.common.transformations import AlgTrans
from psyclone.domain.gocean.transformations import (
    RaisePSyIR2GOceanKernTrans, GOceanAlgInvoke2PSyCallTrans)
from psyclone.domain.lfric.algorithm import LFRicBuiltinFunctor
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP
from psyclone.domain.lfric.transformations import (
    LFRicAlgTrans, RaisePSyIR2LFRicKernTrans, LFRicAlgInvoke2PSyCallTrans)
from psyclone.errors import GenerationError, InternalError
from psyclone.line_length import FortLineLength
from psyclone.parse import ModuleManager
from psyclone.parse.algorithm import parse
from psyclone.parse.kernel import get_kernel_filepath
from psyclone.parse.utils import ParseError, parse_fp2
from psyclone.profiler import Profiler
from psyclone.psyGen import PSyFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Loop, Container, Routine
from psyclone.psyir.symbols import UnresolvedInterface
from psyclone.psyir.transformations import TransformationError
from psyclone.version import __VERSION__

# Those APIs that do not have a separate Algorithm layer
API_WITHOUT_ALGORITHM = ["nemo"]

# TODO issue #1618 remove temporary LFRIC_TESTING flag, associated
# logic and Alg class plus tests (and ast variable).
#
# Temporary flag to allow optional testing of new LFRic metadata
# implementation (mainly that the PSyIR works with algorithm-layer
# code) whilst keeping the original implementation as default
# until it is working.
LFRIC_TESTING = False


def handle_script(script_name, info, function_name, is_optional=False):
    '''Loads and applies the specified script to the given algorithm or
    psy layer. The relevant script function (in 'function_name') is
    called with 'info' as the argument.

    :param str script_name: name of the script to load.
    :param info: PSyclone representation of the algorithm or psy layer \
        to which the script is applied.
    :type info: :py:class:`psyclone.psyGen.PSy` | \
        :py:class:`psyclone.psyir.nodes.Node`
    :param str function_name: the name of the function to call in the \
        script.
    :param bool is_optional: whether the function is optional or \
        not. Defaults to False.

    :raises IOError: if the file is not found.
    :raises GenerationError: if the file does not have .py extension \
        or can not be imported.
    :raises GenerationError: if the script function can not be called.

    :raises GenerationError: if any exception is raised when the \
        script function is called.

    '''
    # pylint: disable=too-many-locals
    sys_path_prepended = False
    try:
        filepath, filename = os.path.split(script_name)
        module_name, fileext = os.path.splitext(filename)
        # the file must either be:
        # a) at the given path or, given no path, in the current directory; or
        # b) given no path, in the system path
        if not (os.path.isfile(script_name) or
                not filepath and any(os.path.isfile(os.path.join(p, filename))
                                     for p in sys.path)):
            raise GenerationError(
                f"generator: script file '{script_name}' not found")
        # the file must have the .py extension
        if fileext != '.py':
            raise GenerationError(
                f"generator: expected the script file '{filename}' to have "
                f"the '.py' extension")
        # prepend file path - if none, the empty string equates to the current
        # working directory - to the system path to guarantee we find the user
        # provided module instead of a similarly named module that might
        # already exist elsewhere in the system path
        sys_path_prepended = True
        sys.path.insert(0, filepath)
        try:
            transmod = __import__(module_name)
        except Exception as error:
            raise GenerationError(
                f"generator: attempted to import specified PSyclone "
                f"transformation module '{module_name}' but a problem was "
                f"found: {error}") from error
        if callable(getattr(transmod, function_name, None)):
            try:
                func_call = getattr(transmod, function_name)
                info = func_call(info)
            except Exception:
                exc_type, exc_value, exc_traceback = sys.exc_info()
                lines = traceback.format_exception(exc_type, exc_value,
                                                   exc_traceback)
                e_str = '{\n' + ''.join('    ' + ln for ln in lines[2:]) + '}'
                # pylint: disable=raise-missing-from
                raise GenerationError(
                    f"generator: specified PSyclone transformation module "
                    f"'{module_name}'\nraised the following exception during "
                    f"execution...\n{e_str}\nplease check your script")
        elif not is_optional:
            raise GenerationError(
                f"generator: attempted to use specified PSyclone "
                f"transformation module '{module_name}' but it does not "
                f"contain a '{function_name}' function")
    finally:
        if sys_path_prepended:
            sys.path.pop(0)


def generate(filename, api="", kernel_paths=None, script_name=None,
             line_length=False,
             distributed_memory=None,
             kern_out_path="",
             kern_naming="multiple"):
    # pylint: disable=too-many-arguments, too-many-statements
    # pylint: disable=too-many-branches, too-many-locals
    '''Takes a PSyclone algorithm specification as input and outputs the
    associated generated algorithm and psy codes suitable for
    compiling with the specified kernel(s) and support
    infrastructure. Uses the :func:`parse.algorithm.parse` function to
    parse the algorithm specification, the :class:`psyGen.PSy` class
    to generate the PSy code and the :class:`alg_gen.Alg` class to
    generate the modified algorithm code.

    :param str filename: the file containing the algorithm specification.
    :param str api: the name of the API to use. Defaults to empty string.
    :param kernel_paths: the directories from which to recursively
        search for the files containing the kernel source (if
        different from the location of the algorithm specification).
        Defaults to None.
    :type kernel_paths: Optional[List[str]]
    :param str script_name: a script file that can apply optimisations
        to the PSy layer (can be a path to a file or a filename that
        relies on the PYTHONPATH to find the module). Defaults to None.
    :param bool line_length: a logical flag specifying whether we care
        about line lengths being longer than 132 characters. If so,
        the input (algorithm and kernel) code is checked to make sure
        that it conforms. The default is False.
    :param bool distributed_memory: a logical flag specifying whether
        to generate distributed memory code. The default is set in the
        'config.py' file.
    :param str kern_out_path: directory to which to write transformed
        kernel code. Defaults to empty string.
    :param bool kern_naming: the scheme to use when re-naming transformed
        kernels. Defaults to "multiple".
    :return: 2-tuple containing the fparser1 AST for the algorithm code and
        the fparser1 AST or a string (for NEMO) of the psy code.
    :rtype: Tuple[:py:class:`fparser.one.block_statements.BeginSource`,
        :py:class:`fparser.one.block_statements.Module`] |
        Tuple[:py:class:`fparser.one.block_statements.BeginSource`, str]

    :raises GenerationError: if an invalid API is specified.
    :raises GenerationError: if an invalid kernel-renaming scheme is specified.
    :raises GenerationError: if there is an error raising the PSyIR to
        domain-specific PSyIR.
    :raises GenerationError: if a kernel functor is not named in a use
        statement.
    :raises IOError: if the filename or search path do not exist.
    :raises NoInvokesError: if no invokes are found in the algorithm file.

    For example:

    >>> from psyclone.generator import generate
    >>> alg, psy = generate("algspec.f90")
    >>> alg, psy = generate("algspec.f90", kernel_paths=["src/kernels"])
    >>> alg, psy = generate("algspec.f90", script_name="optimise.py")
    >>> alg, psy = generate("algspec.f90", line_length=True)
    >>> alg, psy = generate("algspec.f90", distributed_memory=False)

    '''
    if kernel_paths is None:
        kernel_paths = []

    if distributed_memory is None:
        distributed_memory = Config.get().distributed_memory

    if api == "":
        api = Config.get().default_api
    else:
        if api not in Config.get().supported_apis:
            raise GenerationError(
                f"generate: Unsupported API '{api}' specified. Supported "
                f"types are {Config.get().supported_apis}.")

    # Store Kernel-output options in our Configuration object
    Config.get().kernel_output_dir = kern_out_path
    try:
        Config.get().kernel_naming = kern_naming
    except ValueError as verr:
        raise GenerationError(
            f"Invalid kernel-renaming scheme supplied: {str(verr)}") from verr

    if not os.path.isfile(filename):
        raise IOError(f"File '{filename}' not found")
    for kernel_path in kernel_paths:
        if not os.access(kernel_path, os.R_OK):
            raise IOError(
                f"Kernel search path '{kernel_path}' not found")

    # TODO #2011: investigate if kernel search path and module manager
    # can be combined.
    ModuleManager.get().add_search_path(kernel_paths)

    ast, invoke_info = parse(filename, api=api, invoke_name="invoke",
                             kernel_paths=kernel_paths,
                             line_length=line_length)

    if api in API_WITHOUT_ALGORITHM or \
       (api == "dynamo0.3" and not LFRIC_TESTING):
        psy = PSyFactory(api, distributed_memory=distributed_memory)\
            .create(invoke_info)
        if script_name is not None:
            handle_script(script_name, psy, "trans")
        alg_gen = None

    elif api == "gocean1.0" or (api == "dynamo0.3" and LFRIC_TESTING):
        # Create language-level PSyIR from the Algorithm file
        reader = FortranReader()
        if api == "dynamo0.3":
            # avoid undeclared builtin errors in PSyIR by adding a
            # wildcard use statement.
            fp2_tree = parse_fp2(filename)
            # Choose a module name that is invalid Fortran so that it
            # does not clash with any existing names in the algorithm
            # layer.
            builtins_module_name = "_psyclone_builtins"
            add_builtins_use(fp2_tree, builtins_module_name)
            psyir = Fparser2Reader().generate_psyir(fp2_tree)
            # Check that there is only one module/program per file.
            check_psyir(psyir, filename)
        else:
            psyir = reader.psyir_from_file(filename)

        # Raise to Algorithm PSyIR
        if api == "gocean1.0":
            alg_trans = AlgTrans()
        else:  # api == "dynamo0.3"
            alg_trans = LFRicAlgTrans()
        try:
            alg_trans.apply(psyir)
        except TransformationError as info:
            raise GenerationError(
                f"In algorithm file '{filename}':\n{info.value}") from info

        if not psyir.walk(AlgorithmInvokeCall):
            raise NoInvokesError(
                "Algorithm file contains no invoke() calls: refusing to "
                "generate empty PSy code")

        if script_name is not None:
            # Call the optimisation script for algorithm optimisations
            handle_script(script_name, psyir, "trans_alg", is_optional=True)

        # For each kernel called from the algorithm layer
        kernels = {}
        for invoke in psyir.walk(AlgorithmInvokeCall):
            kernels[id(invoke)] = {}
            for kern in invoke.walk(KernelFunctor):
                if isinstance(kern, LFRicBuiltinFunctor):
                    # Skip builtins
                    continue
                if isinstance(kern.symbol.interface, UnresolvedInterface):
                    # This kernel functor is not specified in a use statement.
                    # Find all container symbols that are in scope.
                    st_ref = kern.scope.symbol_table
                    container_symbols = [
                        symbol.name for symbol in st_ref.containersymbols]
                    while st_ref.parent_symbol_table():
                        st_ref = st_ref.parent_symbol_table()
                        container_symbols += [
                            symbol.name for symbol in st_ref.containersymbols]
                    message = (
                        f"Kernel functor '{kern.name}' in routine "
                        f"'{kern.ancestor(Routine).name}' from algorithm file "
                        f"'{filename}' must be named in a use statement "
                        f"(found {container_symbols})")
                    if api == "dynamo0.3":
                        message += (
                            f" or be a recognised built-in (one of "
                            f"{list(BUILTIN_MAP.keys())})")
                    message += "."
                    raise GenerationError(message)
                container_symbol = kern.symbol.interface.container_symbol

                # Find the kernel file containing the container
                filepath = get_kernel_filepath(
                    container_symbol.name, kernel_paths, filename)

                try:
                    # Create language-level PSyIR from the kernel file
                    kernel_psyir = reader.psyir_from_file(filepath)
                except InternalError as info:
                    print(f"In kernel file '{filepath}':\n{str(info.value)}",
                          file=sys.stderr)
                    sys.exit(1)

                # Raise to Kernel PSyIR
                if api == "gocean1.0":
                    kern_trans = RaisePSyIR2GOceanKernTrans(kern.symbol.name)
                    kern_trans.apply(kernel_psyir)
                else:  # api == "dynamo0.3"
                    kern_trans = RaisePSyIR2LFRicKernTrans()
                    kern_trans.apply(
                        kernel_psyir,
                        options={"metadata_name": kern.symbol.name})

                kernels[id(invoke)][id(kern)] = kernel_psyir

        # Transform 'invoke' calls into calls to PSy-layer subroutines
        if api == "gocean1.0":
            invoke_trans = GOceanAlgInvoke2PSyCallTrans()
        else:  # api == "dynamo0.3"
            invoke_trans = LFRicAlgInvoke2PSyCallTrans()
        for invoke in psyir.walk(AlgorithmInvokeCall):
            invoke_trans.apply(
                invoke, options={"kernels": kernels[id(invoke)]})
        if api == "dynamo0.3":
            # Remove any use statements that were temporarily added to
            # avoid the PSyIR complaining about undeclared builtin
            # names.
            for node in psyir.walk((Routine, Container)):
                symbol_table = node.symbol_table
                if builtins_module_name in symbol_table:
                    symbol = symbol_table.lookup(builtins_module_name)
                    symbol_table.remove(symbol)

        # Create Fortran from Algorithm PSyIR
        writer = FortranWriter()
        alg_gen = writer(psyir)

        # Create the PSy-layer
        # TODO: issue #1629 replace invoke_info with alg and kern psyir
        psy = PSyFactory(api, distributed_memory=distributed_memory)\
            .create(invoke_info)

        if script_name is not None:
            # Call the optimisation script for psy-layer optimisations
            handle_script(script_name, psy, "trans")

    # TODO issue #1618 remove Alg class and tests from PSyclone
    if api == "dynamo0.3" and not LFRIC_TESTING:
        alg_gen = Alg(ast, psy).gen

    # Add profiling nodes to schedule if automatic profiling has
    # been requested.
    for invoke in psy.invokes.invoke_list:
        Profiler.add_profile_nodes(invoke.schedule, Loop)

    return alg_gen, psy.gen


def main(args):
    '''
    Parses and checks the command line arguments, calls the generate
    function if all is well, catches any errors and outputs the
    results.

    :param args: the list of command-line arguments that PSyclone has \
        been invoked with.
    :type args: List[str]

    '''
    # pylint: disable=too-many-statements,too-many-branches

    # Make sure we have the supported APIs defined in the Config singleton,
    # but postpone loading the config file till the command line was parsed
    # in case that the user specifies a different config file.
    Config.get(do_not_load_file=True)

    parser = argparse.ArgumentParser(
        description='Run the PSyclone code generator on a particular file')
    parser.add_argument('-oalg', help='filename of transformed algorithm code')
    parser.add_argument(
        '-opsy', help='filename of generated PSy code')
    parser.add_argument('-okern',
                        help='directory in which to put transformed kernels, '
                        'default is the current working directory.')
    parser.add_argument('-api',
                        help=f'choose a particular api from '
                        f'{str(Config.get().supported_apis)}, '
                        f'default \'{Config.get().default_api}\'.')
    parser.add_argument('filename', help='algorithm-layer source code')
    parser.add_argument('-s', '--script', help='filename of a PSyclone'
                        ' optimisation script')
    parser.add_argument(
        '-d', '--directory', default=[], action="append", help='path to a '
        'root directory structure containing kernel source code. Multiple '
        'roots can be specified by using multiple -d arguments.')
    # Make the default an empty list so that we can check whether the
    # user has supplied a value(s) later
    parser.add_argument(
        '-I', '--include', default=[], action="append",
        help='path to Fortran INCLUDE or module files')
    parser.add_argument(
        '-l', '--limit', dest='limit', default='off',
        choices=['off', 'all', 'output'],
        help='limit the Fortran line length to 132 characters (default '
        '\'%(default)s\'). Use \'all\' to apply limit to both input and '
        'output Fortran. Use \'output\' to apply line-length limit to output '
        'Fortran only.')
    parser.add_argument(
        '-dm', '--dist_mem', dest='dist_mem', action='store_true',
        help='generate distributed memory code')
    parser.add_argument(
        '-nodm', '--no_dist_mem', dest='dist_mem', action='store_false',
        help='do not generate distributed memory code')
    parser.add_argument(
        '--kernel-renaming', default="multiple",
        choices=configuration.VALID_KERNEL_NAMING_SCHEMES,
        help="Naming scheme to use when re-naming transformed kernels")
    parser.add_argument(
        '--profile', '-p', action="append", choices=Profiler.SUPPORTED_OPTIONS,
        help=("Add profiling hooks for either 'kernels' or 'invokes/routines'."
              " The 'kernels' option is not permitted for the 'nemo' API."))
    parser.add_argument(
        '--backend', dest='backend',
        choices=['enable-validation', 'disable-validation'],
        help=("Options to control the PSyIR backend used for code generation. "
              "Use 'disable-validation' to disable the validation checks that "
              "are performed by default."))
    parser.set_defaults(dist_mem=Config.get().distributed_memory)

    parser.add_argument("--config", help="Config file with "
                        "PSyclone specific options.")
    parser.add_argument(
        '--version', '-v', action='version',
        version=f'PSyclone version: {__VERSION__}',
        help=f'Display version information ({__VERSION__})')

    args = parser.parse_args(args)

    # If an output directory has been specified for transformed kernels
    # then check that it is valid
    if args.okern:
        if not os.path.exists(args.okern):
            print(f"Specified kernel output directory ({args.okern}) does "
                  f"not exist.", file=sys.stderr)
            sys.exit(1)
        if not os.access(args.okern, os.W_OK):
            print(f"Cannot write to specified kernel output directory "
                  f"({args.okern}).", file=sys.stderr)
            sys.exit(1)
        kern_out_path = args.okern
    else:
        # We write any transformed kernels to the current working directory
        kern_out_path = os.getcwd()

    # If no config file name is specified, args.config is none
    # and config will load the default config file.
    Config.get().load(args.config)

    # Check API, if none is specified, take the setting from the config file
    if args.api is None:
        # No command line option, use the one specified in Config - which
        # is either based on a parameter in the config file, or otherwise
        # the default:
        api = Config.get().api
    elif args.api not in Config.get().supported_apis:
        print(f"Unsupported API '{args.api}' specified. Supported APIs are "
              f"{Config.get().supported_apis}.", file=sys.stderr)
        sys.exit(1)
    else:
        # There is a valid API specified on the command line. Set it
        # as API in the config object as well.
        api = args.api
        Config.get().api = api

    # Record any profiling options.
    if args.profile:
        try:
            Profiler.set_options(args.profile, api)
        except ValueError as err:
            print(f"Invalid profiling option: {err}", file=sys.stderr)
            sys.exit(1)
    if args.backend:
        # A command-line flag overrides the setting in the Config file (if
        # any).
        Config.get().backend_checks_enabled = (
            str(args.backend) == "enable-validation")

    # The Configuration manager checks that the supplied path(s) is/are
    # valid so protect with a try
    try:
        if args.include:
            Config.get().include_paths = args.include
        else:
            # Default is to instruct fparser2 to look in the directory
            # containing the file being parsed
            Config.get().include_paths = ["./"]
    except ConfigurationError as err:
        print(str(err), file=sys.stderr)
        sys.exit(1)

    try:
        alg, psy = generate(args.filename, api=api,
                            kernel_paths=args.directory,
                            script_name=args.script,
                            line_length=(args.limit == 'all'),
                            distributed_memory=args.dist_mem,
                            kern_out_path=kern_out_path,
                            kern_naming=args.kernel_renaming)
    except NoInvokesError:
        _, exc_value, _ = sys.exc_info()
        print(f"Warning: {exc_value}")
        # no invoke calls were found in the algorithm file so we do
        # not need to process it, or generate any psy layer code, so
        # output the original algorithm file and set the psy file to
        # be empty
        with open(args.filename, encoding="utf8") as alg_file:
            alg = alg_file.read()
        psy = ""
    except (OSError, IOError, ParseError, GenerationError,
            RuntimeError):
        _, exc_value, _ = sys.exc_info()
        print(exc_value, file=sys.stderr)
        sys.exit(1)
    except Exception:  # pylint: disable=broad-except
        print("Error, unexpected exception, please report to the authors:",
              file=sys.stderr)
        traceback.print_exception(*sys.exc_info(), file=sys.stderr)
        sys.exit(1)
    if args.limit != 'off':
        # Limit the line length of the output Fortran to ensure it conforms
        # to the 132 characters mandated by the standard.
        fll = FortLineLength()
        psy_str = fll.process(str(psy))
        alg_str = fll.process(str(alg))
    else:
        psy_str = str(psy)
        alg_str = str(alg)
    if args.oalg is not None:
        with open(args.oalg, mode='w', encoding="utf8") as alg_file:
            alg_file.write(alg_str)
    else:
        print(f"Transformed algorithm code:\n{alg_str}")

    if not psy_str:
        # empty file so do not output anything
        pass
    elif args.opsy is not None:
        with open(args.opsy, mode='w', encoding="utf8") as psy_file:
            psy_file.write(psy_str)
    else:
        print(f"Generated psy layer code:\n{psy_str}")


def check_psyir(psyir, filename):
    '''Check the supplied psyir to make sure that it contains a
    single program or module.

    :param psyir: the psyir to check.
    :type psyir: py:class:`psyclone.psyir.nodes.FileContainer`

    :raises GenerationError: if the algorithm file contains \
        multiple modules or programs.
    :raises GenerationError: if the algorithm file is not a \
        module or a program.

    '''
    if len(psyir.children) != 1:
        raise GenerationError(
            f"Expecting LFRic algorithm-layer code within file "
            f"'{filename}' to be a single program or module, but "
            f"found '{len(psyir.children)}' of type "
            f"{[type(node).__name__ for node in psyir.children]}.")
    if (not isinstance(psyir.children[0], Container) and not
        (isinstance(psyir.children[0], Routine) and
         psyir.children[0].is_program)):
        raise GenerationError(
            f"Expecting LFRic algorithm-layer code within file "
            f"'{filename}' to be a single program or module, but "
            f"found '{type(psyir.children[0]).__name__}'.")


def add_builtins_use(fp2_tree, name):
    '''Modify the fparser2 tree adding a 'use <name>' so that builtin kernel
    functors do not appear to be undeclared.

    :param fp2_tree: the fparser2 tree to modify.
    :type fp2_tree: py:class:`fparser.two.Program`
    :param str name: the name of the module imported by the use
        statement.

    '''
    for node in fp2_tree.children:
        if isinstance(node, (Fortran2003.Module, Fortran2003.Main_Program)):
            # add "use <name>" to the module or program
            if not isinstance(
                    node.children[1], Fortran2003.Specification_Part):
                # Create a valid use statement then modify its name as
                # the supplied name may be invalid Fortran to avoid
                # clashes with existing Fortran names.
                fp2_reader = get_reader("use dummy")
                spec_part = Fortran2003.Specification_Part(fp2_reader)
                use_stmt = spec_part.children[0]
                use_name = use_stmt.children[2]
                use_name.string = name
                node.children.insert(1, spec_part)
            else:
                spec_part = node.children[1]
                # Create a valid use statement then modify its name as
                # the supplied name may be invalid Fortran to avoid
                # clashes with existing Fortran names.
                use_stmt = Fortran2003.Use_Stmt("use dummy")
                use_name = use_stmt.children[2]
                use_name.string = name
                spec_part.children.insert(0, use_stmt)
