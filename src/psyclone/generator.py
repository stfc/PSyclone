# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified A. J. Voysey, Met Office
# Modified work Copyright (c) 2018 by J. Henrichs, Bureau of Meteorology

'''
    This module provides the PSyclone 'main' routine which is intended
    to be driven from the bin/psyclone executable script. 'main'
    takes an algorithm file as input and produces modified algorithm
    code and generated PSy code. A function, 'generate', is also provided
    which has the same functionality as 'main' but can be called
    from within another Python program.
'''

from __future__ import absolute_import, print_function

import argparse
import io
import os
import sys
import traceback

import six

from psyclone import configuration
from psyclone.alg_gen import Alg, NoInvokesError
from psyclone.configuration import Config, ConfigurationError
from psyclone.domain.common.algorithm import AlgorithmInvokeCall
from psyclone.domain.common.transformations import (AlgTrans,
                                                    AlgInvoke2PSyCallTrans)
from psyclone.errors import GenerationError
from psyclone.line_length import FortLineLength
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.profiler import Profiler
from psyclone.psyGen import PSyFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Loop
from psyclone.version import __VERSION__

# Those APIs that do not have a separate Algorithm layer
API_WITHOUT_ALGORITHM = ["nemo"]


def handle_script(script_name, info, function_name, is_optional=False):
    '''Loads and applies the specified script to the given algorithm or
    psy layer. The relevant script function (in 'function_name') is
    called with 'info' as the argument.

    :param str script_name: name of the script to load.
    :param info: PSyclone representation of the algorithm or psy layer \
        to which the script is applied.
    :type info: :py:class:`psyclone.psyGen.PSy` or \
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
    sys_path_appended = False
    try:
        # a script has been provided
        filepath, filename = os.path.split(script_name)
        if filepath:
            # a path to a file has been provided
            # we need to check the file exists
            if not os.path.isfile(script_name):
                raise IOError("script file '{0}' not found".
                              format(script_name))
            # it exists so we need to add the path to the python
            # search path
            sys_path_appended = True
            sys.path.append(filepath)
        filename, fileext = os.path.splitext(filename)
        if fileext != '.py':
            raise GenerationError(
                "generator: expected the script file '{0}' to have "
                "the '.py' extension".format(filename))
        try:
            transmod = __import__(filename)
        except ImportError as error:
            raise GenerationError(
                f"generator: attempted to import '{filename}' but script "
                f"file '{script_name}' has not been found") from error
        except SyntaxError as error:
            raise GenerationError(
                f"generator: attempted to import '{filename}' but script "
                f"file '{script_name}' is not valid python") from error
        if callable(getattr(transmod, function_name, None)):
            try:
                func_call = getattr(transmod, function_name)
                info = func_call(info)
            except Exception:
                exc_type, exc_value, exc_traceback = sys.exc_info()
                lines = traceback.format_exception(exc_type, exc_value,
                                                   exc_traceback)
                e_str = '{\n' +\
                    ''.join('    ' + line for line in lines[2:]) + '}'
                # pylint: disable=raise-missing-from
                raise GenerationError(
                    "Generator: script file '{0}'\nraised the "
                    "following exception during execution "
                    "...\n{1}\nPlease check your script".format(
                        script_name, e_str))
        elif not is_optional:
            raise GenerationError(
                f"generator: attempted to import '{filename}' but script file "
                f"'{script_name}' does not contain a '{function_name}' "
                f"function")
    except Exception as msg:
        if sys_path_appended:
            os.sys.path.pop()
        raise msg
    if sys_path_appended:
        os.sys.path.pop()


def generate(filename, api="", kernel_paths=None, script_name=None,
             line_length=False,
             distributed_memory=None,
             kern_out_path="",
             kern_naming="multiple"):
    # pylint: disable=too-many-arguments
    '''Takes a PSyclone algorithm specification as input and outputs the
    associated generated algorithm and psy codes suitable for
    compiling with the specified kernel(s) and support
    infrastructure. Uses the :func:`parse.algorithm.parse` function to
    parse the algorithm specification, the :class:`psyGen.PSy` class
    to generate the PSy code and the :class:`alg_gen.Alg` class to
    generate the modified algorithm code.

    :param str filename: the file containing the algorithm specification.
    :param str api: the name of the API to use. Defaults to empty string.
    :param kernel_paths: the directories from which to recursively \
        search for the files containing the kernel source (if \
        different from the location of the algorithm specification). \
        Defaults to None.
    :type kernel_paths: list of str or NoneType
    :param str script_name: a script file that can apply optimisations \
        to the PSy layer (can be a path to a file or a filename that \
        relies on the PYTHONPATH to find the module). Defaults to None.
    :param bool line_length: a logical flag specifying whether we care \
        about line lengths being longer than 132 characters. If so, \
        the input (algorithm and kernel) code is checked to make sure \
        that it conforms. The default is False.
    :param bool distributed_memory: a logical flag specifying whether \
        to generate distributed memory code. The default is set in the \
        'config.py' file.
    :param str kern_out_path: directory to which to write transformed \
        kernel code. Defaults to empty string.
    :param bool kern_naming: the scheme to use when re-naming transformed \
        kernels. Defaults to "multiple".
    :return: 2-tuple containing the fparser1 AST for the algorithm code and \
        the fparser1 AST or a string (for NEMO) of the psy code.
    :rtype: (:py:class:`fparser.one.block_statements.BeginSource`, \
             :py:class:`fparser.one.block_statements.Module`) or \
            (:py:class:`fparser.one.block_statements.BeginSource`, str)

    :raises GenerationError: if an invalid API is specified.
    :raises GenerationError: if an invalid kernel-renaming scheme is specified.
    :raises IOError: if the filename or search path do not exist.

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
                "generate: Unsupported API '{0}' specified. Supported "
                "types are {1}.".format(api, Config.get().supported_apis))

    # Store Kernel-output options in our Configuration object
    Config.get().kernel_output_dir = kern_out_path
    try:
        Config.get().kernel_naming = kern_naming
    except ValueError as verr:
        six.raise_from(
            GenerationError("Invalid kernel-renaming scheme supplied: {0}".
                            format(str(verr))), verr)

    if not os.path.isfile(filename):
        raise IOError("File '{0}' not found".format(filename))
    for kernel_path in kernel_paths:
        if not os.access(kernel_path, os.R_OK):
            raise IOError(
                "Kernel search path '{0}' not found".format(kernel_path))

    ast, invoke_info = parse(filename, api=api, invoke_name="invoke",
                             kernel_paths=kernel_paths,
                             line_length=line_length)
    if api != "gocean1.0":
        psy = PSyFactory(api, distributed_memory=distributed_memory)\
            .create(invoke_info)
        if script_name is not None:
            handle_script(script_name, psy, "trans")

    alg_gen = None

    if api == "gocean1.0":
        # Create language-level PSyIR from the Fortran file
        reader = FortranReader()
        psyir = reader.psyir_from_file(filename)

        # Raise to Algorithm PSyIR
        alg_trans = AlgTrans()
        alg_trans.apply(psyir)

        if script_name is not None:
            # Call the optimisation script for algorithm optimisations
            handle_script(script_name, psyir, "trans_alg", is_optional=True)

        # Transform 'invoke' calls into calls to PSy-layer subroutines
        invoke_trans = AlgInvoke2PSyCallTrans()
        for invoke in psyir.walk(AlgorithmInvokeCall):
            invoke_trans.apply(invoke)

        # Create Fortran from Algorithm PSyIR
        writer = FortranWriter()
        alg_gen = writer(psyir)

        # Create the PSy-layer
        # TODO: issue #1629 replace invoke_info with alg psyir
        psy = PSyFactory(api, distributed_memory=distributed_memory)\
            .create(invoke_info)

        if script_name is not None:
            # Call the optimisation script for psy-layer optimisations
            handle_script(script_name, psy, "trans")

    elif api not in API_WITHOUT_ALGORITHM:
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
    :param list args: the list of command-line arguments that PSyclone has \
                      been invoked with.
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
                        help='choose a particular api from {0}, '
                             'default \'{1}\'.'
                        .format(str(Config.get().supported_apis),
                                Config.get().default_api))
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
        help="Add profiling hooks for either 'kernels' or 'invokes'")
    parser.set_defaults(dist_mem=Config.get().distributed_memory)

    parser.add_argument("--config", help="Config file with "
                        "PSyclone specific options.")
    parser.add_argument(
        '-v', '--version', dest='version', action="store_true",
        help='Display version information ({0})'.format(__VERSION__))

    args = parser.parse_args(args)

    if args.version:
        print("PSyclone version: {0}".format(__VERSION__))

    if args.profile:
        Profiler.set_options(args.profile)

    # If an output directory has been specified for transformed kernels
    # then check that it is valid
    if args.okern:
        if not os.path.exists(args.okern):
            print("Specified kernel output directory ({0}) does not exist.".
                  format(args.okern), file=sys.stderr)
            sys.exit(1)
        if not os.access(args.okern, os.W_OK):
            print("Cannot write to specified kernel output directory ({0}).".
                  format(args.okern), file=sys.stderr)
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
        print("Unsupported API '{0}' specified. Supported APIs are "
              "{1}.".format(args.api, Config.get().supported_apis),
              file=sys.stderr)
        sys.exit(1)
    else:
        # There is a valid API specified on the command line. Set it
        # as API in the config object as well.
        api = args.api
        Config.get().api = api

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
        print("Warning: {0}".format(exc_value))
        # no invoke calls were found in the algorithm file so we do
        # not need to process it, or generate any psy layer code, so
        # output the original algorithm file and set the psy file to
        # be empty
        alg_file = open(args.filename)
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
        write_unicode_file(alg_str, args.oalg)
    else:
        print("Transformed algorithm code:\n%s" % alg_str)

    if not psy_str:
        # empty file so do not output anything
        pass
    elif args.opsy is not None:
        write_unicode_file(psy_str, args.opsy)
    else:
        print("Generated psy layer code:\n", psy_str)


def write_unicode_file(contents, filename):
    '''Wrapper routine that ensures that a string is encoded as unicode before
    writing to file.

    :param str contents: string to write to file.
    :param str filename: the name of the file to create.

    '''
    encoding = {'encoding': 'utf-8'}
    with io.open(filename, mode='w', **encoding) as file_object:
        file_object.write(contents)
