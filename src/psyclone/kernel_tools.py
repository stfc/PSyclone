# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

'''
    This module provides the PSyclone kernel-generation 'run' routine
    which is intended to be driven from the bin/psyclone-kern executable
    script. It processes the various command-line options and then calls
    the appropriate routines to do one of the following:

    1. use the metadata to construct an appropriate Fortran subroutine stub
       for completion by a kernel developer;
    2. construct an Algorithm-layer driver program that performs the necessary
       setup and then calls the supplied kernel with an `invoke`.

'''

from __future__ import absolute_import, print_function

import argparse
import io
import sys
import traceback

from psyclone import alg_gen, gen_kernel_stub
from psyclone.configuration import Config, ConfigurationError
from psyclone.errors import GenerationError, InternalError
from psyclone.line_length import FortLineLength
from psyclone.parse.utils import ParseError
from psyclone.version import __VERSION__

# Dictionary of supported generation modes where values are a brief
# description of what is created (used when outputting to stdout).
GEN_MODES = {"alg": "Algorithm code",
             "stub": "Kernel-stub code"}


def run(args):
    '''
    Driver for the psyclone-kern tool.

    Parses and checks the command line arguments, calls the appropriate
    generate function(s) if all is well, catches any errors and outputs the
    results.

    :param list args: the list of command-line arguments with which \
                      psyclone-kern has been invoked.
    '''
    # pylint: disable=too-many-statements,too-many-branches

    # Make sure we have the supported APIs defined in the Config singleton,
    # but postpone loading the config file till the command line was parsed
    # in case that the user specifies a different config file.
    Config.get(do_not_load_file=True)

    # We specify the name of the program so that the expected messages are
    # produced if running within pytest.
    parser = argparse.ArgumentParser(
        prog="psyclone-kern",
        description='Run the PSyclone kernel generator on a particular file.')

    parser.add_argument('-gen', choices=GEN_MODES.keys(), default="stub",
                        help="what to generate for the supplied kernel "
                        "(alg=algorithm layer, stub=kernel-stub "
                        "subroutine). Defaults to stub.")
    parser.add_argument('-o', dest='out_file', default=None,
                        help="filename for created code.")
    parser.add_argument('-api',
                        help=f"choose a particular API from "
                        f"{Config.get().supported_apis}, default "
                        f"'{Config.get().default_api}'.")
    parser.add_argument('filename', help='file containing Kernel metadata.')

    # Make the default an empty list so that we can check whether the
    # user has supplied a value(s) later
    parser.add_argument(
        '-I', '--include', default=[], action="append",
        help='path to Fortran INCLUDE or module files.')
    parser.add_argument(
        '-l', '--limit', dest='limit', default='off',
        choices=['off', 'all', 'output'],
        help='limit the Fortran line length to 132 characters (default '
        '\'%(default)s\'). Use \'all\' to apply limit to both input and '
        'output Fortran. Use \'output\' to apply line-length limit to output '
        'Fortran only.')

    parser.add_argument("--config", help="config file with "
                        "PSyclone specific options.")
    parser.add_argument(
        '-v', '--version', dest='version', action="store_true",
        help=f"display version information ({__VERSION__})")

    args = parser.parse_args(args)

    if args.version:
        print(f"psyclone-kern version: {__VERSION__}", file=sys.stdout)

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
        if args.gen == "alg":
            # Generate algorithm
            code = alg_gen.generate(args.filename, api=api)
        elif args.gen == "stub":
            # Generate kernel stub
            code = gen_kernel_stub.generate(args.filename, api=api)
        else:
            raise InternalError(f"Expected -gen option to be one of "
                                f"{list(GEN_MODES.keys())} but got {args.gen}")

    except (IOError, ParseError, GenerationError, RuntimeError) as error:
        print("Error:", error, file=sys.stderr)
        sys.exit(1)

    except Exception:   # pylint: disable=broad-except
        print("Error, unexpected exception:\n", file=sys.stderr)
        exc_type, exc_value, exc_traceback = sys.exc_info()
        print(exc_type, file=sys.stderr)
        print(exc_value, file=sys.stderr)
        traceback.print_tb(exc_traceback)
        sys.exit(1)

    if args.limit != "off":
        # Apply line-length limiting to the output code.
        fll = FortLineLength()
        code_str = fll.process(str(code))
    else:
        code_str = str(code)

    if args.out_file:
        with io.open(args.out_file, mode='w', encoding='utf-8') as fobj:
            fobj.write(code_str)
    else:
        print(f"{GEN_MODES[args.gen]}:\n", code_str, file=sys.stdout)
