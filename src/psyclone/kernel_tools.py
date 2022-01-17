# -*- coding: utf-8 -*-
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
    the appropriate routines to do one or both of the following:

    1. use the metadata to construct an appropriate Fortran subroutine stub
       for completion by a kernel developer;
    2. construct an Algorithm-layer driver program that performs the necessary
       setup and then `invoke`s the supplied kernel.

'''

from __future__ import absolute_import, print_function

import argparse
import io
import sys
import traceback

from psyclone.configuration import Config, ConfigurationError
from psyclone.errors import GenerationError
from psyclone.line_length import FortLineLength
from psyclone.parse.utils import ParseError
from psyclone.version import __VERSION__


def run():
    '''
    Parses and checks the command line arguments, calls the generate
    function if all is well, catches any errors and outputs the
    results.

    :param list args: the list of command-line arguments which which \
                      psyclone-kern has been invoked.
    '''
    # pylint: disable=too-many-statements,too-many-branches

    # Make sure we have the supported APIs defined in the Config singleton,
    # but postpone loading the config file till the command line was parsed
    # in case that the user specifies a different config file.
    Config.get(do_not_load_file=True)

    parser = argparse.ArgumentParser(
        description='Run the PSyclone kernel generator on a particular file')
    parser.add_argument('--alg-gen', action="store_true",
                        help='generate an algorithm layer for the supplied '
                        'kernel')
    parser.add_argument('--stub-gen', action="store_true",
                        help='generate a stub kernel subroutine')
    parser.add_argument('-oalg', help="filename of created algorithm code "
                        "(implies '--alg-gen'")
    parser.add_argument('-ostub', help="filename of created kernel stub code "
                        "(implies '--stub-gen')")
    parser.add_argument('-api',
                        help='choose a particular api from {0}, '
                             'default \'{1}\'.'
                        .format(str(Config.get().supported_apis),
                                Config.get().default_api))
    parser.add_argument('filename', help='file containing Kernel metadata')
    parser.add_argument('-s', '--script', help='filename of a PSyclone'
                        ' optimisation script')

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

    parser.add_argument("--config", help="Config file with "
                        "PSyclone specific options.")
    parser.add_argument(
        '-v', '--version', dest='version', action="store_true",
        help='Display version information ({0})'.format(__VERSION__))

    args = parser.parse_args()

    if args.version:
        print(f"psyclone-kern version: {__VERSION__}")

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
        print("Unsupported API '{0}' specified. Supported API's are "
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

    alg = None
    stub = None
    try:
        if args.alg_gen or args.oalg:
            # Generate algorithm
            args.alg_gen = True
            pass
        if args.stub_gen or args.ostub:
            # Generate kernel stub
            args.stub_gen = True
            from psyclone.gen_kernel_stub import generate
            stub = generate(args.filename, api=api)
            pass
    except (IOError, ParseError, GenerationError, RuntimeError) as error:
        print("Error:", error, file=sys.stderr)
        exit(1)
    except Exception:   # pylint: disable=broad-except
        print("Error, unexpected exception:\n")
        exc_type, exc_value, exc_traceback = sys.exc_info()
        print(exc_type, file=sys.stderr)
        print(exc_value, file=sys.stderr)
        traceback.print_tb(exc_traceback)
        exit(1)

    if not (args.stub_gen or args.alg_gen):
        print("ARPDBG need one of xxx or yyy", file=sys.stderr)
        sys.exit(1)

    if args.limit:
        fll = FortLineLength()
        if stub:
            stub_str = fll.process(str(stub))
        if alg:
            alg_str = fll.process(str(alg))
    else:
        if stub:
            stub_str = str(stub)
        if alg:
            alg_str = str(alg)

    if stub:
        if args.ostub:
            write_unicode_file(stub_str, args.ostub)
        else:
            print("Kernel stub code:\n", stub_str, file=sys.stdout)

    if alg:
        if args.oalg:
            write_unicode_file(alg_str, args.oalg)
        else:
            print("Algorithm code:\n", alg_str, file=sys.stdout)


def write_unicode_file(contents, filename):
    '''Wrapper routine that ensures that a string is encoded as unicode before
    writing to file.

    :param str contents: string to write to file.
    :param str filename: the name of the file to create.

    '''
    encoding = {'encoding': 'utf-8'}
    with io.open(filename, mode='w', **encoding) as file_object:
        file_object.write(contents)
