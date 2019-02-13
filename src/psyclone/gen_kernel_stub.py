# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
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
# Author R. Ford STFC Daresbury Lab
# Modified work Copyright (c) 2017 by J. Henrichs, Bureau of Meteorology

'''A python script and python function to generate an empty kernel
    subroutine with the required arguments and datatypes (which we
    call a stub) when presented with Kernel Metadata.
'''

from __future__ import print_function
import os
import sys
import traceback

import fparser
from psyclone.dynamo0p3 import DynKern, DynKernMetadata
from psyclone.psyGen import GenerationError
from psyclone.parse.utils import ParseError
from psyclone.configuration import Config
from psyclone.line_length import FortLineLength


def generate(filename, api=""):

    '''Generates an empty kernel subroutine with the required arguments
       and datatypes (which we call a stub) when presented with Kernel
       Metadata. This is useful for Kernel developers to make sure
       they are using the correct arguments in the correct order.  The
       Kernel Metadata must be presented in the standard Kernel
       format.

       :param str filename: The name of the file for which to create a \
               kernel stub for.
       :param str api: The name of the API for which to create a kernel \
              stub. Must be one of the supported stub APIs.

       :raise GenerationError: if an invalid stub API is specified.
       :raise IOError: if filename does not specify a file.
       :raise ParseError: if the given file could not be parsed.
    '''
    if api == "":
        api = Config.get().default_stub_api
    if api not in Config.get().supported_stub_apis:
        print("Unsupported API '{0}' specified. Supported API's are {1}.".
              format(api, Config.get().supported_stub_apis))
        raise GenerationError(
            "generate: Unsupported API '{0}' specified. Supported types are "
            "{1}.".format(api, Config.get().supported_stub_apis))

    if not os.path.isfile(filename):
        raise IOError("file '{0}' not found".format(filename))

    # drop cache
    fparser.one.parsefortran.FortranParser.cache.clear()
    fparser.logging.disable(fparser.logging.CRITICAL)
    try:
        ast = fparser.api.parse(filename, ignore_comments=False)

    except (fparser.common.utils.AnalyzeError, AttributeError) as error:
        raise ParseError("Code appears to be invalid Fortran: " +
                         str(error))

    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    return kernel.gen_stub


def run():
    ''' Top-level driver for the kernel-stub generator. Handles command-line
    flags, calls generate() and applies line-length limiting to the output (if
    requested). '''
    import argparse
    parser = argparse.ArgumentParser(description="Create Kernel stub code from"
                                                 " Kernel metadata")
    parser.add_argument("-o", "--outfile", help="filename of output")
    parser.add_argument("-api", default=Config.get().default_stub_api,
                        help="choose a particular api from {0}, default {1}".
                        format(str(Config.get().supported_stub_apis),
                               Config.get().default_stub_api))
    parser.add_argument('filename', help='Kernel metadata')
    parser.add_argument(
        '-l', '--limit', dest='limit', action='store_true', default=False,
        help='limit the fortran line length to 132 characters')

    args = parser.parse_args()

    try:
        stub = generate(args.filename, api=args.api)
    except (IOError, ParseError, GenerationError, RuntimeError) as error:
        print("Error:", error)
        exit(1)
    except Exception as error:   # pylint: disable=broad-except
        print("Error, unexpected exception:\n")
        exc_type, exc_value, exc_traceback = sys.exc_info()
        print(exc_type)
        print(exc_value)
        traceback.print_tb(exc_traceback)
        exit(1)

    if args.limit:
        fll = FortLineLength()
        stub_str = fll.process(str(stub))
    else:
        stub_str = str(stub)
    if args.outfile is not None:
        my_file = open(args.outfile, "w")
        my_file.write(stub_str)
        my_file.close()
    else:
        print("Kernel stub code:\n", stub_str)
