# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
#             T. Vockerodt, Met Office
# Modified by A. Pirrie, Met Office

'''Top-level driver functions for PSyAD : the PSyclone Adjoint
support. Transforms an LFRic tangent linear kernel to its adjoint.

'''
import argparse
import logging
import os
import re
import sys

from psyclone.configuration import Config, LFRIC_API_NAMES
from psyclone.line_length import FortLineLength
from psyclone.psyad.tl2ad import generate_adjoint_str
from psyclone.psyad.transformations import TangentLinearError


def main(args):
    '''Takes an LFRic tangent linear kernel source file as input and
    produces its adjoint.

    :param list args: the list of command-line arguments that PSyAD has \
                      been invoked with.

    '''
    # Make sure we have the supported APIs defined in the Config singleton,
    # but postpone loading the config file till the command line was parsed
    # in case that the user specifies a different config file.
    Config.get(do_not_load_file=True)

    # pylint: disable=too-many-statements, too-many-branches
    # TODO #1863 - expose line-length limiting as a command-line flag.
    line_length_limit = True

    logger = logging.getLogger(__name__)

    # There is a well known bug in argparse when mixing positional and
    # variable arguments. The simplest solution is to separate them
    # with --. The usage message is therefore manually updated to
    # reflect this workaround.
    def msg():
        '''Function to overide the argpass usage message'''
        return ("psyad [-h] [-oad OAD] [-v] [-t] [-api API] "
                "[-coord-arg COORD_ARG] [-panel-id-arg PANEL_ID_ARG] "
                "[-otest TEST_FILENAME] [-c CONFIG] "
                "-a ACTIVE [ACTIVE ...] -- filename")

    parser = argparse.ArgumentParser(
        description="Run the PSyclone adjoint code generator on a "
        "tangent-linear kernel file", usage=msg())
    parser.add_argument(
        '-a', '--active', nargs='+', help='names of active variables',
        required=True)
    parser.add_argument(
        '-c', '--config', help='config file with PSyclone specific options')
    parser.add_argument(
        '-v', '--verbose', help='increase the verbosity of the output',
        action='store_true')
    parser.add_argument(
        '-t', '--gen-test',
        help='generate a standalone unit test for the adjoint code',
        action='store_true')
    parser.add_argument('-api', default=None,
                        help='the PSyclone API that the TL kernel conforms '
                        'to (if any)')
    parser.add_argument('-coord-arg', default=None, type=int,
                        help='the position of the coordinate (chi) field in '
                        'the meta_args list of arguments in the kernel '
                        'metadata (LFRic only)')
    parser.add_argument('-panel-id-arg', default=None, type=int,
                        help='the position of the panel-ID field in the '
                        'meta_args list of arguments in the kernel metadata '
                        '(LFRic only)')
    parser.add_argument('-otest', default=None,
                        help='filename for the unit test (implies -t)',
                        dest='test_filename')
    parser.add_argument('-oad', help='filename for the transformed code')
    parser.add_argument('filename', help='tangent-linear kernel source')

    args = parser.parse_args(args)

    # If no config file name is specified, args.config is none
    # and config will load the default config file.
    Config.get().load(args.config)

    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)

    # Specifying an output file for the test harness is taken to mean that
    # the user wants us to generate it.
    generate_test = args.gen_test or args.test_filename

    # Check the command-line arguments for consistency.
    if args.api not in LFRIC_API_NAMES:
        if args.coord_arg is not None:
            logger.error(
                "The '-coord-arg' argument is only applicable to the 'lfric' "
                "API.")
            sys.exit(1)
        if args.panel_id_arg is not None:
            logger.error("The '-panel-id-arg' argument is only applicable to "
                         "the 'lfric' API.")
            sys.exit(1)

    # Processing test filename
    test_name = "adjoint_test"
    if generate_test:
        if args.api in LFRIC_API_NAMES:
            filename_standard = "adjt_.+_alg_mod.[Xx]90|atlt_.+_alg_mod.[Xx]90"
            regex_search = re.search(filename_standard, args.test_filename)
            if regex_search is None:
                logger.error("Filename '%s' with 'lfric' API "
                             "must be of the form "
                             "<path>/adjt_<name>_alg_mod.[Xx]90 or "
                             "<path>/atlt_<name>_alg_mod.[Xx]90.",
                             args.test_filename)
                sys.exit(1)
            # At this stage filename should be valid, so we take the base name
            test_name = os.path.basename(args.test_filename).split("_mod.")[0]

    # TL Fortran code
    filename = args.filename
    logger.info("Reading kernel file %s", filename)
    try:
        with open(filename, mode="r", encoding="utf-8") as my_file:
            tl_fortran_str = my_file.read()
            tl_fortran_str = str(tl_fortran_str)
    except FileNotFoundError:
        logger.error("psyad error: file '%s', not found.", filename)
        sys.exit(1)

    try:
        # Create the adjoint (and associated test framework if requested)
        ad_fortran_str, test_fortran_str = generate_adjoint_str(
            tl_fortran_str, args.active, api=args.api,
            coord_arg_index=args.coord_arg,
            panel_id_arg_index=args.panel_id_arg,
            create_test=generate_test,
            test_name=test_name)
    except TangentLinearError as info:
        print(str(info.value))
        sys.exit(1)
    except (KeyError, NotImplementedError) as info:
        print(str(info))
        sys.exit(1)

    fll = FortLineLength()

    # Output the Fortran code for the adjoint kernel
    if line_length_limit:
        ad_fortran_str = fll.process(ad_fortran_str)

    if args.oad:
        logger.info("Writing adjoint of kernel to file %s", args.oad)
        with open(args.oad, mode="w", encoding="utf-8") as adj_file:
            adj_file.write(ad_fortran_str)
    else:
        print(ad_fortran_str, file=sys.stdout)

    # Output test harness if requested
    if generate_test:

        if line_length_limit:
            test_fortran_str = fll.process(test_fortran_str)

        if args.test_filename:
            logger.info("Writing test harness for adjoint kernel to file %s",
                        args.test_filename)
            with open(args.test_filename, mode="w", encoding="utf-8") as tfile:
                tfile.write(test_fortran_str)
        else:
            print(test_fortran_str, file=sys.stdout)


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for.
__all__ = ["main"]
