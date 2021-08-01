# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''Top-level driver functions for PSyAD : the PSyclone Adjoint
support. Transforms an LFRic tangent linear kernel to its adjoint.

'''
from __future__ import absolute_import, print_function
import argparse
import logging
import sys

import six

from psyclone.generator import write_unicode_file
from psyclone.psyad import generate_adjoint_str


def main(args):
    '''Takes an LFRic tangent linear kernel source file as input and
    produces its adjoint.

    :param list args: the list of command-line arguments that PSyAD has \
                      been invoked with.

    '''
    logger = logging.getLogger(__name__)

    parser = argparse.ArgumentParser(
        description="Run the PSyclone adjoint code generator on an LFRic "
        "tangent-linear kernel file")
    parser.add_argument(
        '-v', '--verbose', help='increase the verbosity of the output',
        action='store_true')
    parser.add_argument('-oad', help='filename for the transformed code')
    parser.add_argument('filename', help='LFRic tangent-linear kernel source')
    parser.add_argument('-a', nargs='+', help='active variable names')

    args = parser.parse_args(args)

    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)

    # TL Fortran code
    filename = args.filename
    logger.info("Reading kernel file %s", filename)
    with open(filename) as my_file:
        tl_fortran_str = my_file.read()
        tl_fortran_str = six.text_type(tl_fortran_str)

    from psyclone.psyad.transformations import TangentLinearError
    try:
        ad_fortran_str = generate_adjoint_str(
            tl_fortran_str, args.a)
    except KeyError as info:
        print(str(info))
        exit(1)
    except TangentLinearError as info:
        print(str(info))
        exit(1)

    # AD Fortran code
    if args.oad:
        logger.info("Writing adjoint of kernel to file %s", args.oad)
        write_unicode_file(ad_fortran_str, args.oad)
    else:
        print(ad_fortran_str, file=sys.stdout)


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ["main"]
