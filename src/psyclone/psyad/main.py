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
import argparse
import logging
from io import StringIO 
import sys

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.symbols import LocalInterface
from psyclone.generator import write_unicode_file


def main(args):
    '''Takes an LFRic tangent linear kernel source file as input and
    produces its adjoint.

    :param list args: the list of command-line arguments that PSyAD has \
                      been invoked with.

    '''
    parser = argparse.ArgumentParser(
        description="Run the PSyclone adjoint code generator on an LFRic "
        "tangent-linear kernel file")
    parser.add_argument(
        '-v', '--verbose', help='increase the verbosity of the output',
        action='store_true')
    parser.add_argument('-oad', help='filename for the transformed code')
    parser.add_argument('filename', help='LFRic tangent-linear source code')

    args = parser.parse_args(args)

    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)
        
    # TL Fortran code
    filename = args.filename
    logging.info("Reading file {0}".format(filename))
    with open(filename) as my_file:
        tl_fortran_str = my_file.read()

    ad_fortran_str = psyad(tl_fortran_str)

    # AD Fortran code
    if args.oad:
        logging.info("Writing file {0}".format(args.oad))
        write_unicode_file(ad_fortran_str, args.oad)
    else:
        print(ad_fortran_str)


def psyad(tl_fortran_str):
    '''Takes an LFRic tangent-linear kernel as input and returns its
    adjoint.

    :param str tl_fortran_str: a string containing the LFRic \
        tangent-linear kernel.

    :returns: a string containing the adjoint of the supplied \
        tangent-linear kernel.
    :rtype: str

    '''
    logging.debug(tl_fortran_str)

    # TL fparser parse tree
    reader = FortranStringReader(tl_fortran_str, ignore_comments=False)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(reader)
    logging.debug(repr(parse_tree))

    # TL Language-level PSyIR
    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)
    with Capturing() as output:
        psyir.view()
    logging.debug("\n".join(output))

    # TL LFRic-specific PSyIR
    logging.debug(
        "Translation from generic PSyIR to LFRic-specific PSyIR should be "
        "done now.")

    # Transform from TL to AD
    logging.debug("Transformation from TL to AD should be done now")

    # AD Fortran code
    writer = FortranWriter()
    adjoint_str = writer(psyir)
    logging.debug(adjoint_str)

    return adjoint_str


class Capturing(list):
    '''Utility to capture stdout from a function.'''
    def __enter__(self):
        self._stdout = sys.stdout
        sys.stdout = self._stringio = StringIO()
        return self
    def __exit__(self, *args):
        self.extend(self._stringio.getvalue().splitlines())
        sys.stdout = self._stdout


__all__ = [main, psyad]
