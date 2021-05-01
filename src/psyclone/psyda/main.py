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

'''xxx'''

import argparse

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranFileReader

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.symbols import LocalInterface

def main(args):
    '''
    xxx

    :param list args: the list of command-line arguments that PSyDA has \
                      been invoked with.

    '''
    parser = argparse.ArgumentParser(
        description="Run the PSyDA code generator on an LFRic tangent-linear "
        "kernel file")
    parser.add_argument('filename', help='tangent-linear source code')

    args = parser.parse_args(args)

    # TL Fortran code
    filename = args.filename
    print ("Reading file {0}".format(filename))

    # TL fparser parse tree
    reader = FortranFileReader(filename, ignore_comments=False)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(reader)
    print (repr(parse_tree))

    # TL Language-level PSyIR
    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)
    psyir.view()

    # TL LFRic-specific PSyIR

    # Transform from TL to AD

    # AD Fortran code
    writer = FortranWriter()
    alg_gen = writer(psyir)
    print(alg_gen)

if __name__ == "__main__":
    main(sys.argv[1:])
