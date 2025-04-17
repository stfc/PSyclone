#!/usr/bin/env python
# Modified work Copyright (c) 2017-2019 Science and Technology
# Facilities Council
# Original work Copyright (c) 1999-2008 Pearu Peterson
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

""" Example script to parse a Fortran program using fparser """

import logging
import sys
from fparser.scripts.script_options import set_fparser_options

logging.basicConfig()

try:
    from iocbio.optparse_gui import OptionParser
except ImportError:
    from optparse import OptionParser


def runner(_, options, args):
    """
    Function to read, parse and output Fortran source code.

    :param options: object constructed by OptionParser with cmd-line flags.
    :param args: list of Fortran files to parse.
    :type args: list of str

    """
    from fparser.two.parser import ParserFactory
    from fparser.two.Fortran2003 import FortranSyntaxError, InternalError
    from fparser.common.readfortran import FortranFileReader

    if not args:
        print("Error: No fortran files specified", file=sys.stderr)
        raise SystemExit(1)
    for filename in args:
        print("File: '{0}'".format(filename), file=sys.stderr)
        try:
            reader = FortranFileReader(filename, ignore_comments=False)
        except IOError as error:
            print(error, file=sys.stderr)
            continue
        try:
            fparser = ParserFactory().create(std=options.std)
            program = fparser(reader)
            if options.task == "show":
                print(str(program))
            if options.task == "repr":
                print(repr(program))
        except FortranSyntaxError as msg:
            print(f"Syntax error: {msg}", file=sys.stderr)
        except InternalError as msg:
            print(f"Internal error in fparser: {msg}", file=sys.stderr)


def main():
    """Check arguments before parsing code"""
    parser = OptionParser()
    set_fparser_options(parser)
    options, args = parser.parse_args()
    runner(parser, options, args)


if __name__ == "__main__":
    main()  # pragma: no cover
