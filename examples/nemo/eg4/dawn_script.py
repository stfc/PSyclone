# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab

'''Example wrapper to run PSyclone generated SIR code in DAWN.'''

import textwrap
import os.path
from optparse import OptionParser
from ctypes import c_char_p, CDLL
from config import __dawn_install_dawnclib__
from dawn import *
# In theory the 'sir_printer' import should not be required as it will
# be covered by the previous '*' import. However, for some reason, it
# is still needed, otherwise an error is raised.
from dawn import sir_printer

DAWN = CDLL(__dawn_install_dawnclib__)

# PSyclone code start
# PSyclone code end

PARSER = OptionParser()
PARSER.add_option("-v", "--verbose",
                  action="store_true", dest="verbose", default=False,
                  help="print the SIR")

(OPTIONS, _) = PARSER.parse_args()


# Print the SIR to stdout only in verbose mode
if OPTIONS.verbose:
    T = textwrap.TextWrapper(initial_indent=' '*1, width=120,
                             subsequent_indent=' '*1)
    DES = sir_printer.SIRPrinter()

    for stencil in hir.stencils:
        DES.visit_stencil(stencil)

# serialize the hir to pass it to the compiler
HIR_STR = hir.SerializeToString()

# create the options to control the compiler
DAWN_OPTIONS = DAWN.dawnOptionsCreate()
# we set the backend of the compiler to cuda
BACKEND = DAWN.dawnOptionsEntryCreateString("cuda".encode('utf-8'))
DAWN.dawnOptionsSet(DAWN_OPTIONS, "Backend".encode('utf-8'), BACKEND)

# call the compiler that generates a translation unit
TRANS_UNIT = DAWN.dawnCompile(HIR_STR, len(HIR_STR), DAWN_OPTIONS)
B_STENCIL_NAME = stencil_name.encode('utf-8')
# get the code of the translation unit for the given stencil
CODE = DAWN.dawnTranslationUnitGetStencil(TRANS_UNIT, B_STENCIL_NAME)

# write to file
MY_FILE = open(os.path.join(os.path.dirname(os.path.realpath(__file__)),
                            stencil_name + ".cpp"), "w")
MY_FILE.write(c_char_p(CODE).value.decode("utf-8"))

MY_FILE.close()
