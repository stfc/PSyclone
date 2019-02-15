# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter STFC Daresbury Lab

'''Utility module containing classes and functions that are used by
the parser modules.

'''

from psyclone.configuration import Config
from psyclone.line_length import FortLineLength

# Exceptions


class ParseError(Exception):
    '''Provides a PSyclone-specific error class for the situation when
    the PSyclone code parsing finds an error in the input.

    :param str value: the message associated with the error.

    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Parse Error: " + value

    def __str__(self):
        return repr(self.value)

# support functions


def check_api(api):
    '''Check that the supplied API is valid.

    :param str api: The API to check.
    :raises ParseError: if the supplied API is not recognised.

    '''
    _config = Config.get()

    if api not in _config.supported_apis:
        raise ParseError(
            "utils.py:check_api: Unsupported API '{0}' specified. "
            "Supported types are {1}.".format(api,
                                              _config.supported_apis))


def check_ll(alg_filename):
    '''Check that the code contained within the alg_filename file
    conforms to the 132 line length limit.

    :param str alg_filename: The file containing the algorithm code.

    :except ParseError: if one of more lines are longer than the 132 \
    line length limit.

    '''
    fll = FortLineLength()
    with open(alg_filename, "r") as myfile:
        code_str = myfile.read()
    if fll.long_lines(code_str):
        raise ParseError(
            "the file does not conform to the specified {0} line "
            "length limit".format(str(fll.length)))
