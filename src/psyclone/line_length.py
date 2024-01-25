# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Modified by A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Provides support for breaking long fortran lines into smaller ones
to allow the code to conform to the maximum line length limits (132
for f90 free format is the default)'''

import re

from psyclone.errors import InternalError


def find_break_point(line, max_index, key_list):
    ''' Finds the most appropriate line break point for the Fortran code in
    line.

    :param str line: the Fortran code string to find the line break point for.
    :param int max_index: the maximum index in line to search for the line
                          break point.
    :param key_list: list of potential symbols to break the line at. The
                     members of the list early in the ordering have priority
                     for breaking the line, i.e. if the list contains multiple
                     elements, any possible position of the first element will
                     be found before trying any other element of the list.
    :type key_list: List[str]

    :returns: index to break the line into multiple lines.
    :rtype: int

    :raises InternalError: if no suitable break point is found in line.
    '''
    # We should never break the line before the first element on the
    # line.
    first_non_whitespace = len(line) - len(line.lstrip())
    for key in key_list:
        idx = line.rfind(key, first_non_whitespace+1, max_index)
        if idx > 0:
            return idx+len(key)
    raise InternalError(
        f"Error in find_break_point. No suitable break point found"
        f" for line '{line[:max_index]}' and keys '{str(key_list)}'")


class FortLineLength():

    ''' This class take a free format fortran code as a string and
    line wraps any lines that are larger than the specified line
    length'''

    # pylint: disable=too-many-instance-attributes
    def __init__(self, line_length=132):
        self._line_length = line_length
        self._cont_start = {"statement": "&",
                            "openmp_directive": "!$omp& ",
                            "openacc_directive": "!$acc& ",
                            "comment": "!& ",
                            "unknown": "&"}
        self._cont_end = {"statement": "&",
                          "openmp_directive": " &",
                          "openacc_directive": " &",
                          "comment": "",
                          "unknown": "&"}
        self._key_lists = {"statement": [", ", ",", " "],
                           "openmp_directive": [" ", ",", ")", "="],
                           "openacc_directive": [" ", ",", ")", "="],
                           "comment": [" ", ".", ","],
                           "unknown": [" ", ",", "=", "+", ")"]}
        self._stat = re.compile(r'^\s*(INTEGER|REAL|TYPE|CALL|SUBROUTINE|USE)',
                                flags=re.I)
        self._omp = re.compile(r'^\s*!\$OMP', flags=re.I)
        self._acc = re.compile(r'^\s*!\$ACC', flags=re.I)
        self._comment = re.compile(r'^\s*!')

    def long_lines(self, fortran_in):
        '''returns true if at least one of the lines in the input code is
           longer than the allowed length. Otherwise returns false '''
        for line in fortran_in.split('\n'):
            if len(line) > self._line_length:
                return True
        return False

    @property
    def length(self):
        ''' returns the maximum allowed line length'''
        return self._line_length

    def process(self, fortran_in):
        ''' Processes unlimited line-length Fortran code into Fortran
        code with long lines wrapped appropriately.

        :param str fortran_in: Fortran code to be line wrapped.

        :returns: line wrapped Fortran code.
        :rtype: str

        '''
        fortran_out = ""
        for line in fortran_in.split('\n'):
            if len(line) > self._line_length:
                line_type = self._get_line_type(line)

                c_start = self._cont_start[line_type]
                c_end = self._cont_end[line_type]
                key_list = self._key_lists[line_type]

                try:
                    break_point = find_break_point(
                        line, self._line_length-len(c_end), key_list)
                except InternalError:
                    # Couldn't find a valid point to break the line.
                    # Remove indentation and try again.
                    line = line.lstrip()
                    if len(line) < self._line_length:
                        fortran_out += line + "\n"
                        continue
                    break_point = find_break_point(
                        line, self._line_length-len(c_end), key_list)

                fortran_out += line[:break_point] + c_end + "\n"
                line = line[break_point:]
                while len(line) + len(c_start) > self._line_length:
                    break_point = find_break_point(
                        line, self._line_length-len(c_end)-len(c_start),
                        key_list)
                    fortran_out += c_start + line[:break_point] + c_end + "\n"
                    line = line[break_point:]
                if line:
                    fortran_out += c_start + line + "\n"
            else:
                fortran_out += line + "\n"

        # We add an extra newline so remove it when we return
        return fortran_out[:-1]

    def _get_line_type(self, line):
        ''' Classes lines into diffrent types. This is required as
        directives need different continuation characters to fortran
        statements. It also enables us to know a little about the
        structure of the line which could be useful at some point.'''

        if self._stat.match(line):
            return "statement"
        if self._omp.match(line):
            return "openmp_directive"
        if self._acc.match(line):
            return "openacc_directive"
        if self._comment.match(line):
            return "comment"
        return "unknown"
