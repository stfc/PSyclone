# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Provides support for breaking long fortran lines into smaller ones
to allow the code to conform to the maximum line length limits (132
for f90 free format is the default)'''

import re

from fparser.common.readfortran import Comment, FortranStringReader
from fparser.common.sourceinfo import FortranFormat

from psyclone.errors import InternalError


def find_break_point(line: str, max_index: int, key_list: list[str]) -> int:
    ''' Finds the most appropriate line break point for the Fortran code in
    line.

    :param line: the Fortran code string to find the line break point for.
    :param max_index: the maximum index in line to search for the line
                      break point.
    :param key_list: list of potential symbols to break the line at. The
                     members of the list early in the ordering have priority
                     for breaking the line, i.e. if the list contains multiple
                     elements, any possible position of the first element will
                     be found before trying any other element of the list.

    :returns: index at which to break the line into multiple lines.

    :raises InternalError: if no suitable break point is found in line.

    '''
    # We should never break the line before the first element on the
    # line.
    first_non_whitespace = len(line) - len(line.lstrip())
    # We create the appropriate substring and then index it backwards.
    search_string = (line[first_non_whitespace+1:max_index])[::-1]
    for key in key_list:
        if isinstance(key, re.Pattern):
            match = re.search(key, search_string)
        else:
            # For string input keys, we reverse them in case they are of
            # length >1 as we are searching backwards.
            match = re.search(re.escape(key[::-1]), search_string)
        if match:
            # If max_index is larger than the line length, we need to compute
            # the matched index from the the end of the line.
            end = min(max_index, len(line))
            # Since we're working backwards we can return the start of the
            # match (which is the end of the matched section of the forward
            # string).
            return end-match.start()
    raise InternalError(
        f"Error in find_break_point. No suitable break point found"
        f" for line '{line[:max_index]}' and keys '{str(key_list)}'")


class FortLineLength():
    ''' This class take a free-format fortran code as a string and
    line wraps any lines that are larger than the specified line
    length

    :param line_length: the maximum line-length permitted in the output.

    .. warning::
        The :class:`line_length.FortLineLength` class is only partially aware
        of Fortran syntax. This awareness is required so that appropriate
        continuation characters can be used (for example ``&`` at the end of
        a line and ``!$omp&`` at the start of a line for OpenMP directives,
        ``&`` at the end of a line for statements and ``&`` at the end of a
        line and ``&`` at the beginning of a line for strings).

        Whilst statements only require an ``&`` at the end of the line when
        line wrapping with free-form Fortran, they may optionally also have an
        ``&`` at the beginning of the subsequent line. In contrast, when
        splitting a string over multiple lines an ``&`` is required at both
        locations. Therefore an instance of the
        :class:`line_length.FortLineLength` class will always add ``&`` at the
        beginning of a continuation line for a statement, in case the line is
        split within a string.

        One known situation that could cause an instance of the
        :class:`line_length.FortLineLength` class to fail is when an *inline*
        comment at the end of a line containing a *directive* takes it over
        the 132-character limit. (TODO fparser/#468)

    '''
    # pylint: disable=too-many-instance-attributes
    def __init__(self, line_length: int = 132):
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
        # If using regexs to defined keys of length >1, the regex
        # must be in reverse as the matching is done on the reversed
        # string. Non-regex keys are reversed automatically if needed.
        self._key_lists = {"statement": [", ", ",", " "],
                           "openmp_directive": [" ", ",", ")", "="],
                           "openacc_directive": [" ", ",", ")", "="],
                           "comment": [" ", ".", ",",
                                       # Comments should never fail, so
                                       # we have backups of increasing
                                       # desperation.
                                       re.compile(r"[+-\\/\"'`]"),
                                       re.compile(r"[a-zA-Z0-9]"),
                                       # Finally anything is ok.
                                       re.compile(r"(.)")
                                       ],
                           "unknown": [" ", ",", "=", "+", ")"]}
        self._stat = re.compile(r'^\s*(INTEGER|REAL|TYPE|CALL|SUBROUTINE|USE)',
                                flags=re.I)
        self._omp = re.compile(r'^\s*!\$OMP', flags=re.I)
        self._acc = re.compile(r'^\s*!\$ACC', flags=re.I)
        self._comment = re.compile(r'^\s*!')

    def long_lines(self, fortran_in: str) -> bool:
        '''
        Checks whether any lines in the supplied text are longer
        than the allowed length.

        :param fortran_in: the Fortran code to check.

        :returns: True if at least one of the lines in the input code is
            longer than the allowed length. Otherwise returns False.
        '''
        return any(len(line) > self._line_length for
                   line in fortran_in.split('\n'))

    @property
    def length(self) -> int:
        ''':returns: the maximum allowed line length.'''
        return self._line_length

    def process(self, fortran_in: str) -> str:
        ''' Processes unlimited line-length Fortran code into Fortran
        code with long lines wrapped appropriately.

        :param fortran_in: Fortran code to be line wrapped.

        :returns: line-wrapped Fortran code.

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

                if line_type != "comment":
                    # Check whether the proposed break point falls within an
                    # in-line comment.
                    line_no_indent = line.lstrip()
                    indent_size = len(line) - len(line_no_indent)
                    # FortranStringReader will return separate Line and Comment
                    # objects for a source line containing an in-line comment.
                    freader = FortranStringReader(line, ignore_comments=False,
                                                  process_directives=True)
                    # Use free format.
                    freader.set_format(FortranFormat(True, True))
                    fline = freader.next()
                    # This won't work for a directive with an in-line comment
                    # as FortranStringReader returns a single Comment object
                    # for the whole thing (TODO fparser/#468).
                    if ((break_point - indent_size) > len(fline.line) and
                            isinstance(freader.next(), Comment)):
                        # Breakpoint is inside a comment so change the chars
                        # used for the line-continuation end and start.
                        line_type = "comment"
                        c_start = self._cont_start[line_type]
                        c_end = self._cont_end[line_type]
                        key_list = self._key_lists[line_type]

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

    def _get_line_type(self, line: str) -> str:
        ''' Classifies lines into different types. This is required as
        directives need different continuation characters to Fortran
        statements. It also enables us to know a little about the
        structure of the line which could be useful at some point.

        :param line: the line of code to analyse.

        :returns: the type of the line (one of "statement", "openmp_directive",
                  "openacc_directive", "comment" or "unknown").
        '''
        if self._stat.match(line):
            return "statement"
        if self._omp.match(line):
            return "openmp_directive"
        if self._acc.match(line):
            return "openacc_directive"
        if self._comment.match(line):
            return "comment"
        return "unknown"
