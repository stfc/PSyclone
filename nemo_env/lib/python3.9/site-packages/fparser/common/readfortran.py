#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Modified work Copyright (c) 2017-2024 Science and Technology
# Facilities Council.
# Original work Copyright (c) 1999-2008 Pearu Peterson

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

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
#
# Author: Pearu Peterson <pearu@cens.ioc.ee>
# Created: May 2006
# Modified by R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified by P. Elson, Met Office
# Modified by J. Henrichs, Bureau of Meteorology

"""Provides Fortran reader classes.

Overview

Provides FortranReader classes for reading Fortran codes from files and
strings. FortranReader handles comments and line continuations of both
fix and free format Fortran codes.

Examples::

    >> from fparser.common.readfortran import FortranFileReader
    >>> import os
    >>> reader = FortranFileReader(os.path.expanduser('~/src/blas/daxpy.f'))
    >>> print reader.next()
    line #1 'subroutine daxpy(n,da,dx,incx,dy,incy)'
    >>> print `reader.next()`
    Comment('c     constant times a vector plus a vector.\\n
    c     uses unrolled loops for increments equal to one.\\n
    c     jack dongarra, linpack, 3/11/78.\\n
    c     modified 12/3/93, array(1) declarations changed to array(*)',(3, 6))
    >>> print `reader.next()`
    Line('double precision dx(*),dy(*),da',(8, 8),'')
    >>> print `reader.next()`
    Line('integer i,incx,incy,ix,iy,m,mp1,n',(9, 9),'')

Note that the ``.next()`` method may return `Line`, `SyntaxErrorLine`,
`Comment`, `MultiLine`, or `SyntaxErrorMultiLine` instance.
Let us continue with the above example session to illustrate the `Line`
methods and attributes::

    >>> item = reader.next()
    >>> item
        Line('if (da .eq. 0.0d0) return',(12, 12),'')
    >>> item.line
        'if (da .eq. 0.0d0) return'
    >>> item.strline
        'if (F2PY_EXPR_TUPLE_5) return'
    >>> item.strlinemap
        {'F2PY_EXPR_TUPLE_5': 'da .eq. 0.0d0'}
    >>> item.span
        (12, 12)
    >>> item.get_line()
        'if (F2PY_EXPR_TUPLE_5) return'

To read a Fortran code from a string, use `FortranStringReader` class::

    >>> from fparser.common.sourceinfo import FortranFormat
    >>> from fparser.common.readfortran import FortranStringReader
    >>> code = '''
    ...       subroutine foo(a)
    ...         integer a
    ...         print*,\"a=\",a
    ...       end
    ... '''
    >>> reader = FortranStringReader(code)
    >>> reader.set_format(FortranFormat(False, True))
    >>> reader.next()
        Line('subroutine foo(a)',(2, 2),'')
    >>> reader.next()
        Line('integer a',(3, 3),'')
    >>> reader.next()
        Line('print*,\"a=\",a',(4, 4),'')

"""

import logging
import os
import re
import sys
import traceback
from io import StringIO
import fparser.common.sourceinfo
from fparser.common.splitline import String, string_replace_map, splitquote


__all__ = [
    "FortranFileReader",
    "FortranStringReader",
    "FortranReaderError",
    "Line",
    "SyntaxErrorLine",
    "Comment",
    "MultiLine",
    "SyntaxErrorMultiLine",
]

_SPACEDIGITS = " 0123456789"
_CF2PY_RE = re.compile(r"(?P<indent>\s*)!f2py(?P<rest>.*)", re.I)
_LABEL_RE = re.compile(r"\s*(?P<label>\d+)\s*(\b|(?=&)|\Z)", re.I)
_CONSTRUCT_NAME_RE = re.compile(r"\s*(?P<name>\w+)\s*:\s*(\b|(?=&)|\Z)", re.I)
_IS_INCLUDE_LINE = re.compile(
    r'\s*include\s*("[^"]+"' + r"|\'[^\']+\')\s*\Z", re.I
).match


def _is_fix_cont(line):
    return line and len(line) > 5 and line[5] != " " and line[:5] == 5 * " "


def _is_fix_comment(line, isstrict, f2py_enabled):
    """
    Check whether line is a comment line in fixed format Fortran source.

    References - Fortran2008 3.3.3.

    :param str line: line of code to check.
    :param bool isstrict: whether we are strictly enforcing fixed/free fmt.
    :param bool f2py_enabled: whether support for f2py directives is enabled.

    :returns: whether or not the supplied line is a fixed-format comment.
    :rtype: bool

    """
    if line:
        if line[0] in "*cC!":
            if f2py_enabled and line[1:5].lower() == "f2py":
                return False
            return True
        if not isstrict:
            i = line.find("!")
            if i != -1:
                start = line[:i].lstrip()
                if not start:
                    if i == 5:
                        # line continuation
                        return False
                    return True
                # inline comment or ! is used in character context
                # inline comments are handled elsewhere
    elif line == "":
        return True
    return False


_HOLLERITH_START_SEARCH = re.compile(
    r"(?P<pre>\A|,\s*)" + r"(?P<num>\d+)h", re.I
).search
_IS_CALL_STMT = re.compile(r"call\b", re.I).match


def extract_label(line):
    """Look for an integer label at the start of 'line' and if there is
    one then remove it from 'line' and store it as an integer in
    'label', returning both in a tuple.

    :param str line: a string that potentially contains a label at the \
        start.

    :returns: a 2-tuple containing the label and updated line if a \
        label is found or None and the unchanged line if a label is \
        not found.
    :rtype: (int or NoneType, str)

    """
    label = None
    match = _LABEL_RE.match(line)
    if match:
        label = int(match.group("label"))
        line = line[match.end() :].lstrip()
    return label, line


def extract_construct_name(line):
    """Look for a construct name at the start of 'line' and if there is
    one then remove it from 'line' and return it as a string in
    'name', returning both in a tuple.

    :param str line: a string that potentially contains a construct \
        name at the start.

    :returns: a 2-tuple containing the construct name and updated line \
        if a construct name is found or None and the unchanged line if \
        a construct name is not found.
    :rtype: (str or NoneType, str)

    """
    construct_name = None
    match = _CONSTRUCT_NAME_RE.match(line)
    if match:
        construct_name = match.group("name")
        line = line[match.end() :].lstrip()
    return construct_name, line


class FortranReaderError(Exception):
    """
    Thrown when there is an error reading the Fortran source file.
    """

    pass


class Line:
    """Holds a Fortran source line.

    Attributes::

        line : str
          code line
        span : 2-tuple
          starting and ending line numbers
        label : {int, None}
          Specify statement label
        name : {str, None}
          Specify construct name.
        reader : FortranReaderBase
        strline : {None, str}
        is_f2py_directive : bool
          the line contains f2py directive

    """

    def __init__(self, line, linenospan, label, name, reader):
        self.line = line.strip()
        if not self.line:
            raise FortranReaderError(
                "Got empty line: '{0}'. linenospan={1}, "
                "label='{2}'".format(line, linenospan, label)
            )
        self.span = linenospan
        assert label is None or isinstance(label, int), repr(label)
        assert name is None or isinstance(name, str) and name != "", repr(name)
        self.label = label
        self.name = name
        self.reader = reader
        self.strline = None
        self.is_f2py_directive = linenospan[0] in reader.f2py_comment_lines
        self.parse_cache = {}

    def has_map(self):
        """
        Returns true when a substitution map has been registered.
        """
        return hasattr(self, "strlinemap") and self.strlinemap

    def apply_map(self, line):
        """
        Substitutes magic strings in a line with values specified in a map.
        """
        if not hasattr(self, "strlinemap") or not self.strlinemap:
            return line
        return self.strlinemap(line)

    def copy(self, line=None, apply_map=False):
        """
        Creates a Line object from a string.

        If no line argument is specified a copy is made of this Line.

        If a substitution map is provided it is used while making the copy.
        """
        if line is None:
            line = self.line
        if apply_map:
            line = self.apply_map(line)
        return Line(line, self.span, self.label, self.name, self.reader)

    def clone(self, line):
        """
        This Line has its contents overwitten by the passed string. The
        incoming string has substitution applied.
        """
        self.line = self.apply_map(line)
        self.strline = None

    def __repr__(self):
        return self.__class__.__name__ + "(%r,%s,%r,%r,<reader>)" % (
            self.line,
            self.span,
            self.label,
            self.name,
        )

    def __str__(self):
        s = "line #%s" % (self.span[0])
        if self.label is not None:
            s += " %s " % (self.label)
        if self.name is not None:
            s += "%s: " % (self.name)
        return s + repr(self.line)

    def isempty(self, ignore_comments=False):
        return not (self.line or self.label is not None or self.name is not None)

    def get_line(self, apply_map=False):
        if apply_map:
            return self.apply_map(self.get_line(apply_map=False))
        if self.strline is not None:
            return self.strline
        line = self.line

        if self.reader.format.is_f77:
            # Handle Hollerith constants by replacing them
            # with char-literal-constants.
            # H constants may appear only in DATA statements and
            # in the argument list of CALL statement.
            # Hollerith constants were removed from the Fortran 77 standard.
            # The following handling is not perfect but works for simple
            # usage cases.
            # todo: Handle hollerith constants in DATA statement
            if _IS_CALL_STMT(line):
                l2 = self.line[4:].lstrip()
                i = l2.find("(")
                if i != -1 and l2[-1] == ")":
                    substrings = ["call " + l2[: i + 1]]
                    start_search = _HOLLERITH_START_SEARCH
                    l2 = l2[i + 1 : -1].strip()
                    m = start_search(l2)
                    while m:
                        substrings.append(l2[: m.start()])
                        substrings.append(m.group("pre"))
                        num = int(m.group("num"))
                        substrings.append("'" + l2[m.end() : m.end() + num] + "'")
                        l2 = l2[m.end() + num :]
                        m = start_search(l2)
                    substrings.append(l2)
                    substrings.append(")")
                    line = "".join(substrings)

        line, str_map = string_replace_map(line, lower=not self.reader.format.is_pyf)
        self.strline = line
        self.strlinemap = str_map
        return line

    def parse_line(self, cls, parent_cls):
        if cls not in self.parse_cache:
            self.parse_cache[cls] = None
            obj = cls(self.line, parent_cls=parent_cls)
            self.parse_cache[cls] = obj
        else:
            obj = self.parse_cache[cls]
        return obj

    def parse_block(self, reader, cls, parent_cls):
        key = cls, tuple(parent_cls)
        if key not in self.parse_cache:
            obj = cls(reader, parent_cls=parent_cls)
            self.parse_cache[key] = obj
        else:
            obj = self.parse_cache[key]
        return obj


class SyntaxErrorLine(Line, FortranReaderError):
    """
    Indicates a syntax error while processing a line.
    """

    def __init__(self, line, linenospan, label, name, reader, message):
        Line.__init__(self, line, linenospan, label, name, reader)
        FortranReaderError.__init__(self, message)


class Comment:
    """Holds a Fortran comment.

    :param str comment: String containing the text of a single or \
    multi-line comment
    :param linenospan: A 2-tuple containing the start and end line \
    numbers of the comment from the input source.
    :type linenospan: (int, int)
    :param reader: The reader object being used to read the input \
    source.
    :type reader: :py:class:`fparser.common.readfortran.FortranReaderBase`

    """

    def __init__(self, comment, linenospan, reader):
        self.comment = comment
        self.span = linenospan
        self.reader = reader
        # self.line provides a common way to retrieve the content from
        # either a 'Line' or a 'Comment' class. This is useful for
        # tests as a reader can return an instance of either class and
        # we might want to check the contents in a consistent way.
        self.line = comment

    def __repr__(self):
        return self.__class__.__name__ + "(%r,%s)" % (self.comment, self.span)

    def isempty(self, ignore_comments=False):
        """
        Whether or not this comment is in fact empty (or we are ignoring
        it). Provided for compatibility with Line.isempty()

        :param bool ignore_comments: whether we ignore comments
        :return: True if we are ignoring comments, False otherwise
        :rtype: bool
        """
        return ignore_comments


class MultiLine:
    """Holds PYF file multiline.

    PYF file multiline is represented as follows::

        prefix+'''+lines+'''+suffix.

    :param str prefix: the prefix of the line(s)
    :param block: list of lines
    :type block: List[:py:class:`fparser.common.readfortran.Line`]
    :param str suffix: the suffix of the block of lines
    :param linenospan: starting and ending line numbers
    :type linenospan: Tuple[int, int]
    :param reader: the current reader instance.
    :type reader: :py:class:`fparser.common.readfortran.FortranReaderBase`

    """

    def __init__(self, prefix, block, suffix, linenospan, reader):
        self.prefix = prefix
        self.block = block
        self.suffix = suffix
        self.span = linenospan
        self.reader = reader

    def __repr__(self):
        string = "{cls}({prefix!r},{block},{suffix!r},{span})"
        return string.format(
            cls=self.__class__.__name__,
            prefix=self.prefix,
            block=self.block,
            suffix=self.suffix,
            span=self.span,
        )

    def isempty(self, ignore_comments=False):
        """
        Returns true if there is no significant text in this multi-line
        string.
        """
        return not (self.prefix or self.block or self.suffix)


class SyntaxErrorMultiLine(MultiLine, FortranReaderError):
    """
    Indicates a syntax error while processing Python multi-line strings.
    """

    def __init__(self, prefix, block, suffix, linenospan, reader, message):
        MultiLine.__init__(self, prefix, block, suffix, linenospan, reader)
        FortranReaderError.__init__(self, message)


class CppDirective(Line):
    """Holds a preprocessor directive source line.

    :param str line: string containing the text of a single or \
                     multi-line preprocessor directive.
    :param linenospan: a 2-tuple containing the start and end line \
                       numbers of the directive from the input source.
    :type linenospan: (int, int)
    :param reader: The reader object being used to read the input \
                   source.
    :type reader: :py:class:`fparser.common.readfortran.FortranReaderBase`

    """

    def __init__(self, line, linenospan, reader):
        super(CppDirective, self).__init__(line, linenospan, None, None, reader)


##############################################################################


class FortranReaderBase:
    """
    Base class for reading Fortran sources.

    A Fortran source must be a file-like object (have a ``.next()``
    method) and it may hold Fortran 77 code, fixed format Fortran
    code, free format Fortran code, or PYF signatures (with extended
    free format Fortran syntax).

    :param source: a file-like object with .next() method used to \
                   retrive a line.
    :type source: :py:class:`StringIO` or a file handle
    :param mode: a FortranFormat object as returned by \
                 `sourceinfo.get_source_info()`
    :type mode: :py:class:`fparser.common.sourceinfo.Format`
    :param bool isstrict: whether we are strictly enforcing fixed format.
    :param bool ignore_comments: whether or not to discard comments.
    :param Optional[bool] include_omp_conditional_lines: whether or not the
        content of a line with an OMP sentinel is parsed or not. Default is
        False (in which case it is treated as a Comment).

    The Fortran source is iterated by `get_single_line`,
    `get_next_line`, `put_single_line` methods.

    """

    def __init__(
        self, source, mode, ignore_comments, include_omp_conditional_lines=False
    ):
        self.source = source
        self._include_omp_conditional_lines = include_omp_conditional_lines
        self.set_format(mode)
        self.linecount = 0  # the current number of consumed lines
        self.isclosed = False
        # This value for ignore_comments can be overridden by using the
        # ignore_comments optional argument to e.g. get_single_line()
        self._ignore_comments = ignore_comments

        self.filo_line = []  # used for un-consuming lines.
        self.fifo_item = []
        self.source_lines = []  # source lines cache

        self.f2py_comment_lines = []  # line numbers of f2py directives

        self.reader = None
        self.include_dirs = ["."]

        self.source_only = None

        self.exit_on_error = True
        self.restore_cache = []

    ##########################################################################

    def __repr__(self):
        return "%s(%r, %r, %r)" % (
            self.__class__.__name__,
            self.source,
            self._format.is_free,
            self._format.is_strict,
        )

    def find_module_source_file(self, mod_name):
        """
        Scans registered dependees for a named module.
        """
        from .utils import get_module_file, module_in_file

        if self.source_only:
            for sf in self.source_only:
                if module_in_file(mod_name, sf):
                    return sf
        else:
            fn = None
            for d in self.include_dirs:
                fn = get_module_file(mod_name, d)
                if fn is not None:
                    return fn

    def set_format(self, mode):
        """
        Set Fortran code mode (fixed/free format etc). If handling of
        OMP sentinels is also enabled, this function will also create
        the required regular expressions to handle conditional sentinels
        depending on the (new) format

        :param mode: Object describing the desired mode for the reader
        :type mode: :py:class:`fparser.common.sourceinfo.FortranFormat`
        """
        self._format = mode
        if not self._include_omp_conditional_lines:
            return

        if self._format.is_fixed or self._format.is_f77:
            # Initial lines fixed format sentinels: !$, c$, *$ in first
            # column:
            sentinel = r"^([\!\*c]\$)"

            # Then only spaces and digits up to column 5, and a
            # space or 0 at column 6
            init_line = r"[ 0-9]{3}[ 0]"

            # Continued lines fixed format sentinels: the sentinel as
            # above followed by three spaces, and a non-space, non-0 character
            # in column 6:
            cont_line = r"   [^ 0]"
            # Combine these two regular expressions
            self._re_omp_sentinel = re.compile(
                f"{sentinel}({init_line}|{cont_line})", re.IGNORECASE
            )
        else:
            # Initial free format sentinels: !$ as the first non-space
            # character followed by a space.
            self._re_omp_sentinel = re.compile(r"^ *(\!\$) ", re.IGNORECASE)
            # Continued lines free format sentinels: !$ as the first non-space
            # character with an optional & that can have spaces or not. The
            # important implication of the latter is that a continuation line
            # can only be properly detected if the previous line had a
            # sentinel (since it does not require a space after the sentinel
            # anymore. Without the requirement of a space, the regular
            # expression for continuation lines will also match !$omp
            # directives). So we need to have two different regular
            # expressions for free format, and the detection of continuation
            # lines need to be done in a later stage, when multiple lines
            # are concatenated.
            self._re_omp_sentinel_cont = re.compile(r"^ *(\!\$) *&?", re.IGNORECASE)

    @property
    def format(self):
        """
        :returns: the currently applicable format.
        :rtype: :py:class:`fparser.sourceinfo.FortranFormat`
        """
        return self._format

    @property
    def name(self):
        """
        :returns: the name of this reader.
        :rtype: str
        """
        return "{source} mode={mode}".format(source=self.source, mode=self._format.mode)

    def close_source(self):
        """Called when self.source.next() raises StopIteration."""
        pass

    # For handling raw source lines:

    def put_single_line(self, line):
        """Put single line to FILO line buffer.

        ``linecount`` will be decremented, that is, the line was
        returned by ``get_single_line`` call then it will be
        un-consumed.

        See also - get_single_line, get_next_line
        """
        self.filo_line.append(line)
        self.linecount -= 1

    def get_single_line(self, ignore_empty=False, ignore_comments=None):
        """ Return line from FILO line buffer or from source.

        First try getting the line from FILO line buffer.

        If FILO line buffer is empty then get the next line from
        source. Tabs in source line are expanded, ending blank and new
        line characters will be removed.  The source line will be
        added to ``source_lines`` list. If source line is empty then
        recursively get next non-empty line.

        In both situations ``linecount`` will be incremented, that is,
        the line will be consumed.

        :param bool ignore_empty: if True then ignore empty lines.
        :param bool ignore_comments: if True then ignore comments (overrides \
                                     self._ignore_comments)

        See also - put_single_line, get_next_line
        """
        if ignore_comments is None:
            ignore_comments = self._ignore_comments

        try:
            line = self.filo_line.pop()
            self.linecount += 1
            return line
        except IndexError:
            pass
        if self.isclosed:
            return None
        try:
            line = next(self.source)
        except StopIteration:
            self.isclosed = True
            self.close_source()
            return None
        self.linecount += 1

        # expand tabs, replace special symbols, get rid of nl characters
        line = line.expandtabs().replace("\xa0", " ").rstrip()
        if self._include_omp_conditional_lines and self._format.is_fixed:
            # Fixed-format line sentinels can be handled here, since a
            # continuation line does not depend on the previous line. The
            # regular expression checks for both an initial or a continuation
            # line, and if it is found, the sentinel is replaced with two
            # spaces:
            line, _ = self.replace_omp_sentinels(line, self._re_omp_sentinel)

        self.source_lines.append(line)

        if ignore_comments and (self._format.is_fixed or self._format.is_f77):
            # Check for a fixed-format comment. If the current line *is*
            # a comment and we are ignoring them, then recursively call this
            # routine again to get the next source line.
            if _is_fix_comment(
                line,
                isstrict=self._format.is_strict,
                f2py_enabled=self._format.f2py_enabled,
            ):
                return self.get_single_line(ignore_empty, ignore_comments)

        if ignore_empty and not line:
            return self.get_single_line(ignore_empty, ignore_comments)

        return line

    def get_next_line(self, ignore_empty=False, ignore_comments=None):
        """Return next non-empty line from FILO line buffer or from source.

        The line will be put to FILO line buffer. So, this method can
        be used for looking forward lines without consuming them.

        See also - get_single_line, put_single_line
        """
        if ignore_comments is None:
            ignore_comments = self._ignore_comments

        line = self.get_single_line(ignore_empty, ignore_comments)
        if line is None:
            return
        self.put_single_line(line)
        return line

    # Parser methods:
    def get_item(self, ignore_comments=None):
        """Return next item."""
        if ignore_comments is None:
            ignore_comments = self._ignore_comments

        try:
            item = self.next(ignore_comments=ignore_comments)
        except StopIteration:
            return
        return item

    def put_item(self, item):
        """Insert item into FIFO buffer of 'innermost' reader object.

        :param item: the item to insert into the FIFO.
        :type item: :py:class:`fparser.common.readfortran.Line` | \
                    :py:class:`fparser.common.readfortran.MultiLine` | \
                    :py:class:`fparser.common.readfortran.Comment`
        """
        if self.reader:
            # We are reading an INCLUDE file so put this item in the FIFO
            # of the corresponding reader.
            self.reader.put_item(item)
        else:
            self.fifo_item.insert(0, item)

    # Iterator methods:

    def __iter__(self):
        """Make FortranReader an iterator."""
        return self

    def __next__(self):
        return self.next()

    def next(self, ignore_comments=None):
        """Return the next Fortran code item. Include statements are dealt
        with here.

        :param bool ignore_comments: When True then act as if Fortran \
        code does not contain any comments or blank lines. if this \
        optional arguement is not provided then use the default \
        value.

        :returns: the next line item. This can be from a local fifo \
                  buffer, from an include reader or from this reader.
        :rtype: py:class:`fparser.common.readfortran.Line`

        :raises StopIteration: if no more lines are found.
        :raises StopIteration: if a general error has occured.

        """
        if ignore_comments is None:
            ignore_comments = self._ignore_comments
        try:
            if self.reader is not None:
                # inside INCLUDE statement
                try:
                    return self.reader.next(ignore_comments)
                except StopIteration:
                    # There is nothing left in the include
                    # file. Setting reader to None indicates that
                    # we should now read from the main reader.
                    self.reader = None
            item = self._next(ignore_comments)
            if isinstance(item, Line) and _IS_INCLUDE_LINE(item.line):
                # catch INCLUDE statement and create a new FortranReader
                # to enter to included file.
                reader = item.reader
                filename = item.line.strip()[7:].lstrip()[1:-1]
                include_dirs = self.include_dirs[:]
                path = filename
                for incl_dir in include_dirs:
                    path = os.path.join(incl_dir, filename)
                    if os.path.exists(path):
                        break
                if not os.path.isfile(path):
                    # The include file does not exist in the specified
                    # locations.
                    #
                    # The Fortran standard states that an INCLUDE line
                    # is not a Fortran statement. However, fparser is
                    # a parser not a compiler and some subsequent tool
                    # might need to make use of this include so we
                    # return it and let the parser deal with it.
                    #
                    return item
                reader.info("including file %r" % (path), item)
                self.reader = FortranFileReader(
                    path, include_dirs=include_dirs, ignore_comments=ignore_comments
                )
                result = self.reader.next(ignore_comments=ignore_comments)
                return result
            return item
        except StopIteration:
            raise
        # TODO can we specify one or more specific exception types
        # rather than catching *every* exception.
        except Exception as err:
            message = self.format_message(
                "FATAL ERROR", "while processing line", self.linecount, self.linecount
            )
            logging.getLogger(__name__).critical(message)
            message = "Traceback\n" + "".join(traceback.format_stack())
            logging.getLogger(__name__).debug(message)
            logging.getLogger(__name__).debug(str(err))
            logging.getLogger(__name__).critical("STOPPED READING")
            raise StopIteration

    def _next(self, ignore_comments=None):
        """
        Return the next item from FIFO item buffer or construct
        one from source line.

        Resolves ``;`` statement terminations.

        See also - next, get_source_item

        :param bool ignore_comments: Whether or not to ignore comments \
                                     (overrides self._ignore_comments)

        :returns: the next line of Fortran.
        :rtype: :py:class:`fparser.common.readfortran.Line`

        :raises StopIteration: if no new items are found.

        """
        if ignore_comments is None:
            ignore_comments = self._ignore_comments
        fifo_item_pop = self.fifo_item.pop
        while 1:
            try:
                # first empty the FIFO item buffer:
                item = fifo_item_pop(0)
            except IndexError:
                # construct a new item from source
                item = self.get_source_item()
            if item is None:
                raise StopIteration
            if not item.isempty(ignore_comments):
                break
            # else ignore empty lines and comments by getting next line

        if not isinstance(item, Comment):
            # resolve `;` statement terminations
            if (
                not self._format.is_pyf
                and isinstance(item, Line)
                and not item.is_f2py_directive
                and ";" in item.get_line()
            ):
                # ;-separator not recognized in pyf-mode
                items = []
                # Deal with each Fortran statement separately.
                split_line_iter = iter(item.get_line().split(";"))
                first = next(split_line_iter)
                # The full line has already been processed as a Line
                # object in 'item' (and may therefore have label
                # and/or construct name properties extracted from the
                # start of the line). The simplest way to avoid losing
                # any label or construct name properties for the first
                # statement is to copy the 'item' object and update it
                # so that it only includes text for the first
                # statement (rather than the full line). Subsequent
                # statements need to be processed into Line
                # objects.
                items.append(item.copy(first.strip(), apply_map=True))
                for line in split_line_iter:
                    # Any subsequent statements have not been processed
                    # before, so new Line objects need to be created.
                    line = line.strip()
                    if line:
                        # The statement might have a label and/or construct
                        # name.
                        label, line = extract_label(line)
                        name, line = extract_construct_name(line)
                        # Create a new Line object and append to items
                        # using the existing span (line numbers) and
                        # reader.
                        new_line = Line(
                            item.apply_map(line), item.span, label, name, item.reader
                        )
                        items.append(new_line)
                items.reverse()
                for newitem in items:
                    self.fifo_item.insert(0, newitem)
                return fifo_item_pop(0)
        return item

    # Interface to returned items:

    def line_item(self, line, startlineno, endlineno, label, name, errmessage=None):
        """Construct Line item."""
        if errmessage is None:
            return Line(line, (startlineno, endlineno), label, name, self)
        return SyntaxErrorLine(
            line, (startlineno, endlineno), label, name, self, errmessage
        )

    def multiline_item(
        self, prefix, lines, suffix, startlineno, endlineno, errmessage=None
    ):
        """Construct MultiLine item."""
        if errmessage is None:
            return MultiLine(prefix, lines, suffix, (startlineno, endlineno), self)
        return SyntaxErrorMultiLine(
            prefix, lines, suffix, (startlineno, endlineno), self, errmessage
        )

    def comment_item(self, comment, startlineno, endlineno):
        """Construct Comment item."""
        return Comment(comment, (startlineno, endlineno), self)

    def cpp_directive_item(self, line, startlineno, endlineno):
        """
        Construct :py:class:`fparser.common.readfortran.CppDirective` item.

        :param str line: string containing the text of a single or \
                         multi-line preprocessor directive.
        :param int startlineno: start line number of the directive from \
                                the input source.
        :param int endlineno: end line number of the directive from \
                              the input source.

        """
        return CppDirective(line, (startlineno, endlineno), self)

    # For handling messages:

    def format_message(
        self, kind, message, startlineno, endlineno, startcolno=0, endcolno=-1
    ):
        """
        Prepares a string for logging.
        """
        back_index = {"warning": 2, "error": 3, "info": 0}.get(kind.lower(), 3)
        r = ["While processing %r (mode=%r).." % (self.id, self._format.mode)]
        for i in range(max(1, startlineno - back_index), startlineno):
            r.append("%5d:%s" % (i, self.source_lines[i - 1]))
        for i in range(
            startlineno, min(endlineno + back_index, len(self.source_lines)) + 1
        ):
            if i == 0 and not self.source_lines:
                break
            linenostr = "%5d:" % (i)
            if i == endlineno:
                sourceline = self.source_lines[i - 1]
                l0 = linenostr + sourceline[:startcolno]
                if endcolno == -1:
                    l1 = sourceline[startcolno:]
                    l2 = ""
                else:
                    l1 = sourceline[startcolno:endcolno]
                    l2 = sourceline[endcolno:]
                r.append("%s%s%s <== %s" % (l0, l1, l2, message))
            else:
                r.append(linenostr + self.source_lines[i - 1])
        return "\n".join(r)

    def format_error_message(
        self, message, startlineno, endlineno, startcolno=0, endcolno=-1
    ):
        """Create a string with an error message."""
        return self.format_message(
            "ERROR", message, startlineno, endlineno, startcolno, endcolno
        )

    def format_warning_message(
        self, message, startlineno, endlineno, startcolno=0, endcolno=-1
    ):
        """Create a string with a warning message."""
        return self.format_message(
            "WARNING", message, startlineno, endlineno, startcolno, endcolno
        )

    def info(self, message, item=None):
        """
        Logs an information message.
        """
        if item is None:
            m = self.format_message(
                "INFORMATION",
                message,
                len(self.source_lines) - 2,
                len(self.source_lines),
            )
        else:
            m = self.format_message("INFORMATION", message, item.span[0], item.span[1])
        logging.getLogger(__name__).info(m)

    def error(self, message, item=None):
        """
        Logs an error message.
        """
        if item is None:
            m = self.format_error_message(
                message, len(self.source_lines) - 2, len(self.source_lines)
            )
        else:
            m = self.format_error_message(message, item.span[0], item.span[1])
        logging.getLogger(__name__).error(m)
        if self.exit_on_error:
            sys.exit(1)

    def warning(self, message, item=None):
        """
        Logs a warning message.
        """
        if item is None:
            m = self.format_warning_message(
                message, len(self.source_lines) - 2, len(self.source_lines)
            )
        else:
            m = self.format_warning_message(message, item.span[0], item.span[1])
        logging.getLogger(__name__).warning(m)

    # Auxiliary methods for processing raw source lines:

    @staticmethod
    def replace_omp_sentinels(line, regex):
        """Checks if the specified line matches the regex, which represents
        a conditional OpenMP sentinel. If it is a match, the sentinel (which
        must be the first group in the regex) is replaced with two spaces.

        :param str line: the line to check if it contains an OpenMP sentinel
        :param regex: the compiled regular expression to use for detecting a
            conditional sentinel.
        :type regex: :py:class:`re.Pattern`

        :returns: 2-tuple consisting of the (potentially modified) line,
            and whether a sentinel was found or not.
        :rtype: tuple[str, bool]

        """
        grp = regex.match(line)
        if grp:
            # Replace the OMP sentinel with two spaces
            line = line[: grp.start(1)] + "  " + line[grp.end(1) :]
            return (line, True)
        return (line, False)

    def handle_cpp_directive(self, line):
        """
        Determine whether the current line is likely to hold
        C preprocessor directive.

        If the first non whitespace character of a line is the symbol ``#``
        it is assumed this is a preprocessor directive.

        Preprocessor directives can be used only in Fortran codes. They are
        ignored when used inside PYF files.

        The actual line content is not altered.

        :param str line: the line to be tested for directives.

        :return: a tuple containing the line and True/False depending on \
                 whether it holds a C preprocessor directive.
        :rtype: (str, bool)

        """
        if not line or self._format.is_pyf:
            return (line, False)
        return (line, line.lstrip().startswith("#"))

    def handle_cf2py_start(self, line):
        """
        Process any f2py directives contained in the supplied line. If
        support for such directives has been disabled then the line is
        returned unchanged.

        F2py directives are specified in the beginning of the line.

        f2py directives can be used only in Fortran codes.  They are
        ignored when used inside PYF files.

        :param str line: the line to check for f2py directives.

        :returns: the line with any f2py directives applied (if they are \
                  enabled in the reader).
        :rtype: str

        """
        if not line or self._format.is_pyf or not self._format.f2py_enabled:
            return line
        if self._format.is_fixed:
            if line[0] in "*cC!#":
                if line[1:5].lower() == "f2py":
                    line = 5 * " " + line[5:]
                    self.f2py_comment_lines.append(self.linecount)
            if self._format.is_f77:
                return line
        m = _CF2PY_RE.match(line)
        if m:
            newline = m.group("indent") + 5 * " " + m.group("rest")
            self.f2py_comment_lines.append(self.linecount)
            assert len(newline) == len(line), repr((newline, line))
            return newline
        return line

    def handle_inline_comment(self, line, lineno, quotechar=None):
        """
        Any in-line comment is extracted from the line. If
        keep_inline_comments==True then the extracted comments are put back
        into the fifo sequence where they will subsequently be processed as
        a comment line.

        :param str line: line of code from which to remove in-line comment
        :param int lineno: line-no. in orig. file
        :param quotechar: String to use as character-string delimiter
        :type quotechar: {None, str}

        :return: line_with_no_comments, quotechar, had_comment
        :rtype: 3-tuple of str, str, bool

        """
        had_comment = False
        if (
            quotechar is None
            and "!" not in line
            and '"' not in line
            and "'" not in line
        ):
            # There's no comment on this line
            return line, quotechar, had_comment

        idx = line.find("!")
        put_item = self.fifo_item.append
        if quotechar is None and idx != -1:
            # first try a quick method:
            newline = line[:idx]
            if '"' not in newline and "'" not in newline:
                if self.format.is_f77 or not line[idx:].startswith("!f2py"):
                    put_item(self.comment_item(line[idx:], lineno, lineno))
                    return newline, quotechar, True

        # We must allow for quotes...
        items, newquotechar = splitquote(line, quotechar)
        noncomment_items = []
        noncomment_items_append = noncomment_items.append

        commentline = None
        for idx, item in enumerate(items[:]):
            if isinstance(item, String) or "!" not in item:
                noncomment_items_append(item)
                continue
            j = item.find("!")
            noncomment_items_append(item[:j])
            items[idx] = item[j:]
            # The rest of the line must be a comment.
            commentline = "".join(items[idx:])
            # As such, any quotation marks in it can be ignored.
            newquotechar = None
            break
        if commentline is not None:
            if self._format.f2py_enabled and commentline.startswith("!f2py"):
                # go to next iteration:
                newline = "".join(noncomment_items) + commentline[5:]
                self.f2py_comment_lines.append(lineno)
                return self.handle_inline_comment(newline, lineno, quotechar)
            put_item(self.comment_item(commentline, lineno, lineno))
            had_comment = True
        return "".join(noncomment_items), newquotechar, had_comment

    def handle_multilines(self, line, startlineno, mlstr):
        '''
        Examines line for Python triple quote strings (f2py feature).

        :param str line: line of Fortran source text
        :param int startlineno: the number of the line on which this \
                                 multi-line string began.
        :param list mlstr: list of delimiters for a multi-line string \
                           (e.g. '"""')
        '''
        i = line.find(mlstr)
        if i != -1:
            prefix = line[:i]
            # skip fake multiline starts
            p, k = prefix, 0
            while p.endswith("\\"):
                p, k = p[:-1], k + 1
            if k % 2:
                return
        if i != -1 and "!" not in prefix:
            # Note character constants like 'abc"""123',
            # so multiline prefix should better not contain `'' or `"' not `!'.
            for quote in "\"'":
                if prefix.count(quote) % 2:
                    message = (
                        "multiline prefix contains odd number of"
                        + " {!r} characters".format(quote)
                    )
                    message = self.format_warning_message(
                        message, startlineno, startlineno, 0, len(prefix)
                    )
                    logging.getLogger(__name__).warning(message)

            suffix = None
            multilines = []
            line = line[i + 3 :]
            while line is not None:
                j = line.find(mlstr)
                if j != -1 and "!" not in line[:j]:
                    multilines.append(line[:j])
                    suffix = line[j + 3 :]
                    break
                multilines.append(line)
                line = self.get_single_line()
            if line is None:
                message = "multiline block never ends"
                message = self.format_error_message(
                    message, startlineno, startlineno, i
                )
                return self.multiline_item(
                    prefix, multilines, suffix, startlineno, self.linecount, message
                )
            suffix, qc, had_comment = self.handle_inline_comment(suffix, self.linecount)
            # no line continuation allowed in multiline suffix
            if qc is not None:
                message = "following character continuation: {!r}, expected None."
                message = self.format_message(
                    "ASSERTION FAILURE(pyf)",
                    message.format(qc),
                    startlineno,
                    self.linecount,
                )
                logging.getLogger(__name__).warning(message)
            # XXX: should we do line.replace('\\'+mlstr[0],mlstr[0])
            #      for line in multilines?
            return self.multiline_item(
                prefix, multilines, suffix, startlineno, self.linecount
            )

    # The main method of interpreting raw source lines within
    # the following contexts: f77, fixed, free, pyf.

    def get_source_item(self):
        """
        Return the next source item.

        A source item is:
        - a fortran line
        - a list of continued fortran lines
        - a multiline - lines inside triple-quotes, only when in ispyf mode
        - a comment line
        - a preprocessor directive line

        :returns: the next source item.
        :rtype: :py:class:`fparser.common.readfortran.Line` or \
            :py:class:`fparser.common.readfortran.MultiLine` or \
            :py:class:`fparser.common.readfortran.Comment` or \
            :py:class:`fparser.common.readfortran.CppDirective` or \
            :py:class:`fparser.common.readfortran.SyntaxErrorLine` or \
            :py:class:`fparser.common.readfortran.SyntaxErrorMultiLine`

        """
        # pylint: disable=too-many-return-statements, too-many-branches
        # pylint: disable=too-many-statements
        get_single_line = self.get_single_line
        line = get_single_line()
        if line is None:
            return None
        startlineno = self.linecount
        line, is_cpp_directive = self.handle_cpp_directive(line)
        if is_cpp_directive:
            # CPP directive line
            lines = []
            while line.rstrip().endswith("\\"):
                # Line continuation
                lines.append(line.rstrip()[:-1])
                line = get_single_line()
            lines.append(line)
            endlineno = self.linecount
            return self.cpp_directive_item("".join(lines), startlineno, endlineno)

        line = self.handle_cf2py_start(line)
        had_omp_sentinels = False
        # Free format omp sentinels need to be handled here, since a
        # continuation line can only be properly detected if there was a
        # previous non-continued conditional sentinel:
        if self._format.is_free and self._include_omp_conditional_lines:
            line, had_omp_sentinels = self.replace_omp_sentinels(
                line, self._re_omp_sentinel
            )

        is_f2py_directive = (
            self._format.f2py_enabled and startlineno in self.f2py_comment_lines
        )
        isstrict = self._format.is_strict
        have_comment = False
        label = None
        name = None

        if self._format.is_pyf:
            # handle multilines
            for mlstr in ['"""', "'''"]:
                multiline = self.handle_multilines(line, startlineno, mlstr)
                if multiline:
                    return multiline
        if self._format.is_fixed:
            if _is_fix_comment(line, isstrict, self._format.f2py_enabled):
                # comment line:
                return self.comment_item(line, startlineno, startlineno)

            for i in range(min(5, len(line))):
                # check that fixed format line starts according to Fortran
                # standard
                if line[i] not in _SPACEDIGITS:
                    message = (
                        "non-space/digit char %r found in column %i"
                        " of fixed Fortran code" % (line[i], i + 1)
                    )
                    if i == 0:
                        message += ", interpreting line as comment line"
                    if self._format.is_fix:
                        if i != 0:
                            message += ", switching to free format mode"
                        message = self.format_warning_message(
                            message, startlineno, self.linecount
                        )
                        logging.getLogger(__name__).warning(message)
                        if i == 0:
                            # non standard comment line:
                            return self.comment_item(line, startlineno, startlineno)
                        mode = fparser.common.sourceinfo.FortranFormat(True, False)
                        self.set_format(mode)
                    else:
                        message = self.format_warning_message(
                            message, startlineno, self.linecount
                        )
                        logging.getLogger(__name__).warning(message)
                        if i == 0:
                            # non standard comment line:
                            return self.comment_item(line, startlineno, startlineno)
                        # return line item with error message
                        # TODO: handle cases with line[6:]==''
                        message = self.format_error_message(
                            message, startlineno, self.linecount
                        )
                        return self.line_item(
                            line[6:], startlineno, self.linecount, label, name, message
                        )
            if self._format.is_fixed:  # Check for switched to free format
                # check for label
                s = line[:5].strip().lower()
                if s:
                    label = int(s)
                if not self._format.is_f77:
                    m = _CONSTRUCT_NAME_RE.match(line[6:])
                    if m:
                        name = m.group("name")
                        line = line[:6] + line[6:][m.end() :].lstrip()
                if not line[6:].strip():
                    # check for a blank line
                    if name is not None:
                        self.error("No construct following construct-name.")
                    elif label is not None:
                        self.warning(
                            "Label must follow nonblank character (F2008:3.2.5_2)"
                        )
                    return self.comment_item("", startlineno, self.linecount)
                # line is not a comment and the start of the line is valid

        if self._format.is_f77 and not is_f2py_directive:
            # Fortran 77 is easy..
            lines = [line[6:72]]
            # get_next_line does not actually consume lines - they are put
            # into the FILO buffer as well as being returned. This means
            # that we can ignore comments for the purposes of dealing
            # with the continued line and then handle them as though they
            # follow on after the single line constructed from the multiple
            # continued lines.
            while _is_fix_cont(
                self.get_next_line(ignore_empty=True, ignore_comments=True)
            ):
                # handle fix format line continuations for F77 code
                line = get_single_line()
                lines.append(line[6:72])
            return self.line_item(
                "".join(lines), startlineno, self.linecount, label, name
            )

        handle_inline_comment = self.handle_inline_comment

        endlineno = self.linecount
        if self._format.is_fix and not is_f2py_directive:
            # handle inline comment
            newline, qc, had_comment = handle_inline_comment(line[6:], startlineno)
            have_comment |= had_comment
            lines = [newline]
            next_line = self.get_next_line()

            while _is_fix_cont(next_line) or _is_fix_comment(
                next_line, isstrict, self._format.f2py_enabled
            ):
                # handle fix format line continuations for F90 or
                # newer code.  Mixing fix format and free format line
                # continuations is not allowed nor detected, just
                # eject warnings.
                line2 = get_single_line()  # consume next_line as line2
                if _is_fix_comment(line2, isstrict, self._format.f2py_enabled):
                    # handle fix format comments inside line continuations
                    # after the line construction
                    citem = self.comment_item(line2, self.linecount, self.linecount)
                    self.fifo_item.append(citem)
                else:
                    # line continuation
                    newline, qc, had_comment = self.handle_inline_comment(
                        line2[6:], self.linecount, qc
                    )
                    have_comment |= had_comment
                    lines.append(newline)
                    endlineno = self.linecount
                next_line = self.get_next_line()
            # no character continuation should follows now
            if qc is not None:
                message = "following character continuation: {!r}, expected None."
                message = self.format_message(
                    "ASSERTION FAILURE(fix)",
                    message.format(qc),
                    startlineno,
                    self.linecount,
                )
                logging.getLogger(__name__).warning(message)
            if len(lines) > 1:
                for i in range(len(lines)):
                    line = lines[i]
                    if line.rstrip().endswith("&"):
                        message = (
                            "free format line continuation character "
                            + "`&' detected in fix format code"
                        )
                        location = line.rfind("&") + 5
                        message = self.format_warning_message(
                            message, startlineno + i, startlineno + i, location
                        )
                        logging.getLogger(__name__).warning(message)
            return self.line_item("".join(lines), startlineno, endlineno, label, name)

        # line is free format or fixed format with f2py directive (that
        # will be interpreted as free format line).

        start_index = 0
        if self._format.is_fix:
            start_index = 6
        lines = []
        lines_append = lines.append
        put_item = self.fifo_item.append
        qchar = None
        while line is not None:
            if had_omp_sentinels:
                # In free-format we can only have a continuation line
                # if we had a omp line previously:
                line, _ = self.replace_omp_sentinels(line, self._re_omp_sentinel_cont)
            if start_index:  # fix format code
                line, qchar, had_comment = handle_inline_comment(
                    line[start_index:], self.linecount, qchar
                )
                have_comment |= had_comment
                is_f2py_directive = self.linecount in self.f2py_comment_lines
            else:
                # free format
                line_lstrip = line.lstrip()
                if lines:
                    if line_lstrip.startswith("!"):
                        # check for comment line within line continuation
                        put_item(
                            self.comment_item(
                                line_lstrip, self.linecount, self.linecount
                            )
                        )
                        have_comment = True
                        line = get_single_line()
                        continue
                    elif line_lstrip == "":
                        # skip blank lines within a line continuation
                        line = get_single_line()
                        continue
                else:
                    # Extract label and/or construct name from line if
                    # there is one.
                    label, line = extract_label(line)
                    name, line = extract_construct_name(line)

                line, qchar, had_comment = handle_inline_comment(
                    line, self.linecount, qchar
                )
                have_comment |= had_comment
                is_f2py_directive = self.linecount in self.f2py_comment_lines

            i = line.rfind("&")
            if i != -1:
                line_i1_rstrip = line[i + 1 :].rstrip()
            if not lines:
                # first line
                if i == -1 or line_i1_rstrip:
                    lines_append(line)
                    break
                endlineno = self.linecount
                lines_append(line[:i])
                line = get_single_line()
                continue
            if i == -1 or line_i1_rstrip:
                # no line continuation follows
                i = len(line)
            k = -1
            if i != -1:
                # handle the beginning of continued line
                k = line[:i].find("&")
                if k != 1 and line[:k].lstrip():
                    k = -1
            endlineno = self.linecount
            lines_append(line[k + 1 : i])
            if i == len(line):
                break
            line = get_single_line()

        if qchar is not None:
            message = "following character continuation: {!r}, expected None."
            message = self.format_message(
                "ASSERTION FAILURE(free)", message.format(qchar), startlineno, endlineno
            )
            logging.getLogger(__name__).error(message)
        line_content = "".join(lines).strip()
        if line_content:
            return self.line_item(line_content, startlineno, endlineno, label, name)
        if label is not None:
            message = "Label must follow nonblank character (F2008:3.2.5_2)"
            self.warning(message)
        if name is not None:
            self.error("No construct following construct-name.")

        # If this point is reached, the line is a comment or is
        # blank. If it is a comment, it has been pushed onto the
        # fifo_item list.
        try:
            return self.fifo_item.pop(0)
        except IndexError:
            # A blank line is represented as an empty comment
            return Comment("", (startlineno, endlineno), self)


class FortranFileReader(FortranReaderBase):
    """
    Constructs a FortranFileReader object from a file.

    :param file_candidate: A filename or file-like object.
    :param list include_dirs: Directories in which to look for inclusions.
    :param list source_only: Fortran source files to search for modules
        required by "use" statements.
    :param bool ignore_comments: Whether or not to ignore comments
    :param Optional[bool] ignore_encoding: whether or not to ignore
        Python-style encoding information (e.g. "-*- fortran -*-") when
        attempting to determine the format of the file. Default is True.
    :param Optional[bool] include_omp_conditional_lines: whether or not the
        content of a line with an OMP sentinel is parsed or not. Default is
        False (in which case it is treated as a Comment).

    For example::

        >>> from fparser.common.readfortran import FortranFileReader
        >>> import os
        >>> reader = FortranFileReader(\'myfile.f90\')

    """

    def __init__(
        self,
        file_candidate,
        include_dirs=None,
        source_only=None,
        ignore_comments=True,
        ignore_encoding=True,
        include_omp_conditional_lines=False,
    ):
        # The filename is used as a unique ID. This is then used to cache the
        # contents of the file. Obviously if the file changes content but not
        # filename, problems will ensue.
        #
        self._close_on_destruction = False
        if isinstance(file_candidate, str):
            self.id = file_candidate
            # The 'fparser-logging' handler for errors ensures that any invalid
            # characters in the input are skipped but logged.
            self.file = open(
                file_candidate, "r", encoding="UTF-8", errors="fparser-logging"
            )
            self._close_on_destruction = True
        elif hasattr(file_candidate, "read") and hasattr(
            file_candidate, "name"
        ):  # Is likely a file
            self.id = file_candidate.name
            self.file = file_candidate
        else:  # Probably not something we can deal with
            message = "FortranFileReader is used with a filename"
            message += " or file-like object."
            raise ValueError(message)
        mode = fparser.common.sourceinfo.get_source_info(
            file_candidate, ignore_encoding
        )

        super().__init__(
            self.file,
            mode,
            ignore_comments,
            include_omp_conditional_lines=include_omp_conditional_lines,
        )

        if include_dirs is None:
            self.include_dirs.insert(0, os.path.dirname(self.id))
        else:
            self.include_dirs = include_dirs[:]
        if source_only is not None:
            self.source_only = source_only[:]

    def __del__(self):
        if self._close_on_destruction:
            self.file.close()

    def close_source(self):
        self.file.close()


class FortranStringReader(FortranReaderBase):
    """
    Reads Fortran source code as a string.

    :param str string: string to read
    :param list include_dirs: List of dirs to search for include files
    :param list source_only: Fortran source files to search for modules
        required by "use" statements.
    :param bool ignore_comments: Whether or not to ignore comments
    :param Optional[bool] ignore_encoding: whether or not to ignore
        Python-style encoding information (e.g. "-*- fortran -*-") when
        attempting to determine the format of the source. Default is True.
    :param Optional[bool] include_omp_conditional_lines: whether or not
        the content of a line with an OMP sentinel is parsed or not. Default
        is False (in which case it is treated as a Comment).

    For example:

    >>> from fparser.common.readfortran import FortranStringReader
    >>> code = \'\'\'
             subroutine foo(a)
                integer a
                print*,\"a=\",a
              end
        \'\'\'
    >>> reader = FortranStringReader(code)

    """

    def __init__(
        self,
        string,
        include_dirs=None,
        source_only=None,
        ignore_comments=True,
        ignore_encoding=True,
        include_omp_conditional_lines=False,
    ):
        # The Python ID of the string was used to uniquely identify it for
        # caching purposes. Unfortunately this ID is only unique for the
        # lifetime of the string. In CPython it is the address of the string
        # and the chance of a new string being allocated to the same address
        # is actually quite high. Particularly in a unit-testing scenario.
        #
        # For this reason the hash is used instead. A much better solution
        # anyway.
        #
        self.id = "string-" + str(hash(string))
        source = StringIO(string)
        mode = fparser.common.sourceinfo.get_source_info_str(
            string, ignore_encoding=ignore_encoding
        )
        super().__init__(
            source,
            mode,
            ignore_comments,
            include_omp_conditional_lines=include_omp_conditional_lines,
        )
        if include_dirs is not None:
            self.include_dirs = include_dirs[:]
        if source_only is not None:
            self.source_only = source_only[:]
