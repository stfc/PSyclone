# Modified work Copyright (c) 2017-2018 Science and Technology
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
#
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

"""Public API for Fortran parser.

Module content
--------------
"""
# Author: Pearu Peterson <pearu@cens.ioc.ee>
# Created: Oct 2006


# import all Statement classes:
from fparser.common.base_classes import classes
from fparser.common.utils import AnalyzeError

__autodoc__ = ["get_reader", "parse", "walk"]


def get_reader(
    source,
    isfree=None,
    isstrict=None,
    include_dirs=None,
    source_only=None,
    ignore_comments=True,
):
    """
    Returns Fortran reader instance.

    If ``source`` is a C filename then the functions searches for comment
    lines starting with ``/*f2py`` and reads following lines as PYF file
    content until a line ``*/`` is found.

    :param str source: Specify a string or filename containing Fortran code.
    :param bool isfree: True if Fortran is free format
    :param bool isstrict: True if we are to strictly enforce free/fixed format
    :param list include_dirs: Specify a list of include directories. The
                              default list (when include_dirs=None) contains
                              the current working directory and the directory
                              of ``source``.
    :param list source_only: Specify a list of Fortran file names that are
                             searched when the ``USE`` statement is
                             encountered.
    :param bool ignore_comments: Whether or not to ignore (and discard)
                                 comments when parsing the source.

    :returns: a reader instance
    :rtype: :py:class:`fparser.common.readfortran.FortranReader`
    """
    import os
    import re
    from fparser.common.readfortran import FortranFileReader, FortranStringReader
    from fparser.common.sourceinfo import FortranFormat

    if os.path.isfile(source):
        _name, ext = os.path.splitext(source)
        if ext.lower() in [".c"]:
            # get signatures from C file comments starting with
            # `/*f2py` and ending with `*/`.
            # TODO: improve parser to take line number offset making line
            #       numbers in parser messages correct.
            f2py_c_comments = re.compile(r"/[*]\s*f2py\s.*[*]/", re.I | re.M)
            handle = open(source, "r")
            c_input = ""
            for line in f2py_c_comments.findall(handle.read()):
                c_input += line[2:-2].lstrip()[4:] + "\n"
            handle.close()
            if isfree is None:
                isfree = True
            if isstrict is None:
                isstrict = True
            return parse(c_input, isfree, isstrict, include_dirs)
        reader = FortranFileReader(
            source,
            include_dirs=include_dirs,
            source_only=source_only,
            ignore_comments=ignore_comments,
        )
    elif isinstance(source, str):
        reader = FortranStringReader(
            source,
            include_dirs=include_dirs,
            source_only=source_only,
            ignore_comments=ignore_comments,
        )
    else:
        raise TypeError("Expected string or filename input but got %s" % (type(input)))
    if isfree is None:
        isfree = reader.format.is_free
    if isstrict is None:
        isstrict = reader.format.is_strict
    reader.set_format(FortranFormat(isfree, isstrict))
    return reader


def parse(
    source,
    isfree=None,
    isstrict=None,
    include_dirs=None,
    source_only=None,
    ignore_comments=True,
    analyze=True,
    clear_cache=True,
):
    """
    Parse input and return Statement tree. Raises an AnalyzeError if the
    parser can not parse the Fortran code.

    :param str source: Specify a string or filename containing Fortran code.
    :param bool isfree: Whether the Fortran source is free-format.
    :param bool isstrict: Whether we are to strictly enforce the `isfree`
                          setting.
    :param list include_dirs: Specify a list of include directories. The
                              default list (when include_dirs=None) contains
                              the current working directory and the directory
                              of ``filename``.
    :param list source_only: A list of Fortran file names that are searched
                             when the ``USE`` statement is encountered.
    :param bool ignore_comments: When True then discard all comment lines in
                                 the Fortran code.
    :param bool analyze: When True then apply analyze() method on the Fortran
                         code tree.
    :param bool clear_cache: Whether or not to wipe the parser cache prior
                             to parsing. Necessary when a new tree object
                             is required, even if the Fortran to be parsed has
                             been seen before.

    :returns: Abstract Syntax Tree of Fortran source.
    :rtype: :py:class:`fparser.api.BeginSource`
    """
    from fparser.one.parsefortran import FortranParser

    if clear_cache:
        # Wipe the parser cache if requested
        FortranParser.cache.clear()

    reader = get_reader(
        source,
        isfree,
        isstrict,
        include_dirs,
        source_only,
        ignore_comments=ignore_comments,
    )
    parser = FortranParser(reader, ignore_comments=ignore_comments)
    try:
        parser.parse()
    except AnalyzeError:
        raise
    if analyze:
        parser.analyze()

    return parser.block


def walk(stmt, depth=-1, _initial_depth=None):
    """Generate Fortran statements by walking the stmt tree until given depth.

    For each block statement in stmt, the walk functions yields a
    tuple ``(statement, depth)`` where ``depth`` is the depth of tree
    stucture for statement.

    Parameters
    ----------
    stmt : Statement
    depth : int
      If depth is positive then walk in the tree until given depth.
      If depth is negative then walk the whole tree.

    Returns
    -------
    generator

    Examples
    --------

      ::

        from fparser import api
        source_str = '''
        subroutine foo
          integer i, r
          do i=1,100
            r = r + i
          end do
        end
        '''
        tree = api.parse(source_str)
        for stmt, depth in api.walk(tree):
            print depth, stmt.item

      that will print::

        1 line #2'subroutine foo'
        2 line #3'integer i, r'
        2 line #4'do i=1,100'
        3 line #5'r = r + i'
        2 line #6'end do'
        1 line #7'end'

    """
    if _initial_depth is None:
        if depth == 0:
            return
        _initial_depth = depth
    if not isinstance(stmt, classes.BeginSource):
        yield stmt, _initial_depth - depth
    if isinstance(stmt, classes.BeginStatement):
        last_stmt = stmt.content[-1]
        last_index = len(stmt.content)
        if isinstance(last_stmt, classes.EndStatement):
            last_index -= 1
        else:
            last_stmt = None
        if depth != 0:
            for substmt in stmt.content[:last_index]:
                for statement, statement_depth in walk(
                    substmt, depth - 1, _initial_depth
                ):
                    yield statement, statement_depth
        if last_stmt is not None:
            yield last_stmt, _initial_depth - depth
