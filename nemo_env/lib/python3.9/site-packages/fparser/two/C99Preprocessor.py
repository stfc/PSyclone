# Copyright (c) 2020 Science and Technology Facilities Council

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

"""C99 Preprocessor Syntax Rules.
"""
# Author: Balthasar Reuter <balthasar.reuter@ecmwf.int>
# Based on previous work by Martin Schlipf (https://github.com/martin-schlipf)
# First version created: Jan 2020

import re
import sys

from fparser.common.readfortran import FortranReaderBase, CppDirective
from fparser.two import pattern_tools as pattern
from fparser.two.utils import Base, StringBase, WORDClsBase


# The list of classes that implement preprocessor directives
# This list is used in match_cpp_directive() and
# fparser.two.utils.BlockBase.match().
CPP_CLASS_NAMES = [
    "Cpp_If_Stmt",
    "Cpp_Elif_Stmt",
    "Cpp_Else_Stmt",
    "Cpp_Endif_Stmt",
    "Cpp_Include_Stmt",
    "Cpp_Macro_Stmt",
    "Cpp_Undef_Stmt",
    "Cpp_Line_Stmt",
    "Cpp_Error_Stmt",
    "Cpp_Warning_Stmt",
    "Cpp_Null_Stmt",
]


def match_cpp_directive(reader):
    """Creates single-line C99 preprocessor directive object from the
    current line, if any is found.

    :param reader: the fortran file reader containing the line \
                   of code that we are trying to match.
    :type reader: :py:class:`fparser.common.readfortran.FortranFileReader` \
                  or \
                  :py:class:`fparser.common.readfortran.FortranStringReader`

    :return: the matched preprocessor directive object or `None`.
    :rtype: one of (:py:class:`fparser.two.C99Preprocess.Cpp_*_Stmt`,) or \
            `NoneType`

    """
    # Assume we have potentially a CPP directive and only check for an
    # instance of CppDirective if reader is a subclass of FortranReaderBase
    is_potential_cpp_directive = True
    if isinstance(reader, FortranReaderBase):
        item = reader.get_item()
        is_potential_cpp_directive = isinstance(item, CppDirective)
        if item:
            reader.put_item(item)
        # Do not bail out early here to have a catch all return statement
        # at the end (that would not be reachable by tests otherwise)
    if is_potential_cpp_directive:
        for cls in CPP_CLASS_NAMES:
            obj = getattr(sys.modules[__name__], cls)(reader)
            if obj:
                return obj
    return None


#
# ISO/IEC 9899: 1999 (C99)
#

#
# 6.10 Preprocessing directives
#


class Cpp_Pp_Tokens(Base):
    """Generic class for preprocessor tokens that form the right hand
    side of a preprocessor directive (such as #error, #line, #if, etc.).
    """

    subclass_names = []

    @staticmethod
    def match(string):
        """Implements the matching for arbitrary preprocessor tokens
        that form the right hand side of a preprocessor directive. It
        does not impose any restrictions other than the string not
        being empty.

        :param str string: the string to be matched as pp-tokens.

        :return: a 1-tuple containing the matched string or None.
        :rtype: (str,) or NoneType

        """
        if not string:
            return None
        line = string.strip()
        if not line:
            return None
        return (line,)

    def tostr(self):
        """
        :return: this pp-tokens as a string.
        :rtype: str
        """
        return self.items[0]


# 6.10.1 Conditional inclusion
# Deviating from the standard's definition, not entire if-else-endif
# blocks are treated as constructs but instead the relevant preprocessor
# directives are kept as single line nodes.


class Cpp_If_Stmt(WORDClsBase):
    """
    C99 6.10.1 Conditional inclusion

    if-stmt is  # if constant-expression new-line
                or ifdef identifier new-line
                or ifndef identifier new-line

    """

    subclass_names = []
    use_names = ["Cpp_Macro_Identifier", "Cpp_Pp_Tokens"]

    _regex = re.compile(r"#\s*(ifdef|ifndef|if)\b")
    _if_pattern = pattern.Pattern("<if>", r"^\s*#\s*if\b", value="#if")
    _def_pattern = (
        pattern.Pattern("<ifdef>", r"^\s*#\s*ifdef\b", value="#ifdef"),
        pattern.Pattern("<ifndef>", r"^\s*#\s*ifndef\b", value="#ifndef"),
    )

    @staticmethod
    def match(string):
        """Implements the matching for an if preprocessor directive
        (or its variations ifdef, ifndef). For ifdef and ifndef
        statements it matches the macro identifier using
        :py:class:`fparser.two.C99Preprocesser.Cpp_Macro_Identifier`
        otherwise it uses :py:class:`fparser.two.C99Preprocessor.Cpp_Pp_Tokens`
        to accept any non-empty string as rhs.

        :param str string: the string to match with as an if statement.

        :return: a tuple of size 2 containing the statement's keyword \
                  and the right hand side, or `None` if there is no match.
        :rtype: \
          (`str`, py:class:`fparser.two.C99Preprocessor.Cpp_Macro_Identifier`)\
          or (`str`, py:class:`fparser.two.C99Preprocessor.Cpp_Pp_Tokens`) \
          or `NoneType`

        """
        if not string:
            return None
        result = WORDClsBase.match(
            Cpp_If_Stmt._if_pattern,
            Cpp_Pp_Tokens,
            string,
            colons=False,
            require_cls=True,
        )
        return result or WORDClsBase.match(
            Cpp_If_Stmt._def_pattern,
            Cpp_Macro_Identifier,
            string,
            colons=False,
            require_cls=True,
        )

    def tostr(self):
        """
        :return: this if-stmt as a string.
        :rtype: str
        """
        return "{0} {1}".format(*self.items)


class Cpp_Elif_Stmt(WORDClsBase):
    """
    C99 6.10.1 Conditional inclusion

    elif-stmt is  # elif constant-expression new-line

    """

    subclass_names = []
    use_names = ["Cpp_Pp_Tokens"]

    _pattern = pattern.Pattern("<elif-stmt>", r"^\s*#\s*elif\b", value="#elif")

    @staticmethod
    def match(string):
        """Implements the matching for an elif preprocessor directive.

        :param str string: the string to match with as an elif statement.

        :return: a tuple of size 2 containing the statements keyword and \
                  right hand side, or `None` if there is no match.
        :rtype: \
            (`str`, :py:class:`fparser.two.C99_Preprocessor.Cpp_Pp_Tokens`) \
            or `NoneType`

        """
        if not string:
            return None
        return WORDClsBase.match(
            Cpp_Elif_Stmt._pattern,
            Cpp_Pp_Tokens,
            string,
            colons=False,
            require_cls=True,
        )

    def tostr(self):
        """
        :return: this elif-stmt as a string.
        :rtype: str
        """
        return "{0} {1}".format(*self.items)


class Cpp_Else_Stmt(StringBase):
    """
    C99 6.10.1 Conditional inclusion

    else-stmt is  # else new-line

    """

    subclass_names = []

    _pattern = pattern.Pattern("<else>", r"^\s*#\s*else\b")

    @staticmethod
    def match(string):
        """Implements the matching for an else preprocessor directive.

        :param str string: the string to match with as an else statement.

        :return: a 1-tuple containing the matched string or `None` if \
                  there is no match.
        :rtype: (str,) or NoneType

        """
        if not string:
            return None
        return StringBase.match(Cpp_Else_Stmt._pattern, string)

    def tostr(self):
        """
        :return: this else-stmt as a string.
        :rtype: str
        """
        return self.string


class Cpp_Endif_Stmt(StringBase):
    """
    C99 6.10.1 Conditional inclusion

    endif-stmt is  # endif new-line

    """

    subclass_names = []

    _pattern = pattern.Pattern("<endif>", r"^\s*#\s*endif\b", value="#endif")

    @staticmethod
    def match(string):
        """Implements the matching for an endif preprocessor directive.

        :param str string: the string to match with as an endif statement.

        :return: a 1-tuple containing the matched string or `None` if \
                  there is no match.
        :rtype: (str,) or NoneType

        """
        if not string:
            return None
        return StringBase.match(Cpp_Endif_Stmt._pattern, string)

    def tostr(self):
        """
        :return: this endif-stmt as a string.
        :rtype: str
        """
        return self.string


class Cpp_Include_Stmt(Base):  # 6.10.2 Source file inclusion
    """
    C99 6.10.2 Source file inclusion

    include_stmt is # include [ <h-char-sequence>
                               or "q-char-sequence"
                               or pp-tokens ] new-line
    """

    _regex = re.compile(r"#\s*include\b")

    use_names = ["Include_Filename"]

    @staticmethod
    def match(string):
        """Implements the matching for an include statement.

        Allows for the filename to appear in double quotes or angle
        brackets. Only very loose restrictions are enforced for the
        filename, which is matched by
        py:class:`fparser.two.Fortran2003.Include_Filename`.

        :param str string: the string to match with as an include statement.

        :return: a tuple of size 1 containing a \
            py:class:`fparser.two.C99Preprocessor.Cpp_Include_Filename` \
            object with the matched filename if there is a match, \
            or `None` if there is not.
        :rtype: (:py:class:`fparser.two.Fortran2003.Include_Filename`, ) \
                or `NoneType`

        """
        from fparser.two.Fortran2003 import Include_Filename

        if not string:
            return None
        line = string.strip()
        found = Cpp_Include_Stmt._regex.match(line)
        if not found:
            # The line does not match an include statement
            return None
        rhs = line[found.end() :].strip()
        if rhs is None or len(rhs) < 3:
            # Either we didn't find any includes or the content after
            # the include token is too short to be valid (it must at
            # least contain quotes and one character.
            return None
        if not (rhs[0] == '"' and rhs[-1] == '"') and not (
            rhs[0] == "<" and rhs[-1] == ">"
        ):
            # The filename should be surrounded by double
            # quotes or '<...>' but this is not the case.
            return None
        # Remove the quotes.
        file_name = rhs[1:-1]
        # Pass the potential filename to the relevant class.
        return (Include_Filename(file_name),)

    def tostr(self):
        """
        :return: this include-stmt as a string.
        :rtype: str
        """
        return '#include "{0}"'.format(self.items[0])


class Cpp_Macro_Stmt(Base):  # 6.10.3 Macro replacement
    """
    C99 6.10.3 Macro replacement

    macro_stmt is # define identifier [( [identifier-list] ) or (...) ]
                  [ replacement-list ] new-line

    Important: No preceding whitespace is allowed for the left parenthesis of
    the optional identifier-list. If a preceding whitespace is encountered,
    the bracket is considered part of the replacement-list.
    """

    use_names = ["Cpp_Macro_Identifier", "Cpp_Macro_Identifier_List", "Cpp_Pp_Tokens"]

    _regex = re.compile(r"#\s*define\b")

    @staticmethod
    def match(string):
        """Implements the matching for a preprocessor macro definition.

        It matches define directives with macro identifier, optional
        identifier list, and optional replacement list. The macro
        identifier is matched using
        :py:class:`fparser.two.C99Preprocessor.Cpp_Macro_Identifier`
        and the optional argument identifier list using
        :py:class:`fparser.two.C99Preprocessor.Cpp_Macro_Identifier_List`.

        Important: No preceding whitespace is allowed for the left
        parentheses of the dentifier-list. If a preceding  whitespace is
        encountered, the it is considered part of the replacement-list.

        :param str string: the string to match with as an if statement.

        :return: a tuple of size 3 containing the macro identifier, \
                  identifier list or None, and replacement list or None, \
                  or `None` if there is no match.
        :rtype: \
          (py:class:`fparser.two.C99Preprocessor.Cpp_Macro_Identifier`, \
           py:class:`fparser.two.C99Preprocessor.Cpp_Macro_Identifier_List` \
           or NoneType, \
           py:class:`fparser.two.C99Preprocessor.Cpp_Pp_Tokens` or NoneType) \
          or `NoneType`

        """
        if not string:
            return None
        line = string.strip()
        found = Cpp_Macro_Stmt._regex.match(line)
        if not found:
            # The line does not match a define statement
            return None
        rhs = line[found.end() :].strip()
        found = pattern.macro_name.match(rhs)
        if not found:
            return None
        name = Cpp_Macro_Identifier(found.group())
        definition = rhs[found.end() :]
        # note no strip here because '#define MACRO(x)' and
        # '#define MACRO (x)' are functionally different
        if not definition:
            return (name, None, None)
        if definition[0] == "(":
            found = Cpp_Macro_Identifier_List._pattern.match(definition)
            if not found:
                # The definition starts with a bracket (without preceding
                # white space) but does not match an identifier list
                return None
            parameter_list = Cpp_Macro_Identifier_List(found.group())
            # Note that the definition can potentially be an empty string
            # which is nonetheless included explicitly to distinguish a
            # macro definition with arguments from one without but with
            # a definition
            definition = definition[found.end() :]
        else:
            parameter_list = None
        # now that definition only holds the replacement list, we can strip
        definition = definition.strip()
        if definition:
            definition = Cpp_Pp_Tokens(definition)
        else:
            definition = None
        return (name, parameter_list, definition)

    def tostr(self):
        """
        :return: this macro-stmt as a string.
        :rtype: str
        """
        return "#define {0}{1}{2}{3}".format(
            self.items[0],
            self.items[1] or "",
            " " if self.items[2] else "",
            self.items[2] or "",
        )


class Cpp_Macro_Identifier(StringBase):  # pylint: disable=invalid-name
    """Implements the matching of a macro identifier."""

    # There are no other classes. This is a simple string match.
    subclass_names = []

    @staticmethod
    def match(string):
        """Implements the matching of a macro identifier.

        It matches the string with the regular expression abs_macro_name
        in the pattern_tools file. The macro identifier may contain
        only letters and underscore.

        :param str string: the string to match with the pattern rule.

        :return: a tuple of size 1 containing a string with the \
                 matched name if there is a match, or None if there is not.
        :rtype: (str)  or NoneType

        """
        return StringBase.match(pattern.abs_macro_name, string.strip())


class Cpp_Macro_Identifier_List(StringBase):
    """Implements the matching of an identifier list in a macro definition.

    identifier-list is (identifier [, identifier-list or ...])
                       or (...)
    """

    subclass_names = []

    _pattern = pattern.Pattern(
        "<identifier-list>",
        r"\((\s*[A-Za-z_]\w*"
        r"(?:\s*,\s*[A-Za-z_]\w*)*"
        r"(?:\s*,\s*\.{3})?|\.{3})?\s*\)",
    )

    @staticmethod
    def match(string):
        """Implements the matching of a macro identifier list as part of
        a macro definition. It must consist of one or more macro
        identifier separated by comma, or "..." for a variadic argument
        list, and must be surrouned by parentheses.

        For simplicity, the matched list is kept as a single string and not
        matched as
        :py:class:`fparser.two.C99Preprocessor.Cpp_Macro_Identifier`.

        :param str string: the string to match with the pattern rule.

        :return: a tuple of size 1 containing a string with the \
                 matched identifier list if there is a match, or None if \
                 there is not.
        :rtype: (`str`,) or `NoneType`

        """
        if not string:
            return None
        return StringBase.match(Cpp_Macro_Identifier_List._pattern, string)

    def tostr(self):
        """
        :return: this macro-identifier-list as a string.
        :rtype: str
        """
        return self.string


class Cpp_Undef_Stmt(WORDClsBase):
    """Implements the matching of a preprocessor undef statement for a macro.

    undef-stmt is # undef identifier new-line

    Strictly, this is part of 6.10.3 but since it is identified by a different
    directive keyword (undef instead of define) we treat it separately.

    """

    subclass_names = []

    use_names = ["Cpp_Macro_Identifier"]

    _pattern = pattern.Pattern("<undef>", r"^\s*(#\s*undef)\b", value="#undef")

    @staticmethod
    def match(string):
        """Implements the matching for a preprocessor undef statement
        for a macro. The macro identifier is matched using
        :py:class:`fparser.two.C99Preprocessor.Cpp_Macro_Identifier`.

        :param str string: the string to match with as an if statement.

        :return: a tuple of size 1 containing the macro identifier, or\
                  `None` if there is no match.
        :rtype: (py:class:`fparser.two.C99Preprocessor.Cpp_Macro_Identifier`) \
                or `NoneType`

        """
        if not string:
            return None
        return WORDClsBase.match(
            Cpp_Undef_Stmt._pattern,
            Cpp_Macro_Identifier,
            string,
            colons=False,
            require_cls=True,
        )

    def tostr(self):
        """
        :return: this undef-stmt as a string.
        :rtype: str
        """
        return "{0} {1}".format(*self.items)


class Cpp_Line_Stmt(WORDClsBase):  # 6.10.4 Line control
    """
    C99 6.10.4 Line control

    line-stmt is # line digit-sequence [ "s-char-sequence" ] new-line
                        or pp-tokens new-line
    """

    subclass_names = []
    use_names = ["Cpp_Pp_Tokens"]

    _pattern = pattern.Pattern("<line>", r"^\s*#\s*line\b", value="#line")

    @staticmethod
    def match(string):
        """Implements the matching for a line preprocessor directive.
        The right hand side of the directive is not matched any further
        but simply kept as a string.

        :param str string: the string to match with as a line statement.

        :return: a tuple of size 1 with the right hand side as a string, \
                  or `None` if there is no match.
        :rtype: (`str`) or `NoneType`

        """
        if not string:
            return None
        return WORDClsBase.match(
            Cpp_Line_Stmt._pattern,
            Cpp_Pp_Tokens,
            string,
            colons=False,
            require_cls=True,
        )

    def tostr(self):
        """
        :return: this line-stmt as a string.
        :rtype: str
        """
        return "{0} {1}".format(*self.items)


class Cpp_Error_Stmt(WORDClsBase):  # 6.10.5 Error directive
    """
    C99 6.10.5 Error directive

    error-stmt is # error [pp-tokens] new-line

    """

    subclass_names = []
    use_names = ["Cpp_Pp_Tokens"]

    _pattern = pattern.Pattern("<error>", r"^\s*#\s*error\b", value="#error")

    @staticmethod
    def match(string):
        """Implements the matching for an error preprocessor directive.
        The optional right hand side of the directive is not matched any
        further but simply kept as a string.

        :param str string: the string to match with as a line statement.

        :return: an empty tuple or a tuple of size  1 with the right hand \
                  side as a string, or `None` if there is no match.
        :rtype: () or (`str`) or `NoneType`

        """
        if not string:
            return None
        return WORDClsBase.match(
            Cpp_Error_Stmt._pattern,
            Cpp_Pp_Tokens,
            string,
            colons=False,
            require_cls=False,
        )

    def tostr(self):
        """
        :return: this error-stmt as a string.
        :rtype: str
        """
        if self.items[1]:
            return "{0} {1}".format(*self.items)
        return self.items[0]


class Cpp_Warning_Stmt(WORDClsBase):
    """
    Not actually part of C99 but supported by most preprocessors and
    with syntax identical to Cpp_Error_Stmt

    warning-stmt is # warning [pp-tokens] new-line

    """

    subclass_names = []
    use_names = ["Cpp_Pp_Tokens"]

    _pattern = pattern.Pattern("<warning>", r"^\s*#\s*warning\b", value="#warning")

    @staticmethod
    def match(string):
        """Implements the matching for a warning preprocessor directive.
        The optional right hand side of the directive is not matched any
        further but simply kept as a string.

        :param str string: the string to match with as a line statement.

        :return: an empty tuple or a tuple of size  1 with the right hand \
                  side as a string, or `None` if there is no match.
        :rtype: () or (`str`) or `NoneType`

        """
        if not string:
            return None
        return WORDClsBase.match(
            Cpp_Warning_Stmt._pattern,
            Cpp_Pp_Tokens,
            string,
            colons=False,
            require_cls=False,
        )

    def tostr(self):
        """
        :return: this warning-stmt as a string.
        :rtype: str
        """
        if self.items[1]:
            return "{0} {1}".format(*self.items)
        return self.items[0]


# 6.10.6 Pragma directive
# Pragma Preprocessor directives not implemented since Fortran has its own
# Pragma syntax in the form of comments. For that reason, most preprocessors
# do not support C preprocess pragmas in Fortran code either.


class Cpp_Null_Stmt(Base):  # 6.10.7 Null directive
    """
    C99 6.10.7  Null directive

    null-stmt is # new-line

    """

    subclass_names = []

    @staticmethod
    def match(string):
        """Implements the matching for a Null (empty) directive.

        :param str string: the string to match with as a line statement.

        :return: an empty tuple or `None` if there is no match.
        :rtype: () or `NoneType`

        """
        if not string:
            return None
        line = string.strip()
        if not line == "#":
            return None
        return ()

    def tostr(self):
        """
        :return: this null-stmt as a string.
        :rtype: str
        """
        return "#"
