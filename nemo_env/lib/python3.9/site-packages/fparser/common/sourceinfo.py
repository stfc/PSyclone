# Modified work Copyright (c) 2017-2023 Science and Technology
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

"""
Provides functions to determine whether a piece of Fortran source is free or
fixed format. It also tries to differentiate between strict and "pyf" although
I'm not sure what that is.

"""
import os
import re


##############################################################################


class FortranFormat:
    """
    Describes the nature of a piece of Fortran source.

    Source can be fixed or free format. It can also be "strict" or
    "not strict" although it's not entirely clear what that means. It may
    refer to the strictness of adherance to fixed format although what that
    means in the context of free format I don't know.

    :param bool is_free: True for free format, False for fixed.
    :param bool is_strict: some amount of strictness.
    :param bool enable_f2py: whether f2py directives are enabled or treated \
                             as comments (the default).
    """

    def __init__(self, is_free, is_strict, enable_f2py=False):
        if is_free is None:
            raise Exception("FortranFormat does not accept a None is_free")
        if is_strict is None:
            raise Exception("FortranFormat does not accept a None is_strict")

        self._is_free = is_free
        self._is_strict = is_strict
        self._f2py_enabled = enable_f2py

    @classmethod
    def from_mode(cls, mode):
        """
        Constructs a FortranFormat object from a mode string.

        Arguments:
            mode - (String) One of 'free', 'fix', 'f77' or 'pyf'
        """
        if mode == "free":
            is_free, is_strict = True, False
        elif mode == "fix":
            is_free, is_strict = False, False
        elif mode == "f77":
            is_free, is_strict = False, True
        elif mode == "pyf":
            is_free, is_strict = True, True
        else:
            raise NotImplementedError(repr(mode))
        return cls(is_free, is_strict)

    def __eq__(self, other):
        if isinstance(other, FortranFormat):
            return (
                self.is_free == other.is_free
                and self.is_strict == other.is_strict
                and self.f2py_enabled == other.f2py_enabled
            )
        raise NotImplementedError

    def __str__(self):
        if self.is_strict:
            string = "Strict"
        else:
            string = "Non-strict"

        if self.is_free:
            string += " free"
        else:
            string += " fixed"

        return string + " format"

    @property
    def is_free(self):
        """
        Returns true for free format.
        """
        return self._is_free

    @property
    def is_fixed(self):
        """
        Returns true for fixed format.
        """
        return not self._is_free

    @property
    def is_strict(self):
        """
        Returns true for strict format.
        """
        return self._is_strict

    @property
    def is_f77(self):
        """
        Returns true for strict fixed format.
        """
        return not self._is_free and self._is_strict

    @property
    def is_fix(self):
        """
        Returns true for slack fixed format.
        """
        return not self._is_free and not self._is_strict

    @property
    def is_pyf(self):
        """
        Returns true for strict free format.
        """
        return self._is_free and self._is_strict

    @property
    def f2py_enabled(self):
        """
        :returns: whether or not f2py directives are enabled.
        :rtype: bool
        """
        return self._f2py_enabled

    @property
    def mode(self):
        """
        Returns a string representing this format.
        """
        if self._is_free and self._is_strict:
            mode = "pyf"
        elif self._is_free:
            mode = "free"
        elif self.is_fix:
            mode = "fix"
        elif self.is_f77:
            mode = "f77"
        # While mode is determined by is_free and is_strict all permutations
        # are covered. There is no need for a final "else" clause as the
        # object cannot get wedged in an invalid mode.
        return mode


##############################################################################

_HAS_F_EXTENSION = re.compile(r".*[.](for|ftn|f77|f)\Z", re.I).match

_HAS_F_HEADER = re.compile(r"-[*]-\s*(fortran|f77)\s*-[*]-", re.I).search
_HAS_F90_HEADER = re.compile(r"-[*]-\s*f90\s*-[*]-", re.I).search
_HAS_F03_HEADER = re.compile(r"-[*]-\s*f03\s*-[*]-", re.I).search
_HAS_F08_HEADER = re.compile(r"-[*]-\s*f08\s*-[*]-", re.I).search
_HAS_FREE_HEADER = re.compile(r"-[*]-\s*(f90|f95|f03|f08)\s*-[*]-", re.I).search
_HAS_FIX_HEADER = re.compile(r"-[*]-\s*fix\s*-[*]-", re.I).search
_HAS_PYF_HEADER = re.compile(r"-[*]-\s*pyf\s*-[*]-", re.I).search

_FREE_FORMAT_START = re.compile(r"[^c*!]\s*[^\s\d\t]", re.I).match


def get_source_info_str(source, ignore_encoding=True):
    """
    Determines the format of Fortran source held in a string.

    :param bool ignore_encoding: whether or not to ignore any Python-style \
                                 encoding information in the first line of the file.

    :returns: a FortranFormat object.
    :rtype: :py:class:`fparser.common.sourceinfo.FortranFormat`

    """
    lines = source.splitlines()
    if not lines:
        return FortranFormat(False, False)

    if not ignore_encoding:
        # We check to see whether the file contains a comment describing its
        # encoding. This has nothing to do with the Fortran standard (see e.g.
        # https://peps.python.org/pep-0263/) and hence is not done by default.
        firstline = lines[0].lstrip()
        if _HAS_F_HEADER(firstline):
            # -*- fortran -*- implies Fortran77 so fixed format.
            return FortranFormat(False, True)
        if _HAS_FIX_HEADER(firstline):
            return FortranFormat(False, False)
        if _HAS_FREE_HEADER(firstline):
            return FortranFormat(True, False)
        if _HAS_PYF_HEADER(firstline):
            return FortranFormat(True, True)

    line_tally = 10000  # Check up to this number of non-comment lines
    is_free = False
    while line_tally > 0 and lines:
        line = lines.pop(0).rstrip()
        if line and line[0] != "!":
            line_tally -= 1
            if line[0] != "\t" and _FREE_FORMAT_START(line[:5]) or line[-1:] == "&":
                is_free = True
                break

    return FortranFormat(is_free, False)


##############################################################################


def get_source_info(file_candidate, ignore_encoding=True):
    """
    Determines the format of Fortran source held in a file.

    :param file_candidate: a filename or a file object
    :type file_candidate: str or _io.TextIOWrapper (py3)

    :returns: the Fortran format encoded as a string.
    :rtype: str

    """
    if hasattr(file_candidate, "name") and hasattr(file_candidate, "read"):
        filename = file_candidate.name

        # Under Python 3 file.name holds an integer file handle when
        # associated with a file without a name.
        if isinstance(filename, int):
            filename = None

    elif isinstance(file_candidate, str):
        filename = file_candidate
    else:
        message = "Argument must be a filename or file-like object."
        raise ValueError(message)

    if filename:
        _, ext = os.path.splitext(filename)
        if ext == ".pyf":
            return FortranFormat(True, True)

    if hasattr(file_candidate, "read"):
        # If the candidate object has a "read" method we assume it's a file
        # object.
        #
        # If it is a file object then it may be in the process of being read.
        # As such we need to take a note of the current state of the file
        # pointer so we can restore it when we've finished what we're doing.
        #
        pointer = file_candidate.tell()
        file_candidate.seek(0)
        source_info = get_source_info_str(
            file_candidate.read(), ignore_encoding=ignore_encoding
        )
        file_candidate.seek(pointer)
        return source_info

    # It isn't a file and it passed the type check above so it must be
    # a string.
    #
    # If it's a string we assume it is a filename. In which case we need
    # to open the named file so we can read it.
    #
    # It is closed on completion so as to return it to the state it was
    # found in.
    #
    # The 'fparser-logging' handler is setup in fparser/__init__.py and
    # ensures any occurrences of invalid characters are skipped and
    # logged.
    with open(
        file_candidate, "r", encoding="utf-8", errors="fparser-logging"
    ) as file_object:
        string = get_source_info_str(
            file_object.read(), ignore_encoding=ignore_encoding
        )
    return string


##############################################################################
