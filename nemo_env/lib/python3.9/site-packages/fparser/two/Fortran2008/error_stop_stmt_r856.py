# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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

"""
    Module containing Fortran2008 Error_Stop_Stmt rule R856
"""
from fparser.two.Fortran2003 import Stop_Code
from fparser.two.utils import StmtBase, WORDClsBase


class Error_Stop_Stmt(StmtBase, WORDClsBase):  # R856
    """
    Fortran 2008 rule R856
    error-stop-stmt is ERROR STOP [ stop-code ]

    """

    subclass_names = []
    use_names = ["Stop_Code"]

    @staticmethod
    def match(string):
        """Check whether the input matches the rule

        :param str string: Text that we are trying to match.

        :returns: None if there is no match or, if there is a match, a \
            2-tuple containing a string matching 'ERROR STOP' and an \
            instance of :py:class:`fparser.two.Fortran2003.Stop_Code` \
            (or None if an instance of 'Stop_Code' is not required and \
            not provided).
        :rtype: (str, :py:class:`fparser.two.Fortran2003.Stop_Code` or None) \
            or NoneType

        """
        return WORDClsBase.match("ERROR STOP", Stop_Code, string)
