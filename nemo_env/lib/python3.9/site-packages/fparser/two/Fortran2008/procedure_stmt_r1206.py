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
    Module containing Fortran2008 Procedure_Stmt rule R1206
"""
from fparser.two.Fortran2003 import Procedure_Stmt as Procedure_Stmt_2003


class Procedure_Stmt(Procedure_Stmt_2003):  # R1206
    """
    Fortran 2008 Rule 1206.

    procedure-stmt is [ MODULE ] PROCEDURE [ :: ] procedure-name-list

    """

    @staticmethod
    def match(string):
        """:param str string: Fortran code to check for a match

        :returns: 3-tuple containing a boolean indicating whether the \
            optional MODULE keyword is included, a boolean indicating \
            whether the optional '::' is included and a Procedure_Name_List \
            instance, or None if there is no match.
        :rtype: Optional[Tuple[ \
            bool, bool, \
            :py:class:`fparser.two.Fortran2003.Procedure_Name_List`]]]

        """
        # Avoid circular dependencies by importing here.
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import Procedure_Name_List

        line = string.lstrip()
        optional_module = None
        if line[:6].upper() == "MODULE":
            line = line[6:].lstrip()
            optional_module = "MODULE"
        if line[:9].upper() != "PROCEDURE":
            return None
        line = line[9:].lstrip()
        optional_colons = None
        if line[:2] == "::":
            line = line[2:].lstrip()
            optional_colons = "::"
        return (Procedure_Name_List(line), optional_module, optional_colons)

    def tostr(self):
        """
        :returns: the string representation of this node.
        :rtype: str
        """
        result = "PROCEDURE"
        if self.items[1]:
            result = f"MODULE {result}"
        if self.items[2]:
            result = f"{result} ::"
        return f"{result} {self.items[0]}"
