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
    Module containing Fortran2008 End_Submodule_Stmt rule R1119
"""
from fparser.two.utils import EndStmtBase


class End_Submodule_Stmt(EndStmtBase):  # R1119
    """
    Fortran 2008 rule R1119
    end-submodule-stmt is END [ SUBMODULE [ submodule-name ] ]

    """

    subclass_names = []
    use_names = ["Submodule_Name"]

    @staticmethod
    def match(fstring):
        """Check whether the input matches the rule

        param string fstring : contains the Fortran that we are trying
        to match

        :return: instances of the Classes that have matched if there
        is a match or `None` if there is no match

        """
        # Avoid circular dependencies by importing here.
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import Submodule_Name

        return EndStmtBase.match("SUBMODULE", Submodule_Name, fstring)

    def get_name(self):  # C1114
        """Fortran 2008 constraint C1114 return the submodule name as
        specified by the end submodule statement or `None` if one is
        not specified. This is used by the base class to check whether
        this name matches the submodule name.

        :return: the name of the submodule stored in a Name class
        :return type: :py:class:`fparser.two.Fortran2003.Name` or `None`

        """
        return self.items[1]
