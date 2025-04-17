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
    Module containing Fortran2008 Open_Stmt rule R904
"""
from fparser.two.Fortran2003 import Open_Stmt as Open_Stmt_2003
from fparser.two.utils import CALLBase


class Open_Stmt(Open_Stmt_2003):  # R904
    """
    Fortran2008 Rule R904.

    open-stmt is OPEN ( connect-spec-list )

    """

    subclass_names = []
    use_names = ["Connect_Spec_List"]

    @staticmethod
    def match(string):
        """
        Attempts to match the supplied string as an Open_Stmt.

        :param str string: the string to attempt to match.

        :returns: a new Open_Stmt object if the match is successful, None otherwise.
        :rtype: Optional[:py:class:`fparser.two.Fortran2008.Open_Stmt]

        """
        # Avoid circular dependencies by importing here.
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import Connect_Spec_List

        obj = CALLBase.match("OPEN", Connect_Spec_List, string, require_rhs=True)
        if not obj:
            return None

        # Apply constraints now that we have the full Connect_Spec_List.
        have_unit = False
        have_newunit = False
        connect_specs = []
        spec_list = obj[1].children
        for spec in spec_list:
            if spec.children[0] in connect_specs:
                # C903 - no specifier can appear more than once.
                return None
            connect_specs.append(spec.children[0])
            if spec.children[0] == "UNIT":
                have_unit = True
            elif spec.children[0] == "NEWUNIT":
                have_newunit = True
            if have_unit and have_newunit:
                # C906 - cannot have both UNIT and NEWUNIT
                return None
        if not (have_unit or have_newunit):
            # C904 - a file unit number must be specified.
            return None
        return obj
