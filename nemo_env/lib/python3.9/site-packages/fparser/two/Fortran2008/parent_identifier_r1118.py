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
    Module containing Fortran2008 Parent_Identifier rule R1118
"""
from fparser.two.utils import Base


class Parent_Identifier(Base):  # R1118 (C1113)
    """Fortran 2008 rule R1118
    parent-identifier is ancestor-module-name [ : parent-submodule-name ]

    C1113 The ancestor-module-name shall be the name of a nonintrinsic
    module; the parent-submodule name shall be the name of a
    descendant of that module.
    This constraint can not be tested by fparser in general as the
    module or submodule may be in a different file. We therefore do
    not check this constraint in fparser.

    """

    use_names = ["Ancestor_Module_Name", "Parent_SubModule_Name"]

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
        from fparser.two.Fortran2008 import Ancestor_Module_Name, Parent_SubModule_Name

        split_string = fstring.split(":")
        len_split_string = len(split_string)
        lhs_name = split_string[0].lstrip().rstrip()
        if len_split_string == 1:
            return Ancestor_Module_Name(lhs_name), None
        if len_split_string == 2:
            rhs_name = split_string[1].lstrip().rstrip()
            return Ancestor_Module_Name(lhs_name), Parent_SubModule_Name(rhs_name)
        # we expect at most one ':' in our input so the match fails
        return None

    def tostr(self):
        """return the fortran representation of this object"""
        # return self.string  # this returns the original code
        if self.items[1]:
            return f"{self.items[0]}:{self.items[1]}"
        return str(self.items[0])
