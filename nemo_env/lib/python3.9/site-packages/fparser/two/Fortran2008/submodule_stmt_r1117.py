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
    Module containing Fortran2008 Submodule_Stmt rule R1117
"""
from fparser.two.Fortran2008.parent_identifier_r1118 import Parent_Identifier
from fparser.two.utils import Base, ScopingRegionMixin


class Submodule_Stmt(Base, ScopingRegionMixin):  # R1117
    """
    Fortran 2008 rule R1117::

        submodule-stmt is SUBMODULE ( parent-identifier ) submodule-name

    """

    subclass_names = []
    use_names = ["Submodule_Name", "Parent_Identifier"]

    @staticmethod
    def match(fstring):
        """Check whether the input matches the rule

        :param st fstring: contains the Fortran that we are trying to match.

        :returns: instances of the Classes that have matched if there is a \
                  match or `None` if there is no match.
        :rtype: Optional[Tuple[:py:class:`fparser.two.Fortran2008.Parent_Identifier`, \
                               :py:class:`fparser.two.Fortran2008.Submodule_Name`]]

        """
        # Avoid circular dependencies by importing here.
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import Submodule_Name

        # Import here as monkeypatching in tests fails if imported
        # from outer level.
        from fparser.common.splitline import splitparen

        # First look for "SUBMODULE"
        name = "SUBMODULE"
        if fstring[: len(name)].upper() != name:
            # the string does not start with SUBMODULE so does not
            # match
            return None
        # "SUBMODULE is found so strip it out and split the remaining
        # line by parenthesis
        splitline = splitparen(fstring[len(name) :].lstrip())
        # We expect 2 entries, the first being parent_identifier with
        # brackets and the second submodule_name. However for some
        # reason we get an additional empty 1st entry when using
        # splitline
        if len(splitline) != 3:
            # format is not ( parent_identifier ) submodule_name
            return None
        spurious = splitline[0]
        parent_id_brackets = splitline[1]
        submodule_name = splitline[2]
        if spurious:
            # this should be empty
            return None
        # Make sure the parent identifier contained in splitline[1] is
        # enclosed with round brackets
        if parent_id_brackets[0] != "(":
            return None
        if parent_id_brackets[-1] != ")":
            return None
        parent_id = parent_id_brackets[1:-1].lstrip().rstrip()
        # Format is OK from this Class' perspective. Pass on
        # parent_identifier and submodule name to appropriate classes
        return Parent_Identifier(parent_id), Submodule_Name(submodule_name)

    def tostr(self):
        """return the fortran representation of this object"""
        return f"SUBMODULE ({self.items[0]}) {self.items[1]}"

    def get_name(self):  # C1114
        """Fortran 2008 constraint C1114
        return the submodule name. This is used by the base class to check
        whether the submodule name matches the name used for the end
        submodule statement if one is provided.

        :return: the name of the submodule stored in a Name class
        :rtype: :py:class:`fparser.two.Fortran2003.Name`
        """
        return self.items[1]
