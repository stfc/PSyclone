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
    Module containing Fortran2008 Specification_Part constraint C1112
"""
from fparser.two.Fortran2003 import Specification_Part, Use_Stmt, Import_Stmt
from fparser.two.Fortran2008.declaration_construct_c1112 import (
    Declaration_Construct_C1112,
)
from fparser.two.Fortran2008.implicit_part_c1112 import Implicit_Part_C1112
from fparser.two.utils import BlockBase


class Specification_Part_C1112(Specification_Part):  # C1112
    """Fortran 2008 constraint C1112
    C1112 A submodule specification-part shall not contain a
    format-stmt, entry-stmt, or stmt-function-stmt.

    These statements are found in the following rule hierarchy

    format-stmt Specification_Part/implicit_part/implicit_part_stmt/format_stmt
                Specification_Part/declaration_construct/format_stmt
    entry-stmt Specification_Part/implicit_part/implicit_part_stmt/entry_stmt
               Specification_Part/declaration_construct/entry_stmt
    stmt-function-stmt Specification_Part/declaration_construct/
                       stmt-function-stmt

    Therefore we need to specialise implicit_part, implicit_part_stmt
    and declaration_construct

    """

    use_names = [
        "Use_Stmt",
        "Import_Stmt",
        "Implicit_Part_C1112",
        "Declaration_Construct_C1112",
    ]

    @staticmethod
    def match(reader):
        """Check whether the input matches the rule

        param reader: the fortran file reader containing the line(s)
                      of code that we are trying to match
        :type reader: :py:class:`fparser.common.readfortran.FortranFileReader` \
            | :py:class:`fparser.common.readfortran.FortranStringReader`

        :returns: `tuple` containing a single `list` which contains
                 instance of the classes that have matched if there is
                 a match or `None` if there is no match
        :rtype: Optional[Tuple[List[:py:class:`fparser.two.utils.Base`]]]
        """
        return BlockBase.match(
            None,
            [Use_Stmt, Import_Stmt, Implicit_Part_C1112, Declaration_Construct_C1112],
            None,
            reader,
        )
