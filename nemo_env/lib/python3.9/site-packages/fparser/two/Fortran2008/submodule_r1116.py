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
    Module containing Fortran2008 Submodule rule R1116
"""
from fparser.two.Fortran2003 import Module_Subprogram_Part
from fparser.two.Fortran2008.end_submodule_stmt_r1119 import End_Submodule_Stmt
from fparser.two.Fortran2008.specification_part_c1112 import Specification_Part_C1112
from fparser.two.Fortran2008.submodule_stmt_r1117 import Submodule_Stmt
from fparser.two.utils import BlockBase


class Submodule(BlockBase):  # R1116 [C1112,C1114]
    """Fortran 2008 rule R1116.

    .. code-block:: fortran

        submodule is submodule-stmt
                     [ specification-part ]
                     [ module-subprogram-part ]
                     end-submodule-stmt

    C1112 A submodule specification-part shall not contain a
    format-stmt, entry-stmt, or stmt-function-stmt.
    This constraint is handled by specialising the Specification_Part
    class.

    C1114 If a submodule-name appears in the end-submodule-stmt, it
    shall be identical to the one in the submodule-stmt.
    This constraint is handled by the Base class with the names being
    provided by the 'Submodule_Stmt and 'End_Submodule_Stmt' classes
    via a `get_name` method.

    """

    subclass_names = []
    use_names = [
        "Submodule_Stmt",
        "Specification_Part_C1112",
        "Module_Subprogram_Part",
        "End_Submodule_Stmt",
    ]

    @staticmethod
    def match(reader):
        """Check whether the input matches the rule

        param reader: the fortran file reader containing the line(s)
                      of code that we are trying to match
        :type reader: :py:class:`fparser.common.readfortran.FortranFileReader`
                      or
                      :py:class:`fparser.common.readfortran.FortranStringReader`
        :return: `tuple` containing a single `list` which contains
                 instance of the classes that have matched if there is
                 a match or `None` if there is no match

        """

        result = BlockBase.match(
            Submodule_Stmt,
            [Specification_Part_C1112, Module_Subprogram_Part],
            End_Submodule_Stmt,
            reader,
        )
        return result
