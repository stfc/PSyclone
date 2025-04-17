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
    Module containing Fortran2008 Critical_Construct rule R810
"""
from fparser.two.Fortran2003 import Execution_Part_Construct
from fparser.two.Fortran2008.critical_stmt_r811 import Critical_Stmt
from fparser.two.Fortran2008.end_critical_stmt_r812 import End_Critical_Stmt
from fparser.two.utils import BlockBase


class Critical_Construct(BlockBase):
    """
    Fortran 2008 Rule 810.

    critical-construct is critical-stmt
                            block
                            end-critical-stmt

    TODO: Should disallow RETURN (C809) and CYCLE or EXIT to outside block (C811)
    """

    subclass_names = []
    use_names = ["Critical_Stmt", "Execution_Part_Construct", "End_Critical_Stmt"]

    @staticmethod
    def match(reader):
        """
        Attempt to match the supplied content with this Rule.

        :param reader: the fortran file reader containing the line(s)
                      of code that we are trying to match
        :type reader: :py:class:`fparser.common.readfortran.FortranFileReader` \
            | :py:class:`fparser.common.readfortran.FortranStringReader`

        :returns: instance of class that has matched or `None` if no match.
        :rtype: :py:class:`fparser.two.utils.BlockBase` | NoneType

        """
        return BlockBase.match(
            Critical_Stmt,
            [Execution_Part_Construct],
            End_Critical_Stmt,
            reader,
            match_names=True,  # C810
            strict_match_names=True,  # C810
        )
