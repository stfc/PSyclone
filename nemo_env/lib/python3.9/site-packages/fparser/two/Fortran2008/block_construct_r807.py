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
    Module containing Fortran2008 Block_Construct rule R807
"""
from fparser.two.Fortran2003 import Specification_Part, Execution_Part_Construct
from fparser.two.Fortran2008.block_stmt_r808 import Block_Stmt
from fparser.two.Fortran2008.end_block_stmt_r809 import End_Block_Stmt
from fparser.two.utils import BlockBase


class Block_Construct(BlockBase):
    """
    Fortran 2008 Rule 807.

    block-construct is block-stmt
                            [ specification-part ]
                            block
                            end-block-stmt

    TODO #394: Should disallow COMMON, EQUIVALENCE, IMPLICIT, INTENT,
    NAMELIST, OPTIONAL, VALUE, and statement functions (C806) (which are all
    valid members of Specification_Part).
    """

    subclass_names = []
    use_names = [
        "Block_Stmt",
        "Specification_Part",
        "Execution_Part_Construct",
        "End_Block_Stmt",
    ]

    @staticmethod
    def match(reader):
        return BlockBase.match(
            Block_Stmt,
            [Specification_Part, Execution_Part_Construct],
            End_Block_Stmt,
            reader,
            match_names=True,  # C810
            strict_match_names=True,  # C810
        )
